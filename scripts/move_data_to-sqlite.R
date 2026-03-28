# -------------------------------------------------------------------------
# 脚本名称：move_data_to-sqlite.R
# 功能描述：将现有的 RDS 数据迁移到 SQLite 数据库中
# -------------------------------------------------------------------------

# 1. 检查并加载必要的包
required_packages <- c("RSQLite", "dplyr", "stringr", "jsonlite", "DBI")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(RSQLite)
library(dplyr)
library(stringr)
library(jsonlite)
library(DBI)

# 2. 路径配置
# 尝试加载配置
if (file.exists("config/config.R")) {
  source("config/config.R")
  db_path <- SoyCross$config$paths$db_path
} else {
  db_path <- "data/db/soy_cross.db"
}

parent_rds <- "data/parent_table.rds"
cross_rds <- "data/cross_table.rds"

# 3. 准备工作
if (!dir.exists(dirname(db_path))) {
  dir.create(dirname(db_path), recursive = TRUE)
}

# 4. 读取源数据
message("🔄 正在读取 RDS 源文件...")

if (!file.exists(parent_rds)) stop("❌ 错误：未找到亲本文件 data/parent_table.rds")
parents_df <- readRDS(parent_rds)

# 确保有 ID
if (!"id" %in% names(parents_df)) {
  message("⚠️ 警告：亲本表缺少 ID 字段，正在自动生成...")
  parents_df$id <- sprintf("P%04d", 1:nrow(parents_df))
}

cross_df <- NULL
if (file.exists(cross_rds)) {
  cross_df <- readRDS(cross_rds)
} else {
  message("⚠️ 警告：未找到组合文件 data/cross_table.rds，将只迁移亲本数据。")
}

# 5. 连接数据库
con <- dbConnect(SQLite(), db_path)
message("✅ 已连接数据库：", db_path)

# -------------------------------------------------------------------------
# 迁移逻辑 - 亲本表 (parents)
# -------------------------------------------------------------------------
message("🔄 正在迁移亲本数据...")

# 规范化列名：保留原始中文列名作为属性，但确保核心字段存在
# 核心字段：id, name (对应 '名称'), active, created_at, updated_at
# 其他字段：转为 JSON 存入 'traits' 列，或者直接作为宽表列存储
# 鉴于 SQLite 的灵活性，我们选择将所有列写入，并添加元数据列

# 准备数据
parents_to_db <- parents_df %>%
  rename(name = 名称) %>%
  mutate(
    active = 1,
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) %>%
  select(id, name, everything()) # id 和 name 放前面

# 写入数据库
dbWriteTable(con, "parents", parents_to_db, overwrite = TRUE)
message("✅ 亲本数据迁移完成，共 ", nrow(parents_to_db), " 条记录。")

# 创建索引
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_parents_name ON parents(name)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_parents_id ON parents(id)")


# -------------------------------------------------------------------------
# 迁移逻辑 - 组合表 (crosses)
# -------------------------------------------------------------------------
if (!is.null(cross_df)) {
  message("🔄 正在迁移组合数据...")
  
  # 1. 过滤有效数据：content 不为空
  valid_crosses <- cross_df %>%
    filter(!is.na(content) & content != "")
  
  if (nrow(valid_crosses) > 0) {
    # 2. 确定 female_id 和 male_id
    # 优先使用 ma_id/pa_id，如果不存在则回退到行号映射
    
    if (all(c("ma_id", "pa_id") %in% names(valid_crosses))) {
      message("ℹ️ 使用现有的 ma_id / pa_id 字段进行关联。")
      cross_data <- valid_crosses %>%
        mutate(
          female_id = ma_id,
          male_id = pa_id
        )
    } else {
      message("⚠️ 未找到 ma_id / pa_id，尝试使用行号 (ma_row/pa_row) 进行关联...")
      # 创建行号到ID的映射
      # 注意：这里假设 parents_df 的顺序与生成 cross_table 时的顺序一致
      # 这是一个高风险假设，但在没有 ID 的情况下是唯一办法
      id_map <- parents_df$id 
      
      cross_data <- valid_crosses %>%
        mutate(
          female_id = id_map[ma_row],
          male_id = id_map[pa_row]
        )
    }
    
    # 3. 清洗与转换
    # 策略更新：采用 Event-Based 模式
    # - 仅迁移有效记录 (content非空)
    # - ID 包含 batch，允许同一亲本组合在不同批次中有多次记录 (保留历史)
    # - "唯一性检查" (即不重复配置) 将由应用层的 Plan 模块负责
    crosses_to_db <- cross_data %>%
      mutate(
        # 提取反交状态
        is_reciprocal = as.integer(str_detect(content, "反交")),
        
        # 提取纯净的批次名
        batch = str_remove(content, "反交"),
        
        # 生成唯一 ID（仅母_父，不含批次/content）
        id = paste(female_id, male_id, sep = "_"),
        
        # 组合名（用于业务展示；迁移阶段默认：female-male）
        name = paste(female_id, male_id, sep = "-"),
        
        # 杂交粒数（迁移阶段未知，置为 NA）
        seed_count = as.integer(NA),
        
        # 时间戳
        created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        
        status = "planned" # 默认状态
      ) %>%
      select(
        id,
        female_id,
        male_id,
        batch,
        name,
        seed_count,
        is_reciprocal,
        status,
        created_at,
        updated_at
      ) %>%
      # 去重：允许不同批次保留历史，但同批次同组合唯一
      distinct(female_id, male_id, batch, .keep_all = TRUE)
    
    # 4. 写入数据库
    dbWriteTable(con, "crosses", crosses_to_db, overwrite = TRUE)
    message("✅ 组合数据迁移完成，共 ", nrow(crosses_to_db), " 条记录。")
    
    # 5. 创建索引
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_crosses_female ON crosses(female_id)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_crosses_male ON crosses(male_id)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_crosses_batch ON crosses(batch)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_crosses_name ON crosses(name)")
    
  } else {
    message("ℹ️ 组合表中没有有效记录（content为空），跳过数据迁移。")
    # 创建空表结构以防万一
    dbExecute(con, "CREATE TABLE IF NOT EXISTS crosses (
      id TEXT PRIMARY KEY,
      female_id TEXT,
      male_id TEXT,
      batch TEXT,
      is_reciprocal INTEGER,
      status TEXT,
      created_at TEXT,
      updated_at TEXT
    )")
  }
}

# -------------------------------------------------------------------------
# 验证与清理
# -------------------------------------------------------------------------
message("\n================ 迁移报告 ================")
tables <- dbListTables(con)
message("数据库表清单：", paste(tables, collapse = ", "))

p_count <- dbGetQuery(con, "SELECT count(*) as n FROM parents")$n
message("Parents 表记录数：", p_count)

if ("crosses" %in% tables) {
  c_count <- dbGetQuery(con, "SELECT count(*) as n FROM crosses")$n
  message("Crosses 表记录数：", c_count)
}

dbDisconnect(con)
message("\n✅ 数据库连接已关闭。迁移脚本执行完毕。")