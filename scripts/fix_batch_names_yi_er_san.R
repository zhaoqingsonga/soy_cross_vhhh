
library(RSQLite)
library(DBI)

# 设置数据库路径
# 尝试加载配置
if (file.exists("config/config.R")) {
  source("config/config.R")
  db_path <- SoyCross$config$paths$db_path
} else {
  db_path <- "data/db/soy_cross.db"
}

if (!file.exists(db_path)) {
  # 尝试绝对路径
  db_path <- "e:/FangCloudSync/R_WD360/Project/soy_cross_v2/data/db/soy_cross.db"
}

if (!file.exists(db_path)) stop("❌ 数据库文件不存在")

con <- dbConnect(SQLite(), db_path)
on.exit(dbDisconnect(con))

# 待替换的字符
targets <- c("一", "二", "三")
replacement <- "N24WH"

message("🔍 开始检查并替换批次名称...")

for (t in targets) {
  # 1. 查询待修改的批次
  sql_query <- sprintf("SELECT DISTINCT batch FROM crosses WHERE batch LIKE '%%%s%%'", t)
  old_batches <- dbGetQuery(con, sql_query)
  
  if (nrow(old_batches) == 0) {
    message(sprintf("ℹ️  未发现包含 '%s' 的批次。", t))
  } else {
    message(sprintf("🔍 发现包含 '%s' 的批次：", t))
    print(old_batches$batch)
    
    # 2. 执行更新
    # 使用 SQLite 的 REPLACE 函数
    sql_update <- sprintf("UPDATE crosses SET batch = REPLACE(batch, '%s', '%s') WHERE batch LIKE '%%%s%%'", t, replacement, t)
    
    rows_affected <- dbExecute(con, sql_update)
    message(sprintf("✅ 已将 '%s' 替换为 '%s'，共更新 %d 条记录。", t, replacement, rows_affected))
  }
}

# 3. 最终验证
message("\n🔍 最终检查包含 'N24WH' 的批次：")
new_batches <- dbGetQuery(con, "SELECT DISTINCT batch FROM crosses WHERE batch LIKE '%N24WH%'")
if (nrow(new_batches) > 0) {
  print(new_batches$batch)
} else {
  message("⚠️ 未找到包含 N24WH 的批次（可能替换未生效或原本就没有数据）")
}

message("\n🎉 处理完成！")
