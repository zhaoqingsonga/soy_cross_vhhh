# =============================================================================
# 脚本名称：update_2025_names.R
# 功能描述：批量读取指定目录下的 Excel 文件，提取杂交组合名称并更新到数据库
# =============================================================================

library(readxl)
library(dplyr)
library(DBI)
library(RSQLite)

# === 1. 环境配置 ===

# 获取项目根目录 (假设脚本在项目根目录下运行，或手动指定)
project_root <- "e:/FangCloudSync/R_WD360/Project/soy_cross_v2"
db_path <- file.path(project_root, "data", "db", "soy_cross.db")
source(file.path(project_root, "R", "mod_cross.R"))

# 目标数据目录
target_dir <- file.path(project_root, "2025cross")

# === 2. 执行批量更新 ===

if (!dir.exists(target_dir)) {
  stop("❌ 目录不存在：", target_dir)
}

# 获取所有 xlsx 文件
files <- list.files(target_dir, pattern = "\\.xlsx$", full.names = TRUE)

message(sprintf("📂 找到 %d 个 Excel 文件，准备处理...", length(files)))

for (f in files) {
  message(paste0("\n📄 正在处理文件: ", basename(f)))
  
  tryCatch({
    # 1. 读取数据
    # 优先尝试读取 "planting" 表，如果不存在则读取第1个表
    sheets <- excel_sheets(f)
    if ("planting" %in% sheets) {
      df <- read_excel(f, sheet = "planting")
      message("   - 读取 'planting' 工作表")
    } else {
      df <- read_excel(f, sheet = 1)
      message("   - 未找到 'planting' 表，读取第 1 个工作表")
    }
    
    # 2. 检查必要列
    required_cols <- c("ma", "pa", "name")
    missing_cols <- setdiff(required_cols, names(df))
    
    if (length(missing_cols) > 0) {
      warning(sprintf("   ⚠️  跳过：缺少必要列 %s", paste(missing_cols, collapse = ", ")))
      next
    }
    
    # 3. 提取并清洗数据
    data_to_update <- df %>%
      select(all_of(required_cols)) %>%
      filter(!is.na(ma) & !is.na(pa) & !is.na(name)) %>%
      distinct() # 去重
    
    n_rows <- nrow(data_to_update)
    message(sprintf("   - 提取到 %d 条有效记录", n_rows))
    
    if (n_rows > 0) {
      # 3.1 预先检查未找到的亲本并打印
      con_check <- dbConnect(SQLite(), db_path)
      parents_in_db <- dbGetQuery(con_check, "SELECT name FROM parents")$name
      dbDisconnect(con_check)
      
      # 找出不在数据库中的亲本
      missing_ma <- setdiff(unique(data_to_update$ma), parents_in_db)
      missing_pa <- setdiff(unique(data_to_update$pa), parents_in_db)
      missing_all <- unique(c(missing_ma, missing_pa))
      
      if (length(missing_all) > 0) {
        message("   ⚠️  发现数据库中不存在的亲本：")
        for (p in missing_all) {
           message(sprintf("       - %s", p))
        }
      }
      
      # 交互式确认
      message("\n   ❓ 是否处理此文件？ (y/n/q) [y=是, n=跳过, q=退出脚本]")
      ans <- readline(prompt = "   > ")
      
      if (tolower(ans) == "q") {
        message("👋 用户取消，脚本停止。")
        break
      } else if (tolower(ans) == "n") {
        message("   ⏭️  已跳过该文件")
        next
      }
      
      # 4. 调用更新函数
      # batch = NULL 表示在整个数据库中搜索匹配的亲本组合进行更新
      count <- update_cross_names_from_df(
        data = data_to_update,
        batch = NULL,         
        db_path = db_path,
        is_id = FALSE         # ma, pa 是名称，需要函数内部转换为 ID
      )
      
      message(sprintf("   ✅ 文件处理完成，更新了 %d 条记录", count))
    } else {
      message("   ℹ️  无有效数据，跳过")
    }
    
  }, error = function(e) {
    message(sprintf("   ❌ 处理出错: %s", e$message))
  })
}

message("\n🎉 所有文件处理完毕！")
