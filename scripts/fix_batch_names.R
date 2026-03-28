
library(RSQLite)
library(DBI)

# 尝试加载配置
if (file.exists("config/config.R")) {
  source("config/config.R")
  db_path <- SoyCross$config$paths$db_path
} else {
  db_path <- "data/db/soy_cross.db"
}

# 设置数据库路径
if (!file.exists(db_path)) {
  # 尝试绝对路径 (根据之前的环境信息)
  db_path <- "e:/FangCloudSync/R_WD360/Project/soy_cross_v2/data/db/soy_cross.db"
}

if (!file.exists(db_path)) stop("❌ 数据库文件不存在")

con <- dbConnect(SQLite(), db_path)
on.exit(dbDisconnect(con))

# 1. 查询待修改的批次
old_batches <- dbGetQuery(con, "SELECT DISTINCT batch FROM crosses WHERE batch LIKE '%转二%'")

if (nrow(old_batches) == 0) {
  message("ℹ️  未发现名称中包含 '转二' 的批次。")
} else {
  message("🔍 发现以下批次需要修改：")
  print(old_batches$batch)
  
  # 2. 执行更新
  # 使用 SQLite 的 REPLACE 函数
  sql <- "UPDATE crosses SET batch = REPLACE(batch, '转二', 'G25WH') WHERE batch LIKE '%转二%'"
  
  rows_affected <- dbExecute(con, sql)
  
  message(sprintf("\n✅ 更新完成！共影响 %d 条记录。", rows_affected))
  
  # 3. 验证结果
  new_batches <- dbGetQuery(con, "SELECT DISTINCT batch FROM crosses WHERE batch LIKE '%G25WH%'")
  message("🆕 更新后的批次名称：")
  print(new_batches$batch)
}
