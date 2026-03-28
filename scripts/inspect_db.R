# -------------------------------------------------------------------------
# 脚本名称：inspect_db.R
# 功能描述：查看 soy_cross.db 数据库内容（表结构、记录数、样本数据）
# -------------------------------------------------------------------------

library(RSQLite)
library(dplyr)
library(DBI)

# 1. 连接数据库
# 尝试加载配置
if (file.exists("config/config.R")) {
  source("config/config.R")
  db_path <- SoyCross$config$paths$db_path
} else {
  db_path <- "data/db/soy_cross.db"
}

if (!file.exists(db_path)) {
  stop("❌ 数据库文件不存在：", db_path)
}

con <- dbConnect(SQLite(), db_path)
message("✅ 已连接数据库：", db_path)

# 2. 列出所有表
tables <- dbListTables(con)
message("\n📋 数据库表清单：")
print(tables)

# 3. 查看各表详情
for (tbl in tables) {
  cat(paste0("\n================ 表名：", tbl, " ================\n"))
  count <- dbGetQuery(con, paste0("SELECT count(*) as n FROM ", tbl))$n
  cat("📊 总记录数：", count, "\n")
  fields0 <- dbListFields(con, tbl)
  has_space <- grepl("\\s", fields0)
  if (any(has_space)) {
    to_rename <- fields0[has_space]
    dbBegin(con)
    for (old in to_rename) {
      new <- gsub("\\s+", "", old)
      if (!(new %in% fields0) || new == old) {
        sql <- paste0('ALTER TABLE "', tbl, '" RENAME COLUMN "', old, '" TO "', new, '"')
        dbExecute(con, sql)
        cat("🔧 已重命名字段：", old, " -> ", new, "\n")
      } else {
        cat("⚠️ 跳过重命名，存在同名字段：", old, " -> ", new, "\n")
      }
    }
    dbCommit(con)
  }
  fields <- dbListFields(con, tbl)
  cat("📝 字段列表：", paste(fields, collapse = ", "), "\n")
  if (count > 0) {
    cat("👀 前 5 条数据预览：\n")
    preview <- dbGetQuery(con, paste0("SELECT * FROM ", tbl, " LIMIT 5"))
    print(preview)
  } else {
    cat("⚠️ 表中无数据。\n")
  }
}

# 4. 特殊查询验证（如果存在 crosses 表）
if ("crosses" %in% tables) {
  cat("\n================ 🔍 杂交数据统计 (Crosses) ================\n")
  
  # 统计各状态数量
  if ("status" %in% dbListFields(con, "crosses")) {
    status_summary <- dbGetQuery(con, "SELECT status, count(*) as count FROM crosses GROUP BY status")
    cat("按状态统计：\n")
    print(status_summary)
  }
  
  # 统计各批次数量
  if ("batch" %in% dbListFields(con, "crosses")) {
    batch_summary <- dbGetQuery(con, "SELECT batch, count(*) as count FROM crosses GROUP BY batch")
    cat("\n按批次统计：\n")
    print(batch_summary)
  }
}

# 5. 断开连接
dbDisconnect(con)
message("\n✅ 检查完毕，数据库连接已关闭。")