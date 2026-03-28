library(DBI)
library(RSQLite)

# 尝试加载配置
if (file.exists("config/config.R")) {
  source("config/config.R")
  db_path <- SoyCross$config$paths$db_path
} else {
  db_path <- "../data/db/soy_cross.db"
}

if (!file.exists(db_path)) {
  cat("DB not found")
} else {
  con <- dbConnect(SQLite(), db_path)
  cat(paste(dbListFields(con, "parents"), collapse = "\n"))
  dbDisconnect(con)
}
