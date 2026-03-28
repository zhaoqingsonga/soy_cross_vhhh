library(RSQLite)
library(DBI)

# 尝试加载配置
if (file.exists("config/config.R")) {
  source("config/config.R")
  db_path <- SoyCross$config$paths$db_path
} else {
  db_path <- "data/db/soy_cross.db"
}

con <- dbConnect(SQLite(), db_path)
on.exit(dbDisconnect(con), add = TRUE)

fields <- dbListFields(con, "crosses")
if (!"memo" %in% fields) {
  dbExecute(con, "ALTER TABLE crosses ADD COLUMN memo TEXT")
  message("added memo column")
} else {
  message("memo column exists")
}
