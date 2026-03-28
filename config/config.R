suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

SoyCross <- new.env(parent = emptyenv())

SoyCross$config <- list(
  paths = list(
    db_path = "data/db/soy_cross.db",
    #db_path = "z:/soy_cross.db",
    backup_dir = "data/db"
  ),
  sqlite = list(
    busy_timeout_ms = 10000,
    enable_foreign_keys = TRUE
  ),
  cross = list(
    default_batch_format = "N%y11"
  ),
  field = list(
    default_prefix = "N%y11", #默认组合前缀
    default_start_n = 1,      #起启编号
    default_digits = 3,       #编号位数
    default_place = "武汉",   #默认地点
    default_rows = 2,         #默认行数
    default_rp = 1,           #默认重复数
    default_interval = 999    #默认间隔
  ),
  cross_matrix = list(
    default_memo = "高产"     #杂交特点默认值
  ),
  analysis = list(),
  io = list(),
  safety = list()
)

SoyCross$db_connect <- function() {
  db <- dbConnect(RSQLite::SQLite(), dbname = SoyCross$config$paths$db_path)
  dbExecute(db, sprintf("PRAGMA busy_timeout = %d", SoyCross$config$sqlite$busy_timeout_ms))
  if (isTRUE(SoyCross$config$sqlite$enable_foreign_keys)) dbExecute(db, "PRAGMA foreign_keys = ON")
  db
}

SoyCross$backup_db <- function() {
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  dest <- file.path(SoyCross$config$paths$backup_dir, paste0("soy_cross_backup_", ts, ".db"))
  dir.create(SoyCross$config$paths$backup_dir, showWarnings = FALSE, recursive = TRUE)
  file.copy(SoyCross$config$paths$db_path, dest, overwrite = TRUE)
  dest
}

SoyCross$sanitize_table_columns <- function(con, tbl) {
  if (!(tbl %in% dbListTables(con))) return(invisible(FALSE))
  fields <- dbListFields(con, tbl)
  to_fix <- fields[grepl("\\s", fields)]
  if (!length(to_fix)) return(invisible(FALSE))
  dbWithTransaction(con, {
    for (old in to_fix) {
      new <- gsub("\\s+", "", old)
      if (new != old && !(new %in% fields)) {
        sql <- paste0('ALTER TABLE "', tbl, '" RENAME COLUMN "', old, '" TO "', new, '"')
        dbExecute(con, sql)
      }
    }
  })
  TRUE
}

SoyCross$get_config <- function() SoyCross$config
