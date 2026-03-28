library(DBI)
library(RSQLite)
project_root <- getwd()
cfg_path <- if (file.exists(file.path(project_root, "inst", "config", "config.R"))) file.path(project_root, "inst", "config", "config.R") else file.path(project_root, "config", "config.R")
if (file.exists(cfg_path)) {
  source(cfg_path)
  if (exists("SoyCross$init_paths")) SoyCross$init_paths(project_root)
  if (exists("SoyCross$prompt_db_setup")) SoyCross$prompt_db_setup()
}
args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default = NULL) {
  m <- grepl(paste0("^--", name, "="), args)
  if (any(m)) sub(paste0("^--", name, "="), "", args[m][1]) else default
}
excel_path <- get_arg("excel", file.path(project_root, "data", "parents.xlsx"))
db_path <- get_arg("db", if (exists("SoyCross$config")) SoyCross$config$paths$db_path else file.path(project_root, "data", "db", "soy_cross.db"))
sheet <- get_arg("sheet", NULL)
if (file.exists(file.path(project_root, "R", "utils_io.R"))) source(file.path(project_root, "R", "utils_io.R"))
if (!file.exists(excel_path)) stop("输入文件不存在: ", excel_path)
db_path <- normalize_path(db_path, create_dir = TRUE)
if (exists("backup_db")) try(backup_db(db_path, backup_name = "parents_import"), silent = TRUE)
df <- read_table(excel_path, sheet = if (!is.null(sheet) && nzchar(sheet)) sheet else NULL)
con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
on.exit(DBI::dbDisconnect(con), add = TRUE)
DBI::dbWriteTable(con, "parents", df, overwrite = TRUE)
message("完成覆盖导入: ", nrow(df))
