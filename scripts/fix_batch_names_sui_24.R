library(RSQLite)
library(DBI)

# Database path
# 尝试加载配置
if (file.exists("config/config.R")) {
  source("config/config.R")
  db_path <- SoyCross$config$paths$db_path
} else {
  db_path <- "e:/FangCloudSync/R_WD360/Project/soy_cross_v2/data/db/soy_cross.db"
}

con <- dbConnect(RSQLite::SQLite(), db_path)

# Check for records with '濉24'
target <- "濉24"
replacement <- "N2461"

cat(sprintf("Checking for records with batch containing '%s'...\n", target))
sql_query <- sprintf("SELECT count(*) as count FROM crosses WHERE batch LIKE '%%%s%%'", target)
count_before <- dbGetQuery(con, sql_query)$count
cat(sprintf("Found %d records.\n", count_before))

if (count_before > 0) {
  # Perform update
  cat(sprintf("Replacing '%s' with '%s'...\n", target, replacement))
  sql_update <- sprintf("UPDATE crosses SET batch = REPLACE(batch, '%s', '%s') WHERE batch LIKE '%%%s%%'", target, replacement, target)
  rows_affected <- dbExecute(con, sql_update)
  cat(sprintf("Updated %d rows.\n", rows_affected))
  
  # Verify
  count_after <- dbGetQuery(con, sql_query)$count
  cat(sprintf("Remaining records with '%s': %d\n", target, count_after))
  
  # Check replacement
  sql_check <- sprintf("SELECT count(*) as count FROM crosses WHERE batch LIKE '%%%s%%'", replacement)
  count_new <- dbGetQuery(con, sql_check)$count
  cat(sprintf("Records with '%s': %d\n", replacement, count_new))
} else {
  cat("No records found to update.\n")
}

dbDisconnect(con)
