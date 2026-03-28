load_batches <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (DBI::dbExistsTable(con, "crosses")) {
    batches <- DBI::dbGetQuery(con, "SELECT batch FROM crosses GROUP BY batch ORDER BY MAX(rowid) DESC")
    return(batches$batch)
  } else {
    return(character(0))
  }
}

get_named_preview <- function(batch, db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(
    con,
    "SELECT c.name, pm.name AS ma, pf.name AS pa
     FROM crosses c
     LEFT JOIN parents pf ON c.female_id = pf.id
     LEFT JOIN parents pm ON c.male_id = pm.id
     WHERE c.batch = ? AND c.name IS NOT NULL AND c.name <> ''
     ORDER BY c.name ASC",
    params = list(batch)
  )
}

get_combination_with_name <- function (mydata,
startN = 1,
order = FALSE) 
{
    # 确保 mydata 有必要的列，如果没有则尝试使用默认值
    if (!"ma" %in% names(mydata) && "female_id" %in% names(mydata)) mydata$ma <- mydata$female_id
    if (!"pa" %in% names(mydata) && "male_id" %in% names(mydata)) mydata$pa <- mydata$male_id
    
    mapa <- data.frame(
        name = as.character(mydata$name),
        ma = as.character(mydata$ma),
        pa = as.character(mydata$pa),
        mapa = paste0(as.character(mydata$ma), "/", as.character(mydata$pa)),
        memo = if("memo" %in% names(mydata)) as.character(mydata$memo) else rep("", nrow(mydata)),
        stringsAsFactors = FALSE
    )

    my_len <- length(mapa$mapa)
    user <- get_computer_nodename()
    name_path <- mapa$name
    id <- generate_id(start_num = 1, end_num = my_len)
    f <- rep(0, my_len)
    re_v <- data.frame(fieldid = NA, id = id, user = rep(user, my_len), stageid = NA, f = f, stringsAsFactors = FALSE)
    re_v <- cbind(re_v, mapa)
    re_v$stage <- "杂交"
    re_v$next_stage <- "群体"
    re_v$process <- id
    re_v$path <- name_path
    re_v$sele <- 0
    re_v$source <- NA
    re_v$former_fieldid <- NA
    re_v$former_stageid <- NA
    
    # 尝试使用外部定义的 field 变量进行列排序，如果不存在则跳过或使用默认
    if (exists("field")) {
        field_sub <- subset(field, grepl("combination", table, ignore.case = TRUE))
        if (nrow(field_sub) > 0) {
            for (col in as.character(field_sub$name)) {
                if (!col %in% names(re_v)) {
                    re_v[[col]] <- NA
                }
            }
            # 仅保留 field 中存在的列
            valid_cols <- as.character(field_sub$name)
            valid_cols <- valid_cols[valid_cols %in% names(re_v)]
            if (length(valid_cols) > 0) {
                 re_v <- re_v[valid_cols]
            }
        }
    }
    
    return(re_v)
}
