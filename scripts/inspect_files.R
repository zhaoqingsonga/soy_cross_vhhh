
library(readxl)
library(dplyr)
library(RSQLite)
library(DBI)

target_dir <- "2024cross"
files <- list.files(target_dir, pattern = "\\.xlsx$", full.names = TRUE)

message(sprintf("📂 找到 %d 个文件:", length(files)))

# 尝试加载配置
if (file.exists("config/config.R")) {
  source("config/config.R")
  db_path <- SoyCross$config$paths$db_path
} else {
  db_path <- "data/db/soy_cross.db"
}

# 连接数据库准备匹配
con <- dbConnect(SQLite(), db_path)
on.exit(dbDisconnect(con))

# 获取数据库中 N24WH 的所有组合 (ma_id, pa_id)
# 需要先获取 parents 表来转换 ID -> Name
parents <- dbGetQuery(con, "SELECT id, name FROM parents")
crosses_db <- dbGetQuery(con, "SELECT female_id, male_id, batch, rowid FROM crosses WHERE batch LIKE '%N24WH%'")

# 转换 DB 中的 ID 为 Name
crosses_db <- crosses_db %>%
  left_join(parents, by = c("female_id" = "id")) %>%
  rename(ma_name = name) %>%
  left_join(parents, by = c("male_id" = "id")) %>%
  rename(pa_name = name)

message(sprintf("📊 数据库中 N24WH 记录数: %d", nrow(crosses_db)))

for (f in files) {
  message(paste0("\n📄 读取文件: ", basename(f)))
  
  sheets <- excel_sheets(f)
  sheet <- if ("planting" %in% sheets) "planting" else 1
  
  tryCatch({
    df <- read_excel(f, sheet = sheet)
    
    if (!all(c("ma", "pa") %in% names(df))) {
      message("   ⚠️  缺少 ma/pa 列，跳过")
      next
    }
    
    df <- df %>% filter(!is.na(ma) & !is.na(pa))
    n_file <- nrow(df)
    
    # 尝试匹配数据库
    # 匹配逻辑：ma = ma_name AND pa = pa_name
    matched <- crosses_db %>%
      semi_join(df, by = c("ma_name" = "ma", "pa_name" = "pa"))
    
    n_match <- nrow(matched)
    
    message(sprintf("   - 文件记录数: %d", n_file))
    message(sprintf("   - 数据库匹配数: %d", n_match))
    
    if (n_match > 0) {
       # 看看这些匹配的记录在 DB 中的 rowid 范围
       min_rid <- min(matched$rowid)
       max_rid <- max(matched$rowid)
       message(sprintf("   - 匹配 RowID 范围: %d - %d", min_rid, max_rid))
       
       # 看看文件名是否包含 "一", "二", "三"
       fname <- basename(f)
       if (grepl("一", fname)) message("   👉 文件名包含 '一'")
       if (grepl("二", fname)) message("   👉 文件名包含 '二'")
       if (grepl("三", fname)) message("   👉 文件名包含 '三'")
    }
    
  }, error = function(e) {
    message(paste("   ❌ 读取失败:", e$message))
  })
}
