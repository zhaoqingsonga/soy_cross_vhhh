
library(RSQLite)
library(DBI)
library(dplyr)

# 尝试加载配置
if (file.exists("config/config.R")) {
  source("config/config.R")
  db_path <- SoyCross$config$paths$db_path
} else {
  db_path <- "data/db/soy_cross.db"
}

if (!file.exists(db_path)) db_path <- "e:/FangCloudSync/R_WD360/Project/soy_cross_v2/data/db/soy_cross.db"

con <- dbConnect(SQLite(), db_path)
on.exit(dbDisconnect(con))

# 获取所有 batch 为 N24WH 或 转N24WH 的记录
df <- dbGetQuery(con, "SELECT rowid, * FROM crosses WHERE batch LIKE '%N24WH%'")

message(sprintf("总共找到 %d 条记录。", nrow(df)))

# 1. 检查 '转N24WH'
df_zhuan <- df[df$batch == "转N24WH", ]
message(sprintf("其中 '转N24WH' 有 %d 条 (应该是原来的 '转一')。", nrow(df_zhuan)))

# 2. 检查 'N24WH'
df_main <- df[df$batch == "N24WH", ]
message(sprintf("其中 'N24WH' 有 %d 条 (应该是原来的 一, 二, 三 混合)。", nrow(df_main)))

# 尝试通过 rowid 或 created_at 分组
if (nrow(df_main) > 0) {
  message("\n--- 分析 N24WH 的内部结构 ---")
  
  # 按 rowid 排序看看是否连续
  df_main <- df_main %>% arrange(rowid)
  
  # 简单的聚类分析：看看 rowid 是否有明显的断层
  df_main$diff <- c(1, diff(df_main$rowid))
  breaks <- which(df_main$diff > 1)
  
  message("RowID 断点位置 (前5个):")
  print(head(breaks))
  
  # 如果没有断层，或者断层很多，看看 created_at
  if ("created_at" %in% names(df_main)) {
    message("\n按 created_at 统计:")
    print(table(df_main$created_at))
  }
  
  # 看看是否能凑出 140, 140, 98 的数量
  # 假设数据是按顺序插入的：
  # 一 (140) -> 二 (140) -> 三 (98)
  # 让我们看看前 140 个，中间 140 个，最后 98 个
  
  # 看看 name 字段是否有规律
  message("\nN24WH 前 10 个名称:")
  print(head(df_main$name, 10))
  
  message("\nN24WH 中间 10 个名称 (跳过 100 个):")
  print(df_main$name[101:110])
  
  message("\nN24WH 最后 10 个名称:")
  print(tail(df_main$name, 10))
  
  # 检查名称中是否包含 "一", "二", "三"
  has_yi <- sum(grepl("一", df_main$name))
  has_er <- sum(grepl("二", df_main$name))
  has_san <- sum(grepl("三", df_main$name))
  
  message(sprintf("\n名称中包含 '一': %d", has_yi))
  message(sprintf("名称中包含 '二': %d", has_er))
  message(sprintf("名称中包含 '三': %d", has_san))
  
  if (has_yi + has_er + has_san > 0) {
    message("💡 可以通过 name 字段来恢复批次！")
  } else {
    message("⚠️ 名称中不包含批次信息，只能依赖 RowID 顺序。")
  }
}
