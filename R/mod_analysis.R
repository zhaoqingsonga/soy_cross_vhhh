# =============================================================================
# 模块名称：mod_analysis.R
# 功能描述：杂交组合分析模块 - 统计亲本利用与组合情况
# 创建日期：2025-12-29
# =============================================================================

library(DBI)
library(RSQLite)
library(dplyr)
library(glue)

# ---- 核心分析函数 ----

#' 分析特定亲本的配组详情
#'
#' @description
#' 查询指定亲本作为母本和父本的所有配组记录。
#' 返回包含统计摘要和详细配组名单的列表。
#'
#' @param parent_name 字符串，亲本名称
#' @param db_path 字符串，数据库路径，默认 "data/db/soy_cross.db"
#'
#' @return 列表 (List)，包含：
#'   \item{summary}{数据框，包含作为母本次数、作为父本次数、总次数}
#'   \item{as_female}{数据框，作为母本时的配组详情（含父本名称、批次、组合名）}
#'   \item{as_male}{数据框，作为父本时的配组详情（含母本名称、批次、组合名）}
#' @export
analyze_parent_details <- function(parent_name, db_path = NULL) {
  
  if (is.null(db_path)) {
    db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
  }
  
  if (missing(parent_name) || !nzchar(parent_name)) {
    stop("❌ 参数错误：parent_name 不能为空")
  }
  if (!file.exists(db_path)) {
    stop("❌ 数据库文件不存在：", db_path)
  }
  
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  # 1. 获取亲本 ID
  # 直接在 SQL 中关联查询，避免多次 R 与 DB 交互，但为了清晰，先查 ID
  parent_info <- dbGetQuery(con, "SELECT id FROM parents WHERE name = ?", params = list(parent_name))
  
  if (nrow(parent_info) == 0) {
    stop(glue("❌ 未找到名为 '{parent_name}' 的亲本"))
  }
  
  target_id <- parent_info$id[1]
  
  # 2. 查询作为母本的情况 (female_id = target_id)
  # 关联 parents 表获取父本名称
  sql_as_female <- "
    SELECT 
      c.batch,
      c.name AS cross_name,
      p.name AS father_name,
      c.male_id AS father_id,
      c.status
    FROM crosses c
    LEFT JOIN parents p ON c.male_id = p.id
    WHERE c.female_id = ? AND c.is_reciprocal = 0
    ORDER BY c.batch DESC
  "
  df_as_female <- dbGetQuery(con, sql_as_female, params = list(target_id))
  
  # 3. 查询作为父本的情况 (male_id = target_id)
  # 关联 parents 表获取母本名称
  sql_as_male <- "
    SELECT 
      c.batch,
      c.name AS cross_name,
      p.name AS mother_name,
      c.female_id AS mother_id,
      c.status
    FROM crosses c
    LEFT JOIN parents p ON c.female_id = p.id
    WHERE c.male_id = ? AND c.is_reciprocal = 0
    ORDER BY c.batch DESC
  "
  df_as_male <- dbGetQuery(con, sql_as_male, params = list(target_id))
  
  # 4. 生成汇总统计
  summary_stats <- data.frame(
    parent_name = parent_name,
    parent_id = target_id,
    count_as_female = nrow(df_as_female),
    count_as_male = nrow(df_as_male),
    total_crosses = nrow(df_as_female) + nrow(df_as_male),
    unique_partners = length(unique(c(df_as_female$father_name, df_as_male$mother_name)))
  )
  
  list(
    summary = summary_stats,
    as_female = df_as_female,
    as_male = df_as_male
  )
}

#' 统计所有亲本的利用频次
#'
#' @param db_path 数据库路径
#' @param top_n 返回前 N 个高频亲本，NULL 表示返回所有
#' @return 数据框
#' @export
get_parent_usage_stats <- function(db_path = NULL, top_n = NULL) {
  
  if (is.null(db_path)) {
    db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
  }
  
  if (!file.exists(db_path)) stop("❌ 数据库不存在")
  
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  # 1. 获取所有正交组合及亲本名称
  sql <- "
    SELECT 
      c.female_id, 
      pf.name as female_name,
      c.male_id, 
      pm.name as male_name
    FROM crosses c
    LEFT JOIN parents pf ON c.female_id = pf.id
    LEFT JOIN parents pm ON c.male_id = pm.id
    WHERE c.is_reciprocal = 0
  "
  
  df <- dbGetQuery(con, sql)
  
  if (nrow(df) == 0) {
    return(data.frame(
      name = character(), 
      id = character(), 
      usage_female = integer(), 
      usage_male = integer(), 
      usage_total = integer(),
      males = character(),
      females = character()
    ))
  }
  
  # 2. 统计作为母本的数据 (配偶是父本)
  stats_female <- df %>%
    group_by(id = female_id) %>%
    summarise(
      name = first(female_name),
      usage_female = n(),
      males = paste(unique(na.omit(male_name)), collapse = "、")
    )
  
  # 3. 统计作为父本的数据 (配偶是母本)
  stats_male <- df %>%
    group_by(id = male_id) %>%
    summarise(
      name = first(male_name),
      usage_male = n(),
      females = paste(unique(na.omit(female_name)), collapse = "、")
    )
  
  # 4. 合并统计
  final_stats <- full_join(stats_female, stats_male, by = "id") %>%
    mutate(
      name = coalesce(name.x, name.y),
      usage_female = coalesce(usage_female, 0L),
      usage_male = coalesce(usage_male, 0L),
      usage_total = usage_female + usage_male,
      males = coalesce(males, ""),
      females = coalesce(females, "")
    ) %>%
    select(name, id, usage_female, usage_male, usage_total, males, females) %>%
    ungroup() %>%
    arrange(desc(usage_total))
  
  if (!is.null(top_n)) {
    final_stats <- head(final_stats, top_n)
  }
  
  return(final_stats)
}


#' 查找未使用的配组亲本
#'
#' @description
#' 找出指定亲本尚未配过的对象。
#' - 输入母本：找出所有未作为父本与其配过的活跃亲本。
#' - 输入父本：找出所有未作为母本与其配过的活跃亲本。
#'
#' @param parent_name 字符串，目标亲本名称
#' @param role 字符串，目标亲本的角色，"female" (母本) 或 "male" (父本)
#' @param db_path 数据库路径
#'
#' @return 数据框，包含未配组亲本的 ID 和 Name
#' @export
find_unused_partners <- function(parent_name, role = c("female", "male"), db_path = NULL) {
  
  if (is.null(db_path)) {
    db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
  }
  
  role <- match.arg(role)
  if (missing(parent_name) || !nzchar(parent_name)) stop("❌ 参数错误：parent_name 不能为空")
  if (!file.exists(db_path)) stop("❌ 数据库不存在")
  
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  # 1. 获取目标亲本 ID
  target_info <- dbGetQuery(con, "SELECT id FROM parents WHERE name = ?", params = list(parent_name))
  if (nrow(target_info) == 0) stop(glue("❌ 未找到亲本：{parent_name}"))
  target_id <- target_info$id[1]
  
  # 2. 获取所有活跃亲本作为候选池
  # 排除自身（自交通常不是首选，且逻辑上我们要找的是“其他”亲本）
  all_parents <- dbGetQuery(con, "SELECT id, name FROM parents WHERE active = 1 AND id != ?", params = list(target_id))
  
  # 3. 获取已配组的亲本 ID
  if (role == "female") {
    # 如果输入的是母本，我们要找没配过的父本
    # 查询：该母本的所有正交组合中的父本
    sql_used <- "SELECT DISTINCT male_id FROM crosses WHERE female_id = ? AND is_reciprocal = 0"
    used_partners <- dbGetQuery(con, sql_used, params = list(target_id))$male_id
    
  } else {
    # 如果输入的是父本，我们要找没配过的母本
    # 查询：该父本的所有正交组合中的母本
    sql_used <- "SELECT DISTINCT female_id FROM crosses WHERE male_id = ? AND is_reciprocal = 0"
    used_partners <- dbGetQuery(con, sql_used, params = list(target_id))$female_id
  }
  
  # 4. 筛选未配组亲本
  # 从候选池中剔除已配过的 ID
  unused_partners <- all_parents %>%
    filter(!id %in% used_partners)
  
  message(glue("ℹ️  亲本 '{parent_name}' ({role}) 已配对 {length(used_partners)} 个，剩余 {nrow(unused_partners)} 个可选亲本"))
  
  return(unused_partners)
}

