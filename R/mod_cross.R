# =============================================================================
# 模块名称：mod_cross.R
# 功能描述：杂交计划模块 - 管理杂交组合的创建与计划
# 创建日期：2025-12-29
# =============================================================================

library(RSQLite)
library(DBI)
library(dplyr)
library(glue)

# 加载配置
tryCatch({
  # 尝试从项目根目录加载配置
  proj_root <- getwd()
  config_path <- file.path(proj_root, "config", "config.R")
  if (file.exists(config_path)) {
    source(config_path)
  } else {
    # 如果不在项目根目录，尝试向上查找
    for (i in 1:3) {
      proj_root <- dirname(proj_root)
      config_path <- file.path(proj_root, "config", "config.R")
      if (file.exists(config_path)) {
        source(config_path)
        break
      }
    }
  }
}, error = function(e) {
  # 如果配置文件不存在，使用默认值
  message("配置文件未找到，使用默认值")
})

# -----------------------------------------------------------------------------
# 核心功能：创建杂交计划
# -----------------------------------------------------------------------------

#' 创建杂交计划
#'
#' @description
#' 根据目标批次名、母本列表和父本列表生成杂交计划。
#' 自动检查数据库中是否已存在相同组合，避免重复创建。
#' 
#' @param batch_name 字符串，批次名称（如 "2025春季批次"）
#' @param mothers 字符向量，母本名称列表或母本ID列表
#' @param fathers 字符向量，父本名称列表或父本ID列表
#' @param db_path 字符串，数据库路径，默认 "data/db/soy_cross.db"
#' @param include_reciprocal 逻辑值，是否自动添加反交，默认 TRUE
#' @param status 字符串，初始状态，默认 "planned"
#' @param use_id 逻辑值，mothers/fathers是否为ID（TRUE）还是名称（FALSE），默认FALSE
#'
#' @return 列表，包含以下元素：
#'   \item{summary}{数据框，计划摘要统计}
#'   \item{new_crosses}{数据框，新创建的杂交组合}
#'   \item{skipped}{数据框，跳过的已存在组合}
#'   \item{db_updated}{逻辑值，数据库是否已更新}
#'
#' @export
#'
#' @examples
#' # 使用亲本名称
#' result <- create_cross_plan(
#'   batch_name = "2025春季",
#'   mothers = c("中黄301", "南农66"),
#'   fathers = c("天辰6号", "油6019")
#' )
#' 
#' # 使用亲本ID
#' result <- create_cross_plan(
#'   batch_name = "2025春季",
#'   mothers = c("P0001", "P0002"),
#'   fathers = c("P0101", "P0102"),
#'   use_id = TRUE
#' )
create_cross_plan <- function(
    batch_name,
    mothers,
    fathers,
    db_path = NULL,
    include_reciprocal = TRUE,
    status = "planned",
    use_id = FALSE
) {
  
  if (is.null(db_path)) {
    db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
  }

  # === 参数验证 ===
  if (missing(batch_name) || is.null(batch_name) || !nzchar(batch_name)) {
    stop("❌ 参数错误：batch_name 不能为空")
  }
  if (missing(mothers) || length(mothers) == 0) {
    stop("❌ 参数错误：mothers 列表不能为空")
  }
  if (missing(fathers) || length(fathers) == 0) {
    stop("❌ 参数错误：fathers 列表不能为空")
  }
  if (!file.exists(db_path)) {
    stop("❌ 数据库文件不存在：", db_path)
  }
  
  # === 连接数据库 ===
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  # === 验证表是否存在 ===
  tables <- dbListTables(con)
  if (!"crosses" %in% tables) {
    stop("❌ 数据库中不存在 crosses 表")
  }
  if (!"parents" %in% tables) {
    stop("❌ 数据库中不存在 parents 表")
  }
  
  # === 转换名称为ID（如果需要）===
  if (!use_id) {
    mother_ids <- get_parent_ids_from_names(con, mothers)
    father_ids <- get_parent_ids_from_names(con, fathers)
  } else {
    mother_ids <- mothers
    father_ids <- fathers
  }
  
  # === 生成所有候选组合对 ===
  message("📋 正在生成候选组合...")
  candidates <- expand.grid(
    female_id = mother_ids,
    male_id = father_ids,
    stringsAsFactors = FALSE
  )
  
  # 过滤掉自交（母本=父本）
  candidates <- candidates %>%
    filter(female_id != male_id)
  
  total_candidates <- nrow(candidates)
  message(glue("   共生成 {total_candidates} 个候选组合对（已排除自交）"))
  
  # === 检查已存在的组合 ===
  message("🔍 正在检查数据库中已存在的组合...")
  existing_crosses <- check_existing_crosses(con, candidates)
  
  # 区分：新组合 vs 已存在
  new_crosses <- candidates %>%
    anti_join(existing_crosses, by = c("female_id", "male_id"))
  
  skipped <- candidates %>%
    semi_join(existing_crosses, by = c("female_id", "male_id"))
  
  n_new <- nrow(new_crosses)
  n_skip <- nrow(skipped)
  
  message(glue("   ✅ 新组合：{n_new} 个"))
  message(glue("   ⏭️  已存在：{n_skip} 个（跳过）"))
  
  # === 准备插入数据 ===
  db_updated <- FALSE
  inserted_data <- NULL
  
  if (n_new > 0) {
    message("💾 正在插入新组合到数据库...")
    
    # 构建插入数据
    to_insert <- new_crosses %>%
      mutate(
        id = generate_cross_id(female_id, male_id),
        batch = batch_name,
        name = paste0(female_id, "-", male_id),
        memo = NA_character_,
        seed_count = NA_integer_,
        is_reciprocal = 0L,
        status = status,
        created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ) %>%
      select(id, female_id, male_id, batch, name, memo, seed_count, 
             is_reciprocal, status, created_at, updated_at)
    
    # 插入数据库
    tryCatch({
      dbBegin(con)
      dbAppendTable(con, "crosses", to_insert)
      dbCommit(con)
      db_updated <- TRUE
      inserted_data <- to_insert
      message(glue("   ✅ 成功插入 {n_new} 条正交记录"))
    }, error = function(e) {
      dbRollback(con)
      stop("❌ 插入失败：", e$message)
    })
    
    # === 处理反交 ===
    if (include_reciprocal && n_new > 0) {
      message("🔄 正在添加反交组合...")
      
      reciprocal_data <- new_crosses %>%
        mutate(
          id = generate_cross_id(male_id, female_id),
          batch = batch_name,
          name = paste0(male_id, "-", female_id),
          memo = NA_character_,
          seed_count = NA_integer_,
          is_reciprocal = 1L,
          status = status,
          created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        ) %>%
        rename(female_id = male_id, male_id = female_id) %>%
        select(id, female_id, male_id, batch, name, memo, seed_count, 
               is_reciprocal, status, created_at, updated_at)
      
      # 检查反交是否已存在
      existing_reciprocal <- check_existing_crosses(
        con, 
        reciprocal_data %>% select(female_id, male_id)
      )
      
      reciprocal_to_insert <- reciprocal_data %>%
        anti_join(existing_reciprocal, by = c("female_id", "male_id"))
      
      n_recip <- nrow(reciprocal_to_insert)
      
      if (n_recip > 0) {
        tryCatch({
          dbBegin(con)
          dbAppendTable(con, "crosses", reciprocal_to_insert)
          dbCommit(con)
          message(glue("   ✅ 成功插入 {n_recip} 条反交记录"))
          
          # 合并到插入数据
          inserted_data <- bind_rows(inserted_data, reciprocal_to_insert)
        }, error = function(e) {
          dbRollback(con)
          warning("⚠️  反交插入失败：", e$message)
        })
      } else {
        message("   ⏭️  反交组合已全部存在，跳过")
      }
    }
  } else {
    message("ℹ️  所有组合均已存在，无需插入")
  }
  
  # === 生成摘要 ===
  summary_df <- data.frame(
    batch_name = batch_name,
    total_candidates = total_candidates,
    new_crosses = n_new,
    skipped_existing = n_skip,
    reciprocal_added = if(include_reciprocal) nrow(inserted_data) - n_new else 0L,
    db_updated = db_updated,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )
  
  # === 返回结果 ===
  result <- list(
    summary = summary_df,
    new_crosses = if(!is.null(inserted_data)) inserted_data else data.frame(),
    skipped = if(n_skip > 0) skipped else data.frame(),
    db_updated = db_updated
  )
  
  message("\n" , paste(rep("=", 60), collapse = ""))
  message("📊 计划摘要：")
  message(glue("   批次名称：{batch_name}"))
  message(glue("   候选组合：{total_candidates} 个"))
  message(glue("   新增正交：{n_new} 个"))
  message(glue("   跳过已存在：{n_skip} 个"))
  if (include_reciprocal) {
    message(glue("   新增反交：{summary_df$reciprocal_added} 个"))
  }
  message(glue("   数据库已更新：{ifelse(db_updated, '是', '否')}"))
  message(paste(rep("=", 60), collapse = ""), "\n")
  
  return(result)
}

#' 批量更新杂交组合名称
#'
#' @description
#' 根据输入的数据框批量更新 crosses 表中的 name 字段。
#' 输入的 pa (母本) 和 ma (父本) 可以是名称或 ID，函数会自动识别并转换。
#' 匹配规则：当 batch 非空时使用 batch + female_id + male_id；
#' 当 batch 为空 (NULL 或 "") 时，在全表范围按 female_id + male_id 更新。
#'
#' @param data 数据框，必须包含 ma (父本), pa (母本), name (新名称) 列
#' @param batch 字符串，目标批次名称
#' @param db_path 数据库路径
#' @param is_id 逻辑值，指定 ma/pa 是否已经是 ID。默认 FALSE（即视为名称）。
#'
#' @return 更新记录数的整数
#' @export
 

update_cross_names <- function(
  batch,
  prefix,
  start_n = 1,
  digits = 3,
  db_path = NULL
) {
  if (is.null(db_path)) {
    db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
  }
  if (missing(batch) || is.null(batch) || !nzchar(batch)) stop("批次不能为空")
  if (missing(prefix) || is.null(prefix) || !nzchar(prefix)) stop("前缀不能为空")
  if (!is_latest_batch(batch, db_path = db_path)) {
    stop("仅允许对最新生成的批次进行命名")
  }
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  # 1. 按batch找到相应的记录，筛选 is_reciprocal=0 的记录
  pos <- dbGetQuery(con, "SELECT female_id, male_id FROM crosses WHERE batch = ? AND (is_reciprocal IS NULL OR is_reciprocal = 0) ORDER BY female_id, male_id", params = list(batch))
  if (nrow(pos) == 0) stop("该批次无正交记录")
  
  current_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  sql_update <- "UPDATE crosses SET name = ?, updated_at = ? WHERE batch = ? AND female_id = ? AND male_id = ?"
  
  dbBegin(con)
  tryCatch({
    seqn <- start_n + seq_len(nrow(pos)) - 1
    for (i in seq_len(nrow(pos))) {
      # 2. 正交命名：前缀 + 位数 + F0
      name_i <- paste0(prefix, sprintf(paste0("%0", digits, "d"), seqn[i]), "F0")
      dbExecute(con, sql_update, params = list(name_i, current_time, batch, pos$female_id[i], pos$male_id[i]))
      
      # 3. 紧接着反交命名：正交名基础上 F0 前加 R
      recip_name_i <- paste0(prefix, sprintf(paste0("%0", digits, "d"), seqn[i]), "RF0")
      dbExecute(con, sql_update, params = list(recip_name_i, current_time, batch, pos$male_id[i], pos$female_id[i]))
    }
    dbCommit(con)
    message(glue::glue("✅ 批次 {batch} 的 {nrow(pos)} 条正交及其反交名称已更新"))
    
    # 4. 返回该批次所有记录
    result <- dbGetQuery(con, "SELECT * FROM crosses WHERE batch = ?", params = list(batch))
    return(result)
  }, error = function(e) {
    dbRollback(con)
    stop(e$message)
  })
}





# -----------------------------------------------------------------------------
# 辅助函数
# -----------------------------------------------------------------------------

#' 根据亲本名称获取ID
#' @keywords internal
get_parent_ids_from_names <- function(con, parent_names) {
  if (length(parent_names) == 0) {
    return(character(0))
  }
  
  # 构建查询
  placeholders <- paste(rep("?", length(parent_names)), collapse = ", ")
  sql <- glue("SELECT id, name FROM parents WHERE name IN ({placeholders})")
  
  result <- dbGetQuery(con, sql, params = as.list(parent_names))
  
  # 检查是否所有名称都找到了
  found_names <- result$name
  missing_names <- setdiff(parent_names, found_names)
  
  if (length(missing_names) > 0) {
    warning(glue("⚠️  以下亲本名称在数据库中不存在：{paste(missing_names, collapse = ', ')}"))
  }
  
  return(result$id)
}

#' 检查已存在的杂交组合
#' @keywords internal
check_existing_crosses <- function(con, candidates) {
  if (nrow(candidates) == 0) {
    return(data.frame(female_id = character(), male_id = character()))
  }
  
  # 构建临时表进行批量查询
  temp_table <- candidates
  dbWriteTable(con, "temp_candidates", temp_table, overwrite = TRUE, temporary = TRUE)
  
  sql <- "
    SELECT DISTINCT c.female_id, c.male_id
    FROM crosses c
    INNER JOIN temp_candidates t
      ON c.female_id = t.female_id AND c.male_id = t.male_id
  "
  
  existing <- dbGetQuery(con, sql)
  
  # 清理临时表
  dbRemoveTable(con, "temp_candidates")
  
  return(existing)
}

#' 生成杂交组合ID
#' @keywords internal
generate_cross_id <- function(female_id, male_id) {
  paste0(female_id, "_", male_id)
}


# -----------------------------------------------------------------------------
# 查询函数
# -----------------------------------------------------------------------------

#' 查询批次统计信息
#'
#' @description
#' 统计数据库中各批次的杂交组合数量
#'
#' @param db_path 字符串，数据库路径
#' @param batch_filter 可选，字符向量，仅统计指定批次
#'
#' @return 数据框，包含各批次的统计信息
#' @export
summarize_cross_batches_db <- function(
    db_path = NULL,
    batch_filter = NULL
) {
  if (is.null(db_path)) {
    db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
  }
  if (!file.exists(db_path)) {
    stop("❌ 数据库文件不存在：", db_path)
  }
  
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  sql <- "
    SELECT 
      batch,
      COUNT(*) as total_crosses,
      SUM(CASE WHEN is_reciprocal = 0 THEN 1 ELSE 0 END) as direct_crosses,
      SUM(CASE WHEN is_reciprocal = 1 THEN 1 ELSE 0 END) as reciprocal_crosses,
      COUNT(DISTINCT status) as status_count,
      GROUP_CONCAT(DISTINCT status) as statuses,
      MAX(updated_at) as last_updated
    FROM crosses
  "
  
  if (!is.null(batch_filter) && length(batch_filter) > 0) {
    placeholders <- paste(rep("?", length(batch_filter)), collapse = ", ")
    sql <- paste0(sql, " WHERE batch IN (", placeholders, ") GROUP BY batch ORDER BY last_updated DESC")
    result <- dbGetQuery(con, sql, params = as.list(batch_filter))
  } else {
    sql <- paste0(sql, " GROUP BY batch ORDER BY last_updated DESC")
    result <- dbGetQuery(con, sql)
  }
  
  return(result)
}


#' 获取最新批次（用于命名限制）
#' @export
get_latest_batch <- function(db_path = NULL) {
  if (is.null(db_path)) {
    db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
  }
  if (!file.exists(db_path)) stop("❌ 数据库文件不存在：", db_path)
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  sql <- "
    SELECT
      batch,
      MAX(updated_at) AS last_updated
    FROM crosses
    WHERE batch IS NOT NULL
    GROUP BY batch
    ORDER BY last_updated DESC
    LIMIT 1
  "
  res <- dbGetQuery(con, sql)
  if (nrow(res) == 0 || is.na(res$batch[1])) return("")
  as.character(res$batch[1])
}

#' 判断指定批次是否为最新批次
#' @export
is_latest_batch <- function(batch, db_path = NULL) {
  if (missing(batch) || is.null(batch) || !nzchar(batch)) stop("❌ 参数错误：batch 不能为空")
  latest <- get_latest_batch(db_path = db_path)
  nzchar(latest) && identical(trimws(as.character(batch)), trimws(latest))
}

#' 检查特定组合是否存在
#' 
#' @description
#' 检查数据库中是否存在指定的母本-父本组合
#' 
#' @param female 字符串，母本名称或ID
#' @param male 字符串，父本名称或ID
#' @param db_path 字符串，数据库路径
#' @param use_id 逻辑值，是否使用ID查询（TRUE）还是名称（FALSE）
#'
#' @return 逻辑值，TRUE表示存在，FALSE表示不存在
#' @export
has_cross <- function(
    female,
    male,
    db_path = NULL,
    use_id = FALSE
) {
  if (is.null(db_path)) {
    db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
  }
  if (!file.exists(db_path)) {
    stop("❌ 数据库文件不存在：", db_path)
  }
  
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  if (!use_id) {
    # 先转换名称为ID
    female_id <- get_parent_ids_from_names(con, female)
    male_id <- get_parent_ids_from_names(con, male)
    
    if (length(female_id) == 0 || length(male_id) == 0) {
      return(FALSE)
    }
  } else {
    female_id <- female
    male_id <- male
  }
  
  sql <- "SELECT COUNT(*) as n FROM crosses WHERE female_id = ? AND male_id = ?"
  result <- dbGetQuery(con, sql, params = list(female_id, male_id))
  
  return(result$n > 0)
}

#' 创建指定组合的杂交计划
#'
#' @description
#' 根据输入的确切组合列表（pairs）创建杂交计划。
#' 自动处理去重、反交生成和数据库插入。
#'
#' @param batch_name 批次名
#' @param pairs 数据框，必须包含 female_id 和 male_id 列
#' @param db_path 数据库路径
#' @param include_reciprocal 是否为这些组合自动生成反交
#' @param limit 可选的插入数量限制（仅针对正交）
#' @param status 初始状态
#'
#' @return 列表：summary/new_crosses/skipped/db_updated
#' @export
create_specific_cross_plan <- function(
    batch_name,
    pairs,
    db_path = NULL,
    include_reciprocal = TRUE,
    limit = NA,
    status = "planned",
    memo = ""
) {
  if (is.null(db_path)) {
    db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
  }
  if (missing(batch_name) || !nzchar(batch_name)) stop("❌ 参数错误：batch_name 不能为空")
  if (missing(pairs) || !is.data.frame(pairs)) stop("❌ 参数错误：pairs 必须是数据框")
  if (!all(c("female_id", "male_id") %in% names(pairs))) stop("❌ pairs 必须包含 female_id 和 male_id 列")
  if (!file.exists(db_path)) stop("❌ 数据库文件不存在：", db_path)

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  # 1. 预处理：去重、去自交
  candidates <- pairs %>%
    select(female_id, male_id) %>%
    distinct() %>%
    filter(female_id != male_id) %>%
    mutate(female_id = as.character(female_id), male_id = as.character(male_id)) # 确保字符型

  if (nrow(candidates) == 0) {
    return(list(summary = data.frame(batch_name, inserted_n=0, reciprocal_added=0, skipped_existing=0, db_updated=FALSE), new_crosses=data.frame(), skipped=data.frame(), db_updated=FALSE))
  }

  # 2. 检查正交已存在
  existing <- check_existing_crosses(con, candidates)
  new_candidates <- candidates %>% anti_join(existing, by = c("female_id", "male_id"))
  skipped <- candidates %>% semi_join(existing, by = c("female_id", "male_id")) %>% mutate(type="正交", reason="已存在")

  # 3. 应用 limit (仅对正交)
  if (!is.na(limit) && limit > 0) {
    to_insert <- head(new_candidates, limit)
  } else {
    to_insert <- new_candidates
  }

  # 4. 准备插入数据 (正交)
  to_insert_df <- to_insert %>% mutate(
    id = generate_cross_id(female_id, male_id),
    batch = batch_name,
    name = paste0(female_id, "-", male_id),
    memo = as.character(memo),
    seed_count = NA_integer_,
    is_reciprocal = 0L,
    status = status,
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) %>% select(id, female_id, male_id, batch, name, memo, seed_count, is_reciprocal, status, created_at, updated_at)

  # 5. 处理反交
  recip_to_insert_df <- data.frame()
  recip_skipped_df <- data.frame()
  
  if (include_reciprocal && nrow(to_insert) > 0) {
    # 基于【计划插入的正交】生成反交
    # 逻辑：只要这个正交计划被写入（或者它是新的），就应该尝试写入对应的反交
    recip_candidates <- data.frame(
      female_id = to_insert$male_id,
      male_id = to_insert$female_id,
      stringsAsFactors = FALSE
    ) %>% distinct()

    if (nrow(recip_candidates) > 0) {
      exr <- check_existing_crosses(con, recip_candidates)
      
      new_recip <- recip_candidates %>% anti_join(exr, by = c("female_id", "male_id"))
      recip_skipped_temp <- recip_candidates %>% semi_join(exr, by = c("female_id", "male_id")) %>% mutate(type="反交", reason="已存在")
      
      recip_to_insert_df <- new_recip %>% mutate(
        id = generate_cross_id(female_id, male_id),
        batch = batch_name,
        name = paste0(female_id, "-", male_id),
        memo = as.character(memo),
        seed_count = NA_integer_,
        is_reciprocal = 1L,
        status = status,
        created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ) %>% select(id, female_id, male_id, batch, name, memo, seed_count, is_reciprocal, status, created_at, updated_at)
      
      recip_skipped_df <- recip_skipped_temp
    }
  }

  # 6. 执行写入
  db_updated <- FALSE
  inserted_data <- NULL
  
  tryCatch({
    dbBegin(con)
    if (nrow(to_insert_df) > 0) dbAppendTable(con, "crosses", to_insert_df)
    if (nrow(recip_to_insert_df) > 0) dbAppendTable(con, "crosses", recip_to_insert_df)
    dbCommit(con)
    db_updated <- TRUE
    inserted_data <- bind_rows(to_insert_df, recip_to_insert_df)
  }, error = function(e) {
    dbRollback(con)
    stop("❌ 插入失败：", e$message)
  })

  # 7. 构造返回
  all_skipped <- bind_rows(skipped, recip_skipped_df)
  
  summary_df <- data.frame(
    batch_name = batch_name,
    inserted_n = nrow(to_insert_df),
    reciprocal_added = nrow(recip_to_insert_df),
    skipped_direct = nrow(skipped),
    skipped_recip = nrow(recip_skipped_df),
    total_inserted = nrow(to_insert_df) + nrow(recip_to_insert_df),
    db_updated = db_updated,
    stringsAsFactors = FALSE
  )

  list(
    summary = summary_df, 
    new_crosses = inserted_data %||% data.frame(), 
    skipped = all_skipped, 
    db_updated = db_updated
  )
}


clear_cross_batches_db <- function(
    batch_names,
    preview = TRUE,
    db_path = NULL,
    ask = TRUE
) {
  if (is.null(db_path)) {
    db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
  }
  if (missing(batch_names) || length(batch_names) == 0) stop("❌ 参数错误：batch_names 不能为空")
  if (!file.exists(db_path)) stop("❌ 数据库文件不存在：", db_path)
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  result <- data.frame(batch = character(0), n = integer(0), last_updated = character(0), first_created = character(0), stringsAsFactors = FALSE)
  for (b in batch_names) {
    info <- dbGetQuery(con, "SELECT COUNT(*) AS n, MAX(updated_at) AS last_updated, MIN(created_at) AS first_created FROM crosses WHERE batch = ?", params = list(b))
    result <- rbind(result, data.frame(
      batch = b,
      n = as.integer(info$n[1]),
      last_updated = as.character(info$last_updated[1]),
      first_created = as.character(info$first_created[1]),
      stringsAsFactors = FALSE
    ))
  }
  suppressWarnings({ ts <- as.POSIXct(result$last_updated, tz = "UTC") })
  ord <- order(ts, decreasing = TRUE, na.last = TRUE)
  result <- result[ord, , drop = FALSE]
  if (preview) return(result)
  total <- sum(result$n)
  if (total == 0) {
    message("ℹ️ 指定批次无可删除记录")
    return(data.frame(batch = result$batch, deleted = integer(length(result$batch)), stringsAsFactors = FALSE))
  }
  if (ask && interactive()) {
    message("⚠️ 即将删除以下批次（按更新时间降序）：")
    print(result)
    resp <- readline(prompt = "确认删除? 输入 yes 或 y 继续：")
    if (!tolower(trimws(resp)) %in% c("y", "yes")) {
      message("✅ 已取消删除")
      return(result)
    }
  }
  dbBegin(con)
  ok <- TRUE
  tryCatch({
    for (b in result$batch) {
      dbExecute(con, "DELETE FROM crosses WHERE batch = ?", params = list(b))
    }
    dbCommit(con)
  }, error = function(e) {
    ok <<- FALSE
    dbRollback(con)
    stop("❌ 批次删除失败：", e$message)
  })
  if (ok) {
    return(data.frame(batch = result$batch, deleted = result$n, stringsAsFactors = FALSE))
  }
}

list_cross_batches_db <- function(
    db_path = NULL
) {
  if (is.null(db_path)) {
    db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
  }
  if (!file.exists(db_path)) stop("❌ 数据库文件不存在：", db_path)
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  res <- dbGetQuery(con, "SELECT DISTINCT batch FROM crosses WHERE batch IS NOT NULL ORDER BY batch ASC")
  return(res)
}

get_crosses_by_batch <- function(
    batch,
    db_path = NULL,
    include_reciprocal = TRUE
) {
  if (is.null(db_path)) {
    db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
  }
  if (missing(batch) || !nzchar(batch)) stop("❌ 参数错误：batch 不能为空")
  if (!file.exists(db_path)) stop("❌ 数据库文件不存在：", db_path)
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  sql <- "SELECT * FROM crosses WHERE batch = ?"
  params <- list(batch)
  
  if (!include_reciprocal) {
    sql <- paste0(sql, " AND is_reciprocal = 0")
  }
  
  sql <- paste0(sql, " ORDER BY updated_at DESC")
  result <- dbGetQuery(con, sql, params = params)
  return(result)
}

#' 关联亲本信息到杂交组合
#'
#' @description
#' 将杂交组合数据框与 parents 表关联，获取母本和父本的详细信息。
#' 亲本字段会自动添加 "female_" 和 "male_" 前缀。
#'
#' @param crosses_data 数据框，必须包含 female_id 和 male_id 列
#' @param db_path 数据库路径
#' @param fields 字符向量，指定要关联的亲本字段（不含id）。如果为 NULL，则关联所有字段。
#'
# -----------------------------------------------------------------------------
# Shiny 模块：杂交组合配置
# -----------------------------------------------------------------------------

#' 杂交组合配置 UI
#' @export
cross_config_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # 步骤 1 & 2：亲本选择（左右分栏）
      column(4,
        h4("1. 选择母本"),
        DT::dataTableOutput(ns("tbl_females"))
      ),
      column(4,
        h4("2. 选择父本"),
        DT::dataTableOutput(ns("tbl_males"))
      ),
      # 步骤 3 & 4：配置与执行
      column(4,
        h4("3. 配置参数"),
        textInput(ns("input_batch"), "批次名称", value = format(Sys.Date(), SoyCross$config$cross$default_batch_format)),
        checkboxInput(ns("check_reciprocal"), "自动生成反交", TRUE),
        hr(),
        h4("4. 确认与生成"),
        numericInput(ns("input_limit"), "限制数量 (可选)", value = NA, min = 1),
        uiOutput(ns("ui_summary")),
        actionButton(ns("btn_run"), "🚀 生成杂交计划", class = "btn-primary btn-lg btn-block")
      )
    )
  )
}

#' 杂交组合配置 Server
#' @export
cross_config_server <- function(id, db_path = NULL) {
  if (is.null(db_path)) {
    db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
  }
  # 尝试加载分析模块以支持冲突检测
  if (file.exists("R/mod_analysis.R")) source("R/mod_analysis.R")

  moduleServer(id, function(input, output, session) {

    # 1. 数据加载 (获取所有字段以支持特征显示)
    parents <- reactive({
      con <- dbConnect(SQLite(), db_path)
      on.exit(dbDisconnect(con))
      # 获取所有列，后续根据列名动态展示
      dbGetQuery(con, "SELECT * FROM parents WHERE active=1")
    })

    # 辅助函数：获取展示列
    get_display_cols <- function(df) {
      base_cols <- c("id", "name")
      # 尝试查找特征列
      extra_cols <- intersect(names(df), c("traits", "description", "features", "remarks"))
      if (length(extra_cols) > 0) base_cols <- c(base_cols, extra_cols)
      base_cols
    }

    # 2. 渲染母本表格
    output$tbl_females <- DT::renderDataTable({
      df <- parents()
      cols <- get_display_cols(df)
      DT::datatable(
        df[, cols, drop = FALSE],
        selection = "multiple",
        filter = "top",
        options = list(pageLength = 10)
      )
    })

    # 3. 准备父本数据 (含冲突检测)
    males_data_reactive <- reactive({
      df <- parents()
      
      # 获取选中的母本
      selected_rows <- input$tbl_females_rows_selected
      if (is.null(selected_rows) || length(selected_rows) == 0) {
        df$is_conflict <- FALSE
        return(df)
      }

      # 获取选中母本的名称 (find_unused_partners 需要名称)
      # 注意：DT 的行号对应 parents() 的行号
      selected_mothers <- df$name[selected_rows]
      
      # 冲突检测逻辑：
      # 我们要高亮那些【已经】与选中母本配过组的父本。
      # find_unused_partners 返回【未】配组的。
      # 所以：冲突 = 所有父本 - 交集(每个母本的未配组对象)
      # 或者更直接：对于每个选中的母本，找出其已配组对象，取并集。
      
      # 为了严格遵循 "调用 mod_analysis::find_unused_partners" 的要求：
      if (exists("find_unused_partners")) {
        # 计算每个母本的未配组父本 ID
        unused_list <- lapply(selected_mothers, function(m_name) {
          tryCatch({
             # role="female" 表示 m_name 是母本，我们要找未配的父本
             find_unused_partners(m_name, role = "female", db_path = db_path)$id
          }, error = function(e) {
             # 如果出错（如找不到亲本），假设没有未配组的（即全部冲突）或全部可用？
             # 安全起见，假设全部可用，避免误报冲突
             df$id 
          })
        })
        
        # 取交集：只有在所有选中母本中都“未配组”的父本，才是真正的“无冲突”
        # 只要与任一选中母本配过组，即视为冲突（高亮提示）
        # Wait. 
        # Case 1: Select M1. Used with P1. Unused with P2. -> P1 Conflict.
        # Case 2: Select M1, M2. 
        # M1 used with P1. M2 used with P2.
        # If I select P1: (M1, P1) is repeat. (M2, P1) is new. -> Conflict? Yes, partial conflict.
        # If I select P2: (M1, P2) is new. (M2, P2) is repeat. -> Conflict? Yes.
        # So, if a father is used by ANY of the selected mothers, it should be highlighted.
        # Used_by_Any = Union(Used_by_M1, Used_by_M2...)
        # Used_by_M = All - Unused_by_M
        # So Used_by_Any = Union( (All - Unused_M1), (All - Unused_M2) )
        # = All - Intersection(Unused_M1, Unused_M2...)
        
        common_unused_ids <- Reduce(intersect, unused_list)
        df$is_conflict <- ! (df$id %in% common_unused_ids)
        
      } else {
        # Fallback if function not found
        df$is_conflict <- FALSE
      }
      
      df
    })

    # 4. 渲染父本表格 (仅显示 id 和 name 两列)
    output$tbl_males <- DT::renderDataTable({
      df <- males_data_reactive()
      cols <- get_display_cols(df)
      
      # 只显示 id 和 name，简化方案
      cols <- c("id", "name")
      
      dt <- DT::datatable(
        df[, cols, drop = FALSE],
        selection = "multiple",
        options = list(pageLength = 10)
      )
      
      dt
    })

    # 3. 实时摘要
    output$ui_summary <- renderUI({
      n_f <- length(input$tbl_females_rows_selected)
      n_m <- length(input$tbl_males_rows_selected)
      n_total <- n_f * n_m
      if (input$check_reciprocal) n_total <- n_total * 2

      tagList(
        p(glue::glue("已选母本: {n_f}")),
        p(glue::glue("已选父本: {n_m}")),
        p(glue::glue("预计组合: {n_total}"), style = "font-weight: bold; color: blue;")
      )
    })

    # 4. 执行逻辑
    observeEvent(input$btn_run, {
      req(input$input_batch)

      # 获取选中行的 ID
      p_data <- parents()
      f_ids <- p_data$id[input$tbl_females_rows_selected]
      m_ids <- p_data$id[input$tbl_males_rows_selected]

      if (length(f_ids) == 0 || length(m_ids) == 0) {
        showNotification("请至少选择一个母本和一个父本", type = "error")
        return()
      }

      withProgress(message = '正在生成计划...', {
        tryCatch({
          if (is.na(input$input_limit)) {
            # 全量生成
            res <- create_cross_plan(
              batch_name = input$input_batch,
              mothers = f_ids,
              fathers = m_ids,
              include_reciprocal = input$check_reciprocal,
              use_id = TRUE,
              db_path = db_path
            )
          } else {
            # 限量生成
            res <- create_cross_plan_n(
              batch_name = input$input_batch,
              mothers = f_ids,
              fathers = m_ids,
              n = input$input_limit,
              include_reciprocal = input$check_reciprocal,
              use_id = TRUE,
              db_path = db_path
            )
          }

          showNotification(glue::glue("成功！新增 {res$summary$new_crosses} 个组合"), type = "message")

        }, error = function(e) {
          showNotification(paste("错误:", e$message), type = "error")
        })
      })
    })
  })
}

# -----------------------------------------------------------------------------
# 辅助函数：关联亲本
# -----------------------------------------------------------------------------

#' 关联亲本信息到杂交记录
#'
#' @description
#' 将杂交记录中的 female_id 和 male_id 与 parents 表关联，获取亲本的详细信息。
#' 同时保留兼容字段 female_name, male_name
#'
#' @param crosses_data 数据框，必须包含 female_id 和 male_id 列
#' @param db_path 数据库路径，默认 "data/db/soy_cross.db"
#'
#' @return 包含亲本详细信息的数据框
#' @export
join_cross_parents <- function(crosses_data, db_path = NULL) {
  if (is.null(db_path)) {
    db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
  }
  if (missing(crosses_data) || !is.data.frame(crosses_data)) {
    stop("❌ 参数错误：crosses_data 必须是一个数据框")
  }
  
  if (!file.exists(db_path)) stop("❌ 数据库文件不存在：", db_path)
  
  # 获取 parents 数据 (获取全部列)
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  parents_df <- dbGetQuery(con, "SELECT * FROM parents")
  
  # 准备母本数据 (前缀 female_)
  female_df <- parents_df
  names(female_df) <- paste0("female_", names(female_df))
  
  # 准备父本数据 (前缀 male_)
  male_df <- parents_df
  names(male_df) <- paste0("male_", names(male_df))
  
  # 使用 left_join 替代 merge 以保持原始顺序
  # 关联母本：female_id -> female_id (parents表原本的id变成了female_id)
  res <- crosses_data %>%
    left_join(female_df, by = "female_id")
    
  # 关联父本：male_id -> male_id (parents表原本的id变成了male_id)
  res <- res %>%
    left_join(male_df, by = "male_id")
  
  # --- 添加兼容性/便利性字段 ---
  
  # 为了 get_combination 兼容性，确保有 ma, pa (代表名称)
  if ("female_name" %in% names(res)) res$pa <- res$female_name
  if ("male_name" %in% names(res)) res$ma <- res$male_name
  
  # --- 调整列顺序：按性状交替排列 (female_XX, male_XX) ---
  
  # 1. 获取 parents 表的原始列名（排除 id）
  parent_base_cols <- setdiff(names(parents_df), "id")
  
  # 2. 构建交替列名列表
  interleaved_cols <- character(0)
  for (col in parent_base_cols) {
    # 用户要求：按 female_XX, male_XX 顺序排列
    interleaved_cols <- c(interleaved_cols, paste0("female_", col), paste0("male_", col))
  }
  
  # 3. 确定最终顺序
  original_cols <- names(crosses_data)
  current_cols <- names(res)
  
  # 确保只包含实际存在的列
  valid_interleaved <- intersect(interleaved_cols, current_cols)
  
  # 其他列（如新添加的兼容性字段，或 crosses_data 中未包含但在 res 中的列）
  other_cols <- setdiff(current_cols, c(original_cols, valid_interleaved))
  
  final_order <- c(original_cols, valid_interleaved, other_cols)
  
  res <- res[, final_order, drop = FALSE]
  
  return(res)
}

