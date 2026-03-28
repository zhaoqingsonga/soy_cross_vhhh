# =============================================================================
# 模块名称: I/O 辅助工具模块
# 文件路径: R/utils_io.R
# 功能描述: 统一文件读写、路径规范化、数据库备份、矩阵导入导出
# 创建日期: 2025-12-29
# =============================================================================

# ---- 依赖检查 ----
# 核心依赖（必需）
if (!requireNamespace("DBI", quietly = TRUE)) {
  stop("❌ 缺少依赖包：DBI，请运行 install.packages('DBI')")
}
if (!requireNamespace("RSQLite", quietly = TRUE)) {
  stop("❌ 缺少依赖包：RSQLite，请运行 install.packages('RSQLite')")
}

# 可选依赖（增强体验）
.has_openxlsx <- requireNamespace("openxlsx", quietly = TRUE)
.has_readxl <- requireNamespace("readxl", quietly = TRUE)
.has_writexl <- requireNamespace("writexl", quietly = TRUE)

# ---- 路径规范化函数 ----

#' 统一路径分隔符并转换为绝对路径
#'
#' @description
#' 将路径规范化为绝对路径，统一处理 Windows 反斜杠和 Unix 正斜杠。
#' 如果路径不存在，尝试创建父目录（可选）。
#'
#' @param p 字符型，文件或目录路径
#' @param must_exist 逻辑型，是否要求路径必须存在（默认 FALSE）
#' @param create_dir 逻辑型，如果父目录不存在是否自动创建（默认 FALSE）
#'
#' @return 规范化后的绝对路径字符串
#'
#' @examples
#' \dontrun{
#' # 规范化相对路径
#' normalize_path("data/db/soy_cross.db")
#' 
#' # 要求路径必须存在
#' normalize_path("data/parent_table.rds", must_exist = TRUE)
#' 
#' # 自动创建父目录
#' normalize_path("output/reports/test.xlsx", create_dir = TRUE)
#' }
#'
#' @export
normalize_path <- function(p, must_exist = FALSE, create_dir = FALSE) {
  if (is.null(p) || !is.character(p) || length(p) == 0 || nchar(p) == 0) {
    stop("❌ 路径参数不能为空")
  }
  
  # 统一路径分隔符（Windows 反斜杠转为正斜杠）
  p <- gsub("\\\\", "/", p)
  
  # 转换为绝对路径
  if (!grepl("^(/|[A-Za-z]:)", p)) {
    p <- file.path(getwd(), p)
  }
  
  # 再次统一分隔符
  p <- normalizePath(p, winslash = "/", mustWork = FALSE)
  
  # 检查路径是否存在
  if (must_exist && !file.exists(p)) {
    stop("❌ 路径不存在: ", p)
  }
  
  # 自动创建父目录
  if (create_dir) {
    parent_dir <- dirname(p)
    if (!dir.exists(parent_dir)) {
      dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
      message("📁 已创建目录: ", parent_dir)
    }
  }
  
  return(p)
}


# ---- 数据库备份函数 ----

#' 备份 SQLite 数据库文件
#'
#' @description
#' 将数据库文件复制到备份目录，文件名包含时间戳。
#' 用于重要操作前的数据保护。
#'
#' @param db_path 字符型，数据库文件路径
#' @param dest_dir 字符型，备份目标目录（默认为数据库所在目录）
#' @param backup_name 字符型，备份子目录名称（默认 "backups"）
#'
#' @return 备份文件的完整路径
#'
#' @examples
#' \dontrun{
#' # 备份数据库到默认位置
#' backup_db("data/db/soy_cross.db")
#' 
#' # 备份到指定目录
#' backup_db("data/db/soy_cross.db", dest_dir = "backups/manual")
#' 
#' # 指定备份子目录名称
#' backup_db("data/db/soy_cross.db", backup_name = "before_import")
#' }
#'
#' @export
backup_db <- function(db_path = NULL, dest_dir = NULL, backup_name = "backups") {
  # 0. 处理默认参数
  if (is.null(db_path)) {
    db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
  }

  # 1. 验证数据库文件是否存在
  db_path <- normalize_path(db_path, must_exist = TRUE)
  
  if (!file.exists(db_path)) {
    stop("❌ 数据库文件不存在: ", db_path)
  }
  
  # 2. 确定备份目录
  if (is.null(dest_dir)) {
    # 尝试从配置加载
    if (exists("SoyCross") && !is.null(SoyCross$config$paths$backup_dir)) {
      dest_dir <- SoyCross$config$paths$backup_dir
    } else {
      dest_dir <- dirname(db_path)
    }
  }
  backup_dir <- file.path(dest_dir, backup_name)
  
  # 3. 创建备份目录
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # 4. 生成带时间戳的备份文件名
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  db_basename <- basename(db_path)
  backup_file <- file.path(
    backup_dir,
    paste0(tools::file_path_sans_ext(db_basename), "_", timestamp, ".backup")
  )
  
  # 5. 尝试执行复制操作
  # 注意：在 Windows 上，如果数据库文件被 SQLite 连接打开，file.copy 可能会失败
  # 因此调用此函数前应确保数据库连接已关闭
  tryCatch({
    # 尝试多次复制（最多3次），每次间隔0.1秒
    success <- FALSE
    max_retries <- 3
    for (attempt in 1:max_retries) {
      success <- file.copy(db_path, backup_file, overwrite = FALSE)
      if (success) break
      if (attempt < max_retries) {
        Sys.sleep(0.1)  # 等待0.1秒后重试
      }
    }
    
    if (!success) {
      # 检查是否是因为文件被占用
      if (file.exists(backup_file)) {
        file.remove(backup_file)  # 清理可能的部分文件
      }
      stop("❌ 复制操作失败：数据库文件可能被占用，请确保所有数据库连接已关闭")
    }
    
    # 6. 验证备份文件大小
    original_size <- file.info(db_path)$size
    backup_size <- file.info(backup_file)$size
    
    if (is.na(backup_size) || backup_size != original_size) {
      if (file.exists(backup_file)) {
        file.remove(backup_file)  # 清理损坏的备份文件
      }
      stop("❌ 备份文件损坏: 文件大小不匹配 (原始: ", original_size, " 字节, 备份: ", backup_size, " 字节)")
    }
  }, error = function(e) {
    # 如果备份失败，清理并报告错误
    if (file.exists(backup_file)) {
      tryCatch(file.remove(backup_file), error = function(e2) {})
    }
    stop("❌ 备份失败: ", e$message, " (目标: ", backup_file, ")")
  })
  
  message("✅ 数据库备份成功: ", backup_file)
  message("   原始文件: ", db_path, " (", round(original_size / 1024, 2), " KB)")
  
  return(invisible(backup_file))
}


# ---- 通用数据读写函数 ----

#' 将数据框列表写入文件
#'
#' @description
#' 支持将一个或多个数据框写入 Excel、CSV 或 RDS 格式。
#' 默认不覆盖已存在的文件，需显式设置 overwrite = TRUE。
#'
#' @param df_list 数据框或命名列表（单个数据框或多个数据框）
#' @param out_path 字符型，输出文件路径
#' @param format 字符型，输出格式（"xlsx", "csv", "rds"），默认自动根据扩展名识别
#' @param overwrite 逻辑型，是否覆盖已存在的文件（默认 FALSE）
#' @param backup_before 逻辑型，覆盖前是否先备份（默认 TRUE）
#'
#' @return 不可见的输出文件路径
#'
#' @examples
#' \dontrun{
#' # 写入单个数据框到 Excel
#' df <- data.frame(x = 1:10, y = letters[1:10])
#' write_table(df, "output/test.xlsx")
#' 
#' # 写入多个数据框到 Excel（多个工作表）
#' df_list <- list(sheet1 = df1, sheet2 = df2)
#' write_table(df_list, "output/multi_sheet.xlsx")
#' 
#' # 覆盖已存在的文件（先备份）
#' write_table(df, "output/test.xlsx", overwrite = TRUE)
#' }
#'
#' @export
write_table <- function(df_list, out_path, format = NULL, 
                       overwrite = FALSE, backup_before = TRUE) {
  
  # 1. 参数验证
  if (is.null(df_list)) {
    stop("❌ df_list 不能为空")
  }
  
  # 将单个数据框转换为列表
  if (is.data.frame(df_list)) {
    df_list <- list(data = df_list)
  }
  
  if (!is.list(df_list)) {
    stop("❌ df_list 必须是数据框或列表")
  }
  
  # 2. 规范化输出路径
  out_path <- normalize_path(out_path, create_dir = TRUE)
  
  # 3. 检查文件是否已存在
  if (file.exists(out_path) && !overwrite) {
    stop("❌ 文件已存在: ", out_path, "\n   如需覆盖，请设置 overwrite = TRUE")
  }
  
  # 4. 覆盖前备份
  if (file.exists(out_path) && overwrite && backup_before) {
    backup_file <- paste0(
      tools::file_path_sans_ext(out_path), 
      "_backup_", 
      format(Sys.time(), "%Y%m%d_%H%M%S"), 
      ".", 
      tools::file_ext(out_path)
    )
    file.copy(out_path, backup_file, overwrite = FALSE)
    message("📋 已备份原文件: ", backup_file)
  }
  
  # 5. 自动识别格式
  if (is.null(format)) {
    format <- tolower(tools::file_ext(out_path))
  }
  format <- match.arg(format, c("xlsx", "csv", "rds"))
  
  # 6. 根据格式写入文件
  tryCatch({
    if (format == "xlsx") {
      write_xlsx_internal(df_list, out_path)
    } else if (format == "csv") {
      write_csv_internal(df_list, out_path)
    } else if (format == "rds") {
      saveRDS(df_list, out_path)
    }
  }, error = function(e) {
    stop("❌ 写入文件失败: ", e$message)
  })
  
  message("✅ 文件已保存: ", out_path)
  return(invisible(out_path))
}


# 内部函数：写入 Excel
write_xlsx_internal <- function(df_list, out_path) {
  if (.has_openxlsx) {
    # 使用 openxlsx（推荐，支持多工作表）
    wb <- openxlsx::createWorkbook()
    
    for (i in seq_along(df_list)) {
      sheet_name <- names(df_list)[i]
      if (is.null(sheet_name) || nchar(sheet_name) == 0) {
        sheet_name <- paste0("Sheet", i)
      }
      
      openxlsx::addWorksheet(wb, sheet_name)
      openxlsx::writeData(wb, sheet_name, df_list[[i]], rowNames = FALSE)
      
      # 自动调整列宽
      openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol(df_list[[i]]), widths = "auto")
    }
    
    openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
    
  } else if (.has_writexl) {
    # 使用 writexl（备选方案）
    writexl::write_xlsx(df_list, out_path)
    
  } else {
    stop("❌ 需要安装 openxlsx 或 writexl 包以支持 Excel 格式\n",
         "   请运行: install.packages('openxlsx')")
  }
}


# 内部函数：写入 CSV
write_csv_internal <- function(df_list, out_path) {
  if (length(df_list) == 1) {
    # 单个数据框，直接写入
    write.csv(df_list[[1]], out_path, row.names = FALSE, fileEncoding = "UTF-8")
  } else {
    # 多个数据框，生成多个文件
    base_path <- tools::file_path_sans_ext(out_path)
    
    for (i in seq_along(df_list)) {
      name <- names(df_list)[i]
      if (is.null(name) || nchar(name) == 0) {
        name <- paste0("table", i)
      }
      
      csv_path <- paste0(base_path, "_", name, ".csv")
      write.csv(df_list[[i]], csv_path, row.names = FALSE, fileEncoding = "UTF-8")
      message("   - ", csv_path)
    }
  }
}


#' 从文件读取数据
#'
#' @description
#' 自动识别文件扩展名（xlsx, csv, rds）并读取为数据框或列表。
#'
#' @param in_path 字符型，输入文件路径
#' @param sheet 字符型或整数型，Excel 工作表名称或索引（仅 xlsx 格式）
#' @param ... 其他参数传递给底层读取函数
#'
#' @return 数据框或列表（对于 RDS 和多工作表 Excel）
#'
#' @examples
#' \dontrun{
#' # 读取 RDS 文件
#' data <- read_table("data/parent_table.rds")
#' 
#' # 读取 Excel 指定工作表
#' data <- read_table("data/crosses.xlsx", sheet = "批次1")
#' 
#' # 读取 CSV 文件
#' data <- read_table("output/export.csv")
#' }
#'
#' @export
read_table <- function(in_path, sheet = NULL, ...) {
  # 1. 验证文件存在
  in_path <- normalize_path(in_path, must_exist = TRUE)
  
  # 2. 识别文件格式
  format <- tolower(tools::file_ext(in_path))
  
  # 3. 根据格式读取
  tryCatch({
    if (format == "rds") {
      return(readRDS(in_path))
      
    } else if (format %in% c("xlsx", "xls")) {
      return(read_xlsx_internal(in_path, sheet, ...))
      
    } else if (format == "csv") {
      return(read.csv(in_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8", ...))
      
    } else {
      stop("❌ 不支持的文件格式: ", format)
    }
  }, error = function(e) {
    stop("❌ 读取文件失败: ", e$message)
  })
}


# 内部函数：读取 Excel
read_xlsx_internal <- function(in_path, sheet = NULL, ...) {
  if (.has_readxl) {
    # 使用 readxl（推荐）
    if (is.null(sheet)) {
      sheet <- 1  # 默认第一个工作表
    }
    df <- readxl::read_excel(in_path, sheet = sheet, ...)
    return(as.data.frame(df))
    
  } else if (.has_openxlsx) {
    # 使用 openxlsx（备选）
    if (is.null(sheet)) {
      sheet <- 1
    }
    df <- openxlsx::read.xlsx(in_path, sheet = sheet, ...)
    return(as.data.frame(df))
    
  } else {
    stop("❌ 需要安装 readxl 或 openxlsx 包以支持 Excel 格式\n",
         "   请运行: install.packages('readxl')")
  }
}


# ---- 矩阵导入导出函数 ----

#' 从数据库导出杂交矩阵视图
#'
#' @description
#' 从 crosses 表生成母本×父本的二维矩阵视图，并导出为 Excel 或其他格式。
#' 支持按批次筛选。
#'
#' @param con SQLite 数据库连接对象，或数据库文件路径
#' @param batch 字符型，批次名称（可选，NULL 表示所有批次）
#' @param out_path 字符型，输出文件路径（可选，NULL 则仅返回矩阵）
#' @param format 字符型，输出格式（"xlsx", "csv", "rds"）
#' @param value_col 字符型，矩阵单元格显示的字段名（默认 "batch"）
#'
#' @return 二维矩阵对象（不可见）
#'
#' @examples
#' \dontrun{
#' # 导出所有批次的矩阵
#' con <- DBI::dbConnect(RSQLite::SQLite(), "data/db/soy_cross.db")
#' export_cross_matrix(con, out_path = "output/matrix_all.xlsx")
#' DBI::dbDisconnect(con)
#' 
#' # 导出指定批次
#' export_cross_matrix("data/db/soy_cross.db", batch = "2025春季", 
#'                    out_path = "output/matrix_2025.xlsx")
#' }
#'
#' @export
export_cross_matrix <- function(con, batch = NULL, out_path = NULL, 
                                format = "xlsx", value_col = "batch") {
  
  # 1. 处理数据库连接
  close_on_exit <- FALSE
  if (is.character(con)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), normalize_path(con, must_exist = TRUE))
    close_on_exit <- TRUE
  }
  
  if (close_on_exit) {
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }
  
  # 2. 构建 SQL 查询
  sql <- sprintf("
    SELECT 
      p1.name AS mother, 
      p2.name AS father, 
      c.%s AS value
    FROM crosses c
    LEFT JOIN parents p1 ON c.female_id = p1.id
    LEFT JOIN parents p2 ON c.male_id = p2.id
    WHERE 1=1
  ", value_col)
  
  if (!is.null(batch)) {
    sql <- paste0(sql, sprintf(" AND c.batch = '%s'", batch))
  }
  
  # 3. 查询数据
  df <- DBI::dbGetQuery(con, sql)
  
  if (nrow(df) == 0) {
    warning("⚠️  查询结果为空，没有符合条件的杂交记录")
    return(invisible(NULL))
  }
  
  # 4. 转换为矩阵
  mothers <- sort(unique(df$mother))
  fathers <- sort(unique(df$father))
  
  mat <- matrix(NA_character_, nrow = length(mothers), ncol = length(fathers))
  rownames(mat) <- mothers
  colnames(mat) <- fathers
  
  for (i in seq_len(nrow(df))) {
    m <- df$mother[i]
    f <- df$father[i]
    v <- df$value[i]
    mat[m, f] <- as.character(v)
  }
  
  message("✅ 已生成矩阵: ", length(mothers), " 个母本 × ", length(fathers), " 个父本")
  
  # 5. 导出文件（可选）
  if (!is.null(out_path)) {
    # 转换为数据框（保留行名作为第一列）
    mat_df <- as.data.frame(mat, stringsAsFactors = FALSE)
    mat_df <- cbind(母本 = rownames(mat), mat_df)
    rownames(mat_df) <- NULL
    
    write_table(list(杂交矩阵 = mat_df), out_path, format = format, overwrite = TRUE)
  }
  
  return(invisible(mat))
}


#' 从历史矩阵文件导入杂交组合
#'
#' @description
#' 读取历史的二维矩阵表（Excel 或 RDS），标准化为长表格式。
#' 返回包含 female_id, male_id, content, batch 的数据框。
#'
#' @param in_path 字符型，输入文件路径（Excel 或 RDS）
#' @param sheet 字符型或整数型，Excel 工作表名称或索引（仅 xlsx）
#' @param batch 字符型，为所有组合指定批次名称（可选）
#' @param skip_na 逻辑型，是否跳过矩阵中的 NA 单元格（默认 TRUE）
#'
#' @return 长表数据框（female_id, male_id, content, batch）
#'
#' @examples
#' \dontrun{
#' # 从 Excel 导入历史矩阵
#' df <- import_cross_matrix("data/历史杂交矩阵.xlsx", sheet = "2024年")
#' 
#' # 从 RDS 导入并指定批次
#' df <- import_cross_matrix("data/matrix_2023.rds", batch = "2023秋季")
#' }
#'
#' @export
import_cross_matrix <- function(in_path, sheet = NULL, batch = NULL, skip_na = TRUE) {
  
  # 1. 读取文件
  in_path <- normalize_path(in_path, must_exist = TRUE)
  format <- tolower(tools::file_ext(in_path))
  
  if (format %in% c("xlsx", "xls")) {
    df <- read_xlsx_internal(in_path, sheet = sheet)
  } else if (format == "csv") {
    df <- read.csv(in_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  } else if (format == "rds") {
    obj <- readRDS(in_path)
    if (is.matrix(obj)) {
      mat <- obj
    } else if (is.data.frame(obj)) {
      # 假设第一列是行名
      rownames_col <- obj[, 1]
      mat <- as.matrix(obj[, -1])
      rownames(mat) <- rownames_col
    } else {
      stop("❌ RDS 文件格式不正确，期望矩阵或数据框")
    }
    df <- as.data.frame(mat, stringsAsFactors = FALSE)
    df <- cbind(母本 = rownames(mat), df)
    rownames(df) <- NULL
  } else {
    stop("❌ 不支持的文件格式: ", format)
  }
  
  # 2. 验证数据框结构
  if (ncol(df) < 2) {
    stop("❌ 矩阵至少需要2列（行名列 + 至少1个父本列）")
  }
  
  # 3. 提取行名列和矩阵数据
  mother_col <- df[, 1]
  father_cols <- colnames(df)[-1]
  mat_data <- df[, -1, drop = FALSE]
  
  # 4. 转换为长表
  result_list <- list()
  
  for (i in seq_len(nrow(mat_data))) {
    for (j in seq_len(ncol(mat_data))) {
      value <- mat_data[i, j]
      
      # 跳过 NA 或空字符串
      if (skip_na && (is.na(value) || nchar(as.character(value)) == 0)) {
        next
      }
      
      result_list[[length(result_list) + 1]] <- data.frame(
        female_id = as.character(mother_col[i]),
        male_id = as.character(father_cols[j]),
        content = as.character(value),
        batch = if (is.null(batch)) NA_character_ else batch,
        stringsAsFactors = FALSE
      )
    }
  }
  
  # 5. 合并结果
  if (length(result_list) == 0) {
    warning("⚠️  没有找到有效的杂交组合数据")
    return(data.frame(
      female_id = character(0),
      male_id = character(0),
      content = character(0),
      batch = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  result_df <- do.call(rbind, result_list)
  
  message("✅ 已导入 ", nrow(result_df), " 个杂交组合")
  
  return(result_df)
}

export_parents_to_file <- function(db_path = NULL, out_path, include_inactive = TRUE, format = "xlsx", overwrite = FALSE, backup_before = TRUE) {
  if (is.null(db_path)) {
    db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
  }
  db_path <- normalize_path(db_path, must_exist = TRUE)
  out_path <- normalize_path(out_path, create_dir = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  df <- DBI::dbReadTable(con, "parents")
  if (!include_inactive && "active" %in% names(df)) {
    df <- df[which(df$active == 1), , drop = FALSE]
  }
  write_table(list(parents = df), out_path, format = format, overwrite = overwrite, backup_before = backup_before)
  invisible(out_path)
}

import_parents_from_file <- function(
  in_path,
  db_path = NULL,
  mode = c("upsert", "update", "insert"),
  key = c("id", "name"),
  sheet = NULL,
  backup_before = FALSE
) {
  if (is.null(db_path)) {
    db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
  }
  mode <- match.arg(mode)
  key <- match.arg(key)
  
  # --- 路径与格式检查 ---
  in_path <- normalize_path(in_path, must_exist = TRUE)
  db_path <- normalize_path(db_path, must_exist = FALSE)

  fmt <- tolower(tools::file_ext(in_path))
  if (fmt %in% c("xlsx", "xls")) {
    df <- read_xlsx_internal(in_path, sheet = sheet)
  } else if (fmt == "csv") {
    df <- read.csv(in_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  } else if (fmt == "rds") {
    obj <- readRDS(in_path)
    df <- if (is.data.frame(obj)) obj else as.data.frame(obj, stringsAsFactors = FALSE)
  } else {
    stop("❌ 不支持的文件格式: ", fmt)
  }

  if (!is.data.frame(df) || nrow(df) == 0) {
    stop("❌ 导入的文件中没有有效数据")
  }

  # 统一列名：去空格 + 转小写，便于匹配 id/name
  orig_names <- names(df)
  clean_names <- trimws(orig_names)
  # 处理可能的 BOM（如 "\ufeffid"）
  clean_names <- sub("^\ufeff", "", clean_names)
  lower_names <- tolower(clean_names)
  map <- c(
    "id" = "id",
    "编号" = "id",
    "亲本id" = "id",
    "name" = "name",
    "名称" = "name",
    "亲本名" = "name"
  )
  lower_names <- ifelse(lower_names %in% names(map), map[lower_names], lower_names)
  names(df) <- lower_names

  # --- 校验必需列：至少包含 key 所指定的列 ---
  if (!(key %in% names(df))) {
    stop("❌ 导入的文件缺少关键列：", key)
  }

  # id / name 转字符，只保留有效行
  if ("id" %in% names(df)) df$id <- trimws(as.character(df$id))
  if ("name" %in% names(df)) df$name <- trimws(as.character(df$name))

  if ("id" %in% names(df)) {
    df <- df[!is.na(df$id) & nzchar(df$id), , drop = FALSE]
  }
  if ("name" %in% names(df)) {
    df <- df[!is.na(df$name) & nzchar(df$name), , drop = FALSE]
  }
  if (nrow(df) == 0) {
    stop("❌ 导入数据为空：有效的 id / name 行数为 0")
  }

  # 检查重复项
  if ("id" %in% names(df) && any(duplicated(df$id))) {
    dup_ids <- df$id[duplicated(df$id)]
    stop("❌ 发现重复的ID: ", paste(head(dup_ids, 5), collapse = ", "))
  }
  
  if ("name" %in% names(df) && any(duplicated(df$name))) {
    dup_names <- df$name[duplicated(df$name)]
    stop("❌ 发现重复的名称: ", paste(head(dup_names, 5), collapse = ", "))
  }

  # --- 补充必要字段 ---
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  if (!"active" %in% names(df)) {
    df$active <- 1L
  } else {
    suppressWarnings(df$active <- as.integer(df$active))
    df$active[is.na(df$active)] <- 1L
  }

  if (!"created_at" %in% names(df)) {
    df$created_at <- now
  }
  if (!"updated_at" %in% names(df)) {
    df$updated_at <- now
  }


  # --- 连接数据库并执行相应操作 ---
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # 检查是否存在 parents 表
  if (!DBI::dbExistsTable(con, "parents")) {
    # 如果不存在 parents 表，直接创建
    DBI::dbWriteTable(con, "parents", df, overwrite = TRUE)
  } else {
    # 如果存在 parents 表，根据模式处理
    db_fields <- DBI::dbListFields(con, "parents")
    # 只保留数据库中存在的列
    df <- df[, intersect(names(df), db_fields), drop = FALSE]
      
    n_ins <- 0L; n_upd <- 0L; n_skip <- 0L
      
    DBI::dbBegin(con)
    on.exit(try(DBI::dbRollback(con), silent = TRUE), add = TRUE)
      
    next_id_base <- 0L
    if ("id" %in% db_fields) {
      ids <- try(DBI::dbGetQuery(con, "SELECT id FROM parents"), silent = TRUE)
      if (!inherits(ids, "try-error") && "id" %in% names(ids)) {
        nums <- suppressWarnings(as.integer(gsub("^P(\\d+)$", "\\1", ids$id)))
        next_id_base <- if (is.finite(max(nums, na.rm = TRUE))) max(nums, na.rm = TRUE) else 0L
      }
    }
    for (i in seq_len(nrow(df))) {
      row <- df[i, , drop = FALSE]
      exists_n <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM parents WHERE ", key, " = ?"), params = list(row[[key]]))$n
        
      if (mode == "insert") {
        if (exists_n > 0) {
          n_skip <- n_skip + 1L
          next
        }
        ins_cols <- names(row)
        vals <- unname(as.list(row[1, ins_cols]))
        if (!("id" %in% ins_cols) && ("id" %in% db_fields)) {
          next_id_base <- next_id_base + 1L
          ins_cols <- c("id", ins_cols)
          vals <- c(sprintf("P%04d", next_id_base), vals)
        }
        if (!("created_at" %in% ins_cols)) { ins_cols <- c(ins_cols, "created_at"); vals <- c(vals, now) }
        if (!("updated_at" %in% ins_cols)) { ins_cols <- c(ins_cols, "updated_at"); vals <- c(vals, now) }
        sql <- paste0("INSERT INTO parents (", paste(ins_cols, collapse = ", "), ") VALUES (", paste(rep("?", length(ins_cols)), collapse = ", "), ")")
        DBI::dbExecute(con, sql, params = unname(vals))
        n_ins <- n_ins + 1L
      } else if (mode == "update") {
        if (exists_n == 0) {
          n_skip <- n_skip + 1L
          next
        }
        upd_cols <- setdiff(names(row), key)
        if (!("updated_at" %in% upd_cols)) { upd_cols <- c(upd_cols, "updated_at"); row[["updated_at"]] <- now }
        set_clause <- paste(paste0(upd_cols, " = ?"), collapse = ", ")
        sql <- paste0("UPDATE parents SET ", set_clause, " WHERE ", key, " = ?")
        params <- c(unname(as.list(row[1, upd_cols])), unname(list(row[[key]])))
        DBI::dbExecute(con, sql, params = unname(params))
        n_upd <- n_upd + 1L
      } else {
        # upsert 模式
        if (exists_n > 0) {
          # 更新
          upd_cols <- setdiff(names(row), key)
          if (!("updated_at" %in% upd_cols)) { upd_cols <- c(upd_cols, "updated_at"); row[["updated_at"]] <- now }
          set_clause <- paste(paste0(upd_cols, " = ?"), collapse = ", ")
          sql <- paste0("UPDATE parents SET ", set_clause, " WHERE ", key, " = ?")
          params <- c(unname(as.list(row[1, upd_cols])), unname(list(row[[key]])))
          DBI::dbExecute(con, sql, params = unname(params))
          n_upd <- n_upd + 1L
        } else {
          # 插入
          ins_cols <- names(row)
          vals <- unname(as.list(row[1, ins_cols]))
          if (!("id" %in% ins_cols) && ("id" %in% db_fields)) {
            next_id_base <- next_id_base + 1L
            ins_cols <- c("id", ins_cols)
            vals <- c(sprintf("P%04d", next_id_base), vals)
          }
          if (!("created_at" %in% ins_cols)) { ins_cols <- c(ins_cols, "created_at"); vals <- c(vals, now) }
          if (!("updated_at" %in% ins_cols)) { ins_cols <- c(ins_cols, "updated_at"); vals <- c(vals, now) }
          sql <- paste0("INSERT INTO parents (", paste(ins_cols, collapse = ", "), ") VALUES (", paste(rep("?", length(ins_cols)), collapse = ", "), ")")
          DBI::dbExecute(con, sql, params = unname(vals))
          n_ins <- n_ins + 1L
        }
      }
    }
      
    DBI::dbCommit(con)
    on.exit(NULL, add = TRUE)
      
    invisible(list(inserted = n_ins, updated = n_upd, skipped = n_skip, total = nrow(df)))
    return()
  }

  # 补充索引
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_parents_name ON parents(name)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_parents_id ON parents(id)")

  invisible(nrow(df))
}

# ---- 模块信息 ----
.utils_io_version <- "1.0.0"
.utils_io_date <- "2025-12-29"

message("✅ utils_io.R 已加载 (版本 ", .utils_io_version, ")")
