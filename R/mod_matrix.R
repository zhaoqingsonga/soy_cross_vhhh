# =============================================================================
# 模块名称：mod_matrix.R
# 功能描述：虚拟矩阵视图 - 动态展示亲本杂交组合矩阵
# 创建日期：2025-12-29
# =============================================================================

library(RSQLite)
library(DBI)
library(dplyr)
library(rhandsontable)
library(shiny)

# -----------------------------------------------------------------------------
# 核心功能：动态生成矩阵框架
# -----------------------------------------------------------------------------

#' 生成杂交矩阵框架
#'
#' @description
#' 根据 parents 表中 active=1 的亲本动态生成空矩阵框架。
#' 行和列均为活跃亲本，用于后续填充杂交信息。
#'
#' @param db_path 字符串，数据库路径，默认 "data/db/soy_cross.db"
#' @param filter_active 逻辑值，是否只显示 active=1 的亲本，默认 TRUE
#' @param name_field 字符串，用作行列名的字段，可选 "name" 或 "id"，默认 "name"
#'
#' @return 列表，包含：
#'   \item{matrix}{空矩阵框架，行列为亲本}
#'   \item{parent_ids}{亲本ID向量}
#'   \item{parent_names}{亲本名称向量}
#'   \item{n_parents}{亲本数量}
#'
#' @export
#'
#' @examples
#' # 生成活跃亲本的矩阵框架
#' framework <- create_matrix_framework()
#' print(framework$matrix[1:5, 1:5])
create_matrix_framework <- function(
    db_path = NULL,
    filter_active = TRUE,
    name_field = "name"
) {
  if (is.null(db_path)) {
    db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
  }
  
  # === 参数验证 ===
  if (!file.exists(db_path)) {
    stop("❌ 数据库文件不存在：", db_path)
  }
  
  if (!name_field %in% c("name", "id")) {
    stop("❌ name_field 必须是 'name' 或 'id'")
  }
  
  # === 连接数据库 ===
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  # === 查询活跃亲本 ===
  message("📋 正在加载亲本数据...")
  
  if (filter_active) {
    sql <- "SELECT id, name FROM parents WHERE active = 1 ORDER BY id"
  } else {
    sql <- "SELECT id, name FROM parents ORDER BY id"
  }
  
  parents_df <- dbGetQuery(con, sql)
  
  if (nrow(parents_df) == 0) {
    stop("❌ 没有找到符合条件的亲本")
  }
  
  n <- nrow(parents_df)
  message(glue::glue("   ✅ 已加载 {n} 个亲本"))
  
  # === 生成空矩阵 ===
  parent_ids <- parents_df$id
  parent_names <- parents_df$name
  
  # 使用指定字段作为行列名
  labels <- if (name_field == "name") parent_names else parent_ids
  
  # 创建空矩阵
  mat <- matrix(
    "", 
    nrow = n, 
    ncol = n,
    dimnames = list(labels, labels)
  )
  
  message("   ✅ 矩阵框架创建完成")
  
  # === 返回结果 ===
  list(
    matrix = mat,
    parent_ids = parent_ids,
    parent_names = parent_names,
    n_parents = n
  )
}


# -----------------------------------------------------------------------------
# 核心功能：异步填充矩阵数据
# -----------------------------------------------------------------------------

#' 填充杂交矩阵数据
#'
#' @description
#' 从 crosses 表查询已存在的杂交组合，填充到矩阵中。
#' 支持批量查询和增量更新，提高性能。
#'
#' @param framework 列表，由 create_matrix_framework() 生成的矩阵框架
#' @param db_path 字符串，数据库路径
#' @param fill_value 字符串，用于填充的值，可选：
#'   - "batch": 批次名称（默认）
#'   - "status": 状态
#'   - "count": 种子数量
#'   - "mark": 简单标记（✓）
#'   - "name": 组合名称
#' @param batch_filter 可选，字符向量，仅显示特定批次的组合
#' @param show_reciprocal 逻辑值，是否显示反交，默认 TRUE
#'
#' @return 填充后的矩阵
#'
#' @export
#'
#' @examples
#' # 创建框架并填充数据
#' framework <- create_matrix_framework()
#' filled_matrix <- fill_matrix_data(framework)
#' print(filled_matrix[1:10, 1:10])
fill_matrix_data <- function(
    framework,
    db_path = NULL,
    fill_value = "batch",
    batch_filter = NULL,
    show_reciprocal = TRUE
) {
  if (is.null(db_path)) {
    db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
  }
  
  # === 参数验证 ===
  if (!file.exists(db_path)) {
    stop("❌ 数据库文件不存在：", db_path)
  }
  
  if (!fill_value %in% c("batch", "status", "count", "mark", "name")) {
    stop("❌ fill_value 必须是 'batch', 'status', 'count', 'mark' 或 'name'")
  }
  
  # === 连接数据库 ===
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  info <- dbGetQuery(con, "PRAGMA table_info(crosses)")
  has_recip <- any(info$name == "is_reciprocal")
  sel_cols <- "female_id, male_id, batch, status, seed_count, name"
  if (has_recip) sel_cols <- paste(sel_cols, ", is_reciprocal")
  
  # === 构建查询 ===
  message("🔍 正在查询杂交组合数据...")
  
  sql <- paste0("SELECT ", sel_cols, " FROM crosses WHERE 1=1")
  
  params <- list()
  
  # 添加批次过滤
  if (!is.null(batch_filter) && length(batch_filter) > 0) {
    placeholders <- paste(rep("?", length(batch_filter)), collapse = ", ")
    sql <- paste0(sql, " AND batch IN (", placeholders, ")")
    params <- as.list(batch_filter)
  }
  
  # 添加反交过滤
  if (!show_reciprocal && has_recip) {
    sql <- paste0(sql, " AND is_reciprocal = 0")
  }
  
  # 执行查询
  if (length(params) > 0) {
    crosses_df <- dbGetQuery(con, sql, params = params)
  } else {
    crosses_df <- dbGetQuery(con, sql)
  }
  
  n_crosses <- nrow(crosses_df)
  message(glue::glue("   ✅ 查询到 {n_crosses} 个杂交组合"))
  
  if (n_crosses == 0) {
    message("   ℹ️  无数据填充，返回空矩阵")
    return(framework$matrix)
  }
  
  # === 填充矩阵 ===
  message("💾 正在填充矩阵数据...")
  
  mat <- framework$matrix
  parent_ids <- framework$parent_ids
  
  # 创建 ID 到索引的映射
  id_to_idx <- setNames(seq_along(parent_ids), parent_ids)
  
  filled_count <- 0
  
  for (i in seq_len(nrow(crosses_df))) {
    row <- crosses_df[i, ]
    
    female_id <- row$female_id
    male_id <- row$male_id
    
    # 检查 ID 是否在框架中
    if (!female_id %in% names(id_to_idx) || !male_id %in% names(id_to_idx)) {
      next
    }
    
    row_idx <- id_to_idx[[female_id]]
    col_idx <- id_to_idx[[male_id]]
    
    # 确定填充值
    cell_value <- switch(
      fill_value,
      "batch" = as.character(row$batch),
      "status" = as.character(row$status),
      "count" = if (is.na(row$seed_count)) "" else as.character(row$seed_count),
      "mark" = "✓",
      "name" = as.character(row$name),
      ""
    )
    
    # 如果单元格已有值，追加（支持多批次）
    if (nzchar(mat[row_idx, col_idx])) {
      mat[row_idx, col_idx] <- paste(mat[row_idx, col_idx], cell_value, sep = "; ")
    } else {
      mat[row_idx, col_idx] <- cell_value
    }
    
    filled_count <- filled_count + 1
  }
  
  message(glue::glue("   ✅ 已填充 {filled_count} 个单元格"))
  
  return(mat)
}


# -----------------------------------------------------------------------------
# 核心功能：创建完整矩阵视图
# -----------------------------------------------------------------------------

#' 创建完整的杂交矩阵视图
#'
#' @description
#' 一步到位创建并填充杂交矩阵，包含框架生成和数据填充。
#'
#' @param db_path 字符串，数据库路径
#' @param filter_active 逻辑值，是否只显示活跃亲本
#' @param name_field 字符串，行列标签字段（"name" 或 "id"）
#' @param fill_value 字符串，填充值类型
#' @param batch_filter 可选，批次过滤
#' @param show_reciprocal 逻辑值，是否显示反交
#'
#' @return 填充后的矩阵
#'
#' @export
#'
#' @examples
#' # 快速创建矩阵视图
#' matrix <- create_cross_matrix_view()
#' print(matrix[1:10, 1:10])
#' 
#' # 仅显示特定批次
#' matrix <- create_cross_matrix_view(
#'   batch_filter = c("2025春季批次"),
#'   fill_value = "status"
#' )
create_cross_matrix_view <- function(
    db_path = NULL,
    filter_active = TRUE,
    name_field = "name",
    fill_value = "batch",
    batch_filter = NULL,
    show_reciprocal = TRUE
) {
  if (is.null(db_path)) {
    db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
  }
  
  message("\n", paste(rep("=", 60), collapse = ""))
  message("📊 创建杂交矩阵视图")
  message(paste(rep("=", 60), collapse = ""))
  
  # 步骤1：创建框架
  framework <- create_matrix_framework(
    db_path = db_path,
    filter_active = filter_active,
    name_field = name_field
  )
  
  # 步骤2：填充数据
  filled_matrix <- fill_matrix_data(
    framework = framework,
    db_path = db_path,
    fill_value = fill_value,
    batch_filter = batch_filter,
    show_reciprocal = show_reciprocal
  )
  
  message(paste(rep("=", 60), collapse = ""))
  message(glue::glue("✅ 矩阵视图创建完成 ({nrow(filled_matrix)} × {ncol(filled_matrix)})"))
  message(paste(rep("=", 60), collapse = ""), "\n")
  
  return(filled_matrix)
}


# -----------------------------------------------------------------------------
# 辅助功能：矩阵统计
# -----------------------------------------------------------------------------

#' 矩阵统计摘要
#'
#' @description
#' 统计矩阵中的填充情况和数据分布
#'
#' @param matrix 矩阵对象
#'
#' @return 数据框，包含统计信息
#'
#' @export
matrix_summary <- function(matrix) {
  
  total_cells <- nrow(matrix) * ncol(matrix)
  filled_cells <- sum(nzchar(matrix))
  empty_cells <- total_cells - filled_cells
  
  # 计算对角线（自交）
  diagonal_filled <- sum(nzchar(diag(matrix)))
  
  # 计算上三角和下三角
  upper_tri <- sum(nzchar(matrix[upper.tri(matrix)]))
  lower_tri <- sum(nzchar(matrix[lower.tri(matrix)]))
  
  summary_df <- data.frame(
    metric = c(
      "总单元格数",
      "已填充单元格",
      "空单元格",
      "填充率 (%)",
      "对角线填充",
      "上三角填充（正交）",
      "下三角填充（反交）"
    ),
    value = c(
      total_cells,
      filled_cells,
      empty_cells,
      round(filled_cells / total_cells * 100, 2),
      diagonal_filled,
      upper_tri,
      lower_tri
    ),
    stringsAsFactors = FALSE
  )
  
  return(summary_df)
}


# -----------------------------------------------------------------------------
# Shiny UI 组件
# -----------------------------------------------------------------------------

#' 创建矩阵视图 UI
#'
#' @description
#' 为 Shiny 应用创建矩阵视图的 UI 组件
#'
#' @param id 字符串，模块 ID
#'
#' @export
matrix_view_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("🧬 杂交组合矩阵视图"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        h4("📋 筛选选项"),
        
        checkboxInput(
          ns("filter_active"),
          "仅显示活跃亲本",
          value = TRUE
        ),
        
        selectInput(
          ns("name_field"),
          "行列标签：",
          choices = c("亲本名称" = "name", "亲本ID" = "id"),
          selected = "name"
        ),
        
        selectInput(
          ns("fill_value"),
          "显示内容：",
          choices = c(
            "批次名称" = "batch",
            "状态" = "status",
            "种子数量" = "count",
            "简单标记" = "mark",
            "组合名称" = "name"
          ),
          selected = "batch"
        ),
        
        selectInput(
          ns("batch_filter"),
          "批次选择：",
          choices = NULL,
          selected = NULL,
          multiple = FALSE,
          width = "100%"
        ),
        
        checkboxInput(
          ns("show_reciprocal"),
          "显示反交",
          value = TRUE
        ),
        
        hr(),
        
        actionButton(
          ns("refresh"),
          "🔄 刷新矩阵",
          class = "btn-primary",
          width = "100%"
        ),
        
        hr(),
        
        h4("📊 矩阵统计"),
        tableOutput(ns("summary_table"))
      ),
      
      mainPanel(
        width = 9,
        
        tabsetPanel(
          tabPanel(
            "矩阵视图",
            br(),
            rHandsontableOutput(ns("matrix_table"), height = "600px")
          ),
          
          tabPanel(
            "数据表格",
            br(),
            DT::dataTableOutput(ns("data_table"))
          )
        )
      )
    )
  )
}


# -----------------------------------------------------------------------------
# Shiny Server 逻辑
# -----------------------------------------------------------------------------

#' 创建矩阵视图 Server
#'
#' @description
#' 为 Shiny 应用创建矩阵视图的 Server 逻辑
#'
#' @param id 字符串，模块 ID
#' @param db_path 字符串，数据库路径
#'
#' @export
matrix_view_server <- function(id, db_path = NULL) {
  if (is.null(db_path)) {
    db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
  }
  moduleServer(id, function(input, output, session) {
    
    load_batches <- function() {
      tryCatch({
        con <- dbConnect(SQLite(), db_path)
        on.exit(dbDisconnect(con), add = TRUE)
        if (dbExistsTable(con, "crosses")) {
          batches <- dbGetQuery(con, "SELECT batch FROM crosses GROUP BY batch ORDER BY MAX(rowid) DESC")
          batches$batch
        } else {
          character(0)
        }
      }, error = function(e) {
        character(0)
      })
    }
    
    observe({
      batches <- load_batches()
      if (length(batches) > 0) {
        updateSelectInput(session, "batch_filter", choices = c("全部批次" = "__ALL__", batches), selected = "__ALL__")
      }
    })
    
    batches_rx <- reactivePoll(2000, session, function() {
      suppressWarnings(file.info(db_path)$mtime)
    }, function() {
      load_batches()
    })
    
    observeEvent(batches_rx(), {
      batches <- batches_rx()
      if (length(batches) > 0) {
        curr <- isolate(input$batch_filter)
        all_choice <- c("全部批次" = "__ALL__", batches)
        selected <- if (!is.null(curr) && curr %in% all_choice) curr else "__ALL__"
        updateSelectInput(session, "batch_filter", choices = all_choice, selected = selected)
      }
    })
    
    # === 响应式数据 ===
    matrix_data <- reactive({
      input$refresh
      input$filter_active
      input$name_field
      input$fill_value
      input$show_reciprocal
      input$batch_filter
      mat <- create_cross_matrix_view(
        db_path = db_path,
        filter_active = input$filter_active,
        name_field = input$name_field,
        fill_value = input$fill_value,
        batch_filter = if (!is.null(input$batch_filter) && nzchar(input$batch_filter) && input$batch_filter != "__ALL__") input$batch_filter else NULL,
        show_reciprocal = input$show_reciprocal
      )
      
      if (!is.null(dim(mat)) && nrow(mat) > 0 && ncol(mat) > 0) {
        row_keep <- apply(mat, 1, function(r) any(nzchar(r)))
        col_keep <- apply(mat, 2, function(c) any(nzchar(c)))
        mat <- mat[row_keep, col_keep, drop = FALSE]
      }
      
      mat
    })
    
    # === 渲染矩阵表格 ===
    output$matrix_table <- renderRHandsontable({
      mat <- matrix_data()
      if (is.null(dim(mat)) || nrow(mat) == 0 || ncol(mat) == 0) return(NULL)
      rn <- rownames(mat)
      max_rn <- if (length(rn) > 0) suppressWarnings(max(nchar(rn), na.rm = TRUE)) else 0
      row_hdr_w <- max(140, min(400, max_rn * 10))
      c1_vals <- mat[, 1, drop = TRUE]
      max_c1 <- suppressWarnings(max(nchar(c1_vals), na.rm = TRUE))
      colw <- rep(100, ncol(mat))
      colw[1] <- max(140, min(300, max_c1 * 9))
      ht <- rhandsontable(mat, readOnly = TRUE)
      ht <- hot_cols(ht, columnSorting = FALSE, colWidths = colw)
      ht <- hot_table(ht, rowHeaderWidth = row_hdr_w)
      ht <- hot_context_menu(ht, allowRowEdit = FALSE, allowColEdit = FALSE)
      ht
    })
    
    # === 渲染数据表格 ===
    output$data_table <- DT::renderDataTable({
      mat <- matrix_data()
      if (is.null(dim(mat)) || nrow(mat) == 0 || ncol(mat) == 0) {
        return(DT::datatable(
          data.frame(母本=character(0), 父本=character(0), 信息=character(0)),
          options = list(pageLength = 20, searching = TRUE, ordering = TRUE),
          rownames = FALSE
        ))
      }
      
      # 转换为长格式
      mat_df <- as.data.frame(mat, stringsAsFactors = FALSE)
      mat_df$母本 <- rownames(mat_df)
      
      mat_long <- tidyr::pivot_longer(
        mat_df,
        cols = -母本,
        names_to = "父本",
        values_to = "批次"
      ) %>%
        filter(nzchar(批次))
      
      DT::datatable(
        mat_long,
        options = list(
          pageLength = 20,
          searching = TRUE,
          ordering = TRUE
        ),
        rownames = FALSE
      )
    })
    
    # === 渲染统计摘要 ===
    output$summary_table <- renderTable({
      mat <- matrix_data()
      matrix_summary(mat)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
  })
}
