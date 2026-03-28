# =============================================================================
# Shiny 应用：杂交组合配置（四步向导）
# =============================================================================

library(shiny)
library(DT)
library(DBI)
library(RSQLite)
library(dplyr)
library(glue)
if (!requireNamespace("rhandsontable", quietly = TRUE)) {
  stop("缺少依赖包：rhandsontable，请先安装：install.packages('rhandsontable')")
} else {
  library(rhandsontable)
}

# 项目根目录检测
tryCatch({
  script_path <- normalizePath(sys.frame(1)$ofile, mustWork = FALSE)
  if (file.exists(script_path)) {
    app_dir <- dirname(script_path)
    project_root <- dirname(app_dir)
  } else {
    project_root <- getwd()
  }
}, error = function(e) { project_root <<- getwd() })
if (basename(project_root) %in% c("apps", "scripts")) project_root <- dirname(project_root)

# 加载配置
tryCatch({
  config_path <- file.path(project_root, "config", "config.R")
  if (file.exists(config_path)) {
    source(config_path)
  }
}, error = function(e) {
  message("配置文件加载失败：", e$message)
})

# 加载模块
mod_cross_path <- file.path(project_root, "R", "mod_cross.R")
if (!file.exists(mod_cross_path)) stop("❌ 找不到文件：", mod_cross_path)
source(mod_cross_path)
mod_analysis_path <- file.path(project_root, "R", "mod_analysis.R")
if (file.exists(mod_analysis_path)) source(mod_analysis_path)

# UI
ui <- navbarPage("杂交组合配置", id = "steps",
  tabPanel("A 母本",
    fluidPage(
      DT::dataTableOutput("tbl_females"),
      actionButton("confirm_females", "母本确认", class = "btn-primary")
    )
  ),
  tabPanel("A 已选母本",
    fluidPage(
      DT::dataTableOutput("tbl_females_selected"),
      actionButton("remove_females", "移除选中", class = "btn-warning"),
      actionButton("goto_males", "进入父本选择", class = "btn-primary")
    )
  ),
  tabPanel("B 父本",
    fluidPage(
      checkboxInput("only_available", "只显示可配父本", FALSE),
      DT::dataTableOutput("tbl_males"),
      actionButton("confirm_males", "父本确认", class = "btn-primary")
    )
  ),
  tabPanel("B 已选父本",
    fluidPage(
      DT::dataTableOutput("tbl_males_selected"),
      actionButton("remove_males", "移除选中", class = "btn-warning"),
      actionButton("goto_matrix", "进入组合矩阵", class = "btn-success")
    )
  ),
  tabPanel("C 组合矩阵",
    fluidPage(
      tags$head(tags$style(HTML("
        .handsontable .existing { background-color:#e2e3e5 !important;} 
        .handsontable .diagonal { background-color:#f8d7da !important;}
        /* DataTables compacted rows */
        table.dataTable tbody td, table.dataTable tbody th {
            padding: 4px 8px !important;
            line-height: 1.2 !important;
        }
        /* DataTables filter inputs visibility */
        .dataTables_wrapper input {
            color: #333 !important;
            background-color: #fff !important;
            border: 1px solid #ccc !important;
            padding: 2px 5px !important;
        }
        /* Ensure column filter inputs show typed text clearly */
        table.dataTable thead th input[type='text'],
        table.dataTable thead td input[type='text'] {
            color: #111 !important;
            background-color: #fff !important;
            border: 1px solid #999 !important;
            width: 100% !important;
            min-width: 80px !important;
            height: 24px !important;
            padding: 2px 6px !important;
            box-sizing: border-box !important;
        }
        table.dataTable thead th select,
        table.dataTable thead td select {
            color: #111 !important;
            background-color: #fff !important;
            border: 1px solid #999 !important;
            width: 100% !important;
            min-width: 80px !important;
            height: 24px !important;
            padding: 2px 6px !important;
            box-sizing: border-box !important;
        }
      "))),
      div(style = 'overflow-x: hidden;', rHandsontableOutput("matrix")),
      verbatimTextOutput("matrix_summary"),
      textInput("batch", "批次名", value = format(Sys.Date(), "%Y春季")),
      textInput("memo", "组合特点（必填）", value = SoyCross$config$cross_matrix$default_memo, placeholder = "请输入组合特点"),
      numericInput("limit", "生成数量 (可选)", value = NA, min = 1),
      actionButton("run_write", "写入数据库", class = "btn-danger"),
      verbatimTextOutput("run_summary"),
      DT::dataTableOutput("generated_table"),
      DT::dataTableOutput("skipped_table")
    )
  ),
  tabPanel("D 批次管理",
    fluidPage(
      fluidRow(
        column(4, style = "border-right: 1px solid #ddd; padding-right: 20px;",
          h4("批次列表"),
          div(style="margin-bottom: 10px;",
            actionButton("refresh_batches", "刷新", icon = icon("refresh"), class = "btn-info btn-sm"),
            actionButton("delete_batch_btn", "删除选中批次", icon = icon("trash"), class = "btn-danger btn-sm")
          ),
          DT::dataTableOutput("batch_list_table")
        ),
        column(8,
          h4("批次详情"),
          DT::dataTableOutput("batch_detail_table")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  raw_db_path <- if (exists("SoyCross") && !is.null(SoyCross$config$paths$db_path)) SoyCross$config$paths$db_path else NA
  if (is.na(raw_db_path) || !nzchar(raw_db_path)) {
    db_path <- file.path(project_root, "data", "db", "soy_cross.db")
  } else {
    if (grepl("^[A-Za-z]:", raw_db_path) || startsWith(raw_db_path, "/") || startsWith(raw_db_path, "\\\\")) {
      db_path <- raw_db_path
    } else {
      db_path <- file.path(project_root, raw_db_path)
    }
  }
  if (!dir.exists(dirname(db_path))) dir.create(dirname(db_path), recursive = TRUE)

  parents <- reactive({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con), add = TRUE)
    dbGetQuery(con, "SELECT * FROM parents WHERE active=1")
  })
  get_display_cols <- function(df) {
    names(df)
  }

  females_sel <- reactiveVal(character(0))
  males_sel <- reactiveVal(character(0))
  selected_pairs <- reactiveVal(data.frame(female_id=character(0), male_id=character(0)))

  # A 母本
  output$tbl_females <- DT::renderDataTable({
    df <- parents(); cols <- get_display_cols(df)
    d <- df[, cols, drop=FALSE]
    d[] <- lapply(d, function(x) if (is.factor(x)) as.character(x) else x)
    num_idx <- which(sapply(d, is.numeric)) - 1L
    char_idx <- which(!sapply(d, is.numeric)) - 1L
    DT::datatable(
      d,
      selection = "multiple",
      filter = "top",
      class = "compact stripe hover",
      extensions = c("SearchPanes", "SearchBuilder"),
      options = list(
        pageLength = 20,
        scrollY = "60vh",
        scrollCollapse = TRUE,
        searchHighlight = TRUE,
        dom = "Plfrtip",
        columnDefs = list(
          list(searchPanes = list(show = TRUE), targets = char_idx),
          list(searchPanes = list(show = FALSE), targets = num_idx)
        ),
        search = list(regex = TRUE, smart = TRUE),
        initComplete = DT::JS(paste0(
          "function() {",
          "  var api = this.api();",
          "  var table = api.table().node();",
          "  var $table = $(table);",
          "  var filterRow = $table.find('thead tr').eq(1);",
          "  if (filterRow.length === 0) { return; }",
          "  var charIdx = ", "[", paste(char_idx, collapse = ","), "]", ";",
          "  var numIdx = ", "[", paste(num_idx, collapse = ","), "]", ";",
          "  api.columns().every(function(){",
          "    var col = this;",
          "    var idx = col.index();",
          "    if (charIdx.indexOf(idx) !== -1 || numIdx.indexOf(idx) !== -1) {",
          "      var cell = filterRow.find('th').eq(idx);",
          "      cell.empty();",
          "      var select = $('<select class=\"dt-col-filter\"><option value=\"\">All</option></select>')",
          "        .appendTo(cell)",
          "        .on('change', function(){",
          "          var val = $(this).val();",
          "          if (val) {",
          "            col.search('^' + $.fn.dataTable.util.escapeRegex(val) + '$', true, false).draw();",
          "          } else {",
          "            col.search('', true, false).draw();",
          "          }",
          "        });",
          "      col.data().unique().sort().each(function(d){",
          "        if (d === null) d = '';",
          "        select.append('<option value=\"' + d + '\">' + d + '</option>');",
          "      });",
          "    }",
          "  });",
          "}"
        ))
      )
    )
  })
  observeEvent(input$confirm_females, {
    df <- parents(); s <- input$tbl_females_rows_selected
    females_sel(df$id[s])
    updateTabsetPanel(session, "steps", selected = "A 已选母本")
  })
  output$tbl_females_selected <- DT::renderDataTable({
    df <- parents(); ids <- females_sel();
    DT::datatable(df[df$id %in% ids, , drop=FALSE], selection = "multiple", class = "compact stripe hover", options = list(pageLength=10))
  })
  observeEvent(input$remove_females, {
    df <- parents(); ids <- females_sel(); s <- input$tbl_females_selected_rows_selected
    if (length(s) > 0) {
      df_sel <- df[df$id %in% ids, , drop=FALSE]
      ids_to_remove <- df_sel$id[s]
      females_sel(setdiff(ids, ids_to_remove))
    }
  })
  observeEvent(input$goto_males, {
    updateTabsetPanel(session, "steps", selected = "B 父本")
  })

  # B 父本 + 冲突检测 + 禁止自交
  males_data <- reactive({
    df_all <- parents()
    f_ids <- females_sel()  # 已选母本的ID
    
    # 先获取已选母本的名称（于过滤前）
    moms <- df_all$name[df_all$id %in% f_ids]
    if (length(moms)==0 || !exists("find_unused_partners")) { 
      df_all$is_conflict <- FALSE
      # 这里也要排除自交（母本不能是父本）
      df_all <- df_all[!(df_all$id %in% f_ids), , drop = FALSE]
      return(df_all)
    }
    
    unused_list <- lapply(moms, function(m) tryCatch(find_unused_partners(m, role="female", db_path=db_path)$id, error=function(e) df_all$id))
    common_unused <- Reduce(intersect, unused_list)
    df_all$is_conflict <- !(df_all$id %in% common_unused)
    
    # 禁止自交：排除已选母本中的ID
    df_all <- df_all[!(df_all$id %in% f_ids), , drop = FALSE]
    
    if (isTRUE(input$only_available)) df_all <- df_all[!df_all$is_conflict, , drop=FALSE]
    df_all
  })
  output$tbl_males <- DT::renderDataTable({
    df <- males_data(); cols <- get_display_cols(df)
    opts <- list(pageLength = 20, scrollY = '60vh', scrollCollapse = TRUE, searchHighlight = TRUE)
    if ("is_conflict" %in% cols) {
      conf_idx <- which(cols == "is_conflict") - 1
      opts$columnDefs <- list(list(visible = FALSE, targets = conf_idx))
    }
    dt <- DT::datatable(df[, cols, drop=FALSE], selection="multiple", filter="top", class = "compact stripe hover", options=opts)
    if ("is_conflict" %in% names(df)) {
      dt <- dt %>% DT::formatStyle('id', valueColumns='is_conflict', target='row', backgroundColor=DT::styleEqual(c(TRUE,FALSE), c('#ffeeba','white')))
    }
    dt
  })
  observeEvent(input$confirm_males, {
    df <- males_data(); s <- input$tbl_males_rows_selected
    males_sel(df$id[s])
    updateTabsetPanel(session, "steps", selected = "B 已选父本")
  })
  output$tbl_males_selected <- DT::renderDataTable({
    df <- parents(); ids <- males_sel();
    DT::datatable(df[df$id %in% ids, , drop=FALSE], selection = "multiple", class = "compact stripe hover", options = list(pageLength=10))
  })
  observeEvent(input$remove_males, {
    df <- parents(); ids <- males_sel(); s <- input$tbl_males_selected_rows_selected
    if (length(s) > 0) {
      df_sel <- df[df$id %in% ids, , drop=FALSE]
      ids_to_remove <- df_sel$id[s]
      males_sel(setdiff(ids, ids_to_remove))
    }
  })
  observeEvent(input$goto_matrix, {
    updateTabsetPanel(session, "steps", selected = "C 组合矩阵")
  })

  # C 矩阵
  # 矩阵：行/列显示亲本名称；已存在（灰），可配置（绿），'-' 忽略（白），自交禁用（粉）
  output$matrix <- renderRHandsontable({
    f_ids <- females_sel(); m_ids <- males_sel(); if (length(f_ids)==0 || length(m_ids)==0) return(NULL)
    dfp <- parents(); id2name <- setNames(dfp$name, dfp$id)
    f_names <- unname(id2name[f_ids]); m_names <- unname(id2name[m_ids])
    con <- dbConnect(SQLite(), db_path); on.exit(dbDisconnect(con), add = TRUE)
    in_f <- paste(sprintf("'%s'", f_ids), collapse = ",")
    in_m <- paste(sprintf("'%s'", m_ids), collapse = ",")
    sql <- paste0("SELECT female_id, male_id, name FROM crosses WHERE (female_id IN (", in_f, ") AND male_id IN (", in_m, ")) OR (female_id IN (", in_m, ") AND male_id IN (", in_f, "))")
    ex_df <- dbGetQuery(con, sql)
    
    # 生成选择矩阵（默认全选 "TRUE"）
    # 增加第一列为 "Name" (母本名称)
    # 增加第一行为 "ID" (父本 ID)
    
    # 1. 构造数据
    # Main Body: Mother Name + Checkboxes
    mat_vals <- matrix("TRUE", nrow=length(f_ids), ncol=length(m_ids))
    df_main <- data.frame(Name = f_names, mat_vals, stringsAsFactors = FALSE, check.names = FALSE)
    # 列名设为 "Name" + 父本 IDs (作为表头显示)
    colnames(df_main) <- c("Name", m_ids)
    
    # Top Row: "" + Father Names (作为第一行数据显示)
    # 第一列留空或设为 "Father Name"
    df_top <- as.data.frame(t(c("", m_names)), stringsAsFactors = FALSE)
    colnames(df_top) <- c("Name", m_ids)
    
    # Combine
    df_mat <- rbind(df_top, df_main)
    
    # 行名为母本 ID (第一行设为 "")
    rownames(df_mat) <- c("", f_ids)
    
    # 2. 设置单元格属性
    cell_props <- list()
    
    # (A) Top Row (Row 0): Header style
    for (c in 0:(ncol(df_mat)-1)) {
       cell_props[[length(cell_props)+1]] <- list(row = 0, col = c, readOnly = TRUE, className = 'htCenter htMiddle header-cell', type = 'text')
    }

    # (B) First Column (Col 0) - starting from Row 1: Name style
    for (r in 1:(nrow(df_mat)-1)) {
       cell_props[[length(cell_props)+1]] <- list(row = r, col = 0, readOnly = TRUE, className = 'htCenter htMiddle name-cell')
    }

    if (nrow(ex_df)>0) {
      for (i in seq_len(nrow(ex_df))) {
        fi <- ex_df$female_id[i]; mi <- ex_df$male_id[i]; nm <- ex_df$name[i]
        
        # 查找坐标 (fi in f_ids, mi in m_ids)
        r_idx <- match(fi, f_ids)
        c_idx <- match(mi, m_ids)
        
        if (!is.na(r_idx) && !is.na(c_idx)) {
          # df_mat Row Index: r_idx (Mother Index) + 1 (Header Row) -> 对应 Row r_idx
          # df_mat Col Index: c_idx (Father Index) + 1 (Name Col) -> 对应 Col c_idx
          # Handsontable is 0-based.
          # Row 0 is ID. Row 1 is Mother 1.
          # r_idx 1 (Mother 1) -> Row 1. Correct.
          
          df_mat[r_idx + 1, c_idx + 1] <- nm
          cell_props[[length(cell_props)+1]] <- list(row = r_idx, col = c_idx, type = 'text', readOnly = TRUE, className = 'existing')
        }
      }
    }
    
    # 重新遍历以处理对角线（自交）
    for (i in seq_along(f_ids)) {
      for (j in seq_along(m_ids)) {
        if (f_ids[i] == m_ids[j]) {
          # df_mat Row: i + 1, Col: j + 1
          df_mat[i + 1, j + 1] <- "FALSE" # 自交默认不选
          cell_props[[length(cell_props)+1]] <- list(row = i, col = j, readOnly = TRUE, className = 'diagonal')
        }
      }
    }

    # 创建表
    ht <- rhandsontable::rhandsontable(df_mat, rowHeaders = c("", f_ids), cell = cell_props)

    # 列设置为 checkbox (处理 "TRUE"/"FALSE" 字符串)
    # 第一列是 Name (Text), 其他是 Checkbox
    widths <- c(120, pmax(80, pmin(300, nchar(m_names)*12)))
    
    # 其他列设置 Checkbox
    for (cn in m_ids) {
      ht <- rhandsontable::hot_col(ht, cn, type = 'checkbox', checkedTemplate = "TRUE", uncheckedTemplate = "FALSE")
    }
    
    ht <- rhandsontable::hot_cols(ht, manualColumnResize = TRUE, colWidths = widths)
    ht <- rhandsontable::hot_cols(ht, renderer = "function (instance, td, row, col, prop, value, cellProperties) {
      // 默认渲染逻辑
      if (cellProperties.type === 'checkbox') {
        Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
      } else {
        Handsontable.renderers.TextRenderer.apply(this, arguments);
      }
      
      // 样式覆盖
      td.style.color = 'black';
      
      if (row === 0) {
          // ID Row
          td.style.background = '#e6e6e6'; 
          td.style.fontWeight = 'bold';
          td.style.textAlign = 'center';
      } else if (col === 0) {
          // Name Column
          td.style.background = '#f2f2f2'; 
          td.style.fontWeight = 'bold';
      } else if (cellProperties.readOnly) {
         if (cellProperties.className && cellProperties.className.indexOf('diagonal') > -1) {
             td.style.background = '#f8d7da'; // Pink for diagonal
         } else if (cellProperties.className && cellProperties.className.indexOf('existing') > -1) {
             td.style.background = '#e2e3e5'; // Gray for existing
         }
      } else {
        td.style.background = 'white';
      }
    }")

    ht <- rhandsontable::hot_table(ht, height = 500, rowHeaderWidth = 120)
    
    ht
  })
  output$matrix_summary <- renderText({
    x <- input$matrix; if (is.null(x)) return("")
    
    # 统计当前选中 (Checkbox 为 "TRUE")
    m_raw <- hot_to_r(x)
    
    # 移除第一列 Name
    if (ncol(m_raw) > 1) m_raw <- m_raw[, -1, drop=FALSE]
    
    # 移除第一行 ID Header
    if (nrow(m_raw) > 1) m_raw <- m_raw[-1, , drop=FALSE]
    
    m_mat <- as.matrix(m_raw)
    n_selected <- sum(m_mat == "TRUE", na.rm = TRUE)
    
    # 重新计算分布
    f_ids <- females_sel(); m_ids <- males_sel();
    con <- dbConnect(SQLite(), db_path); on.exit(dbDisconnect(con), add = TRUE)
    in_f <- paste(sprintf("'%s'", f_ids), collapse = ",")
    in_m <- paste(sprintf("'%s'", m_ids), collapse = ",")
    
    # 只查询正交匹配 (Row=Female, Col=Male)
    sql <- paste0("SELECT female_id, male_id FROM crosses WHERE female_id IN (", in_f, ") AND male_id IN (", in_m, ")")
    ex_df <- dbGetQuery(con, sql)
    ex_df <- unique(ex_df) # 去重
    
    n_db_match <- nrow(ex_df) # DB中匹配当前矩阵位置的记录数 (包含可能的自交脏数据)
    
    # 计算自交 (Row ID == Col ID)
    diag_ids <- intersect(f_ids, m_ids)
    n_diag_total <- length(diag_ids) # 理论上的自交格数 (粉色)
    
    # 检查 DB 中有多少自交记录
    n_diag_in_db <- 0
    if (n_db_match > 0 && n_diag_total > 0) {
      for (d in diag_ids) {
        if (any(ex_df$female_id == d & ex_df$male_id == d)) {
          n_diag_in_db <- n_diag_in_db + 1
        }
      }
    }
    
    # 分类统计：
    # 1. 已存在 (灰色): DB中存在 且 非自交
    n_gray <- n_db_match - n_diag_in_db
    
    # 2. 自交 (粉色): 无论是否在 DB 中，界面上都优先显示为自交
    n_pink <- n_diag_total
    
    # 3. 可配置 (白色): 总数 - 灰色 - 粉色
    total_cells <- length(f_ids) * length(m_ids)
    n_white <- total_cells - n_gray - n_pink
    
    glue("可配置: {n_white}，已存在: {n_gray}，自交: {n_pink} | 当前选中: {n_selected}")
  })
  observeEvent(input$confirm_matrix, {
    # 已移除确认按钮，保留空逻辑占位避免错误引用
  })

  # D 执行
  observeEvent(input$run_write, {
    req(input$batch)
    # 验证 memo 不为空
    if (!nzchar(input$memo)) {
      showNotification("组合特点不能为空，请填写组合特点", type = "error")
      return()
    }
    x <- input$matrix; if (is.null(x)) { showNotification("矩阵为空", type="warning"); return(NULL) }
    
    m_raw <- hot_to_r(x)
    
    # 移除第一行 ID Header
    if (nrow(m_raw) > 1) m_raw <- m_raw[-1, , drop=FALSE]
    
    f_ids_from_row <- rownames(m_raw) # 行名为母本ID
    
    # 移除第一列 Name
    if (ncol(m_raw) > 1) m_raw <- m_raw[, -1, drop=FALSE]
    m <- as.matrix(m_raw)
    
    m_names <- colnames(m)
    dfp <- parents(); name2id <- setNames(dfp$id, dfp$name)
    
    # 查找选中的组合（值为 "TRUE" 的单元格）
    pairs_idx <- which(m == "TRUE", arr.ind=TRUE)
    if (nrow(pairs_idx)==0) { showNotification("无可配置组合", type="warning"); return(NULL) }
    
    # pairs_idx[,1] 是行索引 (对应 f_ids_from_row)
    # pairs_idx[,2] 是列索引 (对应 m_names -> 现已改为 ID)
    
    # 注意：m_names 现在已经是 ID 了 (在 renderRHandsontable 中 colnames(df_main) <- c("Name", m_ids))
    # 所以不需要再用 name2id 进行转换
    
    pairs <- data.frame(
      female_id = f_ids_from_row[pairs_idx[,1]],
      male_id   = m_names[pairs_idx[,2]],
      stringsAsFactors = FALSE
    ) %>% dplyr::filter(female_id != male_id)
    
    # 最终检查：确保没有自交记录（应该为空）
    if (any(pairs$female_id == pairs$male_id)) {
      showNotification("错误：检测到自交组合，无法写入！", type="error")
      return(NULL)
    }
    
    # 调用 R/mod_cross.R 中的封装函数
    tryCatch({
      res <- create_specific_cross_plan(
        batch_name = input$batch,
        pairs = pairs,
        db_path = db_path,
        include_reciprocal = TRUE, # 默认生成反交
        limit = input$limit,
        memo = input$memo
      )
      
      output$run_summary <- renderText(glue(
        "写入成功：正交 {res$summary$inserted_n}，反交 {res$summary$reciprocal_added}，总计 {res$summary$total_inserted}；",
        "跳过：正交 {res$summary$skipped_direct}，反交 {res$summary$skipped_recip}"
      ))
      
      output$generated_table <- DT::renderDataTable({ 
        DT::datatable(res$new_crosses, class = "compact stripe hover", options=list(pageLength=20)) 
      })
      
      output$skipped_table <- DT::renderDataTable({ 
        DT::datatable(res$skipped, class = "compact stripe hover", options=list(pageLength=20)) 
      })
      
      if (res$db_updated) {
        showNotification("写入成功", type="message")
        load_batches()
        df <- batches_df()
        if (!is.null(df) && nrow(df) > 0) {
          choices <- df$batch
          selected <- if (!is.null(input$batch) && input$batch %in% choices) input$batch else choices[1]
          updateSelectInput(session, "gen_batch", choices = choices, selected = selected)
        }
        new_batch_to_select(input$batch)
      } else {
        showNotification("未写入任何数据（可能全部已存在）", type="warning")
      }
      
    }, error = function(e) {
      output$run_summary <- renderText(paste("错误：", e$message))
      showNotification(paste("写入失败：", e$message), type="error")
    })
  })

  # D 批次管理
  # ------------------------------------------------------------------
  
  # 批次列表数据
  batches_df <- reactiveVal()
  
  load_batches <- function() {
    tryCatch({
      # 检查是否有 summarize_cross_batches_db 函数，如果没有则手动查询
      if (exists("summarize_cross_batches_db")) {
        df <- summarize_cross_batches_db(db_path = db_path)
      } else {
        con <- dbConnect(SQLite(), db_path)
        on.exit(dbDisconnect(con), add = TRUE)
        df <- dbGetQuery(con, "
          SELECT 
            batch,
            COUNT(*) as total_crosses,
            SUM(CASE WHEN is_reciprocal = 0 THEN 1 ELSE 0 END) as direct_crosses,
            SUM(CASE WHEN is_reciprocal = 1 THEN 1 ELSE 0 END) as reciprocal_crosses,
            MAX(updated_at) as last_updated
          FROM crosses
          GROUP BY batch ORDER BY last_updated DESC
        ")
      }
      batches_df(df)
    }, error = function(e) {
      showNotification(paste("加载批次失败:", e$message), type = "error")
    })
  }
  
  # 初始化加载
  observe({
    load_batches()
  })
  
  observeEvent(input$refresh_batches, {
    load_batches()
  })
  
  batches_rx <- reactivePoll(2000, session, function() {
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    if (!DBI::dbExistsTable(con, "crosses")) return("")
    val <- DBI::dbGetQuery(con, "SELECT MAX(updated_at) AS t FROM crosses")$t
    if (length(val) == 0 || is.na(val)) "" else as.character(val[1])
  }, function() {
    tryCatch({
      if (exists("summarize_cross_batches_db")) {
        summarize_cross_batches_db(db_path = db_path)
      } else {
        con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
        on.exit(DBI::dbDisconnect(con), add = TRUE)
        DBI::dbGetQuery(con, "
          SELECT 
            batch,
            COUNT(*) as total_crosses,
            SUM(CASE WHEN is_reciprocal = 0 THEN 1 ELSE 0 END) as direct_crosses,
            SUM(CASE WHEN is_reciprocal = 1 THEN 1 ELSE 0 END) as reciprocal_crosses,
            MAX(updated_at) as last_updated
          FROM crosses
          GROUP BY batch ORDER BY last_updated DESC
        ")
      }
    }, error = function(e) {
      data.frame()
    })
  })
  
  observeEvent(batches_rx(), {
    df <- batches_rx()
    if (is.data.frame(df)) batches_df(df)
  })
  
  # 渲染批次列表
  output$batch_list_table <- DT::renderDataTable({
    df <- batches_df()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    # 只保留：批次、正交数、更新时间
    # 假设 df 列名为: batch, total_crosses, direct_crosses, reciprocal_crosses, last_updated...
    # 我们需要确认列名是否存在，避免报错
    
    cols_to_show <- c("batch", "direct_crosses", "last_updated")
    # 如果是 summarize_cross_batches_db 返回的，列名应该是这些
    # 如果是 fallback SQL 返回的，也是这些
    
    # 简单的防御性检查
    available_cols <- intersect(cols_to_show, names(df))
    df_show <- df[, available_cols, drop = FALSE]
    
    DT::datatable(df_show, 
                  selection = "single", 
                  class = "compact stripe hover", 
                  colnames = c("批次", "正交数", "更新时间"),
                  rownames = FALSE,
                  options = list(pageLength = 15, dom = 'ftp', autoWidth = FALSE))
  })
  
  # 渲染批次详情
  output$batch_detail_table <- DT::renderDataTable({
    s <- input$batch_list_table_rows_selected
    if (length(s) == 0) return(NULL)
    
    df_b <- batches_df()
    batch_name <- df_b$batch[s]
    
    if (exists("get_crosses_by_batch")) {
      details <- get_crosses_by_batch(batch = batch_name, db_path = db_path, include_reciprocal = TRUE)
    } else {
      con <- dbConnect(SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      details <- dbGetQuery(con, "SELECT * FROM crosses WHERE batch = ?", params = list(batch_name))
    }
    DT::datatable(details, class = "compact stripe hover", options = list(pageLength = 15))
  })
  
  # 删除按钮
  observeEvent(input$delete_batch_btn, {
    s <- input$batch_list_table_rows_selected
    if (length(s) == 0) {
      showNotification("请先选择一个批次", type = "warning")
      return()
    }
    df_b <- batches_df()
    batch_name <- df_b$batch[s]
    showModal(modalDialog(
      title = "确认删除",
      tagList(
        p(paste0("确定要删除批次 '", batch_name, "' 吗？此操作将删除该批次下所有记录，且不可恢复！")),
        textInput("confirm_delete_text", "请输入批次名称以确认删除", value = "")
      ),
      footer = tagList(
        modalButton("取消"),
        actionButton("confirm_delete_batch", "确认删除", class = "btn-danger")
      )
    ))
  })
  
  # 确认删除
  observeEvent(input$confirm_delete_batch, {
    s <- input$batch_list_table_rows_selected
    if (length(s) == 0) return()
    df_b <- batches_df()
    batch_name <- df_b$batch[s]
    typed <- trimws(input$confirm_delete_text %||% "")
    if (!identical(typed, trimws(batch_name))) {
      showNotification("输入的批次名称不匹配，未执行删除", type = "error")
      return()
    }
    removeModal()
    tryCatch({
      con <- dbConnect(SQLite(), db_path)
      dbExecute(con, "DELETE FROM crosses WHERE batch = ?", params = list(batch_name))
      dbDisconnect(con)
      showNotification(paste("批次", batch_name, "已删除"), type = "message")
      load_batches() # 刷新列表
    }, error = function(e) {
      showNotification(paste("删除失败:", e$message), type = "error")
    })
  })

  # E 生成采集簿
  # ------------------------------------------------------------------
  
  # 动态更新批次选择
  observe({
    df_b <- batches_df()
    if (!is.null(df_b) && nrow(df_b) > 0) {
      choices <- df_b$batch
      prefer <- new_batch_to_select()
      if (!is.null(prefer) && prefer %in% choices) {
        updateSelectInput(session, "gen_batch", choices = choices, selected = prefer)
        new_batch_to_select(NULL)
      } else {
        curr <- isolate(input$gen_batch)
        selected <- if (!is.null(curr) && curr %in% choices) curr else choices[1]
        updateSelectInput(session, "gen_batch", choices = choices, selected = selected)
      }
    }
  })
  
  store_gen <- reactiveValues(my_combi = NULL, planted = NULL)
  new_batch_to_select <- reactiveVal(NULL)
  
  # 生成预览
  observeEvent(input$btn_calc_preview, {
    req(input$gen_batch)
    
    tryCatch({
      # 1. 获取数据
      if (exists("get_crosses_by_batch")) {
        # 尝试传递 db_path，如果函数支持
        # 根据 run_cross_app.R line 557 的用法，它支持 db_path
        mycross <- get_crosses_by_batch(batch = input$gen_batch, db_path = db_path) 
      } else {
        con <- dbConnect(SQLite(), db_path)
        on.exit(dbDisconnect(con), add = TRUE)
        mycross <- dbGetQuery(con, "SELECT * FROM crosses WHERE batch = ?", params = list(input$gen_batch))
      }
      
      if (nrow(mycross) == 0) {
        showNotification("该批次无数据", type = "warning")
        return()
      }
      
      # join parents info
      if (exists("join_cross_parents")) {
        mydata <- join_cross_parents(mycross)
      } else {
        df_p <- parents()
        # 简单的合并兜底
        mydata <- merge(mycross, df_p, by.x="female_id", by.y="id", suffixes=c("", "_f"))
        mydata <- merge(mydata, df_p, by.x="male_id", by.y="id", suffixes=c("", "_m"))
      }
      
      # 2. 生成组合编号
      if (exists("get_combination")) {
        my_combi <- get_combination(
          data = mydata,
          prefix = input$gen_prefix,
          startN = input$gen_start_n,
          digits = input$gen_digits,
          only = TRUE,
          order = FALSE
        )
        store_gen$my_combi <- my_combi
      } else {
        stop("找不到 get_combination 函数")
      }
      
      # 3. 排图
      if (exists("planting")) {
        planted <- planting(
          mydata = my_combi,
          place = input$gen_place,
          rows = input$gen_rows,
          rp = input$gen_rp,
          interval = input$gen_interval
        )
        store_gen$planted <- planted
      } else {
        stop("找不到 planting 函数")
      }
      
      showNotification("预览生成成功", type = "message")
      
    }, error = function(e) {
      showNotification(paste("生成失败:", e$message), type = "error")
    })
  })
  
  # 渲染表格
  output$tbl_preview_combi <- DT::renderDataTable({
    req(store_gen$my_combi)
    DT::datatable(store_gen$my_combi, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$tbl_preview_plant <- DT::renderDataTable({
    req(store_gen$planted)
    DT::datatable(store_gen$planted, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # 回写数据库
  observeEvent(input$btn_save_db_name, {
    tryCatch({
      if (!is_latest_batch(input$gen_batch, db_path = db_path)) {
        stop("该批次不是最新生成的批次，禁止命名")
      }
      update_cross_names(
        batch = input$gen_batch,
        prefix = input$gen_prefix,
        start_n = input$gen_start_n,
        digits = input$gen_digits,
        db_path = db_path
      )
      showNotification("数据库更新成功", type = "message")
    }, error = function(e) {
      showNotification(paste("更新失败:", e$message), type = "error")
    })
  })
  
  # 导出 Excel
  output$btn_export_xlsx <- downloadHandler(
    filename = function() { paste0("CrossBook_", input$gen_prefix, "_", input$gen_batch, ".xlsx") },
    content = function(file) {
      req(store_gen$my_combi, store_gen$planted)
      
      fields <- c("fieldid", "code", "place", "stageid", "name", "rows", "line_number", "rp")
      
      if (exists("savewb")) {
        savewb(
          origin = store_gen$my_combi,
          planting = store_gen$planted,
          myview = store_gen$planted[, intersect(c(fields, "ma", "pa"), names(store_gen$planted)), drop=FALSE],
          combi_matrix = if(exists("combination_matrix")) combination_matrix(store_gen$my_combi) else NULL,
          filename = file,
          overwrite = TRUE
        )
      } else {
        stop("找不到 savewb 函数")
      }
    }
  )
}

shinyApp(ui, server)
