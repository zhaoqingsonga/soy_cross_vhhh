# =============================================================================
# 模块名称：mod_cross_app.R
# 功能描述：杂交组合配置模块（原 run_cross_app.R 的逻辑封装）
# =============================================================================

library(shiny)
library(DT)
library(DBI)
library(RSQLite)
library(dplyr)
library(glue)
library(rhandsontable)

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

cross_app_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # CSS 已在 app.R 中全局定义，此处移除局部定义
    
    navbarPage("杂交组合配置", id = ns("steps"),
      tabPanel("A 母本",
        fluidPage(
          DT::dataTableOutput(ns("tbl_females")),
          actionButton(ns("confirm_females"), "母本确认", class = "btn-primary"),
          actionButton(ns("refresh_females"), "刷新", icon = icon("refresh"), class = "btn-info btn-sm")
        )
      ),
      tabPanel("A 已选母本",
        fluidPage(
          DT::dataTableOutput(ns("tbl_females_selected")),
          actionButton(ns("remove_females"), "移除选中", class = "btn-warning"),
          actionButton(ns("goto_males"), "进入父本选择", class = "btn-primary")
        )
      ),
      tabPanel("B 父本",
        fluidPage(
          checkboxInput(ns("only_available"), "只显示可配父本", FALSE),
          DT::dataTableOutput(ns("tbl_males")),
          actionButton(ns("confirm_males"), "父本确认", class = "btn-primary"),
          actionButton(ns("refresh_males"), "刷新", icon = icon("refresh"), class = "btn-info btn-sm")
        )
      ),
      tabPanel("B 已选父本",
        fluidPage(
          DT::dataTableOutput(ns("tbl_males_selected")),
          actionButton(ns("remove_males"), "移除选中", class = "btn-warning"),
          actionButton(ns("goto_matrix"), "进入组合矩阵", class = "btn-success")
        )
      ),
      tabPanel("C 组合矩阵",
        fluidPage(
          fluidRow(
            column(9,
              div(style="margin-bottom: 8px;",
                  actionButton(ns("matrix_toggle_all"), "全选/清空", class = "btn-secondary btn-sm")
              ),
              div(style = 'overflow-x: hidden;', rHandsontableOutput(ns("matrix"))),
              verbatimTextOutput(ns("matrix_summary")),
              textInput(ns("batch"), "批次名", value = format(Sys.Date(), SoyCross$config$cross$default_batch_format)),
              textInput(ns("memo"), "组合特点（必填）", value = SoyCross$config$cross_matrix$default_memo, placeholder = "请输入组合特点"),
              numericInput(ns("limit"), "生成数量 (可选)", value = NA, min = 1),
              actionButton(ns("run_write"), "写入数据库", class = "btn-danger"),
              verbatimTextOutput(ns("run_summary")),
              DT::dataTableOutput(ns("generated_table")),
              DT::dataTableOutput(ns("skipped_table"))
            ),
            column(3, style = "border-left: 1px solid #ddd;",
              h4("命名杂交名称"),
              textInput(ns("naming_prefix"), "前缀 (Prefix)", value = format(Sys.Date(), SoyCross$config$field$default_prefix)),
              numericInput(ns("naming_start_n"), "起始编号 (Start N)", value = SoyCross$config$field$default_start_n, min = 1),
              numericInput(ns("naming_digits"), "编号位数 (Digits)", value = SoyCross$config$field$default_digits, min = 1),
              actionButton(ns("run_crossed_name"), "命名杂交名称(crossed_name)", class = "btn-danger"),
              br(), br(),
              h5("已命名预览"),
              DT::dataTableOutput(ns("tbl_named_preview"))
            )
          )
        )
      ),
      tabPanel("D 批次管理",
        fluidPage(
          fluidRow(
            column(4, style = "border-right: 1px solid #ddd; padding-right: 20px;",
              h4("批次列表"),
              div(style="margin-bottom: 10px;",
                actionButton(ns("refresh_batches"), "刷新", icon = icon("refresh"), class = "btn-info btn-sm"),
                actionButton(ns("delete_batch_btn"), "删除选中批次", icon = icon("trash"), class = "btn-danger btn-sm")
              ),
              DT::dataTableOutput(ns("batch_list_table"))
            ),
            column(8,
              h4("批次详情"),
              div(style="margin-bottom: 10px; display: flex; align-items: center;",
                div(style="margin-right: 20px;", checkboxInput(ns("show_reciprocal"), "显示反交", value = TRUE)),
                actionButton(ns("delete_selected_records"), "删除选中记录", icon = icon("trash"), class = "btn-danger btn-sm")
              ),
              DT::dataTableOutput(ns("batch_detail_table")),
              helpText("提示：选中记录后点击'删除选中记录'，将同时删除该记录的正交和反交（如果存在）")
            )
          )
        )
      )
    )
  )
}

cross_app_server <- function(id, db_path = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    if (is.null(db_path)) {
      db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
    }

    # 确保数据库目录存在
    if (!dir.exists(dirname(db_path))) dir.create(dirname(db_path), recursive = TRUE)

    # 加载辅助函数
    # 注意：这里假设 mod_cross.R 已经在主程序中 source 了，或者在这里 source
    # 为安全起见，检查并 source
    # (在 module 中 source 可能会有路径问题，最好由主程序统一加载，这里假设环境中有这些函数)
    
    refresh_key <- reactiveVal(0)
    new_batch_to_select <- reactiveVal(NULL)
    ensure_log <- function() {
      d <- file.path(getwd(), "logs")
      if (!dir.exists(d)) dir.create(d, showWarnings = FALSE)
      file.path(d, "cross_matrix_write_log.csv")
    }
    log_write <- function(batch, inserted, reciprocal, total, skipped_direct, skipped_recip, status = "success", message = "", mothers_used = "", fathers_used = "") {
      f <- ensure_log()
      entry <- data.frame(
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        operation = "matrix_write",
        batch = as.character(batch),
        inserted = as.integer(inserted),
        reciprocal = as.integer(reciprocal),
        total = as.integer(total),
        skipped_direct = as.integer(skipped_direct),
        skipped_recip = as.integer(skipped_recip),
        mothers_used = as.character(mothers_used),
        fathers_used = as.character(fathers_used),
        status = as.character(status),
        message = as.character(message),
        user = Sys.info()[["user"]],
        stringsAsFactors = FALSE
      )
      utils::write.table(entry, f, sep = ",", row.names = FALSE, col.names = !file.exists(f), append = TRUE, fileEncoding = "UTF-8")
    }
    ids_to_names <- function(ids) {
      ids <- unique(as.character(ids))
      ids <- ids[nzchar(ids)]
      if (length(ids) == 0) return(character(0))
      con <- dbConnect(RSQLite::SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      in_clause <- paste(sprintf("'%s'", ids), collapse = ",")
      sql <- paste0("SELECT id, name FROM parents WHERE id IN (", in_clause, ")")
      df <- DBI::dbGetQuery(con, sql)
      if (nrow(df) == 0) return(character(0))
      m <- setNames(df$name, df$id)
      out <- m[ids[ids %in% names(m)]]
      unname(out)
    }
    fetch_parents <- function() {
      con <- dbConnect(SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      dbGetQuery(con, "SELECT * FROM parents WHERE active=1")
    }
    parents <- reactive({
      refresh_key()
      fetch_parents()
    })
    
    get_display_cols <- function(df) {
      names(df)
    }

    females_sel <- reactiveVal(character(0))
    males_sel <- reactiveVal(character(0))
    selected_pairs <- reactiveVal(data.frame(female_id=character(0), male_id=character(0)))
    matrix_default <- reactiveVal("TRUE")
    
    editable_mask <- reactive({
      f_ids <- females_sel(); m_ids <- males_sel()
      if (length(f_ids) == 0 || length(m_ids) == 0) return(NULL)
      con <- dbConnect(SQLite(), db_path); on.exit(dbDisconnect(con), add = TRUE)
      in_f <- paste(sprintf("'%s'", f_ids), collapse = ",")
      in_m <- paste(sprintf("'%s'", m_ids), collapse = ",")
      ex <- dbGetQuery(con, paste0("SELECT female_id, male_id FROM crosses WHERE female_id IN (", in_f, ") AND male_id IN (", in_m, ")"))
      mask <- matrix(TRUE, nrow = length(f_ids), ncol = length(m_ids))
      if (nrow(ex) > 0) {
        for (i in seq_len(nrow(ex))) {
          r <- match(ex$female_id[i], f_ids)
          c <- match(ex$male_id[i], m_ids)
          if (!is.na(r) && !is.na(c)) mask[r, c] <- FALSE
        }
      }
      for (i in seq_along(f_ids)) {
        j <- match(f_ids[i], m_ids)
        if (!is.na(j)) mask[i, j] <- FALSE
      }
      mask
    })

    # A 母本
    output$tbl_females <- DT::renderDataTable({
      df <- parents(); cols <- get_display_cols(df)
      DT::datatable(
        df[, cols, drop=FALSE], 
        selection = "multiple", 
        filter = "top", 
        class = "compact stripe hover",
        options = list(pageLength=20, scrollY = '60vh', scrollCollapse = TRUE, searchHighlight = TRUE, autoWidth = FALSE)
      )
    })
    observeEvent(input$confirm_females, {
      df <- parents(); s <- input$tbl_females_rows_selected
      females_sel(df$id[s])
      updateNavbarPage(session, "steps", selected = "A 已选母本")
    })
    output$tbl_females_selected <- DT::renderDataTable({
      df <- parents(); ids <- females_sel();
      DT::datatable(df[df$id %in% ids, , drop=FALSE], selection = "multiple", class = "compact stripe hover", options = list(pageLength=10, autoWidth = FALSE, scrollX = TRUE))
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
      updateNavbarPage(session, "steps", selected = "B 父本")
    })
    observeEvent(input$refresh_females, {
      refresh_key(refresh_key() + 1)
      showNotification("母本列表已刷新", type = "message")
    })

    # B 父本 + 冲突检测 + 禁止自交
    males_data <- reactive({
      df_all <- parents()
      f_ids <- females_sel()  # 已选母本的ID
      
      # 先获取已选母本的名称（于过滤前）
      moms <- df_all$name[df_all$id %in% f_ids]
      if (length(moms)==0 || !exists("find_unused_partners")) { 
        df_all$is_conflict <- FALSE
        return(df_all)
      }
      
      unused_list <- lapply(moms, function(m) tryCatch(find_unused_partners(m, role="female", db_path=db_path)$id, error=function(e) df_all$id))
      common_unused <- Reduce(intersect, unused_list)
      df_all$is_conflict <- !(df_all$id %in% common_unused)
      
      if (isTRUE(input$only_available)) df_all <- df_all[!df_all$is_conflict, , drop=FALSE]
      df_all
    })
    output$tbl_males <- DT::renderDataTable({
      df <- males_data(); cols <- get_display_cols(df)
      opts <- list(pageLength = 20, scrollY = '60vh', scrollCollapse = TRUE, searchHighlight = TRUE, autoWidth = FALSE)
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
      updateNavbarPage(session, "steps", selected = "B 已选父本")
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
      updateNavbarPage(session, "steps", selected = "C 组合矩阵")
    })
    observeEvent(input$refresh_males, {
      refresh_key(refresh_key() + 1)
      showNotification("父本列表已刷新", type = "message")
    })

    # C 矩阵
    output$matrix <- renderRHandsontable({
      f_ids <- females_sel(); m_ids <- males_sel(); if (length(f_ids)==0 || length(m_ids)==0) return(NULL)
      dfp <- parents(); id2name <- setNames(dfp$name, dfp$id)
      f_names <- unname(id2name[f_ids]); m_names <- unname(id2name[m_ids])
      con <- dbConnect(SQLite(), db_path); on.exit(dbDisconnect(con), add = TRUE)
      in_f <- paste(sprintf("'%s'", f_ids), collapse = ",")
      in_m <- paste(sprintf("'%s'", m_ids), collapse = ",")
      sql <- paste0("SELECT female_id, male_id, name FROM crosses WHERE (female_id IN (", in_f, ") AND male_id IN (", in_m, ")) OR (female_id IN (", in_m, ") AND male_id IN (", in_f, "))")
      ex_df <- dbGetQuery(con, sql)
      
      mat_vals <- matrix(matrix_default(), nrow=length(f_ids), ncol=length(m_ids))
      df_main <- data.frame(Name = f_names, mat_vals, stringsAsFactors = FALSE, check.names = FALSE)
      colnames(df_main) <- c("Name", m_ids)
      
      df_top <- as.data.frame(t(c("", m_names)), stringsAsFactors = FALSE)
      colnames(df_top) <- c("Name", m_ids)
      
      df_mat <- rbind(df_top, df_main)
      rownames(df_mat) <- c("", f_ids)
      
      cell_props <- list()
      for (c in 0:(ncol(df_mat)-1)) {
         cell_props[[length(cell_props)+1]] <- list(row = 0, col = c, readOnly = TRUE, className = 'htCenter htMiddle header-cell', type = 'text')
      }
      for (r in 1:(nrow(df_mat)-1)) {
         cell_props[[length(cell_props)+1]] <- list(row = r, col = 0, readOnly = TRUE, className = 'htCenter htMiddle name-cell')
      }

      if (nrow(ex_df)>0) {
        for (i in seq_len(nrow(ex_df))) {
          fi <- ex_df$female_id[i]; mi <- ex_df$male_id[i]; nm <- ex_df$name[i]
          r_idx <- match(fi, f_ids)
          c_idx <- match(mi, m_ids)
          if (!is.na(r_idx) && !is.na(c_idx)) {
            df_mat[r_idx + 1, c_idx + 1] <- nm
            cell_props[[length(cell_props)+1]] <- list(row = r_idx, col = c_idx, type = 'text', readOnly = TRUE, className = 'existing')
          }
        }
      }
      
      for (i in seq_along(f_ids)) {
        for (j in seq_along(m_ids)) {
          if (f_ids[i] == m_ids[j]) {
            df_mat[i + 1, j + 1] <- "FALSE"
            cell_props[[length(cell_props)+1]] <- list(row = i, col = j, readOnly = TRUE, className = 'diagonal')
          }
        }
      }

      ht <- rhandsontable::rhandsontable(df_mat, rowHeaders = c("", f_ids), cell = cell_props)
      widths <- c(120, pmax(80, pmin(300, nchar(m_names)*12)))
      for (cn in m_ids) {
        ht <- rhandsontable::hot_col(ht, cn, type = 'checkbox', checkedTemplate = "TRUE", uncheckedTemplate = "FALSE")
      }
      ht <- rhandsontable::hot_cols(ht, manualColumnResize = TRUE, colWidths = widths)
      ht <- rhandsontable::hot_cols(ht, renderer = "function (instance, td, row, col, prop, value, cellProperties) {
        if (cellProperties.type === 'checkbox') {
          Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
        } else {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
        }
        td.style.color = 'black';
        if (row === 0) {
            td.style.background = '#e6e6e6'; 
            td.style.fontWeight = 'bold';
            td.style.textAlign = 'center';
        } else if (col === 0) {
            td.style.background = '#f2f2f2'; 
            td.style.fontWeight = 'bold';
        } else if (cellProperties.readOnly) {
           if (cellProperties.className && cellProperties.className.indexOf('diagonal') > -1) {
               td.style.background = '#f8d7da'; 
           } else if (cellProperties.className && cellProperties.className.indexOf('existing') > -1) {
               td.style.background = '#e2e3e5'; 
           }
        } else {
          td.style.background = 'white';
        }
      }")
      ht <- rhandsontable::hot_table(ht, height = 500, rowHeaderWidth = 120)
      ht
    })
    
    observeEvent(input$matrix_toggle_all, {
      if (identical(matrix_default(), "TRUE")) {
        matrix_default("FALSE")
      } else {
        matrix_default("TRUE")
      }
    })
    
    output$matrix_summary <- renderText({
      x <- input$matrix; if (is.null(x)) return("")
      m_raw <- hot_to_r(x)
      if (ncol(m_raw) > 1) m_raw <- m_raw[, -1, drop=FALSE]
      if (nrow(m_raw) > 1) m_raw <- m_raw[-1, , drop=FALSE]
      m_mat <- as.matrix(m_raw)
      n_selected <- sum(m_mat == "TRUE", na.rm = TRUE)
      
      f_ids <- females_sel(); m_ids <- males_sel();
      con <- dbConnect(SQLite(), db_path); on.exit(dbDisconnect(con), add = TRUE)
      in_f <- paste(sprintf("'%s'", f_ids), collapse = ",")
      in_m <- paste(sprintf("'%s'", m_ids), collapse = ",")
      sql <- paste0("SELECT female_id, male_id FROM crosses WHERE female_id IN (", in_f, ") AND male_id IN (", in_m, ")")
      ex_df <- dbGetQuery(con, sql)
      ex_df <- unique(ex_df)
      n_db_match <- nrow(ex_df)
      diag_ids <- intersect(f_ids, m_ids)
      n_diag_total <- length(diag_ids)
      n_diag_in_db <- 0
      if (n_db_match > 0 && n_diag_total > 0) {
        for (d in diag_ids) {
          if (any(ex_df$female_id == d & ex_df$male_id == d)) {
            n_diag_in_db <- n_diag_in_db + 1
          }
        }
      }
      n_gray <- n_db_match - n_diag_in_db
      n_pink <- n_diag_total
      total_cells <- length(f_ids) * length(m_ids)
      n_white <- total_cells - n_gray - n_pink
      glue("可配置: {n_white}，已存在: {n_gray}，自交: {n_pink} | 当前选中: {n_selected}")
    })

    observeEvent(input$run_write, {
      req(input$batch)
      # 验证 memo 不为空
      if (!nzchar(input$memo)) {
        showNotification("组合特点不能为空，请填写组合特点", type = "error")
        return()
      }
      x <- input$matrix; if (is.null(x)) { showNotification("矩阵为空", type="warning"); return(NULL) }
      m_raw <- hot_to_r(x)
      if (nrow(m_raw) > 1) m_raw <- m_raw[-1, , drop=FALSE]
      f_ids_from_row <- rownames(m_raw)
      if (ncol(m_raw) > 1) m_raw <- m_raw[, -1, drop=FALSE]
      m <- as.matrix(m_raw)
      m_names <- colnames(m)
      
      pairs_idx <- which(m == "TRUE", arr.ind=TRUE)
      if (nrow(pairs_idx)==0) { showNotification("无可配置组合", type="warning"); return(NULL) }
      
      pairs <- data.frame(
        female_id = f_ids_from_row[pairs_idx[,1]],
        male_id   = m_names[pairs_idx[,2]],
        stringsAsFactors = FALSE
      ) %>% dplyr::filter(female_id != male_id)
      
      if (any(pairs$female_id == pairs$male_id)) {
        showNotification("错误：检测到自交组合，无法写入！", type="error")
        return(NULL)
      }
      
      tryCatch({
        res <- create_specific_cross_plan(
          batch_name = input$batch,
          pairs = pairs,
          db_path = db_path,
          include_reciprocal = TRUE,
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
          log_write(
            batch = input$batch,
            inserted = res$summary$inserted_n[1],
            reciprocal = res$summary$reciprocal_added[1],
            total = res$summary$total_inserted[1],
            skipped_direct = res$summary$skipped_direct[1],
            skipped_recip = res$summary$skipped_recip[1],
            status = "success",
            message = "",
            mothers_used = paste(sort(unique(ids_to_names(pairs$female_id))), collapse = ";"),
            fathers_used = paste(sort(unique(ids_to_names(pairs$male_id))), collapse = ";")
          )
          load_batches()
          new_batch_to_select(input$batch)
        } else {
          showNotification("未写入任何数据（可能全部已存在）", type="warning")
          log_write(
            batch = input$batch,
            inserted = res$summary$inserted_n[1],
            reciprocal = res$summary$reciprocal_added[1],
            total = res$summary$total_inserted[1],
            skipped_direct = res$summary$skipped_direct[1],
            skipped_recip = res$summary$skipped_recip[1],
            status = "nochange",
            message = "all existing",
            mothers_used = paste(sort(unique(ids_to_names(pairs$female_id))), collapse = ";"),
            fathers_used = paste(sort(unique(ids_to_names(pairs$male_id))), collapse = ";")
          )
        }
      }, error = function(e) {
        output$run_summary <- renderText(paste("错误：", e$message))
        showNotification(paste("写入失败：", e$message), type="error")
        log_write(
          batch = input$batch,
          inserted = 0,
          reciprocal = 0,
          total = 0,
          skipped_direct = 0,
          skipped_recip = 0,
          status = "error",
          message = e$message,
          mothers_used = paste(sort(unique(ids_to_names(pairs$female_id))), collapse = ";"),
          fathers_used = paste(sort(unique(ids_to_names(pairs$male_id))), collapse = ";")
        )
      })
    })

    # 命名杂交名称：执行批量命名并预览
    observeEvent(input$run_crossed_name, {
      req(input$batch)
      if (!nzchar(input$naming_prefix)) {
        showNotification("前缀不能为空", type = "error")
        return()
      }
      if (!is_latest_batch(input$batch, db_path = db_path)) {
        showNotification("该批次不是最新生成的批次，禁止命名", type = "error")
        return()
      }
      start_n <- if (is.null(input$naming_start_n)) 1 else input$naming_start_n
      digits  <- if (is.null(input$naming_digits)) 3 else input$naming_digits
      tryCatch({
        update_cross_names(
          batch   = input$batch,
          prefix  = input$naming_prefix,
          start_n = start_n,
          digits  = digits,
          db_path = db_path
        )
        showNotification("命名完成并已写入数据库", type = "message")
        # 预览当前批次已命名记录
        if (exists("get_named_preview")) {
          preview_df <- get_named_preview(input$batch, db_path = db_path)
        } else {
          con <- dbConnect(RSQLite::SQLite(), db_path)
          on.exit(dbDisconnect(con), add = TRUE)
          preview_df <- DBI::dbGetQuery(
            con,
            "SELECT c.name, pm.name AS ma, pf.name AS pa
             FROM crosses c
             LEFT JOIN parents pf ON c.female_id = pf.id
             LEFT JOIN parents pm ON c.male_id = pm.id
             WHERE c.batch = ? AND c.name IS NOT NULL AND c.name <> ''
             ORDER BY c.name ASC",
            params = list(input$batch)
          )
        }
        output$tbl_named_preview <- DT::renderDataTable({
          DT::datatable(preview_df, class = "compact stripe hover", options = list(pageLength = 15, autoWidth = FALSE))
        })
        # 刷新矩阵摘要与批次列表
        load_batches()
      }, error = function(e) {
        showNotification(paste("命名失败：", e$message), type = "error")
      })
    })

    # D 批次管理
    batches_df <- reactiveVal()
    load_batches <- function() {
      tryCatch({
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
    
    observe({ load_batches() })
    observeEvent(input$refresh_batches, { load_batches() })
    
    batches_rx <- reactivePoll(2000, session, function() {
      suppressWarnings(file.info(db_path)$mtime)
    }, function() {
      tryCatch({
        if (exists("summarize_cross_batches_db")) {
          summarize_cross_batches_db(db_path = db_path)
        } else {
          con <- dbConnect(SQLite(), db_path)
          on.exit(dbDisconnect(con), add = TRUE)
          dbGetQuery(con, "
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
    
    output$batch_list_table <- DT::renderDataTable({
      df <- batches_df()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      cols_to_show <- c("batch", "direct_crosses", "last_updated")
      available_cols <- intersect(cols_to_show, names(df))
      df_show <- df[, available_cols, drop = FALSE]
      DT::datatable(df_show, 
                    selection = "single", 
                    class = "compact stripe hover", 
                    colnames = c("批次", "正交数", "更新时间"),
                    rownames = FALSE,
                    options = list(pageLength = 15, dom = 'lfrtip', autoWidth = FALSE, stateSave = TRUE))
    })
    
    # 存储当前批次详情的 reactive
    current_batch_details <- reactiveVal(NULL)
    
    output$batch_detail_table <- DT::renderDataTable({
      s <- input$batch_list_table_rows_selected
      if (length(s) == 0) return(NULL)
      df_b <- batches_df()
      batch_name <- df_b$batch[s]
      show_recip <- isTRUE(input$show_reciprocal)
      
      if (exists("get_crosses_by_batch")) {
        details <- get_crosses_by_batch(batch = batch_name, db_path = db_path, include_reciprocal = show_recip)
      } else {
        con <- dbConnect(SQLite(), db_path)
        on.exit(dbDisconnect(con), add = TRUE)
        sql <- "SELECT * FROM crosses WHERE batch = ?"
        if (!show_recip) {
          sql <- paste(sql, "AND (is_reciprocal = 0 OR is_reciprocal IS NULL)")
        }
        details <- dbGetQuery(con, sql, params = list(batch_name))
      }
      
      # 存储当前详情数据供删除功能使用
      current_batch_details(details)
      
      DT::datatable(details, 
                    selection = "single",  # 允许单行选择
                    class = "compact stripe hover", 
                    options = list(pageLength = 15, autoWidth = FALSE, scrollX = TRUE))
    })
    
    # 删除选中记录（同时删除正反交）
    observeEvent(input$delete_selected_records, {
      s <- input$batch_detail_table_rows_selected
      if (length(s) == 0) {
        showNotification("请先选择一条要删除的记录", type = "warning")
        return()
      }
      
      details <- current_batch_details()
      if (is.null(details) || nrow(details) == 0) {
        showNotification("无法获取记录详情", type = "error")
        return()
      }
      
      # 获取选中的记录
      selected_record <- details[s, ]
      female_id <- selected_record$female_id
      male_id <- selected_record$male_id
      batch_name <- selected_record$batch
      
      # 判断是否为反交
      is_reciprocal <- isTRUE(selected_record$is_reciprocal == 1)
      
      # 构建显示信息
      if (is_reciprocal) {
        cross_info <- paste0("反交: ", male_id, " × ", female_id)
      } else {
        cross_info <- paste0("正交: ", female_id, " × ", male_id)
      }
      
      # 显示确认对话框
      showModal(modalDialog(
        title = "确认删除记录",
        tagList(
          p(strong("您确定要删除以下记录吗？")),
          p("批次: ", strong(batch_name)),
          p("组合: ", strong(cross_info)),
          hr(),
          p(style = "color: #d9534f;", icon("exclamation-triangle"), 
            "警告：此操作将同时删除该组合的正交和反交记录（如果存在），且不可恢复！"),
          p(style = "color: #999; font-size: 12px;", 
            "提示：输入 DELETE 以确认删除操作")
        ),
        textInput(ns("confirm_delete_record"), "请输入 DELETE 以确认", value = ""),
        footer = tagList(
          modalButton("取消"),
          actionButton(ns("confirm_delete_record_btn"), "确认删除", class = "btn-danger")
        ),
        easyClose = FALSE
      ))
    })
    
    # 确认删除记录
    observeEvent(input$confirm_delete_record_btn, {
      confirm_text <- toupper(trimws(input$confirm_delete_record %||% ""))
      
      if (confirm_text != "DELETE") {
        showNotification("确认文本不正确，请输入 DELETE", type = "error")
        return()
      }
      
      removeModal()
      
      s <- input$batch_detail_table_rows_selected
      details <- current_batch_details()
      selected_record <- details[s, ]
      female_id <- selected_record$female_id
      male_id <- selected_record$male_id
      batch_name <- selected_record$batch
      
      tryCatch({
        con <- dbConnect(SQLite(), db_path)
        on.exit(dbDisconnect(con), add = TRUE)
        
        # 同时删除正交和反交记录
        # 正交: female_id = ? AND male_id = ?
        # 反交: female_id = ? AND male_id = ? (交换)
        
        # 先查询要删除的记录数
        check_sql <- "SELECT COUNT(*) as cnt FROM crosses 
                      WHERE batch = ? AND 
                      ((female_id = ? AND male_id = ?) OR (female_id = ? AND male_id = ?))"
        check_result <- dbGetQuery(con, check_sql, 
                                   params = list(batch_name, female_id, male_id, male_id, female_id))
        delete_count <- check_result$cnt[1]
        
        if (delete_count == 0) {
          showNotification("未找到要删除的记录", type = "warning")
          return()
        }
        
        # 执行删除
        delete_sql <- "DELETE FROM crosses 
                       WHERE batch = ? AND 
                       ((female_id = ? AND male_id = ?) OR (female_id = ? AND male_id = ?))"
        dbExecute(con, delete_sql, 
                  params = list(batch_name, female_id, male_id, male_id, female_id))
        
        showNotification(paste0("成功删除 ", delete_count, " 条记录（包括正交和反交）"), type = "message")
        
        # 清空当前选择，刷新详情表格
        current_batch_details(NULL)
        # 批次列表会通过 reactivePoll 自动刷新（因为数据库文件修改时间变化）
        
      }, error = function(e) {
        showNotification(paste("删除失败:", e$message), type = "error")
      })
    })
    
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
          textInput(ns("confirm_delete_text"), "请输入批次名称以确认删除", value = "")
        ),
        footer = tagList(
          modalButton("取消"),
          actionButton(ns("confirm_delete_batch"), "确认删除", class = "btn-danger")
        )
      ))
    })
    
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
        load_batches()
      }, error = function(e) {
        showNotification(paste("删除失败:", e$message), type = "error")
      })
    })
    
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
  })
}
