library(shiny)
library(DT)
library(DBI)
library(RSQLite)

parent_admin_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$style(HTML("
        .sidebar-scroll { 
          max-height: calc(100vh - 160px); 
          overflow-y: auto; 
          padding-right: 6px;
        }
        .btn-block { width: 100%; margin-top: 6px; }
        .sidebar-section-title { margin: 8px 0 4px 0; font-weight: 600; }
      "))
    ),
    titlePanel("亲本管理"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(class = "sidebar-scroll",
          div(class = "sidebar-section-title", "查询"),
          textInput(ns("search_name"), "按名称搜索", ""),
          checkboxInput(ns("filter_active"), "仅显示活跃亲本", value = TRUE),
          actionButton(ns("btn_refresh"), "刷新列表", icon = icon("sync"), class = "btn-primary btn-block", width = "100%"),
          tags$hr(),
          div(class = "sidebar-section-title", "数据"),
          fileInput(ns("file_import_parents"), "选择亲本Excel", accept = c(".xlsx", ".xls")),
          fluidRow(
            column(6, actionButton(ns("btn_import_parents"), "导入亲本", icon = icon("file-import"), class = "btn-primary btn-block", width = "100%")),
            column(6, downloadButton(ns("btn_export_parents"), "导出亲本", class = "btn-secondary btn-block"))
          )
        )
      ),
      mainPanel(
        # 工具栏
        div(style = "margin-bottom: 15px;",
          actionButton(ns("btn_add"), "新增亲本", icon = icon("plus"), class = "btn-success"),
          span(style = "margin-left: 10px; border-left: 1px solid #ccc; padding-left: 10px;",
            actionButton(ns("btn_edit"), "修改", icon = icon("edit"), class = "btn-warning"),
            actionButton(ns("btn_soft_del"), "停用", icon = icon("ban"), class = "btn-secondary"),
            actionButton(ns("btn_enable"), "启用", icon = icon("check"), class = "btn-success"),
            actionButton(ns("btn_hard_del"), "删除", icon = icon("trash"), class = "btn-danger")
          )
        ),
        DT::dataTableOutput(ns("tbl_parents"))
      )
    )
  )
}

parent_admin_server <- function(id, db_path = NULL) {
  moduleServer(id, function(input, output, session) {
    if (is.null(db_path)) {
      db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
    }
    db_path <- normalizePath(db_path, winslash = "/", mustWork = FALSE)
    pending_delete_id <- reactiveVal(NULL)
    pending_edit_row <- reactiveVal(NULL)
    col_input_id <- function(col) {
      paste0("edit_", paste0(sprintf("%02x", as.integer(charToRaw(enc2utf8(col)))), collapse = ""))
    }
    sql_quote_ident <- function(x) {
      paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
    }
    
    # 尝试加载 utils_io.R - 从项目根目录加载
    # 首先尝试从当前工作目录加载
    if (file.exists(file.path(getwd(), "R", "utils_io.R"))) {
      source(file.path(getwd(), "R", "utils_io.R"))
    } else {
      # 如果在工作目录找不到，尝试从项目根目录加载
      # 假设当前文件在 R 目录下
      project_root <- dirname(getwd())
      if (file.exists(file.path(project_root, "R", "utils_io.R"))) {
        source(file.path(project_root, "R", "utils_io.R"))
      } else {
        # 如果在上一级目录也找不到，尝试从相对路径加载
        if (file.exists("../R/utils_io.R")) {
          source("../R/utils_io.R")
        } else if (file.exists("./utils_io.R")) {
          source("./utils_io.R")
        }
      }
    }
    ensure_log <- function() {
      d <- file.path(getwd(), "logs")
      if (!dir.exists(d)) dir.create(d, showWarnings = FALSE)
      file.path(d, "parent_edit_log.csv")
    }
    log_write <- function(op, details, status = "success") {
      f <- ensure_log()
      entry <- data.frame(
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        operation = op,
        details = details,
        status = status,
        user = Sys.info()[["user"]],
        stringsAsFactors = FALSE
      )
      utils::write.table(entry, f, sep = ",", row.names = FALSE, col.names = !file.exists(f), append = TRUE, fileEncoding = "UTF-8")
    }
    load_parents <- function(active_only = TRUE, name_like = NULL) {
      con <- dbConnect(RSQLite::SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      sql <- "SELECT * FROM parents"
      params <- list()
      where <- c()
      if (active_only && "active" %in% dbListFields(con, "parents")) {
        where <- c(where, "active = 1")
      }
      if (!is.null(name_like) && nzchar(name_like) && "name" %in% dbListFields(con, "parents")) {
        where <- c(where, "name LIKE ?")
        params <- c(params, paste0("%", name_like, "%"))
      }
      if (length(where) > 0) {
        sql <- paste(sql, "WHERE", paste(where, collapse = " AND "))
      }
      if (length(params) > 0) {
        dbGetQuery(con, sql, params = unname(params))
      } else {
        dbGetQuery(con, sql)
      }
    }
    # 响应式触发器，用于刷新数据
    refresh_trigger <- reactiveVal(0)

    # 响应式数据源
    parents_data <- reactive({
      refresh_trigger() # 依赖触发器
      load_parents(input$filter_active, input$search_name)
    })

    # 单一渲染出口
    output$tbl_parents <- DT::renderDataTable({
      DT::datatable(parents_data(), selection = "single", options = list(pageLength = 10, lengthMenu = c(10, 25, 50), autoWidth = FALSE, scrollX = TRUE))
    })

    observeEvent(input$btn_refresh, {
      refresh_trigger(refresh_trigger() + 1)
      showNotification("已刷新", type = "message")
    })
   observeEvent(input$btn_import_parents, {
      f <- input$file_import_parents
      if (is.null(f) || is.null(f$datapath) || !file.exists(f$datapath)) {
        showNotification("请选择Excel文件", type = "error")
        return(NULL)
      }
      
      # 首先尝试使用 id 作为键进行 upsert 操作
      res <- try(
        import_parents_from_file(
          f$datapath,
          db_path = db_path,
          mode = "upsert",
          key = "id",
          backup_before = FALSE
        ),
        silent = TRUE
      )
      
      # 如果失败，再尝试使用 name 作为键进行 upsert 操作
      if (inherits(res, "try-error")) {
        res <- try(
          import_parents_from_file(
            f$datapath,
            db_path = db_path,
            mode = "upsert",
            key = "name",
            backup_before = FALSE
          ),
          silent = TRUE
        )
      }
      
      if (inherits(res, "try-error")) {
        # 获取错误信息并显示
        error_msg <- conditionMessage(attr(res, "condition"))
        ok <- FALSE
        df <- try(read_table(f$datapath), silent = TRUE)
        if (!inherits(df, "try-error")) {
          con <- dbConnect(RSQLite::SQLite(), db_path)
          on.exit(dbDisconnect(con), add = TRUE)
          try(backup_db(db_path, backup_name = "parents_import"), silent = TRUE)
          ok <- !inherits(try(DBI::dbWriteTable(con, "parents", df, overwrite = TRUE), silent = TRUE), "try-error")
        }
        if (!ok) {
          showNotification(paste("导入失败：", error_msg), type = "error")
        } else {
          refresh_trigger(refresh_trigger() + 1)
          showNotification("已覆盖导入", type = "message")
        }
      } else {
        refresh_trigger(refresh_trigger() + 1)
        
        # 显示导入结果
        if (is.list(res) && all(c("inserted", "updated", "skipped") %in% names(res))) {
          msg <- paste0("导入完成 - 新增: ", res$inserted, ", 更新: ", res$updated, ", 跳过: ", res$skipped)
        } else {
          msg <- paste0("导入完成 - 总计: ", ifelse(is.list(res), res$total, res))
        }
        showNotification(msg, type = "message")
      }
    })
    output$btn_export_parents <- downloadHandler(
      filename = function() {
        paste0("parents_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      },
      content = function(file) {
        tryCatch({
          # 确保数据库路径存在
          if (!file.exists(db_path)) {
            stop("数据库文件不存在：", db_path)
          }
          
          # 连接数据库并读取数据（使用当前的筛选条件）
          con <- dbConnect(RSQLite::SQLite(), db_path)
          on.exit(dbDisconnect(con), add = TRUE)
          
          # 构建查询（根据当前筛选条件）
          sql <- "SELECT * FROM parents"
          params <- list()
          where_clauses <- character(0)
          
          # 应用活跃状态筛选
          if (isTRUE(input$filter_active) && "active" %in% dbListFields(con, "parents")) {
            where_clauses <- c(where_clauses, "active = 1")
          }
          
          # 应用名称搜索筛选
          if (!is.null(input$search_name) && nzchar(input$search_name) && "name" %in% dbListFields(con, "parents")) {
            where_clauses <- c(where_clauses, "name LIKE ?")
            params <- list(paste0("%", input$search_name, "%"))
          }
          
          # 构建完整 SQL
          if (length(where_clauses) > 0) {
            sql <- paste(sql, "WHERE", paste(where_clauses, collapse = " AND "))
          }
          
          # 执行查询
          if (length(params) > 0) {
            df <- dbGetQuery(con, sql, params = params)
          } else {
            df <- dbGetQuery(con, sql)
          }
          
          # 检查是否有数据
          if (nrow(df) == 0) {
            stop("没有可导出的数据")
          }
          
          # 使用 openxlsx 或 writexl 写入文件
          if (requireNamespace("openxlsx", quietly = TRUE)) {
            # 使用 openxlsx（推荐）
            wb <- openxlsx::createWorkbook()
            openxlsx::addWorksheet(wb, "parents")
            openxlsx::writeData(wb, "parents", df, rowNames = FALSE)
            # 自动调整列宽
            openxlsx::setColWidths(wb, "parents", cols = 1:ncol(df), widths = "auto")
            openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
          } else if (requireNamespace("writexl", quietly = TRUE)) {
            # 使用 writexl（备选）
            writexl::write_xlsx(list(parents = df), file)
          } else {
            # 如果没有 Excel 包，使用 CSV 格式
            write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8-BOM")
          }
        }, error = function(e) {
          stop("导出失败：", e$message)
        })
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

    get_selected_row <- reactive({
      s <- input$tbl_parents_rows_selected
      if (is.null(s) || length(s) == 0) return(NULL)
      # 使用当前缓存的数据，而不是重新查询数据库
      df <- parents_data()
      df[s[1], , drop = FALSE]
    })
    observeEvent(input$btn_add, {
      showModal(modalDialog(
        title = "新增亲本",
        textInput(session$ns("add_name"), "名称", ""),
        checkboxInput(session$ns("add_active"), "启用", TRUE),
        footer = tagList(
          modalButton("取消"),
          actionButton(session$ns("confirm_add"), "保存", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })
    observeEvent(input$confirm_add, {
      removeModal()
      name <- input$add_name
      active <- if (isTRUE(input$add_active)) 1L else 0L
      if (!nzchar(name)) {
        showNotification("名称不能为空", type = "error")
        return(NULL)
      }
      con <- dbConnect(RSQLite::SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      fields <- dbListFields(con, "parents")
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      dup_name <- if ("name" %in% fields) dbGetQuery(con, "SELECT COUNT(*) AS n FROM parents WHERE name = ?", params = list(name))$n else 0
      if (dup_name > 0) {
        showNotification("名称已存在", type = "error")
        log_write("add", paste("name=", name), "error")
        return(NULL)
      }
      ids <- dbGetQuery(con, "SELECT id FROM parents")
      vec <- ids$id
      nums <- suppressWarnings(as.integer(gsub("^P(\\d+)$", "\\1", vec)))
      base <- if (is.finite(max(nums, na.rm = TRUE))) max(nums, na.rm = TRUE) else 0L
      id <- sprintf("P%04d", base + 1L)
      cols <- intersect(c("id","name","active","created_at","updated_at"), fields)
      vals <- list(id, name, active, now, now)[seq_along(cols)]
      sql <- paste0("INSERT INTO parents (", paste(cols, collapse = ", "), ") VALUES (", paste(rep("?", length(cols)), collapse = ", "), ")")
      dbExecute(con, sql, params = unname(vals))
      log_write("add", paste("id=", id, "name=", name))
      refresh_trigger(refresh_trigger() + 1)
      showNotification("已新增", type = "message")
    })
    observeEvent(input$btn_edit, {
      row <- get_selected_row()
      if (is.null(row)) {
        showNotification("请先选择一行", type = "warning")
        return(NULL)
      }
      pending_edit_row(row)
      output$edit_fields <- renderUI({
        con <- dbConnect(RSQLite::SQLite(), db_path)
        on.exit(dbDisconnect(con), add = TRUE)
        schema <- dbGetQuery(con, "PRAGMA table_info(parents)")
        row <- pending_edit_row()
        if (is.null(row)) return(NULL)
        ns <- session$ns
        items <- lapply(seq_len(nrow(schema)), function(i) {
          col <- schema$name[i]
          val <- if (col %in% names(row)) row[[col]] else ""
          type <- tolower(schema$type[i])
          if (col == "active") {
            checkboxInput(ns(col_input_id(col)), "启用", isTRUE(as.integer(val) == 1))
          } else if (grepl("int|real|num", type)) {
            numericInput(ns(col_input_id(col)), col, suppressWarnings(as.numeric(val)))
          } else {
            textInput(ns(col_input_id(col)), col, as.character(val))
          }
        })
        do.call(tagList, items)
      })
      showModal(modalDialog(
        title = "修改亲本",
        uiOutput(session$ns("edit_fields")),
        footer = tagList(
          modalButton("取消"),
          actionButton(session$ns("confirm_edit"), "保存", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })
    observeEvent(input$confirm_edit, {
      orig_row <- pending_edit_row()
      if (is.null(orig_row)) {
        showNotification("未选择行", type = "error")
        return(NULL)
      }
      con <- dbConnect(RSQLite::SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      fields <- dbListFields(con, "parents")
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      schema <- dbGetQuery(con, "PRAGMA table_info(parents)")
      cols <- schema$name
      # 先读取 modal 中的输入值（必须在 removeModal() 之前）
      vals <- lapply(cols, function(col) {
        v <- input[[col_input_id(col)]]
        if (is.null(v) && col %in% names(orig_row)) orig_row[[col]] else v
      })
      vals <- lapply(vals, function(v) if (is.null(v)) NA else v)
      names(vals) <- cols
      if ("active" %in% cols) {
        vals[["active"]] <- if (isTRUE(vals[["active"]])) 1L else 0L
      }
      if ("updated_at" %in% cols) {
        vals[["updated_at"]] <- now
      }
      if (!("id" %in% cols) || !nzchar(as.character(vals[["id"]]))) {
        showNotification("ID 不能为空", type = "error")
        return(NULL)
      }
      if (!("name" %in% cols) || !nzchar(as.character(vals[["name"]]))) {
        showNotification("名称不能为空", type = "error")
        return(NULL)
      }
      exist <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM parents WHERE id = ?", params = list(orig_row$id))$n
      if (exist == 0) {
        showNotification("ID 不存在", type = "error")
        log_write("edit", paste("id=", orig_row$id), "error")
        return(NULL)
      }
      if (as.character(vals[["id"]]) != as.character(orig_row$id)) {
        dup_id <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM parents WHERE id = ?", params = list(vals[["id"]]))$n
        if (dup_id > 0) {
          showNotification("ID 已存在", type = "error")
          return(NULL)
        }
      }
      if ("name" %in% cols && as.character(vals[["name"]]) != as.character(orig_row$name)) {
        dup_name <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM parents WHERE name = ?", params = list(vals[["name"]]))$n
        if (dup_name > 0) {
          showNotification("名称已存在", type = "error")
          return(NULL)
        }
      }
      upd_cols <- intersect(cols, fields)
      set_clause <- paste(paste0(vapply(upd_cols, sql_quote_ident, character(1)), " = ?"), collapse = ", ")
      sql <- paste0("UPDATE ", sql_quote_ident("parents"), " SET ", set_clause, " WHERE ", sql_quote_ident("id"), " = ?")
      params <- c(unname(as.list(vals[upd_cols])), list(orig_row$id))
      updated <- tryCatch({
        dbExecute(con, sql, params = unname(params))
        TRUE
      }, error = function(e) {
        showNotification(paste0("保存失败：", e$message), type = "error")
        log_write("edit", paste("id=", orig_row$id, "error=", e$message), "error")
        FALSE
      })
      if (!isTRUE(updated)) {
        return(NULL)
      }
      removeModal()
      log_write("edit", paste("id=", orig_row$id))
      refresh_trigger(refresh_trigger() + 1)
      pending_edit_row(NULL)
      showNotification("已修改", type = "message")
    })
    observeEvent(input$btn_enable, {
      row <- get_selected_row()
      if (is.null(row)) {
        showNotification("请先选择一行", type = "warning")
        return(NULL)
      }
      id <- row$id
      con <- dbConnect(RSQLite::SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      dbExecute(con, "UPDATE parents SET active = 1, updated_at = ? WHERE id = ?", params = unname(list(now, id)))
      log_write("enable", paste("id=", id))
      refresh_trigger(refresh_trigger() + 1)
      showNotification("已启用", type = "message")
    })
    observeEvent(input$btn_soft_del, {
      row <- get_selected_row()
      if (is.null(row)) {
        showNotification("请先选择一行", type = "warning")
        return(NULL)
      }
      id <- row$id
      con <- dbConnect(RSQLite::SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      dbExecute(con, "UPDATE parents SET active = 0, updated_at = ? WHERE id = ?", params = unname(list(now, id)))
      log_write("soft_delete", paste("id=", id))
      refresh_trigger(refresh_trigger() + 1)
      showNotification("已停用", type = "message")
    })
    observeEvent(input$btn_hard_del, {
      row <- get_selected_row()
      if (is.null(row)) {
        showNotification("请先选择一行", type = "warning")
        return(NULL)
      }
      id <- row$id
      con <- dbConnect(RSQLite::SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      ref <- dbGetQuery(con, "SELECT (SELECT COUNT(*) FROM crosses WHERE female_id = ?) + (SELECT COUNT(*) FROM crosses WHERE male_id = ?) AS n", params = list(id, id))$n
      if (ref > 0) {
        showNotification("存在引用，禁止物理删除", type = "error")
        log_write("hard_delete", paste("id=", id, "ref=", ref), "error")
        return(NULL)
      }
      pending_delete_id(id)
      showModal(modalDialog(
        title = "确认物理删除",
        div(paste0("即将删除 ID=", id, " 的亲本记录。该操作不可恢复。")),
        footer = tagList(
          modalButton("取消"),
          actionButton(session$ns("confirm_hard_del"), "确认删除", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })
    observeEvent(input$confirm_hard_del, {
      id <- pending_delete_id()
      removeModal()
      if (is.null(id)) {
        showNotification("无待删除记录", type = "error")
        return(NULL)
      }
      con <- dbConnect(RSQLite::SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      dbExecute(con, "DELETE FROM parents WHERE id = ?", params = list(id))
      log_write("hard_delete", paste("id=", id))
      pending_delete_id(NULL)
      refresh_trigger(refresh_trigger() + 1)
      showNotification("已删除", type = "message")
    })
  })
}
