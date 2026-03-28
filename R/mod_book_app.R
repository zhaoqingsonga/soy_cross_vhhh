# =============================================================================
# 模块名称：mod_book_app.R
# 功能描述：采集簿生成模块（原 run_book_app.R 的逻辑封装）
# =============================================================================

library(shiny)
library(DT)
library(DBI)
library(RSQLite)
library(dplyr)
library(glue)

# 加载配置
tryCatch({
  # 尝从项目根目录加载配置
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

book_app_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # CSS 已在 app.R 中全局定义，此处移除局部定义
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("参数配置"),
        hr(),
        
        # 1. 批次选择
        div(style="display: flex; align-items: flex-end;",
            selectInput(ns("gen_batch"), "批次选择 (Batch)", choices = NULL, width = "100%"),
            actionButton(ns("refresh_batches"), "", icon = icon("refresh"), class = "btn-info btn-sm", style="margin-bottom: 15px; margin-left: 5px;")
        ),
        
        # 2. 编号参数
        textInput(ns("gen_prefix"), "组合前缀 (Prefix)", value = format(Sys.Date(), SoyCross$config$field$default_prefix), placeholder = "必填"),
        numericInput(ns("gen_start_n"), "起始编号 (Start N)", value = SoyCross$config$field$default_start_n, min = 1),
        numericInput(ns("gen_digits"), "编号位数 (Digits)", value = SoyCross$config$field$default_digits, min = 1),
        
        hr(),
        h4("种植参数"),
        
        # 3. 种植参数
        textInput(ns("gen_place"), "种植地点 (Place)", value = SoyCross$config$field$default_place, placeholder = "必填"),
        numericInput(ns("gen_rows"), "种植行数 (Rows)", value = SoyCross$config$field$default_rows, min = 1),
        numericInput(ns("gen_rp"), "重复数 (Replicates)", value = SoyCross$config$field$default_rp, min = 1),
        numericInput(ns("gen_interval"), "间隔 (Interval)", value = SoyCross$config$field$default_interval, min = 1),
        
        hr(),
        helpText("说明：配置好参数后，先点击'生成预览'检查数据，确认无误后再回写数据库或导出文件。")
      ),
      
      mainPanel(
        width = 9,
        
        # 操作按钮区
        fluidRow(
          column(12,
                 actionButton(ns("btn_calc_preview"), "1. 生成预览", class = "btn-primary", icon = icon("play")),
                 span(style = "margin: 0 10px;", "|"),
                 downloadButton(ns("btn_export_xlsx"), "2. 导出 Excel 采集簿", class = "btn-success")
          )
        ),
        hr(),
        
        # 结果展示区
        tabsetPanel(
          tabPanel("组合预览 (Combination)", 
                   br(),
                   DT::dataTableOutput(ns("tbl_preview_combi"))
          ),
          tabPanel("种植预览 (Planting)", 
                   br(),
                   DT::dataTableOutput(ns("tbl_preview_plant"))
          )
        )
      )
    )
  )
}

book_app_server <- function(id, db_path = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    if (is.null(db_path)) {
      db_path <- if (exists("SoyCross")) SoyCross$config$paths$db_path else "data/db/soy_cross.db"
    }

    # 确保数据库目录存在
    if (!dir.exists(dirname(db_path))) dir.create(dirname(db_path), recursive = TRUE)
    
    # 存储计算结果
    store_gen <- reactiveValues(
      my_combi = NULL, 
      planted = NULL
    )
    
    # 1. 初始化与批次加载
    
    observe({
      batches <- load_batches(db_path)
      if (length(batches) > 0) {
        updateSelectInput(session, "gen_batch", choices = batches, selected = batches[1])
      }
    })
    
    batches_rx <- reactivePoll(2000, session, function() {
      con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      if (!DBI::dbExistsTable(con, "crosses")) return("none|0|0")
      info <- DBI::dbGetQuery(con, "SELECT MAX(updated_at) AS t, COUNT(*) AS n, MAX(rowid) AS rid FROM crosses")
      t <- if (length(info$t) == 0 || is.na(info$t[1])) "" else as.character(info$t[1])
      n <- if (length(info$n) == 0 || is.na(info$n[1])) 0 else as.integer(info$n[1])
      rid <- if (length(info$rid) == 0 || is.na(info$rid[1])) 0 else as.integer(info$rid[1])
      paste0(t, "|", n, "|", rid)
    }, function() {
      load_batches(db_path)
    })
    
    observeEvent(batches_rx(), {
      batches <- batches_rx()
      if (length(batches) > 0) {
        updateSelectInput(session, "gen_batch", choices = batches, selected = batches[1])
      }
    })
    
    observeEvent(input$refresh_batches, {
      batches <- load_batches(db_path)
      if (length(batches) > 0) {
        updateSelectInput(session, "gen_batch", choices = batches, selected = batches[1])
        showNotification("批次列表已刷新", type = "message")
      } else {
        showNotification("未找到批次", type = "warning")
      }
    })
    
    # 2. 生成预览
    observeEvent(input$btn_calc_preview, {
      if (!nzchar(input$gen_prefix)) { showNotification("组合前缀不能为空", type = "error"); return() }
      if (!nzchar(input$gen_place)) { showNotification("种植地点不能为空", type = "error"); return() }
      req(input$gen_batch)
      
      withProgress(message = '正在计算...', value = 0, {
        tryCatch({
          # A. 获取原始数据
          incProgress(0.2, detail = "获取数据库记录")
          
          if (exists("get_crosses_by_batch")) {
            mycross <- get_crosses_by_batch(batch = input$gen_batch, db_path = db_path, include_reciprocal = FALSE)
          } else {
            con <- dbConnect(SQLite(), db_path)
            on.exit(dbDisconnect(con), add = TRUE)
            mycross <- dbGetQuery(con, "SELECT * FROM crosses WHERE batch = ? AND (is_reciprocal = 0 OR is_reciprocal IS NULL)", params = list(input$gen_batch))
          }
          
          if (nrow(mycross) == 0) {
            showNotification("该批次无正交数据", type = "warning")
            return()
          }
          
          # 直接使用原始数据
          mydata <- mycross
          
          # B. 生成组合编号
          incProgress(0.6, detail = "生成组合编号")
          
          if (exists("get_combination_with_name")) {
            n_rows <- nrow(mydata)
            
            # 提取并处理亲本列，确保为字符型且长度一致
            ma_vec <- if ("ma" %in% names(mydata)) mydata$ma else if ("female_id" %in% names(mydata)) mydata$female_id else rep("", n_rows)
            pa_vec <- if ("pa" %in% names(mydata)) mydata$pa else if ("male_id" %in% names(mydata)) mydata$male_id else rep("", n_rows)
            
            named_input <- data.frame(
              name = if ("name" %in% names(mydata)) as.character(mydata$name) else rep("", n_rows),
              ma = as.character(ma_vec),
              pa = as.character(pa_vec),
              memo = if ("memo" %in% names(mydata)) as.character(mydata$memo) else rep("", n_rows),
              stringsAsFactors = FALSE
            )
            
            # 处理 NA 值
            named_input[is.na(named_input)] <- ""
            
            # 按 name 排序
            if ("name" %in% names(named_input)) {
               named_input <- named_input[order(named_input$name), , drop = FALSE]
            }
            
            # 确保参数有效
            start_n_val <- if (!is.null(input$gen_start_n)) input$gen_start_n else 1
            
            my_combi <- get_combination_with_name(
              named_input,
              startN = start_n_val,
              order = FALSE
            )
            store_gen$my_combi <- my_combi
          } else {
            stop("找不到 get_combination_with_name 函数")
          }
          
          
          
          # C. 将 store_gen$my_combi 中的 ma 与 parents 表中的 id 进行关联，替换为 parents 中的 name（仅替换 ma）
          con <- dbConnect(SQLite(), db_path)
          on.exit(dbDisconnect(con), add = TRUE)
          parents_df <- dbGetQuery(con, "SELECT id, name FROM parents")
          
          # 确保 ma 为字符型，避免因子问题
          store_gen$my_combi$ma <- as.character(store_gen$my_combi$ma)
          
          # 仅替换 ma 字段内容：先建立 id->name 的映射，再替换 ma 列
          id2name <- setNames(parents_df$name, parents_df$id)
          store_gen$my_combi$ma <- id2name[store_gen$my_combi$ma]
          # 如果存在未匹配到的 id，保持原值不变
          store_gen$my_combi$ma[is.na(store_gen$my_combi$ma)] <- 
            as.character(store_gen$my_combi$ma)[is.na(store_gen$my_combi$ma)]
          
          # 同样方法替换 pa 字段
          store_gen$my_combi$pa <- as.character(store_gen$my_combi$pa)
          store_gen$my_combi$pa <- id2name[store_gen$my_combi$pa]
          store_gen$my_combi$pa[is.na(store_gen$my_combi$pa)] <- 
            as.character(store_gen$my_combi$pa)[is.na(store_gen$my_combi$pa)]
            
          
          # D. 生成种植排图
          incProgress(0.8, detail = "生成排图计划")
          
          if (exists("planting")) {
            if (!"year" %in% names(store_gen$my_combi)) {
              store_gen$my_combi$year <- format(Sys.Date(), "%Y")
            }
            planted <- planting(
              store_gen$my_combi,
              place = input$gen_place,
              rows = input$gen_rows,
              rp = input$gen_rp,
              interval = input$gen_interval,
              digits = input$gen_digits,
              s_prefix = input$gen_prefix,
              ck = NULL
            )
            store_gen$planted <- planted
          } else {
            stop("找不到 planting 函数")
          }
          
          incProgress(1.0, detail = "完成")
          showNotification("预览生成成功", type = "message")
          
          
          
        }, error = function(e) {
          showNotification(paste("生成失败:", e$message), type = "error")
        })
      })
    })
    
    # 3. 渲染表格
    output$tbl_preview_combi <- DT::renderDataTable({
      req(store_gen$my_combi)
      DT::datatable(store_gen$my_combi, 
                    options = list(
                      pageLength = 15, 
                      scrollX = TRUE, 
                      dom = 'frtip',
                      autoWidth = FALSE,
                      columnDefs = list(list(
                        targets = "_all",
                        render = JS(
                          "function(data, type, row, meta) {",
                          "  return type === 'display' && data != null && data.length > 10 ?",
                          "    '<span title=\"' + data + '\">' + data.substr(0, 10) + '...</span>' : data;",
                          "}"
                        )
                      ))
                    ),
                    class = "compact stripe hover")
    })
    
    output$tbl_preview_plant <- DT::renderDataTable({
      req(store_gen$planted)
      DT::datatable(store_gen$planted, 
                    options = list(
                      pageLength = 15, 
                      scrollX = TRUE, 
                      dom = 'frtip',
                      autoWidth = FALSE,
                      columnDefs = list(list(
                        targets = "_all",
                        render = JS(
                          "function(data, type, row, meta) {",
                          "  return type === 'display' && data != null && data.length > 10 ?",
                          "    '<span title=\"' + data + '\">' + data.substr(0, 10) + '...</span>' : data;",
                          "}"
                        )
                      ))
                    ),
                    class = "compact stripe hover")
    })
    
    
    
    # 4. （已移除）回写数据库命名功能，迁移至“杂交配置”模块的 C 页右侧
    
    # 5. 导出 Excel
    output$btn_export_xlsx <- downloadHandler(
      filename = function() { 
        paste0("CrossBook_", input$gen_prefix, "_", input$gen_batch, ".xlsx") 
      },
      content = function(file) {
        req(store_gen$my_combi, store_gen$planted)
        tryCatch({
          if (exists("savewb")) {
            fields <- c("fieldid", "code", "place", "stageid", "name", "rows", "line_number", "rp")
            myview_cols <- intersect(c(fields, "ma", "pa"), names(store_gen$planted))
            mat <- NULL
            if (exists("combination_matrix")) {
              mat <- combination_matrix(store_gen$my_combi)
            }
            savewb(
              origin = store_gen$my_combi,
              planting = store_gen$planted,
              myview = store_gen$planted[, myview_cols, drop=FALSE],
              combi_matrix = mat,
              filename = file,
              overwrite = TRUE
            )
          } else {
            stop("找不到 savewb 函数")
          }
        }, error = function(e) {
          showNotification(paste("导出失败:", e$message), type = "error")
        })
      }
    )
  })
}
