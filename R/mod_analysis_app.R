# =============================================================================
# 模块名称：mod_analysis_app.R
# 功能描述：杂交统计分析模块 UI 与 Server
# =============================================================================

# 确保加载必要的库
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(DBI)
library(RSQLite)
# 尝试加载 plotly，如果不存在则回退到 ggplot2
has_plotly <- requireNamespace("plotly", quietly = TRUE)
if (has_plotly) library(plotly)

# === UI 定义 ===

analysis_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # 引入简单的 ValueBox CSS (仿 AdminLTE)
    tags$head(
      tags$style(HTML("
        .small-box {
          border-radius: 2px;
          position: relative;
          display: block;
          margin-bottom: 20px;
          box-shadow: 0 1px 1px rgba(0,0,0,0.1);
          color: #fff; 
          padding: 10px;
          overflow: hidden;
        }
        .small-box.bg-primary { background-color: #3c8dbc !important; }
        .small-box.bg-success { background-color: #00a65a !important; }
        .small-box.bg-warning { background-color: #f39c12 !important; }
        .small-box.bg-danger  { background-color: #dd4b39 !important; }
        .small-box .inner h3 { font-size: 30px; font-weight: bold; margin: 0 0 10px 0; white-space: nowrap; padding: 0; }
        .small-box .inner p { font-size: 15px; }
        .small-box .icon { position: absolute; top: 10px; right: 10px; font-size: 50px; color: rgba(0,0,0,0.15); }
      "))
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("统计分析控制面板"),
        radioButtons(ns("view_mode"), "查看模式", 
                     choices = c("全库概览" = "overview", "单亲本详情" = "detail")),
        hr(),
        
        # 仅在详情模式显示
        conditionalPanel(
          condition = sprintf("input['%s'] == 'detail'", ns("view_mode")),
          selectizeInput(ns("select_parent"), "选择/搜索亲本", choices = NULL, multiple = FALSE),
          helpText("选择一个亲本查看其详细配组历史和未利用潜力。")
        ),
        
        hr(),
        downloadButton(ns("btn_export"), "导出当前报表")
      ),
      
      mainPanel(
        width = 9,
        
        # 概览模式视图
        conditionalPanel(
          condition = sprintf("input['%s'] == 'overview'", ns("view_mode")),
          DT::dataTableOutput(ns("tbl_all_stats"))
        ),
        
        # 详情模式视图
        conditionalPanel(
          condition = sprintf("input['%s'] == 'detail'", ns("view_mode")),
          fluidRow(
            uiOutput(ns("box_total_usage")),
            uiOutput(ns("box_as_female")),
            uiOutput(ns("box_as_male")),
            uiOutput(ns("box_partners"))
          ),
          tabsetPanel(
            tabPanel("配组历史 (作为母本)", 
                     br(),
                     DT::dataTableOutput(ns("tbl_detail_female"))
            ),
            tabPanel("配组历史 (作为父本)", 
                     br(),
                     DT::dataTableOutput(ns("tbl_detail_male"))
            ),
            tabPanel("未利用潜力 (推荐配偶)", 
                     br(),
                     fluidRow(
                       column(6, h4("可用的新父本"), DT::dataTableOutput(ns("tbl_unused_males"))),
                       column(6, h4("可用的新母本"), DT::dataTableOutput(ns("tbl_unused_females")))
                     )
            )
          )
        )
      )
    )
  )
}

# === Server 定义 ===

analysis_server <- function(id, db_path) {
  moduleServer(id, function(input, output, session) {
    
    # 辅助函数：渲染自定义 ValueBox
    render_info_box <- function(title, value, icon_name, color = "bg-primary") {
      div(class = paste("col-sm-3"),
          div(class = paste("small-box", color),
              div(class = "inner",
                  h3(value),
                  p(title)
              ),
              div(class = "icon",
                  icon(icon_name)
              )
          )
      )
    }
    
    # 1. 初始化：加载亲本列表
    observe({
      req(file.exists(db_path))
      con <- dbConnect(SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      
      parents <- dbGetQuery(con, "SELECT name FROM parents ORDER BY name")
      updateSelectizeInput(session, "select_parent", choices = parents$name, server = TRUE)
    })
    
    # 2. 获取概览数据
    all_stats <- reactive({
      get_parent_usage_stats(db_path)
    })
    
    # 3. 概览不展示图表，仅显示数据表
    
    # 5. 渲染概览表格
    output$tbl_all_stats <- DT::renderDataTable({
      df <- all_stats()
      if (nrow(df) > 0) {
        required_cols <- c("name", "id", "usage_female", "usage_male", "usage_total", "males", "females")
        for (col in required_cols) if (!col %in% names(df)) df[[col]] <- NA
        df_display <- df[, required_cols, drop = FALSE]
        colnames(df_display) <- c("亲本", "ID", "母本次数", "父本次数", "总次数", "已配父本", "已配母本")
      } else {
        df_display <- data.frame(亲本=character(0), ID=character(0), 母本次数=integer(0), 父本次数=integer(0), 总次数=integer(0), 已配父本=character(0), 已配母本=character(0), stringsAsFactors = FALSE)
      }
      datatable(df_display,
                options = list(pageLength = 15, scrollX = TRUE),
                rownames = FALSE)
    })
    
    # 5. 获取单亲本详情数据
    parent_details <- reactive({
      req(input$select_parent)
      analyze_parent_details(input$select_parent, db_path)
    })
    
    # 6. 渲染 ValueBoxes
    output$box_total_usage <- renderUI({
      req(parent_details(), parent_details()$summary)
      info <- parent_details()$summary
      total_crosses <- if(is.null(info$total_crosses)) 0 else info$total_crosses
      render_info_box("总配组数", total_crosses, "random", "bg-primary")
    })
    
    output$box_as_female <- renderUI({
      req(parent_details(), parent_details()$summary)
      info <- parent_details()$summary
      count_as_female <- if(is.null(info$count_as_female)) 0 else info$count_as_female
      render_info_box("作为母本", count_as_female, "venus", "bg-danger")
    })
    
    output$box_as_male <- renderUI({
      req(parent_details(), parent_details()$summary)
      info <- parent_details()$summary
      count_as_male <- if(is.null(info$count_as_male)) 0 else info$count_as_male
      render_info_box("作为父本", count_as_male, "mars", "bg-success")
    })
    
    output$box_partners <- renderUI({
      req(parent_details(), parent_details()$summary)
      info <- parent_details()$summary
      unique_partners <- if(is.null(info$unique_partners)) 0 else info$unique_partners
      render_info_box("不同配偶数", unique_partners, "users", "bg-warning")
    })
    
    # 7. 渲染详情表格
    output$tbl_detail_female <- DT::renderDataTable({
      req(parent_details())
      df <- parent_details()$as_female
      if(is.null(df)) df <- data.frame()  # 空数据框作为默认值
      datatable(df, options = list(pageLength = 10), rownames = FALSE)
    })
    
    output$tbl_detail_male <- DT::renderDataTable({
      req(parent_details())
      df <- parent_details()$as_male
      if(is.null(df)) df <- data.frame()  # 空数据框作为默认值
      datatable(df, options = list(pageLength = 10), rownames = FALSE)
    })
    
    # 8. 渲染潜力推荐表格
    output$tbl_unused_males <- DT::renderDataTable({
      req(input$select_parent)
      df <- find_unused_partners(input$select_parent, role = "female", db_path)
      datatable(df, options = list(pageLength = 10), rownames = FALSE, caption = "该亲本作为母本时，可尝试的父本")
    })
    
    output$tbl_unused_females <- DT::renderDataTable({
      req(input$select_parent)
      df <- find_unused_partners(input$select_parent, role = "male", db_path)
      datatable(df, options = list(pageLength = 10), rownames = FALSE, caption = "该亲本作为父本时，可尝试的母本")
    })
    
    # 9. 导出功能 (Excel 格式)
    output$btn_export <- downloadHandler(
      filename = function() {
        paste("parent_stats_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        # 检查是否安装 openxlsx
        if (!requireNamespace("openxlsx", quietly = TRUE)) {
          showNotification("请先安装 openxlsx 包以导出 Excel 文件", type = "error")
          return(NULL)
        }
        
        wb <- openxlsx::createWorkbook()
        
        # Sheet 1: 概览数据
        openxlsx::addWorksheet(wb, "统计概览")
        openxlsx::writeData(wb, "统计概览", all_stats())
        
        # 如果在详情模式且已选择亲本，导出详情
        if (input$view_mode == "detail" && !is.null(input$select_parent) && input$select_parent != "") {
           details <- tryCatch({
             parent_details()
           }, error = function(e) {
             NULL  # 如果出错，返回NULL
           })
           
           if (!is.null(details)) {  # 只有在details不为NULL时才继续
             # Sheet 2: 作为母本
             if (!is.null(details$as_female) && is.data.frame(details$as_female) && nrow(details$as_female) > 0) {
               openxlsx::addWorksheet(wb, "作为母本")
               openxlsx::writeData(wb, "作为母本", details$as_female)
             }
             
             # Sheet 3: 作为父本
             if (!is.null(details$as_male) && is.data.frame(details$as_male) && nrow(details$as_male) > 0) {
               openxlsx::addWorksheet(wb, "作为父本")
               openxlsx::writeData(wb, "作为父本", details$as_male)
             }
           }
           
           # Sheet 4: 推荐父本
           unused_males <- tryCatch(find_unused_partners(input$select_parent, role = "female", db_path), error = function(e) NULL)
           if (!is.null(unused_males) && is.data.frame(unused_males) && nrow(unused_males) > 0) {
             openxlsx::addWorksheet(wb, "推荐父本")
             openxlsx::writeData(wb, "推荐父本", unused_males)
           }
           
           # Sheet 5: 推荐母本
           unused_females <- tryCatch(find_unused_partners(input$select_parent, role = "male", db_path), error = function(e) NULL)
           if (!is.null(unused_females) && is.data.frame(unused_females) && nrow(unused_females) > 0) {
             openxlsx::addWorksheet(wb, "推荐母本")
             openxlsx::writeData(wb, "推荐母本", unused_females)
           }
        }
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
  })
}
