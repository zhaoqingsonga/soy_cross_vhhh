# 杂交统计 App 开发框架

## 1. 概述
本模块旨在提供大豆杂交配组的统计分析视图，基于 `R/mod_analysis.R` 提供的核心算法，帮助育种家了解亲本利用情况、评估配组多样性，并发现潜在的未利用组合。

## 2. 核心功能

### 2.1 亲本利用概览 (Overview)
- **功能**: 展示全库亲本的利用频次排名。
- **数据源**: `get_parent_usage_stats()`
- **展示形式**:
    - **可视化图表 (Plotly/ggplot2)**: 
        - Top 20 高频亲本柱状图（堆叠柱状图：作为母本次数 + 作为父本次数）。
        - 亲本利用率分布直方图。
    - **数据表 (DT)**: 完整统计列表，支持排序和搜索。
        - 字段：亲本名称 (Name)、ID、做母本次数 (Female Usage)、做父本次数 (Male Usage)、总次数 (Total)、已配对父本列表、已配对母本列表。

### 2.2 单一亲本深度分析 (Parent Detail)
- **功能**: 查询特定亲本的详细配组历史及潜力挖掘。
- **交互**: 
    - 侧边栏支持搜索/选择特定亲本。
    - 联动展示该亲本的所有统计信息。
- **数据源**: `analyze_parent_details()` 和 `find_unused_partners()`
- **展示形式**:
    - **摘要指标 (ValueBox)**: 总配组数、做母本次数、做父本次数、不同配偶数。
    - **配组历史表**: 
        - Tab 1: 作为母本的组合 (包含：批次、组合名、父本)。
        - Tab 2: 作为父本的组合 (包含：批次、组合名、母本)。
    - **配组潜力推荐**: 
        - 展示尚未与其配组的活跃亲本列表 (Unused Partners)。
        - 分为“作为母本时的潜在父本”和“作为父本时的潜在母本”。

## 3. UI 布局设计建议

建议采用模块化设计 (`mod_analysis_app.R`)，嵌入到主系统的 `navbarPage` 中。

```r
analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
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
          fluidRow(
            column(12, plotlyOutput(ns("plot_top_parents"), height = "400px"))
          ),
          hr(),
          DT::dataTableOutput(ns("tbl_all_stats"))
        ),
        
        # 详情模式视图
        conditionalPanel(
          condition = sprintf("input['%s'] == 'detail'", ns("view_mode")),
          fluidRow(
            valueBoxOutput(ns("box_total_usage"), width = 3),
            valueBoxOutput(ns("box_as_female"), width = 3),
            valueBoxOutput(ns("box_as_male"), width = 3),
            valueBoxOutput(ns("box_partners"), width = 3)
          ),
          tabsetPanel(
            tabPanel("配组历史 (作为母本)", DT::dataTableOutput(ns("tbl_detail_female"))),
            tabPanel("配组历史 (作为父本)", DT::dataTableOutput(ns("tbl_detail_male"))),
            tabPanel("未利用潜力 (推荐配偶)", 
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
```

## 4. Server 逻辑架构

1.  **初始化**:
    - 调用 `mod_parent.R` 或直接查询 DB 获取所有亲本列表，更新 `select_parent` 的选项。
2.  **概览数据 (Overview)**:
    - 响应式调用 `get_parent_usage_stats()`。
    - 使用 `plotly` 渲染堆叠柱状图 (x=亲本, y=次数, fill=角色)。
    - 渲染 `DT` 表格。
3.  **详情数据 (Detail)**:
    - 监听 `input$select_parent`。
    - 调用 `analyze_parent_details()` 获取历史数据。
    - 调用 `find_unused_partners(role="female")` 和 `find_unused_partners(role="male")` 获取潜力数据。
    - 渲染 ValueBox 和详细表格。

## 5. 集成计划
1.  **新建文件**: `R/mod_analysis_app.R`。
2.  **编码**: 实现上述 UI 和 Server 逻辑。
3.  **依赖**: 确保 `plotly` 包已安装 (在 `renv` 中添加)。
4.  **集成**: 在 `apps/app.R` 中添加新的 `tabPanel("统计分析", analysis_ui("analysis_mod"))` 并调用 server。
