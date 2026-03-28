# =============================================================================
# 大豆杂交管理系统 (Soybean Cross Management System)
# 整合版主程序
# =============================================================================

#!/usr/bin/env Rscript
# 安装依赖脚本 - 运行此脚本安装所有需要的包

cat("
=======================================
  安装 Shiny 应用所需的所有依赖包
=======================================\n\n")

# 设置选项
options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  timeout = 300  # 延长超时时间
)

# 1. 安装CRAN包
cat("1. 安装CRAN包...\n")
cran_packages <- c("shiny", "DT", "DBI", "RSQLite", "dplyr", "glue", "rhandsontable", "remotes")

for (pkg in cran_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("  正在安装: %-15s", pkg))
    install.packages(pkg, quiet = TRUE)
    cat(" [完成]\n")
  } else {
    cat(sprintf("  已安装:   %-15s [跳过]\n", pkg))
  }
}

# 2. 安装GitHub包
cat("\n2. 安装GitHub包...\n")
github_packages <- list(
  soyplant = "zhaoqingsonga/soyplant"
)

for (pkg_name in names(github_packages)) {
  repo <- github_packages[[pkg_name]]
  
  if (!require(pkg_name, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("  正在安装: %-15s", pkg_name))
    cat(sprintf(" (来自: %s)\n", repo))
    
    tryCatch({
      remotes::install_github(repo, quiet = TRUE)
      cat("  安装成功！\n")
    }, error = function(e) {
      cat(sprintf("  安装失败: %s\n", e$message))
      cat("  尝试使用devtools...\n")
      
      if (!require("devtools", quietly = TRUE)) {
        install.packages("devtools", quiet = TRUE)
      }
      devtools::install_github(repo, quiet = TRUE)
    })
  } else {
    cat(sprintf("  已安装:   %-15s [跳过]\n", pkg_name))
  }
}

#cat("\n" + strrep("=", 40) + "\n")
cat("✅ 所有依赖包已安装完成！\n")
cat("现在可以运行主程序了。\n")

# 验证安装
cat("\n验证安装...\n")
library(shiny)
library(soyplant)
cat("✅ 验证通过！\n")


#

# === 加载配置 ===
tryCatch({
  config_path <- file.path(project_root, "config", "config.R")
  if (file.exists(config_path)) {
    source(config_path)
  }
}, error = function(e) {
  message("配置文件加载失败：", e$message)
})

# === 环境配置 ===

# 确定项目根目录
tryCatch({
  script_path <- normalizePath(sys.frame(1)$ofile, mustWork = FALSE)
  if (file.exists(script_path)) {
    app_dir <- dirname(script_path)
    project_root <- dirname(app_dir)
  } else {
    project_root <- getwd()
  }
}, error = function(e) { project_root <<- getwd() })

# 如果当前 wd 是 apps 或 scripts，向上修正
if (basename(project_root) %in% c("apps", "scripts")) project_root <- dirname(project_root)

tryCatch({
  config_path <- file.path(project_root, "config", "config.R")
  if (file.exists(config_path)) {
    source(config_path)
  }
}, error = function(e) {
  message("配置文件加载失败：", e$message)
})

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

# === 加载模块 ===

# 辅助函数模块
source(file.path(project_root, "R", "utils_io.R"))        # 通用IO函数
source(file.path(project_root, "R", "mod_cross.R"))
if (file.exists(file.path(project_root, "R", "mod_book.R"))) {
  source(file.path(project_root, "R", "mod_book.R"))
}
if (file.exists(file.path(project_root, "R", "mod_analysis.R"))) {
  source(file.path(project_root, "R", "mod_analysis.R"))
}

# 业务功能模块
source(file.path(project_root, "R", "mod_parent.R"))      # 亲本管理
source(file.path(project_root, "R", "mod_cross_app.R"))   # 杂交配置
source(file.path(project_root, "R", "mod_book_app.R"))    # 采集簿生成
source(file.path(project_root, "R", "mod_matrix.R"))      # 矩阵视图
source(file.path(project_root, "R", "mod_analysis_app.R")) # 统计分析

# === UI 定义 ===

ui <- navbarPage(
  title = "大豆杂交管理系统",
  theme = NULL, # 可以加载 shinythemes
  id = "main_nav",
  
  # === 全局样式 ===
  header = tags$head(
    tags$style(HTML("
      /* === Global Table Styling === */
      .dataTable tbody tr { 
        height: 20px !important; 
      }
      .dataTable { 
        font-size: 1.0em !important; 
      }
      .dataTable tbody td, .dataTable tbody th {
        padding: 2px 4px !important;
        vertical-align: middle !important;
        line-height: 1.2 !important;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        max-width: 200px; /* 默认最大宽度，配合 ellipsis */
      }
      .dataTable thead th {
        padding: 6px 4px !important;
        background-color: #f8f9fa;
        color: #495057;
        font-weight: 600;
        white-space: nowrap;
      }
      .dataTable tbody tr:hover {
        background-color: #f1f3f5 !important;
      }
      .dataTable tbody tr.selected {
        background-color: #007bff !important;
        color: white !important;
      }
      /* Handsontable Overrides */
      .handsontable .existing { background-color:#e2e3e5 !important;} 
      .handsontable .diagonal { background-color:#f8d7da !important;}
    "))
  ),
  
  # 1. 亲本管理
  tabPanel("亲本管理",
    icon = icon("users"),
    parent_admin_ui("parent_mod")
  ),
  
  # 2. 杂交配置
  tabPanel("杂交配置",
    icon = icon("random"),
    cross_app_ui("cross_mod")
  ),
  
  # 3. 采集簿生成
  tabPanel("采集簿生成",
    icon = icon("book"),
    book_app_ui("book_mod")
  ),
  
  # 4. 矩阵视图
  tabPanel("矩阵视图",
    icon = icon("th"),
    matrix_view_ui("matrix_mod")
  ),
  
  # 5. 统计分析
  tabPanel("统计分析",
    icon = icon("chart-bar"),
    analysis_ui("analysis_mod")
  ),
  
  # 关于/帮助
  tabPanel("关于",
    icon = icon("info-circle"),
    fluidPage(
      h3("大豆杂交管理系统 v2.0"),
      p("集成了亲本管理、杂交组合设计、采集簿生成及排图、数据可视化等功能。"),
      hr(),
      p("项目路径: ", project_root),
      p("数据库路径: ", db_path),
      p("最后更新: ", format(Sys.Date(), "%Y-%m-%d"))
    )
  )
)

# === Server 定义 ===

server <- function(input, output, session) {
  
  # 调用子模块
  # 注意：db_path 必须正确传递
  
  # 1. 亲本管理
  parent_admin_server("parent_mod", db_path = db_path)
  
  # 2. 杂交配置
  cross_app_server("cross_mod", db_path = db_path)
  
  # 3. 采集簿生成
  book_app_server("book_mod", db_path = db_path)
  
  # 4. 矩阵视图
  matrix_view_server("matrix_mod", db_path = db_path)
  
  # 5. 统计分析
  analysis_server("analysis_mod", db_path = db_path)
  
}

# 启动应用
shinyApp(ui = ui, server = server)
