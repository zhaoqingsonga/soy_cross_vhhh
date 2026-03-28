# =============================================================================
# Shiny 应用：杂交组合矩阵视图
# 功能描述：交互式展示和管理杂交组合矩阵
# 创建日期：2025-12-29
# =============================================================================

# 加载必要的包
library(shiny)
library(rhandsontable)
library(DT)
library(dplyr)
library(tidyr)

# 获取项目根目录
# 方法1：尝试使用脚本路径
tryCatch({
  script_path <- normalizePath(sys.frame(1)$ofile, mustWork = FALSE)
  if (file.exists(script_path)) {
    app_dir <- dirname(script_path)
    project_root <- dirname(app_dir)
  } else {
    project_root <- getwd()
  }
}, error = function(e) {
  # 方法2：使用当前工作目录
  project_root <<- getwd()
})

# 如果工作目录已经是 scripts 或 apps，则向上一层
if (basename(project_root) %in% c("scripts", "apps")) {
  project_root <- dirname(project_root)
}

# 加载矩阵视图模块
mod_matrix_path <- file.path(project_root, "R", "mod_matrix.R")
if (!file.exists(mod_matrix_path)) {
  stop("❌ 找不到 mod_matrix.R 文件：", mod_matrix_path, 
       "\n当前项目根目录：", project_root,
       "\n请确保在项目根目录运行此应用")
}
source(mod_matrix_path)

# =============================================================================
# UI 定义
# =============================================================================

ui <- matrix_view_ui("matrix_viewer")

# =============================================================================
# Server 定义
# =============================================================================

server <- function(input, output, session) {
  
  # 数据库路径
  db_path <- file.path(project_root, "data", "db", "soy_cross.db")
  
  # 调用矩阵视图模块
  matrix_view_server(
    "matrix_viewer",
    db_path = db_path
  )
  
}

# =============================================================================
# 运行应用
# =============================================================================

shinyApp(ui = ui, server = server)
