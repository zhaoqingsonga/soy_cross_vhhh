library(shiny)
source("R/mod_parent.R")

# 尝试加载配置
if (file.exists("config/config.R")) {
  source("config/config.R")
  db_path <- SoyCross$config$paths$db_path
} else {
  db_path <- "data/db/soy_cross.db"
}

ui <- parent_admin_ui("admin")
server <- function(input, output, session) {
  parent_admin_server("admin", db_path = db_path)
}
shiny::runApp(shinyApp(ui = ui, server = server))

