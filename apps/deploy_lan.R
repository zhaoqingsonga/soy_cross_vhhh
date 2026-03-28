# =============================================================================
# 局域网共享启动脚本 (LAN Deployment Script)
# 作用：在本地启动服务，并允许局域网内的其他电脑访问
# =============================================================================

library(shiny)

# 1. 自动定位应用目录
# 尝试获取当前脚本所在路径
get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(sub("--file=", "", file_arg)))
  } else if (Sys.getenv("RSTUDIO") == "1") {
    return(dirname(rstudioapi::getSourceEditorContext()$path))
  } else {
    return(getwd())
  }
}

app_dir <- tryCatch(get_script_path(), error = function(e) getwd())

# 如果当前不在 apps 目录下，尝试拼接
if (basename(app_dir) != "apps" && dir.exists(file.path(app_dir, "apps"))) {
  app_dir <- file.path(app_dir, "apps")
}

message("应用目录: ", app_dir)
setwd(app_dir)

# 2. 获取本机 IP 地址 (兼容 Windows 和 Linux/WSL)
ip_info <- list()
os_type <- Sys.info()["sysname"]

if (os_type == "Windows") {
  # Windows: 使用 ipconfig
  tryCatch({
    raw_info <- system("ipconfig", intern = TRUE)
    # 尝试将 GBK (CP936) 转换为 UTF-8，忽略无法转换的字符
    # 这一步是为了解决中文 Windows 下 "Ethernet adapter..." 含有中文导致的 grep 报错
    raw_info_safe <- tryCatch({
        iconv(raw_info, from = "GBK", to = "UTF-8", sub = "")
    }, error = function(e) raw_info)
    
    # 如果转换后为空（极端情况），则使用原始数据
    if (length(raw_info_safe) == 0) raw_info_safe <- raw_info
    
    ipv4_lines <- grep("IPv4", raw_info_safe, value = TRUE)
    for (line in ipv4_lines) {
      ip <- regmatches(line, regexpr("\\d+\\.\\d+\\.\\d+\\.\\d+", line))
      if (length(ip) > 0) ip_info <- c(ip_info, ip)
    }
  }, error = function(e) {
    message("IP 自动检测失败 (非致命错误): ", e$message)
  })
} else {
  # Linux / WSL: 使用 hostname -I 或 ip addr
  tryCatch({
    # 方法1: hostname -I (常见于 Debian/Ubuntu/WSL)
    raw_ips <- system("hostname -I", intern = TRUE)
    ips <- unlist(strsplit(raw_ips, " "))
    ip_info <- c(ip_info, ips[nzchar(ips)])
  }, error = function(e) {
    # 方法2: ip addr (通用 Linux)
    tryCatch({
      raw_info <- system("ip addr", intern = TRUE)
      inet_lines <- grep("inet ", raw_info, value = TRUE)
      inet_lines <- grep("127.0.0.1", inet_lines, invert = TRUE, value = TRUE) # 排除回环
      for (line in inet_lines) {
        ip <- regmatches(line, regexpr("\\d+\\.\\d+\\.\\d+\\.\\d+", line))
        if (length(ip) > 0) ip_info <- c(ip_info, ip)
      }
    }, error = function(e2) {})
  })
}

if (length(ip_info) > 0) {
  message("\n=== 部署成功！局域网内其他电脑请访问以下地址之一 ===")
  for (ip in unique(unlist(ip_info))) {
    message(paste0("http://", ip, ":3838"))
  }
  message("====================================================\n")
} else {
  message("\n注意：未能自动检测到 IP 地址，请手动查看本机 IP。")
  message("应用将在 http://0.0.0.0:3838 启动。\n")
}

# 3. 启动服务
# host = "0.0.0.0" 表示允许任何 IP 访问
tryCatch({
  runApp("app.R", host = "0.0.0.0", port = 3838, launch.browser = TRUE)
}, error = function(e) {
  message("启动失败 (端口 3838 可能被占用): ", e$message)
  message("尝试使用随机端口启动...")
  runApp("app.R", host = "0.0.0.0", launch.browser = TRUE)
})