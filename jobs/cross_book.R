 

#devtools::load_all("E:/FangCloudSync/R_WD360/Project/soyplant")
library(openxlsx)
library(dplyr)
source("R/mod_cross.R")
library(soyplant)

if (isTRUE(getOption("run_cross_book_job", FALSE))) {
mycross<-get_crosses_by_batch("2025春季")

mycross<-join_cross_parents(mycross)


#主要参数
fields <- c("fieldid", "code", "place", "stageid", "name", "rows", "line_number", "rp")
MYPRE<-"G25c6"
##
startN = 1
only = TRUE
order = FALSE
###
interval = 999
s_prefix = MYPRE
place = "武汉"
rp = 1
digits = 3
ck = NULL
rows = 2
##
overwrite = TRUE
# 添加年份
my_combi$year <- 2025

# 组合前缀与文件路径
myfilename <- paste0("output/",MYPRE,"test.xlsx",sep="")

# 构建组合数据
mydata <- data.frame(
  ma = mycross$male_name,
  pa = mycross$female_name,
  stringsAsFactors = FALSE
)
# 检查数据框结构
if (nrow(mydata) == 0) {
  stop("❌ 错误：未获取到任何杂交组合数据，请检查批次名称或数据库连接")
}

#增加排序
mydata <- mydata %>%
  arrange(desc(ma), desc(pa))


# 一：生成组合编码
my_combi <- soyplant::get_combination(
  mydata,
  prefix = MYPRE,
  startN = 1,
  only = TRUE,
  order = FALSE
)

#回写到数据库，按批次命名
update_cross_names(batch = "2025春季", prefix = MYPRE, start_n = startN, digits = digits)



# 二：生成种植计划
planted <- my_combi |>
  soyplant::planting(
    interval = 999,
    s_prefix = MYPRE,
    place = "武汉",
    rp = 1,
    digits = 3,
    ck = NULL,
    rows = 2,
    #startN=1
  )

# 三：保存 Excel 工作簿
soyplant::savewb(
  origin = my_combi,
  planting = planted,
  myview = planted[, c(fields, "ma", "pa")],
  combi_matrix = combination_matrix(my_combi),
  filename = myfilename,
  overwrite = TRUE
)
}


