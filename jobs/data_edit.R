source("R/utils_io.R")

#parents的导入导出
source("R/utils_io.R")

export_parents_to_file(
  db_path = NULL,
  out_path = "temp/parents_edit.xlsx",
  overwrite = TRUE,          # 允许覆盖
  backup_before = TRUE       # 覆盖前自动备份原文件
)

# 编辑后导入（示例为 upsert，以 id 为键）
res <- import_parents_from_file(
  in_path = "temp/parents_edit.xlsx",
  db_path = NULL,
  mode = "upsert",
  key = "id",
  backup_before = TRUE
)
print(res)
