library(readxl)

f <- "e:/FangCloudSync/R_WD360/Project/soy_cross_v2/temp/parents_edit.xlsx"
if(file.exists(f)) {
  df <- read_excel(f, sheet = 1)
  print(names(df))
  
  # Check if "生育期_d" exists
  if ("生育期_d" %in% names(df)) {
    idx <- which(names(df) == "生育期_d")
    message(sprintf("'生育期_d' found at column index: %d", idx))
    message("Columns after '生育期_d':")
    print(names(df)[(idx+1):ncol(df)])
  } else {
    message("'生育期_d' not found in columns.")
  }
} else {
  message("File not found.")
}