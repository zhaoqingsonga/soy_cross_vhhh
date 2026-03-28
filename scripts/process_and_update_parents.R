
library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(DBI)
library(RSQLite)

# Config
input_excel <- "e:/FangCloudSync/R_WD360/Project/soy_cross_v2/temp/parents_edit.xlsx"
processed_excel <- "e:/FangCloudSync/R_WD360/Project/soy_cross_v2/temp/parents_edit_processed.xlsx"

# 尝试加载配置
if (file.exists("config/config.R")) {
  source("config/config.R")
  db_path <- SoyCross$config$paths$db_path
} else {
  db_path <- "data/db/soy_cross.db"
}

# Helper function to process range strings
process_range_value <- function(x) {
  # If NA, return NA
  if (is.na(x)) return(x)
  
  # Ensure character
  x_str <- as.character(x)
  
  # Check if contains "~"
  if (str_detect(x_str, "~")) {
    # Split by "~"
    parts <- str_split(x_str, "~")[[1]]
    # Trim whitespace
    parts <- str_trim(parts)
    # Convert to numeric
    nums <- as.numeric(parts)
    
    # If conversion successful and we have numbers
    if (all(!is.na(nums)) && length(nums) > 0) {
      # Calculate mean and round to 1 decimal place
      avg <- round(mean(nums), 1)
      return(avg)
    }
  }
  
  # Return original if no "~" or parsing failed
  return(x)
}

# ==============================================================================
# Step 1: Process Excel File
# ==============================================================================
message("🚀 Step 1: Processing Excel file...")

if (!file.exists(input_excel)) {
  stop("❌ Input file not found: ", input_excel)
}

# Read Excel
df <- read_excel(input_excel, sheet = 1)
message("   Read ", nrow(df), " rows from ", basename(input_excel))

# Find "生育期_d" column
col_names <- names(df)
target_col <- "生育期_d"
target_idx <- which(col_names == target_col)

if (length(target_idx) == 0) {
  # Try fuzzy match if exact match fails
  target_idx <- grep("生育期", col_names)
  if (length(target_idx) > 0) {
    target_idx <- target_idx[1]
    message("   ⚠️  Exact match for '生育期_d' not found. Using '", col_names[target_idx], "' instead.")
  } else {
    stop("❌ Column '生育期_d' (or similar) not found in Excel file.")
  }
} else {
  message("   Found column '", target_col, "' at index ", target_idx)
}

# Identify columns to process (all columns AFTER the target column)
if (target_idx < length(col_names)) {
  cols_to_process_indices <- (target_idx + 1):length(col_names)
  cols_to_process_names <- col_names[cols_to_process_indices]
  
  message("   Processing ", length(cols_to_process_names), " columns: ", paste(head(cols_to_process_names, 3), collapse = ", "), " ...")
  
  # Apply processing
  for (col_idx in cols_to_process_indices) {
    col_name <- col_names[col_idx]
    
    # Apply function to each element in the column
    # We use sapply to handle vector processing
    df[[col_idx]] <- sapply(df[[col_idx]], process_range_value)
    
    # Try to convert to numeric if possible (if all values are now numbers)
    # suppressWarnings(
    #   if (!any(is.na(as.numeric(df[[col_idx]])))) {
    #     df[[col_idx]] <- as.numeric(df[[col_idx]])
    #   }
    # )
  }
  message("   ✅ Processing complete.")
} else {
  message("   ⚠️  No columns found after '", target_col, "'. Nothing to process.")
}

# Save processed Excel
write_xlsx(df, processed_excel)
message("   💾 Saved processed file to: ", basename(processed_excel))


# ==============================================================================
# Step 2: Update Database
# ==============================================================================
message("\n🚀 Step 2: Updating Database...")

con <- dbConnect(SQLite(), db_path)
on.exit(dbDisconnect(con))

# 2.1 Rename existing 'parents' table
if (dbExistsTable(con, "parents")) {
  # Check if 'parents_discarded' exists
  if (dbExistsTable(con, "parents_discarded")) {
    message("   ⚠️  'parents_discarded' already exists. Dropping it first.")
    dbRemoveTable(con, "parents_discarded")
  }
  
  message("   Renaming 'parents' to 'parents_discarded'...")
  dbExecute(con, "ALTER TABLE parents RENAME TO parents_discarded")
  message("   ✅ Table renamed.")
} else {
  message("   ⚠️  Table 'parents' does not exist. Skipping rename.")
}

# 2.2 Upload new data
message("   Uploading data to new 'parents' table...")

# Ensure column types are compatible (SQLite handles this well, but good to be aware)
# We write the dataframe 'df' directly. 
# dbWriteTable will create the table structure based on the dataframe.

tryCatch({
  dbWriteTable(con, "parents", df, overwrite = TRUE)
  message("   ✅ Data uploaded successfully. Rows: ", nrow(df))
  
  # 2.3 Create Indexes
  message("   Creating indexes...")
  if ("id" %in% names(df)) {
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_parents_id ON parents(id)")
  }
  if ("name" %in% names(df)) {
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_parents_name ON parents(name)")
  }
  message("   ✅ Indexes created.")
  
}, error = function(e) {
  message("   ❌ Error uploading data: ", e$message)
  # Attempt rollback/recovery if needed? 
  # For now, we leave it as is so user can debug or we can fix.
})

# Verify
if (dbExistsTable(con, "parents")) {
  count <- dbGetQuery(con, "SELECT count(*) as n FROM parents")$n
  message("\n🎉 Success! New 'parents' table has ", count, " records.")
  
  # Show schema
  # print(dbListFields(con, "parents"))
}

