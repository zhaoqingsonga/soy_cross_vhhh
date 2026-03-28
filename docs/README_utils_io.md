# utils_io.R 模块使用说明

## 模块概述

`utils_io.R` 是 SoyCross V2 系统的 I/O 辅助工具模块，提供统一的文件读写、路径规范化、数据库备份和矩阵导入导出功能。

**核心特性：**
- ✅ 跨平台路径处理（Windows/Unix）
- ✅ 安全的文件覆盖保护
- ✅ 自动数据库备份
- ✅ 多格式支持（Excel/CSV/RDS）
- ✅ 杂交矩阵导入导出

---

## 函数列表

### 1. 路径规范化

#### `normalize_path(p, must_exist = FALSE, create_dir = FALSE)`

统一路径分隔符并转换为绝对路径。

**参数：**
- `p`: 文件或目录路径
- `must_exist`: 是否要求路径必须存在（默认 FALSE）
- `create_dir`: 如果父目录不存在是否自动创建（默认 FALSE）

**示例：**
```r
# 规范化相对路径
path <- normalize_path("data/db/soy_cross.db")

# 要求路径必须存在
path <- normalize_path("data/parent_table.rds", must_exist = TRUE)

# 自动创建父目录
path <- normalize_path("output/reports/test.xlsx", create_dir = TRUE)
```

---

### 2. 数据库备份

#### `backup_db(db_path, dest_dir = NULL, backup_name = "backups")`

备份 SQLite 数据库文件到指定目录，文件名包含时间戳。

**参数：**
- `db_path`: 数据库文件路径
- `dest_dir`: 备份目标目录（默认为数据库所在目录）
- `backup_name`: 备份子目录名称（默认 "backups"）

**返回值：** 备份文件的完整路径

**示例：**
```r
# 备份到默认位置
backup_db("data/db/soy_cross.db")

# 备份到指定目录
backup_db("data/db/soy_cross.db", dest_dir = "backups/manual")

# 自定义备份子目录
backup_db("data/db/soy_cross.db", backup_name = "before_import")
```

**输出示例：**
```
✅ 数据库备份成功: data/db/backups/soy_cross_20251229_143025.backup
   原始文件: data/db/soy_cross.db (128.5 KB)
```

---

### 3. 通用数据写入

#### `write_table(df_list, out_path, format = NULL, overwrite = FALSE, backup_before = TRUE)`

将一个或多个数据框写入 Excel、CSV 或 RDS 格式。

**参数：**
- `df_list`: 数据框或命名列表
- `out_path`: 输出文件路径
- `format`: 输出格式（"xlsx", "csv", "rds"），默认自动识别
- `overwrite`: 是否覆盖已存在的文件（默认 FALSE）
- `backup_before`: 覆盖前是否先备份（默认 TRUE）

**示例：**
```r
# 写入单个数据框到 Excel
df <- data.frame(x = 1:10, y = letters[1:10])
write_table(df, "output/test.xlsx")

# 写入多个数据框到 Excel（多个工作表）
df_list <- list(
  sheet1 = df1, 
  sheet2 = df2
)
write_table(df_list, "output/multi_sheet.xlsx")

# 覆盖已存在的文件（先备份）
write_table(df, "output/test.xlsx", overwrite = TRUE)

# 写入 CSV（UTF-8 编码）
write_table(df, "output/test.csv")

# 写入 RDS
write_table(df, "output/test.rds")
```

**安全特性：**
- ✅ 默认不覆盖文件，需显式设置 `overwrite = TRUE`
- ✅ 覆盖前自动备份原文件
- ✅ 自动创建输出目录

---

### 4. 通用数据读取

#### `read_table(in_path, sheet = NULL, ...)`

自动识别文件扩展名（xlsx, csv, rds）并读取为数据框或列表。

**参数：**
- `in_path`: 输入文件路径
- `sheet`: Excel 工作表名称或索引（仅 xlsx 格式）
- `...`: 其他参数传递给底层读取函数

**示例：**
```r
# 读取 RDS 文件
data <- read_table("data/parent_table.rds")

# 读取 Excel 指定工作表
data <- read_table("data/crosses.xlsx", sheet = "批次1")
data <- read_table("data/crosses.xlsx", sheet = 2)  # 第2个工作表

# 读取 CSV 文件
data <- read_table("output/export.csv")
```

**支持的格式：**
- Excel: `.xlsx`, `.xls`（需要 readxl 或 openxlsx）
- CSV: `.csv`（UTF-8 编码）
- RDS: `.rds`（R 原生格式）

---

### 5. 杂交矩阵导出

#### `export_cross_matrix(con, batch = NULL, out_path = NULL, format = "xlsx", value_col = "batch")`

从数据库 crosses 表生成母本×父本的二维矩阵视图。

**参数：**
- `con`: SQLite 数据库连接对象，或数据库文件路径
- `batch`: 批次名称（可选，NULL 表示所有批次）
- `out_path`: 输出文件路径（可选，NULL 则仅返回矩阵）
- `format`: 输出格式（"xlsx", "csv", "rds"）
- `value_col`: 矩阵单元格显示的字段名（默认 "batch"）

**返回值：** 二维矩阵对象（不可见）

**示例：**
```r
# 导出所有批次的矩阵
con <- DBI::dbConnect(RSQLite::SQLite(), "data/db/soy_cross.db")
export_cross_matrix(con, out_path = "output/matrix_all.xlsx")
DBI::dbDisconnect(con)

# 导出指定批次（直接使用路径）
export_cross_matrix(
  "data/db/soy_cross.db", 
  batch = "2025春季", 
  out_path = "output/matrix_2025春季.xlsx"
)

# 导出为 RDS 格式
export_cross_matrix(
  "data/db/soy_cross.db", 
  out_path = "output/matrix.rds", 
  format = "rds"
)
```

**输出示例：**
```
✅ 已生成矩阵: 5 个母本 × 3 个父本
✅ 文件已保存: output/matrix_all.xlsx
```

---

### 6. 历史矩阵导入

#### `import_cross_matrix(in_path, sheet = NULL, batch = NULL, skip_na = TRUE)`

读取历史的二维矩阵表（Excel/CSV/RDS），标准化为长表格式。

**参数：**
- `in_path`: 输入文件路径
- `sheet`: Excel 工作表名称或索引（仅 xlsx）
- `batch`: 为所有组合指定批次名称（可选）
- `skip_na`: 是否跳过矩阵中的 NA 单元格（默认 TRUE）

**返回值：** 长表数据框（female_id, male_id, content, batch）

**示例：**
```r
# 从 Excel 导入历史矩阵
df <- import_cross_matrix("data/历史杂交矩阵.xlsx", sheet = "2024年")

# 从 RDS 导入并指定批次
df <- import_cross_matrix("data/matrix_2023.rds", batch = "2023秋季")

# 从 CSV 导入
df <- import_cross_matrix("data/matrix.csv", batch = "历史数据")
```

**输出示例：**
```
✅ 已导入 25 个杂交组合
```

**返回的数据框格式：**
```
  female_id  male_id    content       batch
1 中黄301    徐豆18     批次A        2024年
2 冀豆17     冀豆12     批次B        2024年
3 华豆17     中豆13     批次A        2024年
...
```

---

## 依赖包说明

### 必需依赖
- `DBI` - 数据库接口
- `RSQLite` - SQLite 数据库驱动

### 可选依赖（增强体验）
- `openxlsx` - 推荐，支持 Excel 多工作表读写
- `readxl` - 备选，读取 Excel 文件
- `writexl` - 备选，写入 Excel 文件

**安装命令：**
```r
# 安装必需依赖
install.packages(c("DBI", "RSQLite"))

# 安装推荐的可选依赖
install.packages("openxlsx")
```

---

## 使用场景

### 场景 1：批量操作前的安全备份

```r
# 加载模块
source("R/utils_io.R")

# 生产数据库路径
db_path <- "data/db/soy_cross.db"

# 操作前备份
backup_file <- backup_db(db_path, backup_name = "批量导入前")

# 执行批量操作
# ... 你的数据库操作 ...

cat("如有问题，可从备份恢复:", backup_file, "\n")
```

---

### 场景 2：导出杂交矩阵用于 Excel 编辑

```r
source("R/utils_io.R")

# 导出当前批次的矩阵
export_cross_matrix(
  "data/db/soy_cross.db",
  batch = "2025春季",
  out_path = "output/编辑用矩阵.xlsx"
)

# 用户在 Excel 中编辑后，重新导入
edited_data <- import_cross_matrix(
  "output/编辑后矩阵.xlsx",
  batch = "2025春季_修订"
)

# 写入数据库（需配合其他模块）
```

---

### 场景 3：多格式数据导出

```r
source("R/utils_io.R")

# 准备数据
library(DBI)
con <- dbConnect(RSQLite::SQLite(), "data/db/soy_cross.db")
crosses_data <- dbGetQuery(con, "SELECT * FROM crosses WHERE batch = '2025春季'")
dbDisconnect(con)

# 同时导出为多种格式
write_table(crosses_data, "output/crosses.xlsx")  # Excel（推荐）
write_table(crosses_data, "output/crosses.csv")   # CSV（兼容性好）
write_table(crosses_data, "output/crosses.rds")   # RDS（完整保留）
```

---

### 场景 4：历史数据迁移

```r
source("R/utils_io.R")

# 读取历史 Excel 矩阵
historical_matrix <- import_cross_matrix(
  "历史数据/2020-2024杂交记录.xlsx",
  sheet = "2024年",
  batch = "历史迁移_2024"
)

# 验证数据
cat("导入组合数:", nrow(historical_matrix), "\n")
cat("母本数:", length(unique(historical_matrix$female_id)), "\n")
cat("父本数:", length(unique(historical_matrix$male_id)), "\n")

# 写入标准格式（便于后续处理）
write_table(historical_matrix, "output/标准化数据_2024.csv")
```

---

## 错误处理

所有函数都有清晰的错误提示：

```r
# 文件不存在
read_table("nonexistent.csv")
# ❌ 路径不存在: E:/path/to/nonexistent.csv

# 文件已存在且未设置覆盖
write_table(df, "existing.xlsx")
# ❌ 文件已存在: output/existing.xlsx
#    如需覆盖，请设置 overwrite = TRUE

# 不支持的文件格式
read_table("file.unknown")
# ❌ 不支持的文件格式: unknown

# 空路径
normalize_path("")
# ❌ 路径参数不能为空
```

---

## 测试

运行单元测试验证所有功能：

```r
# 进入测试目录
setwd("tests")

# 运行测试
source("test_utils_io.R")
```

**测试覆盖：**
- ✅ 路径规范化（4个测试）
- ✅ 数据库备份（3个测试）
- ✅ 通用读写（5个测试）
- ✅ 矩阵导出（3个测试）
- ✅ 矩阵导入（3个测试）
- ✅ 错误处理（3个测试）

---

## 运行示例

运行完整示例脚本：

```r
source("scripts/example_utils_io.R")
```

这将演示所有功能并生成示例文件到 `output/` 目录。

---

## 注意事项

1. **文件覆盖保护**：所有写操作默认不覆盖文件，需显式设置 `overwrite = TRUE`
2. **自动备份**：覆盖文件前会自动创建备份（除非设置 `backup_before = FALSE`）
3. **路径规范化**：建议所有路径都通过 `normalize_path()` 处理，确保跨平台兼容
4. **编码问题**：CSV 读写使用 UTF-8 编码，支持中文
5. **数据库连接**：使用完数据库后要关闭连接，或使用 `on.exit()` 确保自动关闭

---

## 更新日志

### v1.0.0 (2025-12-29)
- ✅ 初始版本发布
- ✅ 实现所有核心功能
- ✅ 完成单元测试
- ✅ 添加使用示例

---

## 相关文档

- [开发框架.md](../开发框架.md) - 完整的 V2 系统开发框架
- [mod_field.R](../R/mod_field.R) - 田间账本模块
- [mod_cross.R](../R/mod_cross.R) - 杂交计划模块
- [mod_matrix.R](../R/mod_matrix.R) - 矩阵视图模块

---

**作者**: SoyCross 开发团队  
**最后更新**: 2025-12-29
