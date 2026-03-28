===============================================================================
虚拟矩阵视图模块 (mod_matrix.R) 使用指南
===============================================================================

📁 文件位置：
  - 核心模块：R/mod_matrix.R
  - Shiny应用：scripts/app_matrix_view.R
  - 使用示例：scripts/example_matrix_view.R

===============================================================================
🚀 快速开始
===============================================================================

方式1：使用 R 脚本（快速查看）
------------------------------
source("R/mod_matrix.R")

# 一行代码创建矩阵
matrix <- create_cross_matrix_view()

# 查看矩阵
print(matrix[1:10, 1:10])


方式2：使用 Shiny 应用（交互式）
--------------------------------
# 运行 Shiny 应用
source("scripts/app_matrix_view.R")

# 或在 RStudio 中打开 app_matrix_view.R 点击 Run App


方式3：分步创建（自定义控制）
------------------------------
# 步骤1：创建空框架
framework <- create_matrix_framework()

# 步骤2：填充数据
filled <- fill_matrix_data(framework)

# 步骤3：查看结果
print(filled[1:20, 1:20])

===============================================================================
📋 核心功能
===============================================================================

1. create_matrix_framework() - 创建矩阵框架
   功能：根据 active=1 的亲本动态生成空矩阵
   
   参数：
   - filter_active: 是否仅显示活跃亲本（默认TRUE）
   - name_field: 行列标签字段，"name"或"id"（默认"name"）
   
   返回：
   - matrix: 空矩阵框架
   - parent_ids: 亲本ID向量
   - parent_names: 亲本名称向量
   - n_parents: 亲本数量


2. fill_matrix_data() - 填充矩阵数据
   功能：从 crosses 表查询并填充杂交组合信息
   
   参数：
   - framework: 矩阵框架对象
   - fill_value: 填充内容类型
     * "batch" - 批次名称（默认）
     * "status" - 状态（planned/completed等）
     * "count" - 种子数量
     * "mark" - 简单标记（✓）
     * "name" - 组合名称（如 P0001-P0002）
   - batch_filter: 批次过滤（可选）
   - show_reciprocal: 是否显示反交（默认TRUE）
   
   返回：填充后的矩阵


3. create_cross_matrix_view() - 一步创建完整视图
   功能：组合上述两个功能，一步到位
   
   参数：包含以上所有参数
   
   示例：
   # 显示所有批次
   matrix <- create_cross_matrix_view()
   
   # 仅显示特定批次
   matrix <- create_cross_matrix_view(
     batch_filter = c("2025春季批次")
   )
   
   # 显示状态而非批次
   matrix <- create_cross_matrix_view(
     fill_value = "status"
   )


4. matrix_summary() - 矩阵统计
   功能：统计矩阵填充情况
   
   返回：统计数据框，包含：
   - 总单元格数
   - 已填充/空单元格数
   - 填充率
   - 对角线/上三角/下三角填充数

===============================================================================
💡 使用场景
===============================================================================

场景1：查看整体杂交情况
-----------------------
matrix <- create_cross_matrix_view(
  fill_value = "batch",
  show_reciprocal = TRUE
)

# 快速定位某个亲本的所有组合
row_index <- which(rownames(matrix) == "中黄301")
print(matrix[row_index, ])  # 作为母本
print(matrix[, row_index])  # 作为父本


场景2：检查特定批次
-------------------
matrix <- create_cross_matrix_view(
  batch_filter = c("2025春季批次", "2025夏季批次"),
  fill_value = "batch"
)


场景3：查看组合名称
-------------------
matrix <- create_cross_matrix_view(
  fill_value = "name",  # 显示 P0001-P0002 等组合名称
  show_reciprocal = TRUE
)


场景4：查看完成状态
-------------------
matrix <- create_cross_matrix_view(
  fill_value = "status",
  show_reciprocal = FALSE  # 仅看正交
)


场景5：导出到 Excel
-------------------
matrix <- create_cross_matrix_view()

library(openxlsx)
write.xlsx(
  as.data.frame(matrix),
  "output/cross_matrix.xlsx",
  rowNames = TRUE
)


场景6：使用 ID 标签（大矩阵更紧凑）
----------------------------------
matrix <- create_cross_matrix_view(
  name_field = "id",  # 使用 P0001 而非完整名称
  fill_value = "mark"
)

===============================================================================
🎨 Shiny 应用功能
===============================================================================

运行 Shiny 应用后可以：

1. 实时调整参数
   - 切换活跃/全部亲本
   - 选择不同的显示内容（批次/状态/数量/标记）
   - 开关反交显示
   - 使用名称或ID作为标签

2. 交互式查看
   - 可滚动的大矩阵
   - 搜索和排序
   - 数据表格视图

3. 统计分析
   - 实时显示矩阵统计
   - 填充率可视化

===============================================================================
📊 矩阵解读
===============================================================================

矩阵结构：
- 行 = 母本（female）
- 列 = 父本（male）
- 单元格 = 杂交组合信息

示例：
        P0001  P0002  P0003
P0001   ""     "批次A" ""
P0002   "批次A" ""     "批次B"
P0003   ""     "批次B" ""

解读：
- P0001(母) × P0002(父) = 批次A
- P0002(母) × P0001(父) = 批次A（反交）
- P0002(母) × P0003(父) = 批次B
- 对角线为空（无自交）

===============================================================================
⚙️ 性能优化建议
===============================================================================

1. 大量亲本（>200）时：
   - 使用 name_field = "id" 减少显示内容
   - 使用 fill_value = "mark" 简化数据
   - 考虑分批次查看

2. 大量组合（>5000）时：
   - 使用 batch_filter 过滤批次
   - 设置 show_reciprocal = FALSE 减少显示

3. 导出大矩阵：
   - 优先使用 CSV 格式
   - Excel 有 1048576 行限制

===============================================================================
🔗 相关文件
===============================================================================

- R/mod_matrix.R                  # 核心模块代码
- scripts/app_matrix_view.R       # Shiny 交互应用
- scripts/example_matrix_view.R   # 完整使用示例
- data/db/soy_cross.db            # SQLite 数据库

===============================================================================
📞 常见问题
===============================================================================

Q1: 矩阵为什么是空的？
A: 检查 parents 表中是否有 active=1 的记录，以及 crosses 表是否有数据

Q2: 如何只看正交不看反交？
A: 设置 show_reciprocal = FALSE

Q3: 如何导出矩阵？
A: 使用 write.csv() 或 openxlsx::write.xlsx()

Q4: Shiny 应用如何运行？
A: source("scripts/app_matrix_view.R") 或在 RStudio 中点击 Run App

Q5: 矩阵太大无法显示？
A: 使用 batch_filter 过滤，或使用 print(matrix[1:50, 1:50]) 只看一部分

===============================================================================
