# 采集簿生成 App 框架设计

## 1. 功能概述

本模块旨在集成现有的核心算法函数（`soyplant` 包或 `mod_cross.R` 模块），在 Shiny 应用中实现**组合编号生成**、**田间种植排图**、**数据库回写**及**电子采集簿导出**的完整流程。

**核心原则**：不重复造轮子，直接调用已调试好的现有函数。

## 2. 依赖环境与函数来源

确保以下函数在 R 环境中可用。

### 2.1 核心算法 (来自 `soyplant` 包)
*需确保安装并加载 `soyplant` 包，调用时建议使用 `soyplant::` 前缀以明确来源。*

- `soyplant::get_combination(data, prefix, startN, only, order)`: 生成组合编号。
- `soyplant::planting(mydata, interval, s_prefix, place, rp, digits, ck, rows)`: 生成种植排图。
- `soyplant::savewb(origin, planting, myview, combi_matrix, filename, overwrite)`: 导出 Excel 采集簿。
- `soyplant::combination_matrix(data)`: 生成组合矩阵（用于导出）。

### 2.2 数据库与业务逻辑 (来自 `R/mod_cross.R`)
*需 `source("R/mod_cross.R")`。*

- `get_crosses_by_batch(batch)`: 获取指定批次的杂交记录。
- `join_cross_parents(crosses_data)`: 关联亲本详细信息（自动处理 `female_`/`male_` 前缀及 `ma`/`pa` 兼容字段）。
- `update_cross_names_from_df(data, batch)`: 将生成的组合名称回写到数据库。

### 2.3 依赖包
```r
library(shiny)
library(dplyr)
library(DT)
library(soyplant) # 自研包
source("R/mod_cross.R")
```

## 3. 业务逻辑流程 (示例代码)

以下代码展示了从数据库读取到生成采集簿的完整逻辑链条，已在本地调试通过。
