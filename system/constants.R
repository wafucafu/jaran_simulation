x_vars <- c("wd", "holiday")
x_vars_special <- c("category_id", "device_id", "month")

inter_term <- "sqrt"
response_term <- "sales"


# 試す曲度のリスト
curvature_list <- seq(0.5, 0.9, 0.1)


### シミュレーション関数
cost_range_start <- 500000
cost_range_step <- 500000

cross_validation_kpi <- "sales"

mean_cost_range <- 0.1



### 詳細設定

# 重み設定
weight_value <- 3
weight_range <- 100
weight_range_min <- 2
weight_range_max <- 200

# 下限設定
minimum_cost_default <- 1000
minimum_cost_min <- 0
minimum_cost_max <- 2000

minimum_sales_default <- 1000
minimum_sales_min <- 0
minimum_sales_max <- 2000


### バリデーション

# 生データの管理
raw_data_check <- c("name", "date", "imp", "click", "cost", "新想定売上3_s", "予約数_s", "利用料売上_s",
                    "人泊数_s", "取扱額_s", "新規予約数_s", "旧新規数_s", "新想定売上3_c", "予約数_c", 
                    "利用料売上_c", "人泊数_c", "取扱額_c", "新規予約数_c", "旧新規数_c", "休日", "休日明け", 
                    "大型連休", "特日", "除外日", "dum4", "dum5")

# 上限コストの管理
upper_cost_check　<- c("施策", "上限")


# 日別割合の管理
daily_pct_check <- c("date", "割合")


