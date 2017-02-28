################################## ジャラン月別シミューレション ###############################

# 環境設定
source("program/system_settings.R")

# データ読み込み
data <- load_raw_data(file_name)

# 媒体変数追加
data <- create_category_variable(data)

# 前処理
data <- preprocess_data(data)




### loop through each category for regression model
cost_file_name <- "data/upper_cost/upper_cost_jan.xlsx"
upper_cost <- read_upper_cost(cost_file_name)




for (j in 1:nrow(test_months)){
  print(j)
  
  
  # シミュレート期間作成
  simulated <- create_simulator(date_range, category_pair)
  
  # シミュレート期間前処理
  simulated <- preprocess_simulated(simulated, long_holiday_button, long_holiday_range)

  
  # クロスバリデーション
  curvature_df <- cross_validate(train_range[1], test_range, data, category_pair, model_function)
  
  
  all_df <- simulate_target_kpis(data, curvature_df, eng_kpis, upper_cost, simulated, daily_pct)
  

  
  all_df_final <- all_df %>% 
    select(category_pair, total_cost, sales, booking, revenue, people, cash,
           newbie, old_newbie, sales30, booking_30, revenue_30, people_30,
           cash_30, newbie_30, old_newbie_30)
  
  
  # change to japanese name
  cols <- c("施策", "予算") %>% append(names[6:19])
  
  colnames(all_df_final) <- cols
  
  y <- year(start_date)
  m <- month(start_date)
  
  write.csv(all_df_final, paste0("output/", m, "/jaran_simulation_", y, "_", m, ".csv"), quote=FALSE, row.names=FALSE, fileEncoding = "CP932")
  
}














