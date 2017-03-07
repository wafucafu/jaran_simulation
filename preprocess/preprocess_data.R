# カラム名を英語に変更
change_column_names <- function(data){
  
  # 日本語のカラム名をセーブする
  kpis <<- colnames(data)[6:19]
  
  # カラムネームを英語に変える
  colnames(data) <- c("category_pair", "date", "imp", "click", "cost", 
                      "sales", "booking", "revenue", "people", "cash", 
                      "newbie", "old_newbie", "sales_30", "booking_30", 
                      "revenue_30", "people_30", "cash_30", "newbie_30", 
                      "old_newbie_30", "holiday", "after_holiday", "long_holiday", 
                      "tokubi", "exception", "dum4", "dum5")
  
  # 英語のカラム名をセーブする
  eng_kpis <<- colnames(data)[6:19]
  
  
  return(data)
} 


# データのフィルター
filter_data <- function(data, minimum_cost, minimum_sales){
  
  data <- data %>%
    filter(exception != 1, tokubi != 1,
           cost > minimum_cost, sales > minimum_sales)
  
  return(data)
}


# シミュレーションする媒体のリストを作る
create_category_pair <- function(data, test_range){
  
  # ユニークな媒体*デバイス変数を作る
  test_period <- as.Date(as.Date(test_range[1]):as.Date(test_range[2]), origin = "1970-01-01")
  category_pair <- data %>%
    filter(as.Date(date) %in% test_period) %>% 
    distinct(category_pair) %>% 
    unlist(use.name = FALSE)
  
  return(category_pair)
}


# カテゴリ変数を作る
create_category_variable <- function(data, test_range, category_pair){

  # ユニークな媒体変数を作る
  pc_or_sp <- paste(c("_PC", "_SP"), collapse = "|")
  categories <<- gsub(pc_or_sp, "", category_pair)
  category_list <<- categories %>% unique()
  
  #print(data)
  
  # データに媒体情報を追加
  df <- data.frame(category_pair, category_name = categories)
  data <- data %>% 
    inner_join(df, by = "category_pair")
  
  return(data)
}

# 前処理
preprocess_data <- function(data){
  
  # データタイプの変換
  data <- data %>%
    mutate(date = as.Date(date),
           device_name = as.factor(ifelse(str_detect(category_pair, "PC"), "PC", 
                                          ifelse(str_detect(category_pair, "SP"), "SP", "null"))),
           category_pair = as.factor(category_pair),
           category_name = as.factor(category_name),
           week = as.factor(week(date)),
           month = as.factor(month(date)), 
           year = as.factor(year(date)),
           wd = as.factor(weekdays(date)),
           holiday = as.factor(ifelse(holiday == 1, "hld", "weekd")),
           after_holiday = as.factor(ifelse(after_holiday == 1, "aft_hld", "norm")),
           long_holiday = as.factor(ifelse(long_holiday == 1, "lhld", "non")),
           tokubi = as.factor(ifelse(tokubi == 1, "yes", "no")),
           weight = 1) 
  
  
  ### データを選ぶ
  data <- data %>%
    select(date, cost, 
           sales, booking, revenue,
           people, cash, newbie, old_newbie, 
           sales_30, booking_30, revenue_30, 
           people_30, cash_30, newbie_30, old_newbie_30, 
           category_name, device_name, category_pair, 
           wd, holiday, after_holiday, long_holiday, 
           tokubi, year, month, week, 
           weight, exception)
  
  return(data)
}


# 上限コストの読み込み
process_upper_cost <- function(upper_cost){
  
  upper_cost <- upper_cost
  colnames(upper_cost) <- c("category_pair", "upper_cost")
  upper_cost$category_pair <- upper_cost$category_pair %>% as.factor()

  return(upper_cost)
}

# 日別割合を読み込む
process_daily_pct <- function(daily_pct){
  
  # 日別割合の読み込み
  daily_pct <- daily_pct
  colnames(daily_pct) <- c("date", "pct")
  daily_pct <- daily_pct %>% mutate(date = as.Date(date))
  
  return(daily_pct)
}


