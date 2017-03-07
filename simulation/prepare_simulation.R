# シミュレートする期間を作る
create_simulator <- function(selected_dates, category_pair){
  
  # ユーザ指定の日にちを取得する
  start_date <- as.Date(selected_dates[1], origin = "1970-01-01")
  end_date <- as.Date(selected_dates[2], origin = "1970-01-01")
  simulated_dates <- as.Date(start_date:end_date, origin = "1970-01-01")
  
  pc_or_sp <- paste(c("_PC", "_SP"), collapse = "|")
  
  # 取得した期間と媒体を合体する
  simulated <- expand.grid(date = simulated_dates, category_pair = category_pair)
  simulated <- simulated %>% 
    mutate(device_name =  as.factor(ifelse(str_detect(category_pair, "PC"), "PC", 
                                           ifelse(str_detect(category_pair, "SP"), "SP", "null"))),
           category_name = gsub(pc_or_sp, "", category_pair))
  
  return(simulated)
}


# 前処理
preprocess_simulated <- function(simulated, long_holiday_flag, long_holiday_dates){
  
  # 日本の休日データを読み込む
  jp_holiday <- read_excel("data/jap_holiday.xlsx")
  jp_holiday <- jp_holiday %>% mutate(holiday = as.Date(holiday))

  
  # データタイプを変換する
  simulated <- simulated %>%
    mutate(date = as.Date(date),
           device_name = as.factor(device_name),
           category_pair = as.factor(category_pair),
           category_name = as.factor(category_name),
           week = as.factor(week(date)),
           month = as.factor(month(date)),
           year = as.factor(year(date)),
           wd = as.factor(weekdays(date)),
           holiday = as.factor(ifelse((date %in% jp_holiday$holiday | wd == "日曜日" | wd == "土曜日"),
                                      "hld", "weekd")),
           weight = 1)
  
  
  if (as.character(long_holiday_flag) == "yes"){
    
    simulated <- simulated %>% 
      mutate(long_holiday = ifelse(date >= as.Date(long_holiday_dates[1], origin = "1970-01-01") &
                                     date <= as.Date(long_holiday_dates[2], origin = "1970-01-01")),
                                    "lhld", "non")
    
  }

  
  
  # 休日後を調べるために休日を調べる
  holidays <- simulated %>% filter(holiday == "hld") %>% distinct(date)

  # # 休日後を確認する
  after_holiday <- holidays %>%
    mutate(next_day = date + 1,
           check = ifelse(next_day %in% date, 1, 0)) %>%
    filter(check != 1)

  simulated <- simulated %>%
    mutate(after_holiday = as.factor(ifelse(date %in% after_holiday$next_day,
                                            "aft_hld", "norm")))
  
  return(simulated)
}




