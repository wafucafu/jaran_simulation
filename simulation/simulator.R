create_cost_range <- function(upper_cost, category){
  
  upper <- upper_cost %>% 
    filter(category_pair == category) %>%
    ungroup() %>% 
    select(upper_cost) %>% 
    unlist() 
  
  cost_list <- seq(cost_range_start,  upper, cost_range_step)
  
  
  return(cost_list)
}


predict_kpi <- function(regression, test_set, total_cost){
  
  predicted <- predict(regression, test_set) %>% as.numeric()
  
  predicted_kpi <- cbind(test_set, fit = predicted) %>%
    mutate(total_cost = total_cost)
  
  return(predicted_kpi)
}


# ローディング更新関数
updateProgress <- function(progress, value = NULL, detail = NULL){
  
  if(is.null(value)){
    value <- progress$getValue()
    value <- value + (progress$getMax() - value / 5)
  }
  
  progress$set(value = value, detail = detail)
}


# シミュレーション関数
simulate_target_kpis <- function(data, simulated, curvature_df, 
                                 category_pair, eng_kpis, 
                                 train_start_date, upper_cost, daily_pct,
                                 updateProgress = NULL, progress){
  
  loop_count <- 1
  
  # 各KPIをシミュレートする
  all_df　<- data.frame()
  for (kpi in eng_kpis){
    print(kpi)
    
    Sys.sleep(0.25)
    
    
    # ローディングメッセージを更新する
    if (is.function(updateProgress)){
      text <- paste0("対象KPIは", kpis[loop_count], "です。")
      updateProgress(progress, detail = text)
    }
    
    
    # 各媒体ごとにシミュレートする
    kpi_results <- data.frame()
    for (category in category_pair) {
      print(category)
    
      
      # クロスバリデーションで取得した最高精度の曲度を切り取る
      curvature <- curvature_df %>% 
        filter(category_name == category) %>% 
        slice(which.min(rmspe)) %>%
        select(curvature) %>% 
        unlist()
      
      
      # データを該当媒体に絞り、コストを計算する
      temp <- data %>%
        filter(category_pair == category) %>%
        mutate(sqrtcost = cost^curvature)
      
      
      # 学習データを作成し、直近データに重みをつける
      train_set <- curate_data(temp, curvature, train_start_date, kpi)
      train_set <- add_sample_weights(train_set, weight_value, weight_range)

      
      # 回帰モデルを作成する
      regression <- regress_simulator(train_set, kpi)

      
      # 予測するコスト範囲を決める
      cost_list <- create_cost_range(upper_cost, category)
      cost_loop_df <- data.frame()

      
      # 各トータルコストを試す
      for (total_cost in cost_list){

        # 予測するする媒体をピックアウトする
        temp_simulated <- simulated %>% filter(category_pair == category)

        
        # 日別配分を計算する
        temp_simulated <- temp_simulated %>%
          inner_join(daily_pct, by = "date") %>%
          mutate(cost = total_cost * pct,
                 sqrtcost = cost^(curvature))
        
        test_set <- temp_simulated %>%
          mutate_(.dots = setNames(list(0), kpi))

        
        # 該当コストのKPI予測値
        predicted_kpi <- predict_kpi(regression, test_set, total_cost)

        
        # 各コストの結果を結合する
        cost_loop_df <- rbind(cost_loop_df, predicted_kpi)

        
        # コストのループ
      }

      
      # 結合するデータを整理する
      output <- cost_loop_df %>%
        group_by(category_pair, category_name, device_name, total_cost) %>%
        summarise(kpi = sum(fit)) %>%
        ungroup()


      # 各媒体の結果を結合する
      kpi_results <- rbind(kpi_results, output)

      
      # 媒体のループ
    }
    

    # 各KPIのデータを結合する
    kpi_results <- kpi_results %>%
      arrange(category_pair, total_cost) %>%
      select(category_name, device_name, category_pair, total_cost, kpi)

    if (kpi == "sales"){

      all_df <- kpi_results

    } else{

      all_df <- cbind(all_df, kpi_results$kpi)

    }

    
    loop_count <- loop_count + 1
    
      
    # KPIのループ
  }
  
  
  colnames(all_df) <- c("category_name", "device_name", "category_pair", "total_cost") %>% 
    append(eng_kpis)
  
  
  
  return(all_df)
}




