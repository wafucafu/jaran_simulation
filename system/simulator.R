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


simulate_target_kpis <- function(data, curvature_df, eng_kpis, upper_cost, simulated, daily_pct){
  
  all_df　<- data.frame()
  
  for (kpi in eng_kpis){
    print(kpi)
  
    
    kpi_results <- data.frame()
    
    # 各媒体をシミュレートする
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

      # 回帰を行う
      regression <- regress_simulator(train_set, kpi)

      # 予測するコスト範囲を決める
      cost_list <- create_cost_range(upper_cost, category)
      cost_loop_df <- data.frame()

      
      # 各トータルコストを試す
      for (total_cost in cost_list){
        print(total_cost)

        # シミュレートする媒体をピックアウトする
        temp_simulated <- simulated %>% filter(category_pair == category)

        #print(temp_simulated)
        
        #print(head(daily_pct))
        
        # 日別配分を計算する
        temp_simulated <- temp_simulated %>%
          inner_join(daily_pct, by = "date") %>%
          mutate(cost = total_cost * pct,
                 sqrtcost = cost^(curvature))

        #print(temp_simulated)
        
        
        test_set <- temp_simulated %>%
          mutate_(.dots = setNames(list(0), kpi))

        # 該当コストのKPI予測値
        predicted_kpi <- predict_kpi(regression, test_set, total_cost)

        # 各コストの結果を結合する
        cost_loop_df <- rbind(cost_loop_df, predicted_kpi)


        #print(cost_loop_df)
        
        # コストのループ
      }


      output <- cost_loop_df %>%
        group_by(category_pair, category_name, device_name, total_cost) %>%
        summarise(kpi = sum(fit)) %>%
        ungroup()


      kpi_results <- rbind(kpi_results, output)

      #print(output)
      
      # 媒体のループ
    }
    

    kpi_results <- kpi_results %>%
      arrange(category_pair, total_cost) %>%
      select(category_name, device_name, category_pair, total_cost, kpi)

    if (kpi == "sales"){

      all_df <- kpi_results

    } else{

      all_df <- cbind(all_df, kpi_results$kpi)

    }

    #print(kpi_results)
    
    # KPIのループ
  }
  
  
  colnames(all_df) <- c("category_name", "device_name", "category_pair", "total_cost") %>% 
    append(eng_kpis)
  
  return(all_df)
}




