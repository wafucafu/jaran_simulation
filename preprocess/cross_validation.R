curate_data <- function(temp, curvature, train_start_date, kpi){
  
  temp <- temp %>% 
    select(date, category_name, get(kpi), holiday, after_holiday, cost,
           device_name, month, wd, weight) %>%
    filter(date >= train_start_date) %>%
    mutate(sqrtcost = cost^curvature)
  
  return(temp)
}


standardize <- function(x, weight_range) { 
  (x - min(x)) / (max(x) - min(x)) * (weight_range - 1) + 1
}


add_sample_weights <- function(train_set, weight_value, weight_range){
  
  first_day <- min(train_set$date) - 1
  
  train_set <- train_set %>%
    mutate(weight = standardize(as.numeric(date - first_day)^weight_value, weight_range))
  
  return(train_set)
}


regress_simulator <- function(train_set, kpi){
  
  formula <- paste0(kpi, " ~ -1 + sqrtcost")
  
  regression <- lm(formula, train_set)
  
  return(regression)
}


predict_test_set <- function(regression, test_set){
  
  predicted <- predict(regression, test_set) %>% as.numeric()
  
  predicted_test <- cbind(test_set, fit = predicted)
  
  predicted_test <- predicted_test %>% 
    mutate(residual = fit - sales,
           rse = sqrt((residual)^2),
           rspe = sqrt((residual / sales)^2))
  
  return(predicted_test)
}


calculate_mean_cost <- function(predicted_test, mean_cost_range){
  
  test_mean <- predicted_test %>% select(cost) %>% unlist() %>% mean()
  ten_pct <- test_mean * mean_cost_range
  
  return(list(test_mean, ten_pct))
}


graph_result <- function(predicted_test){
  
  test_mean <- calculate_mean_cost(predicted_test, mean_cost_range)[[1]]
  ten_pct <- calculate_mean_cost(predicted_test, mean_cost_range)[[2]]
  
  
  predicted_test %>%
    mutate(flag = ifelse(cost > test_mean + ten_pct, "平均以上",
                         ifelse(cost < test_mean - ten_pct, "平均以下",
                                "平均回り"))) %>%
    select(date, cost, sales, fit, flag) %>%
    gather(vid, value, -date, -cost, -flag) %>%
    ggplot(aes(x=cost, y=value, color=flag, shape=vid)) + geom_point() +
    ggtitle(paste0('"', category, ": 日別コストvs売上")) + xlim(0, NA) + ylim(0, NA)
  
}


record_accuracy <- function(predicted_test, category, curvature){
  
  test_mean <- calculate_mean_cost(predicted_test, mean_cost_range)[[1]]
  ten_pct <- calculate_mean_cost(predicted_test, mean_cost_range)[[2]]
  
  error_above <- predicted_test %>% 
    filter(cost > test_mean + ten_pct) %>%
    summarise(sales = sum(sales), fit = sum(fit),  count = n()) %>%
    mutate(residual_above = (fit - sales) / sales * 100) %>% 
    select(residual_above) %>% unlist() 
  
  
  error_below <- predicted_test %>% 
    filter(cost < test_mean - ten_pct) %>%
    summarise(sales = sum(sales), fit = sum(fit),  count = n()) %>%
    mutate(residual_below = (fit - sales) / sales * 100) %>%
    select(residual_below) %>% unlist()
  
  error_avg <- predicted_test %>% 
    filter(cost > test_mean - ten_pct,
           cost < test_mean + ten_pct) %>%
    summarise(sales = sum(sales), fit = sum(fit),  count = n()) %>%
    mutate(residual_avg = (fit - sales) / sales * 100) %>% 
    select(residual_avg) %>% unlist()
  
  
  residual <- predicted_test$residual %>% sum()
  rmse <- predicted_test$rse %>% mean()
  rmspe <- predicted_test$rspe %>% mean() * 100
  rmspe_all <- (predicted_test$fit %>% sum() - predicted_test$sales %>% sum()) / predicted_test$sales %>% sum() * 100
  
  result <- data.frame(category_name = category, curvature, rmse, rmspe, rmspe_all, error_below, error_avg, error_above)

  
  return(result)
}


cross_validate <- function(train_start_date, test_range, data, category_pair, model_function){
  
  result_df <- data.frame()
  kpi <- cross_validation_kpi
  test_period <- as.Date(as.Date(test_range[1]):as.Date(test_range[2]), origin = "1970-01-01")
  
  for (category in category_pair) {

    temp <- data %>% filter(category_pair == category)

    for (curvature in curvature_list){

      temp <- curate_data(temp, curvature, train_start_date, kpi)
      train_set <- temp %>% filter(!date %in% test_period)
      test_set <- temp %>% filter(date %in% test_period)

      train_set <- add_sample_weights(train_set, weight_value, weight_range)

      regression <- regress_simulator(train_set, kpi)

      predicted_test <- predict_test_set(regression, test_set)

      #graph_result(predicted_test)

      result <- record_accuracy(predicted_test, category, curvature)

      result_df <- rbind(result_df, result)

    }


  }
  
  
  return(result_df)
  
}


