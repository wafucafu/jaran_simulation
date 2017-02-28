test_period_1 <- as.Date(test_start_date:test_end_date, origin = "1970-01-01")


# create target_set according to plan type
months_back <- test_start_date
month(months_back) <- month(months_back) - 7
day(months_back) <- 1





# モデリング式を確定する
create_formula <- function(){
  
  
  # create regression expression from the predictor variables
  inter_vars <- x_vars
  
  coscate <- paste0(inter_vars, paste0(":", inter_term))
  lnc_inter_vars <- paste(coscate, collapse = " + ")
  sub_const <- paste0(inter_vars, collapse = " - ")
  
  
  cateinter <- paste0(coscate[!str_detect(coscate, "device_name")], ":device_name")
  spr_inter_vars <- paste0(cateinter, collapse = " + ")
  
  
  model_function <- eval(
    parse(
      text = paste(response_term, " ~ -1 + ",
                   lnc_inter_vars, " + ", spr_inter_vars
      )))
  
  
  return(model_function)
}



# get predictor variables and response variable
filter_vars <- x_vars %>% append(inter_term) %>% append(response_term)
