# エラーメッセージ
wrong_column_names <- "アップロードしたファイルに予想外なカラム名があります。"
bad_date_column <- "dateコラムで不正データがあります。"
non_numeric_columns <- "数値ではないコラムがあります。"


# ポップアップ関数
show_pop_up <- function(error_message){
  
  showModal(modalDialog(
    title = "エラーメッセージ",
    error_message,
    easyClose = TRUE,
    footer = modalButton("OK")
  ))
  
}


# カラムネームバリデーション
validate_column_name <- function(data, expected_names){
  
  # カラム名の差異を確認する 
  mismatch <- try(which(colnames(data) != expected_names))

  # 違反を犯していたらポップアップメッセージを出す
  if (length(mismatch) > 0) {
    show_pop_up(wrong_column_names)
    return(NULL)
  }
  
  return(TRUE)
}


# 日付が変換できることを確認する
validate_date_column <- function(data){
  
  date_check <- try(as.Date(data$date))
  
  if (class(date_check) == "try-error" || is.na(date_check)) {
    show_pop_up(bad_dates)
    return(NULL)
  }
  
  return(TRUE)
}


# 入力データが数値であることを確認する
validate_column_types <- function(data, target_columns, target_type){
  
  class_check <- lapply(data, function(x) {class(x)})
  non_numeric <- which(class_check[target_columns] %in% target_type)
  
  if (length(non_numeric) != length(target_columns)){
    show_pop_up(non_numeric_columns)
    return(NULL)
  }
  
  return(TRUE)
}


# メインバリデーション関数
validate_raw_data <- function(data, expected_names, target_columns = NULL, target_type = NULL){
  
  if (is.null(data))
    return(NULL)
  
  if (is.null(validate_column_name(data, expected_names)))
    return(NULL)
  
  if (is.null(validate_date_column(data)))
    return(NULL)
  
  if (is.null(validate_column_types(data, target_columns, target_type)))
    return(NULL)
  
  return(TRUE)
}


validate_upper_cost <- function(data, expected_names, target_columns = NULL, target_type = NULL){
  
  if (is.null(data))
    return(NULL)
  
  if (is.null(validate_column_name(data, expected_names)))
    return(NULL)
  
  if (is.null(validate_column_types(data, target_columns, target_type)))
    return(NULL)
  
  return(TRUE)
}


validate_daily_pct <- function(data, expected_names, target_columns = NULL, target_type = NULL){
  
  if (is.null(data))
    return(NULL)
  
  if (is.null(validate_column_name(data, expected_names)))
    return(NULL)
  
  if (is.null(validate_date_column(data)))
    return(NULL)
  
  if (is.null(validate_column_types(data, target_columns, target_type)))
    return(NULL)
  
  return(TRUE)
}