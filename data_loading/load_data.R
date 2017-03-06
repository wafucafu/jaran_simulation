#　ファイル読み込み
reactive_file_load <- function(datapath){
  
  if (is.null(datapath))
    return(NULL)
  
  data <- read.csv(file(datapath, encoding="shift-jis"), sep=",", header=T)
  
  return(data)
}



# 入力データを表示する
render_sample_table <- function(data, option){
  
  if (is.null(data))
    return(NULL)
  
  return(head(data, option))
}


# ナビゲーションバーのタブを切り替える
switch_tabs <- function(session, destination){
  
  updateNavbarPage(session, "navibar", selected = destination)
  
}

