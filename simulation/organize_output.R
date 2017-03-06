# 出力を整理する
organize_output <- function(all_df, kpis){

  # 必要なカラムをセレクトする
  all_df_final <- all_df %>% 
    select(category_pair, total_cost, sales, booking, revenue, people, cash,
           newbie, old_newbie, sales_30, booking_30, revenue_30, people_30,
           cash_30, newbie_30, old_newbie_30)
  
  # 日本語ヘッダーに変更する
  jap_columns <- c("施策", "予算") %>% append(kpis)
  colnames(all_df_final) <- jap_columns
  
  return(all_df_final)
}

