#################################   Server Side   ########################################


# サーバーサイド
server <- function(input, output, session) {
  
  
  ################################## ローディング ####################################
  
  # 入力ファイルの読み込み
  raw_data <- reactive({ 
    
    reactive_file_load(input$raw_data$datapath) 
    
    })
  
  upper_cost <- reactive({ 
    
    reactive_file_load(input$upper_cost$datapath) 
    
    })
  
  daily_pct <- reactive({
    
    reactive_file_load(input$daily_pct$datapath) 
    
    })
  
  
  # アップロードされたらタブを切り替える
  observeEvent(input$raw_data, {
    
    switch_tabs(session, "過去データ")
    
  })
  
  observeEvent(input$upper_cost, {
    
    switch_tabs(session, "上限データ")
    
  })
  
  observeEvent(input$daily_pct, {
    
    switch_tabs(session, "日別割合")
    
  })
  
  
  # タブ上にサンプルを表示
  output$raw_data_table <- renderTable({ 
    
    render_sample_table(raw_data(), option = 10) 
    
    })
  
  output$upper_cost_table <- renderTable({ 
    
    render_sample_table(upper_cost(), option = 31) 
    
    })
  
  output$daily_pct_table <- renderTable({ 
    
    render_sample_table(daily_pct(), option = 31) 
    
    })
  

  # バリデーション
  validation_status <- reactiveValues(raw_data = NULL, upper_cost = NULL, daily_pct = NULL)
  
  observe({ 
    
    validation_status$raw_data <- 
      validate_raw_data(raw_data(), raw_data_check, 
                        target_columns = 5:19, target_type = c("integer", "numeric"))
    
    })
  
  observe({
    
    validation_status$upper_cost <-
      validate_upper_cost(upper_cost(), upper_cost_check, 
                          target_columns = 2, target_type = c("integer", "numeric"))
    
    })
            
  observe({
    
    validation_status$daily_pct <- 
      validate_daily_pct(daily_pct(), daily_pct_check, 
                         target_columns = 2, target_type = c("integer", "numeric"))
    
  })
  
  
  # 日付バリデーション
  observeEvent(input$date_range, {
    
    if (input$date_range[1] > input$date_range[2]){
      show_pop_up("シミュレーションの初日が最終日より未来に設定されています。")
      }
    
  })
  
  
  observeEvent(input$test_range, {
    
    if (input$test_range[1] > input$test_range[2]){
      show_pop_up("テスト期間のの初日が最終日より未来に設定されています。")
    }
    
  })
  
  observeEvent(input$train_range, {
    
    if (input$train_range[1] > input$train_range[2]){
      show_pop_up("学習期間の初日が最終日より未来に設定されています。")
    }
    
  })
  
  
  
  # 日程指定のデフォルトを入力ファイルに合わせる
  output$date_range <- renderUI({
    
    if (is.null(validation_status$raw_data))
      return(NULL)
    
    last_day <- raw_data() %>% 
      mutate(date = as.Date(date)) %>% 
      distinct(date) %>% 
      unlist() %>% 
      max() %>% 
      as.Date(origin = "1970-01-01")
      
    first_simulation_day <- last_day - day(last_day) + days_in_month(last_day) + 1
    last_simulation_day <- first_simulation_day + days_in_month(first_simulation_day) - 1
    
    dateRangeInput("date_range", "4. シミュレーション期間設定", 
                   start = first_simulation_day,
                   end = last_simulation_day,
                   format = "yyyy-mm-dd",
                   language = "ja")

  })
  
    
  output$test_range <- renderUI({
    
    if (is.null(validation_status$raw_data))
      return(NULL)
    
    last_day <- raw_data() %>% 
      mutate(date = as.Date(date)) %>% 
      distinct(date) %>% 
      unlist() %>% 
      max() %>% 
      as.Date(origin = "1970-01-01")

    dateRangeInput("test_range", "5. テスト期間を設定",
                   start = last_day - 31,
                   end = last_day,
                   format = "yyyy-mm-dd",
                   language = "ja")
    
  })
  
  
  output$train_range <- renderUI({
    
    if (is.null(validation_status$raw_data))
      return(NULL)
    
    last_day <- raw_data() %>% 
      mutate(date = as.Date(date)) %>% 
      distinct(date) %>% 
      unlist() %>% 
      max() %>% 
      as.Date(origin = "1970-01-01")
    
    dateRangeInput("train_range", "6. 学習期間を設定",
                   start = paste0("2016-01-01"),
                   end = last_day - 32,
                   format = "yyyy-mm-dd",
                   language = "ja")
    
  })
  
  
  output$long_holiday_range <- renderUI({
    
    if (is.null(validation_status$raw_data))
      return(NULL)
    
    last_day <- raw_data() %>% 
      mutate(date = as.Date(date)) %>% 
      distinct(date) %>% 
      unlist() %>% 
      max() %>% 
      as.Date(origin = "1970-01-01")
    
    first_simulation_day <- last_day - day(last_day) + days_in_month(last_day) + 1
    last_simulation_day <- first_simulation_day + days_in_month(first_simulation_day) - 1
    
    dateRangeInput("long_holiday_range", "7. 大型連休設定",
                   start = first_simulation_day,
                   end = last_simulation_day,
                   format = "yyyy-mm-dd",
                   language = "ja")
    
  })

  
  output$upper_cost_bar1 <- renderUI({
    
    if (is.null(validation_status$upper_cost))
      return(NULL)
    
    upper_cost <- upper_cost()
    colnames(upper_cost) <- c("category", "upper_cost")
    upper_cost <- upper_cost %>% cbind(rank = rownames(upper_cost))
    
    len <- nrow(upper_cost)
    
    upper_cost <- upper_cost[1:floor(len / 2),]

    print(upper_cost)

    upper_cost_bar <- mapply(function(div_id, bar_name, upper_cost){
      
      ids <- paste("bar", div_id, sep="")
      numericInput(ids, bar_name, upper_cost, 0, max = NA, step = 500000)
      
    }, len, upper_cost$category, upper_cost$upper_cost, SIMPLIFY = FALSE)
    
    
    upper_cost_bar
    
  })
  
  
  output$upper_cost_bar2 <- renderUI({
    
    if (is.null(validation_status$upper_cost))
      return(NULL)
    
    upper_cost <- upper_cost()
    colnames(upper_cost) <- c("category", "upper_cost")
    upper_cost <- upper_cost %>% cbind(rank = rownames(upper_cost))
    
    len <- nrow(upper_cost)
    
    upper_cost <- upper_cost[floor(len / 2):len,]
    
    upper_cost_bar <- mapply(function(div_id, bar_name, upper_cost){
      
      ids <- paste("bar", div_id, sep="")
      numericInput(ids, bar_name, upper_cost, 0, max = NA, step = 500000)
      
    }, len, upper_cost$category, upper_cost$upper_cost, SIMPLIFY = FALSE)
    
    
    upper_cost_bar
    
  })
  
  
  
  ################################## 下準備 ####################################
  
  # シミュレーションボタンの作成
  output$file_validated <- reactive({
    
    file_validation <- (is.null(validation_status$raw_data) | 
                          is.null(validation_status$upper_cost) | 
                          is.null(validation_status$daily_pct))
    
    return(!file_validation)
  })
  
  outputOptions(output, "file_validated", suspendWhenHidden=FALSE)
  
  
  # KPIのセレクトオプションを更新する
  observe({
    
    if (is.null(validation_status$raw_data))
      return(NULL)
    
    kpis <- colnames(raw_data())[6:19]
    
    updateSelectInput(session, "kpis", label = "KPI", choices = kpis)
    
  })
  
  
  # データの前処理を行う
  filtered_data <- reactive({
    
    if (is.null(validation_status$raw_data))
      return(NULL)

    # 英語表記にカラム名を変更する
    filtered_data <- change_column_names(raw_data())
    
    # データのフィルターを行う
    filter_data(filtered_data, input$minimum_cost, input$minimum_sales)
    
  })
  
  
  category_pair <- reactive({

    print("category")
    
    if (is.null(filtered_data()) | is.null(input$test_range))
      return(NULL)
    
    create_category_pair(filtered_data(), input$test_range)
    
  })
  
  preprocessed_data <- reactive({
    
    if (is.null(filtered_data()))
      return(NULL)
    
    # 媒体変数追加
    preprocessed_data <- create_category_variable(filtered_data(), input$test_range, category_pair())
    
    # 前処理
    preprocess_data(preprocessed_data)
    
  })

  
  # 上限コストの前処理
  processed_upper_cost <- reactive({
    
    process_upper_cost(upper_cost())
  
  })
  
  # 日別割合の前処理
  processed_daily_pct <- reactive({
    
    process_daily_pct(daily_pct())
    
  })
  
                            
  # 最適曲度のデータフレームを作成する
  curvature_df <- reactive({
    
    if (is.null(preprocessed_data()))
      return(NULL)
    
    # クロスバリデーション
    cross_validate(input$train_range[1], input$test_range, preprocessed_data(), category_pair(), model_function)
    
    
  })
  
  
  # KPIのセレクトオプションを更新する
  observe({
    
    if (is.null(validation_status$raw_data))
      return(NULL)
    
    updateSelectInput(session, "categories", label = "施策", choices = category_pair())
    
  })
  
  
  # カーブランキング
  output$curve_ranking <- renderTable({
    
    if (is.null(curvature_df()))
      return(NULL)
    
    ranking <- curvature_df()
    ranking <- ranking %>% filter(category_name == input$categories) %>% arrange(rmspe)
    
    colnames(ranking) <- c("施設", "曲度", "誤差距離", "平均誤差距離率", "トータル誤差距離率",
                           "平均以下誤差率", "平均周り誤差率", "平均以上誤差率")
    
    ranking
  })
  
  
  # シミュレーション結果用プロットパネルを作る
  output$plot_tabs <- renderUI({
    
    
    if (is.null(category_pair()))
      return(NULL)
    
    categories <- category_pair()
    id_list <- 1:length(categories)
    
    plot_tabs <- mapply(function(tab_name, div_id){
      
      plotname <- paste("plot", div_id, sep="")
      plot_output <- plotOutput(plotname)
      tabPanel(tab_name, plot_output)
      
    }, categories, id_list, SIMPLIFY = FALSE) %>% 
      unname()
    
    do.call(tabsetPanel, plot_tabs)
    
    
    
    
    
  })
  
  
  
  
  ################################## シミュレーション ####################################
  
  # シミュレーションがスタートしたらタブを切り替える
  observeEvent(input$start_simulation, {
    
    print("start_simulation")
    
    switch_tabs(session, "シミュレーション結果")
    
  })
  
  
  # シミュレーション
  simulated_results <- eventReactive(

    input$start_simulation, {
      
      # 進捗表示
      progress <- shiny::Progress$new()
      progress$set(message = "シミュレーション中です", value = 0)
      on.exit(progress$close())
      
      # シミュレート期間作成
      simulated <- create_simulator(input$date_range, category_pair())
      
      # シミュレート期間前処理
      simulated <- preprocess_simulated(simulated, input$long_holiday_button, input$long_holiday_range)
      
      # シミュレート結果
      simulate_target_kpis(preprocessed_data(), simulated, curvature_df(),
                           category_pair(), eng_kpis, input$train_range[1], 
                           processed_upper_cost(), processed_daily_pct(),
                           updateProgress, progress)
      
    })
  
  
  # プロットののタブの中身
  observe({
    
    
    if (is.null(simulated_results()))
      return(NULL)
    
    for (i in 1:length(category_pair())) {
      
      local({
        
        div_id <- i
        plotname <- paste("plot", div_id, sep="")
        
        category_pair <- category_pair()
        target_category <- category_pair[i]
        
        output[[plotname]] <- renderPlot({ 
          
          test_period <- as.Date(as.Date(input$test_range[1]):as.Date(input$test_range[2]), origin = "1970-01-01")
          
          past_data <- preprocessed_data() %>%
            filter(category_pair == target_category, 
                   date %in% test_period) %>%
            rename(total_cost = cost) %>%
            select(category_pair, total_cost, 
                   sales, booking, revenue, 
                   people, cash, newbie, old_newbie, 
                   sales_30, booking_30, revenue_30, 
                   people_30, cash_30, newbie_30, old_newbie_30) %>% 
            mutate(flag = "実績")
          
          jap_columns <- c("施策", "予算") %>% append(kpis) %>% append("flag")
          colnames(past_data) <- jap_columns
          
          
          plot <- simulated_results()
          
          plot <- plot %>%
            filter(category_pair == target_category) %>%
            select(category_pair, total_cost, 
                   sales, booking, revenue, 
                   people, cash, newbie, old_newbie, 
                   sales_30, booking_30, revenue_30, 
                   people_30, cash_30, newbie_30, old_newbie_30) %>% 
            mutate(flag = "予測")
          
          jap_columns <- c("施策", "予算") %>% append(kpis) %>% append("flag")
          colnames(plot) <- jap_columns
          
          varval <- lazyeval::interp(~target / n, 
                                     target = as.name("予算"), 
                                     n = (as.Date(input$date_range[2]) - as.Date(input$date_range[1]) + 1) %>% as.numeric())
          
          plot <- plot %>% mutate_(.dots = setNames(list(varval), "予算"))
          
          varval <- lazyeval::interp(~target / n, 
                                     target = as.name(input$kpis), 
                                     n = (as.Date(input$date_range[2]) - as.Date(input$date_range[1]) + 1) %>% as.numeric())
          
          plot <- plot %>% mutate_(.dots = setNames(list(varval), input$kpis))
          
          
          plot <- plot %>% rbind(past_data) 
          
          plot <- plot %>% select(予算, get(input$kpis), flag)
          
          
          plot %>%
            ggplot(aes(x=予算, y=get(input$kpis), color = flag)) + geom_point() + 
            xlim(0, NA) + ylim(0, NA) +
            labs(y = input$kpis)
          
        })
        
      })
      
    }
    
  })
  

  # ダウンロードボタンをシミュレーションが終わるまで隠す
  output$simulation_status <- reactive({
    
    return(!is.null(simulated_results()))
    
  })
  
  outputOptions(output, "simulation_status", suspendWhenHidden=FALSE)
  
  
  # ダウンロード処理
  output$report_download <- downloadHandler(
    
    filename = function(){
      paste("jaran_simulation", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(simulated_results(), file, row.names = FALSE, quote = FALSE, fileEncoding = "CP932")
    }
    
  )
  
}



