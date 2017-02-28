# 環境設定
library(shiny)
library(readxl)
library(lubridate)
library(dplyr)
library(stringr)


source("preprocess_data.R")
source("prepare_simulation.R")
source("cross_validation.R")
source("constants.R")
source("simulator.R")

#################################   Front End   ########################################


# フロント仕様
ui <- fluidPage(
  
  
  # タイトル
  headerPanel("ROASシミュレーション"),
  
  HTML('</br>'),
  
  
  # サイドパネル
  sidebarPanel(
    
    
    # 生データ入力ボタン
    fileInput(inputId = "raw_data",
              label = "1. 生データをアップロードしてください"),
    
    HTML('</br>'),
    
    
    # 上限コスト入力ボタン
    fileInput(inputId = "upper_cost",
              label = "2. 上限コストをアップロードしてください"),
    
    HTML('</br>'),
    
    
    # デイリーアロケ率入力ボタン
    fileInput(inputId = "daily_pct",
              label = "3. 日別割合をアップロードしてください"),
    
    HTML('</br>'),
    
    
    # シミューレション期間を決める
    dateRangeInput("date_range", "4. シミュレーション期間設定", 
                   start = paste0("2017-01-01"),
                   end = paste0("2017-01-31"),
                   format = "yyyy-mm-dd",
                   language = "ja"),
    
    
    # テスト期間を決める
    dateRangeInput("test_range", "5. テスト期間を設定",
                   start = paste0("2016-11-14"),
                   end = paste0("2016-12-14"),
                   format = "yyyy-mm-dd",
                   language = "ja"),
    
    
    # 学習期間を決める
    dateRangeInput("train_range", "6. 学習期間を設定",
                   start = paste0("2016-01-01"),
                   end = paste0("2016-12-14"),
                   format = "yyyy-mm-dd",
                   language = "ja"),
    
    
    # シミュレーション内での大型連休を決める
    radioButtons("long_holiday_button", "6. 大型連休設定", 
                 c("該当なし" = "no", "該当あり" = "yes")),
    
    
    conditionalPanel(condition = "input.long_holiday_button == 'yes' ",
                     dateRangeInput("long_holiday_range", "6. 大型連休設定",
                                    start = paste0(Sys.Date() - day(Sys.Date()) + 1),
                                    end = paste0(Sys.Date() + days_in_month(Sys.Date()) - day(Sys.Date())),
                                    format = "yyyy-mm-dd",
                                    language = "ja")),
    
    HTML('</br>'),
    
    
    actionButton("start_simulation", "シミュレーション開始"),
    
    
    HTML('</br>')
    
    #category_pair <- list(),
    
    #conditionalPanel(condition = "output.fileUploaded == true ",
    #                 selectInput("category", "媒体", 
    #                             c(as.character(category_pair))))
    
  ),
  
  
  # メインパネル
  mainPanel(
    
    # 入力データを出力
    tableOutput("raw_data_table"),
    
    # 上限コストを出力
    tableOutput("upper_cost_table"),
    
    # 日別割合出力
    tableOutput("daily_pct_table"),
    
    
    tableOutput("processed_table"),

    
    tableOutput("simulated_table")
    
    
    
  )
  
  # ダウンロードボタン
  #downloadButton('downloadData', 'ダウンロード')
  
  )







#################################   Server Side   ########################################



# サーバーサイド
server <- function(input, output) {
  
  
  output$raw_data_table <- renderTable({
    
    inFile <- input$raw_data
    
    if (is.null(inFile))
      return(NULL)
    
    
    raw_data <- read.csv(file(input$raw_data$datapath,encoding="shift-jis"),sep=",",header=T)
    
    head(raw_data, 1)
    
  })
  
  
  output$upper_cost_table <- renderTable({
    
    inFile <- input$upper_cost
    
    if (is.null(inFile))
      return(NULL)
    
    upper_cost <- read.csv(file(input$upper_cost$datapath,encoding="shift-jis"),sep=",",header=T)
    
    head(upper_cost, 1)
    
  })
  
  
  # 
  output$daily_pct_table <- renderTable({
    
    inFile <- input$daily_pct
    
    if (is.null(inFile))
      return(NULL)
    
    daily_pct <- read.csv(file(input$daily_pct$datapath,encoding="shift-jis"),sep=",",header=T)
    
    head(daily_pct, 1)
    
  })
  
  
  
  #　生データ→前処理
  data <- reactive({
    
    if (is.null(input$raw_data))
      return(NULL)
    
    # データ読み込み
    data <- load_raw_data(input$raw_data$datapath)
    
    # 媒体変数追加
    data <- create_category_variable(data)
    
    # 前処理
    data <- preprocess_data(data)
    
    data
  })
  
  
  # 最適曲度のデータフレーム
  curvature_df <- reactive({
    
    if (is.null(input$raw_data))
      return(NULL)
    
    # クロスバリデーション
    cross_validate(input$train_range[1], input$test_range, data(), category_pair, model_function)
    
  })
  
  
  # シミュレーション
  simulated_results <- reactive({
    
    if (is.null(input$raw_data))
      return(NULL)
    
    if (is.null(input$upper_cost))
      return(NULL)
    
    if (is.null(input$daily_pct))
      return(NULL)
    
    # シミュレート期間作成
    simulated <- create_simulator(input$date_range, category_pair)
    
    # シミュレート期間前処理
    simulated <- preprocess_simulated(simulated, input$long_holiday_button, input$long_holiday_range)
    
    # 上限コストの読み込み
    upper_cost <- read_upper_cost(input$upper_cost$datapath)

    # 日別割合の読み込み
    daily_pct <- read_daily_pct(input$daily_pct$datapath)
    
    # シミュレート結果
    simulate_target_kpis(data(), curvature_df(), eng_kpis, upper_cost, simulated, daily_pct)
    
    
  })

  
  output$fileUploaded <- reactive({
    return(!is.null(data()))
    
  })
  
  
  outputOptions(output, "fileUploaded", suspendWhenHidden=FALSE)
  
  
  output$tabs <- renderUI({
    
    if (is.null(input$raw_data))
      return(NULL)
    
    tabs <- lapply(paste(category_pair), tabPanel, value = plotOutput("plot"))
    do.call(tabsetPanel, tabs)
    
  })
  
  

  output$processed_table <- renderTable({
    
    if (is.null(input$raw_data$datapath))
      return(NULL)
    
    if (is.null(input$upper_cost))
      return(NULL)
    
    if (is.null(input$daily_pct))
      return(NULL)
    
    #curvature_df()
    
  })
  
  
  output$simulated_table <- renderTable({
    
    if (is.null(input$raw_data$datapath))
      return(NULL)
    
    simulated_results()
    
  })
  
}



shinyApp(ui = ui, server = server)







