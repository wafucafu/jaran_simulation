#################################   Front End   ########################################

# フロント仕様
ui <- fluidPage(
  
  # タイトル
  headerPanel("ROASシミュレーション"),
  
  HTML('</br>'),
  
  
  # サイドパネル
  sidebarPanel(

    # 設定タブ
    tabsetPanel(

      # 基本設定タブ
      tabPanel("基本設定", 
               
               HTML('</br>'),
               
               
               # 生データ入力ボタン
               fileInput(inputId = "raw_data",
                         label = "1. 生データをアップロードしてください",
                         accept = c('.csv')),
               
               HTML('</br>'),
               
               
               #上限コスト入力ボタン
               fileInput(inputId = "upper_cost",
                         label = "2. 上限コストをアップロードしてください",
                         accept = c('.csv')),
               
               HTML('</br>'),
               
               
               # デイリーアロケ率入力ボタン
               fileInput(inputId = "daily_pct",
                         label = "3. 日別割合をアップロードしてください",
                         accept = c('.csv')),
               
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
               
               HTML('</br>'),
               
               
               # シミュレーションスタートボタン
               conditionalPanel(condition = "output.file_validated == true",
                                actionButton("start_simulation", "シミュレーション開始")
               ),
               
               HTML('</br>'),
               
               
               # ダウンロードボタン
               conditionalPanel(condition = "output.simulation_status == true",
                                downloadButton('start_download', 'ダウンロード')
               ),
               
               HTML('</br>')
               
      ),
      
      
      # 詳細設定タブ
      tabPanel("詳細設定", 
               
               HTML('</br>'),
               
               
               # シミュレーション内での大型連休の有無を決める
               radioButtons("long_holiday_button", "7. 大型連休設定", 
                            c("該当なし" = "no", "該当あり" = "yes")),
               
               
               # 大型連休ありの場合の期間選択
               conditionalPanel(condition = "input.long_holiday_button == 'yes' ",
                                dateRangeInput("long_holiday_range", "7. 大型連休設定",
                                               start = paste0(Sys.Date() - day(Sys.Date()) + 1),
                                               end = paste0(Sys.Date() + days_in_month(Sys.Date()) - day(Sys.Date())),
                                               format = "yyyy-mm-dd",
                                               language = "ja")),
               
               HTML('</br>'),
               
               
               # 直近データ重みタイプの選択
               selectInput("weight_type", "8. 重みの種類", c("累乗", "線形", "なし")),
               
               
               # 直近データ重みの強さ
               conditionalPanel(condition = "input.weight_type != 'なし' ",
                                sliderInput("weight_power", "8. 重みの強さ", 
                                            min = weight_range_min, max = weight_range_max, 
                                            value = weight_range)),
               
               HTML('</br>'),
               
               
               # コスト下限
               sliderInput("minimum_cost", "9. コスト下限", 
                           min = minimum_cost_min, max = minimum_cost_max, 
                           value = minimum_cost_default),
               
               
               # セールス下限
               sliderInput("minimum_sales", "10. セールス下限", 
                           min = minimum_sales_min, max = minimum_sales_max, 
                           value = minimum_sales_default)
               
               
      ),
      
      
      # プロット設定タブ
      tabPanel("プロット設定", 
               
               HTML('</br>'),
               
               
               # プロットするKPIセレクト
               selectInput("kpis", "KPI", c())
               
      )
      
      
      # 設定タブ
    )
    
    
    # サイドバーパネル
  ),
  
  
  # メインパネル
  mainPanel(
    
    # メインパネルタブ
    navbarPage(paste0("日付 : ", Sys.Date()), id = "navibar",
               
               # シミュレーション結果プロット
               tabPanel("シミュレーション結果",
                        
                        # プロットパネル
                        h3("シミュレーション結果のプロット"),
                        
                        uiOutput("plot_tabs"),
                        
                        HTML('</br>')),
               
               
               # 入力データサンプル
               tabPanel("過去データ",
                        
                        # 入力データを出力
                        h5("1. 生データのサンプル"),
                        
                        tableOutput("raw_data_table")),
               
               
               # 上限コストサンプル
               tabPanel("上限データ",
                        
                        # 上限コストを出力
                        h5("2. 上限コストのサンプル"),
                        
                        tableOutput("upper_cost_table")),
               
               
               # 日別割合のサンプル
               tabPanel("日別割合",
                        
                        # 日別割合出力
                        h5("3. 日別割合のサンプル"),
                        
                        tableOutput("daily_pct_table"))
               
               
               # メインパネルタブ
            )
    
    
    # メインパネル
  ),
  
  
  # タグの作成
  tags$head(
    tags$style(HTML("
                    .shiny-output-error-validation {
                    color: red;
                    }
                    ")))
  
  
  # UI仕様  
  )