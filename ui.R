
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assessing Order Completeness"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      selectInput("use_googleSheet", "使用google試算表嗎?", c("", "Yes", "No")),
      
      conditionalPanel(condition = "input.use_googleSheet != ''",
                       fileInput("orders_data", "訂單資料", multiple = TRUE, 
                                 accept = ".csv")
                       ),
      
      conditionalPanel(condition = "input.use_googleSheet == 'Yes'",
                       textInput("googleSheet_url", "請輸入google試算表網址或
                                 檔案名稱")
                       ),
      
      conditionalPanel(condition = "input.use_googleSheet == 'No'",
                       fileInput("stocks_arrival","到貨日期資料",
                                 accept = ".csv")
                       ),
      
      conditionalPanel(condition = "input.use_googleSheet != ''",
                       actionButton("filter_execute", "執行", icon = icon("play"),
                                     width = "100%"),
                       hr(),
                       downloadButton("download_file", "下載")
                       ),
      hr(),
      uiOutput("delivery_UI"),
      uiOutput("payment_UI"),
      uiOutput("orders_status_UI"),
      conditionalPanel(condition = "input.filter_execute != 0",
                       actionButton("reset_parameter", "重置", width = "250px"),
                       hr(),
                       div(id = "filtering_setting",
                           fluidRow(
                             column(4, selectInput("totalAmount_filtering",
                                                   "合計篩選符號",
                                                   c("", ">=", ">", "==", "<=",
                                                     "<"))),
                             column(4, textInput("totalAmount_threshold",
                                                 "合計金額"))
                             ),
                           
                           fluidRow(
                             column(4, selectInput("frequencies_filtering", 
                                                  "樣數篩選符號",
                                                  c("", ">=", ">","==", "<=",
                                                    "<"))),
                             column(4, textInput("frequencies_threshold",
                                                 "樣數"))
                             ),
                           
                           fluidRow(
                             column(4, textInput("product_name_threshold",
                                                 "產品名稱")),
                             column(4, textInput("product_spec_threshold",
                                                 "顏色與類型"))
                           ),
                           radioButtons("output_file_type", "輸出型態",
                                        c("簡易輸出", "完整輸出"),
                                        inline = TRUE)))
                       ),
    
    # Show a plot of the generated distribution
    mainPanel(
      dataTableOutput("output_document")
    )
  )
))
