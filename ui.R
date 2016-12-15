
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assessing Order Completion"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      selectInput("google", "使用google試算表嗎?", c("", "Yes", "No")),
      conditionalPanel(condition = "input.google != ''",
                       fileInput("mainfile", "訂單資料", multiple = TRUE, 
                                 accept = ".csv")),
      conditionalPanel(condition = "input.google == 'Yes'",
                       textInput("googleurl", "請輸入google試算表網址或
                                 檔案名稱")),
      conditionalPanel(condition = "input.google == 'No'",
                       fileInput("supportfile","到貨日期資料", accept = ".csv"),
                       radioButtons("dataformat", "到貨日期資料編碼",
                                    c("UTF-8","big5"))),
      conditionalPanel(condition = "input.google != ''",
                       actionButton("Launch", "執行", icon = icon("play"),
                                     width = "100%"),
                       downloadButton("Gotit", "下載")),
      hr(),
      uiOutput("AdditionalSelect1"),
      uiOutput("AdditionalSelect2"),
      uiOutput("AdditionalSelect3"),
      conditionalPanel(condition = "input.Launch != 0",
                       actionButton("resetparameter", "重置",width = "250px"),
                       hr(),
                       div(id = "filtering",
                           fluidRow(
                             column(4,selectInput("relationship1",
                                                  "合計篩選符號",
                                                  c("", ">=", ">", "==", "<=", 
                                                    "<"))),
                             column(4,textInput("value1", "合計金額"))
                           ),
                           fluidRow(
                             column(4,selectInput("relationship2", 
                                                  "樣數篩選符號",
                                                  c("", ">=", ">","==", "<=",
                                                    "<"))),
                             column(4,textInput("value2", "樣數"))
                           ),
                           fluidRow(
                             column(4, textInput("value3", "產品名稱")),
                             column(4, textInput("value4", "顏色與類型"))
                           ),
                           radioButtons("elaboration", "輸出型態",
                                        c("簡易輸出", "完整輸出"),
                                        inline = TRUE)))
                       ),
    
    # Show a plot of the generated distribution
    mainPanel(
      dataTableOutput("pikachu")
    )
  )
))
