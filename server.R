
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(data.table)
library(plyr)
library(pipeR)
library(foreach)
library(googlesheets)
library(shinyjs)
library(stringr)
library(checkenc)

shinyServer(function(input, output, session) {
  
  # set the limitation of the data size
  options(shiny.maxRequestSize = 30 * 1024 ^ 2) 
    
  # Read data in
  read_orders_data <- eventReactive(input$filter_execute, {
    temp_orders_data <- input$orders_data
    if (is.null (temp_orders_data)) {
      return (NULL)
    } else {
      encode <- lapply(temp_orders_data$datapath, checkenc) %>>%
        unlist %>>%
        unique
      lapply(temp_orders_data$datapath, function(k) {
        read.csv(k, stringsAsFactors = FALSE, fileEncoding = encode) 
      }) %>>%
        rbind.fill %>>%
        setDT %>>%
        return
    }
  })
  
  read_stocks_arrival <- eventReactive(input$filter_execute, {
    
    # Considering the situtation that input$google is No
    if (input$use_googleSheet == "No") {
      temp_stocks_arrival <- input$stocks_arrival
      if (is.null (temp_stocks_arrival)) {
        return(NULL)
      } else {
        encode <- checkenc(temp_stocks_arrival$datapath)
        read.csv(temp_stocks_arrival$datapath, fileEncoding = encode,
                 stringsAsFactors = FALSE) %>>% return
      }
      
      # Considering the situation that input$google is Yes
    } else if (input$use_googleSheet == "Yes") {
      if (is.null (input$googleSheet_url)) {
        return (NULL)
        } else if (grepl("^https", input$googleSheet_url)) {
          options("googlesheets.httr_oauth_cache" = "gs_auth")
          gs_auth(new_user = TRUE)
          gs_file_by_url <- gs_url(input$googleSheet_url)
          foreach(i = 1 : 3) %do% {
            gs_read(gs_file_by_url, i) %>>% setDT
            } %>>%
            rbindlist(fill = TRUE) %>>%
            return
          } else {
            options("googlesheets.httr_oauth_cache" = "gs_auth")
            gs_auth(new_user = TRUE)
            gs_file_by_title <- gs_title(input$googleSheet_url)
            foreach(j = 1 : 3) %do% {
              gs_read(gs_file_by_title, j) %>>% setDT
              } %>>%
              rbindlist(fill = TRUE) %>>%
              return
          }
      }
    })
  
  # Manipulating on AOC procedure
  manipulated_file <- reactive({
    processed_orders_data <- read_orders_data() %>>% setDT
    processed_stocks_arrival <- read_stocks_arrival() %>>% setDT
    
    # Clean data which enclude processed transactions
    file_under_AOC <- processed_orders_data %>>%
      `[`(i = !(訂單狀態 == "已取消" | 訂單狀態 == "已完成"))
    file_under_AOC[, 訂單日期 := as.Date(訂單日期)]
    
    # extract the transactions with preorder item
    file_under_AOC[grepl("現貨", 選項), 預購商品 := "N"]
    tmpfile <- file_under_AOC[, (預購商品 == "Y") %>>% any, by = 訂單號碼]
    ready_order_number <- tmpfile[V1 == FALSE, 訂單號碼] %>>% as.character
    file_under_AOC <- file_under_AOC %>>%
      `[`(訂單號碼 %in% tmpfile[V1 == TRUE, 訂單號碼])
    
    # Starting to judge if the preorder item have arrived
    file_under_AOC[預購商品 == "N", detection := TRUE]
    file_under_AOC %>>%
      `[`(j = 商品貨號 := str_replace_all(商品貨號, "\\[.*\\]", "") %>>% trimws)
    processed_stocks_arrival <- processed_stocks_arrival %>>% 
      `[`(as.Date(arrival) <= Sys.Date())
    processed_stocks_arrival[, `:=`(Start_Date = as.Date(Start_Date),
                                    End_Date = as.Date(End_Date))]
    processed_stocks_arrival %>>% 
      `[`(j = SKU := str_replace_all(SKU, "\\[.*\\]", "") %>>% trimws)
    
    # Launching filtering process
    detectlist <- foreach(i = 1 : nrow(processed_stocks_arrival)) %do% {
      if (is.na(processed_stocks_arrival$Start_Date[i]) &
          is.na(processed_stocks_arrival$End_Date[i])) {
        file_under_AOC$商品貨號 %in% processed_stocks_arrival$SKU[i]
      } else {
        (file_under_AOC$商品貨號 %in% processed_stocks_arrival$SKU[i]) & 
          between(file_under_AOC$訂單日期,
                  processed_stocks_arrival$Start_Date[i],
                  processed_stocks_arrival$End_Date[i])
      }
    }
    
    foreach(j = seq_along(detectlist)) %do% {
      file_under_AOC[detectlist[[j]], detection := TRUE]
    }
    
    file_under_AOC[is.na(detection), detection := FALSE]
    tmpfile <- file_under_AOC[, all(detection), 訂單號碼]
    bookingID <- tmpfile[V1 == TRUE, 訂單號碼] %>>% as.character
    ReadyToGo <- c(bookingID, ready_order_number)
    processed_orders_data[訂單號碼 %in% ReadyToGo] %>>% return
  })
  
  # Design delivery filtering UI
  output$delivery_UI <- renderUI({
    manipulation <- manipulated_file()
    deliveryMethod <- manipulation$送貨方式 %>>%
      as.factor %>>%
      levels %>>% 
      { c("", .) }
    selectInput("delivery_method", "送貨方式", choices = deliveryMethod)
  })
  
  # Design payment status filtering UI
  output$payment_UI <- renderUI({
    manipulation <- manipulated_file()
    paymentStatus <- manipulation$付款狀態 %>>%
      as.factor %>>%
      levels %>>%
      { c("", .) }
    selectInput("payment_status", "付款狀態", choices = paymentStatus)
  })
  
  # Design order status filtering UI
  output$orders_status_UI <- renderUI({
    manipulation <- manipulated_file()
    orderStatus <- manipulation$訂單狀態 %>>%
      as.factor %>>%
      levels %>>%
      { c("", .) }
    selectInput("orders_status", "訂單狀態", choices = orderStatus)
  })
  
  # Filtering the column designed above
  payment_delivery_filtered_data <- reactive({
    temp_payment_delivery_filtering_data <- manipulated_file()
    delivery_method <- ifelse(input$delivery_method == "", "",
                              "送貨方式 == input$delivery_method")
    payment_status <- ifelse(input$payment_status == "", "",
                             "付款狀態 == input$payment_status")
    orders_status <- ifelse(input$orders_status == "", "",
                            "訂單狀態 == input$orders_status")
    if (delivery_method == "" & payment_status == "" & orders_status == "") {
      return (temp_payment_delivery_filtering_data)
    } else {
      dvec <- c(delivery_method, payment_status, orders_status) %>>%
        {.[!. == ""]} %>>%
        paste(collapse = "&")
      temp_payment_delivery_filtering_data %>>%
        `[`(parse(text = dvec) %>>% eval) %>>%
        return
    }
  })
  
  # Filtering total value of every ordering
  totalAmount_filtered_data <- reactive({
    temp_total_filtering_Data <- payment_delivery_filtered_data()
    totalAmount_filtering <- paste("訂單合計", input$totalAmount_filtering,
                                   input$totalAmount_threshold)
    if (input$totalAmount_filtering != "" &
        input$totalAmount_threshold != "") {
      temp_total_filtering_Data[, 訂單合計 := first(訂單合計), 訂單號碼]
      temp_total_filtering_Data %>>%
        `[`(parse(text = totalAmount_filtering) %>>% eval) %>>%
        return
    } else {
      return (temp_total_filtering_Data)
    }
  })
  
  # Filtering ordering item number
  frequencies_filtered_data <- reactive({
    temp_frequencies_filtering_data <- totalAmount_filtered_data()
    frequencies_filtering <- paste(".N", input$frequencies_filtering,
                                   input$frequencies_threshold)
    if (input$frequencies_filtering != "" &
        input$frequencies_threshold != "") {
      tempnumber <- temp_frequencies_filtering_data %>>%
        #problem point
        `[`(i = parse(text = frequencies_filtering) %>>% eval, 訂單號碼) %>>% 
        {.[V1 == TRUE, 訂單號碼] }
      temp_frequencies_filtering_data[訂單號碼 %in% tempnumber] %>>% return
    } else {
      return (temp_frequencies_filtering_data)
    }  
  })
  
  # Filtering itemname and specifications
  itemNameSpec_filtered_data <- reactive({
    temp_itemNameSpec_filtering_data <- frequencies_filtered_data()
    if (input$product_name_threshold != "" |
        input$product_spec_threshold != "") {
      temp_selection <- temp_itemNameSpec_filtering_data %>>% 
        `[`(i = (grepl(input$product_name_threshold, 商品名稱) & 
                 grepl(input$product_spec_threshold, 選項)) %>>% any, 訂單號碼) 
      temp_itemNameSpec_filtering_data %>>% 
        `[`(訂單號碼 %in% temp_selection[V1 == TRUE, 訂單號碼]) %>>% return
    } else {
      return (temp_itemNameSpec_filtering_data)
    } 
  })
  
  # Reseting parameter by reset function with shinyjs package
  observeEvent(input$reset_parameter, {
    reset("filtering_setting")
    reset("delivery_method")
    reset("orders_status")
    reset("payment_status")
  })
  
  # Deciding output form
  ready_output_file <- reactive({
    if (input$output_file_type == "簡易輸出") {
      simple_output_data <- itemNameSpec_filtered_data()
      orders_ID <- unique(simple_output_data$訂單號碼)
      data.table(ID = seq_along(orders_ID),
                 orderID = orders_ID) %>>% return
    } else {
      intact_output_data <- itemNameSpec_filtered_data()
      intact_output_data <- intact_output_data[, .SD[1], 訂單號碼]
      return(intact_output_data)
    }
  })
  
  # Output final result
  output$output_document <- renderDataTable({
    ready_output_file()
  })
  
  # Handling download file
  output$download_file <- downloadHandler(
    filename = function() { 
      paste('CompletedOrder', 'csv', sep='.') 
    },
    content = function(file) {
      write.csv(ready_output_file(), file, row.names = FALSE,
                fileEncoding = "big5")
    }
  )
})
