
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(data.table)
library(plyr)
library(dplyr)
library(data.table)
library(googlesheets)
library(shinyjs)
library(stringr)
library(checkenc)

shinyServer(function(input, output, session) {
  
  # set the limitation of the data size
  options(shiny.maxRequestSize = 30 * 1024 ^ 2) 
    
  # Read data in
  datafile1 <- eventReactive(input$Launch, {
    inFile1 <- input$mainfile
    if (is.null (inFile1)) {
      return (NULL)
    } else {
      coding <- lapply(inFile1$datapath, checkenc) %>% unlist %>% unique
      lapply(inFile1$datapath, function(k) {
        read.csv(k, fileEncoding = coding, stringsAsFactors = FALSE) 
      }) %>% rbind.fill %>% setDT %>% return
    }
  })
  
  datafile2 <- eventReactive(input$Launch, {
    
    # Considering the situtation that input$google is No
    if (input$google == "No") {
      inFile2 <- input$supportfile
      if (is.null (inFile2)) {
        return(NULL)
      } else {
        FileCoding <- checkenc(inFile2$datapath)
        read.csv(inFile2$datapath, fileEncoding = FileCoding,
                 stringsAsFactors = FALSE) %>% return
      }
    }
    
    # Considering the situation that input$google is Yes
    else if (input$google == "Yes") {
      if (is.null (input$googleurl)) {
        return (NULL)
      } else if (grepl("^https", input$googleurl)) {
        options("googlesheets.httr_oauth_cache" = "gs_auth")
        gs_auth(new_user = TRUE)
        gs_url(input$googleurl) %>% gs_read %>% setDT %>% 
          .[is.na(spec), spec := ""] %>% return
      } else {
        options("googlesheets.httr_oauth_cache" = "gs_auth")
        gs_auth(new_user = TRUE)
        gs_title(input$googleurl) %>% gs_read %>% setDT %>%
          .[is.na(spec), spec := ""] %>% return
      }
    }
  })
  
  # Manipulating on AOC procedure
  zoopy <- reactive({
    salesdata <- datafile1() %>% setDT
    arrival <- datafile2() %>% setDT
    
    # Clean data which enclude processed transactions
    salesdata[, `:=`(訂單日期 = first(訂單日期),
                     訂單狀態 = first(訂單狀態),
                     送貨狀態 = first(送貨狀態)) , by = 訂單號碼]
    mutatedata <- salesdata[!(訂單狀態 == "已取消" | 訂單狀態 == "已完成")]
    
    # extract the transactions with preorder item
    tmpfile <- mutatedata[, (grepl("預購", 商品名稱) | grepl("預購", 選項)) %>%
                            any , by = 訂單號碼]
    Instocknum <- tmpfile[V1 == FALSE, 訂單號碼]
    mutatedata <- mutatedata[訂單號碼 %in% tmpfile[V1 == TRUE, 訂單號碼]]
    
    # Starting to judge if the preorder item have arrived
    mutatedata[! (grepl("預購", 商品名稱) | grepl("預購", 選項)) | 
                    grepl("現貨", 選項), detection := TRUE]
    arrival[, itemname := gsub("?", "", itemname, fixed = TRUE)]
    arrival <- arrival[as.Date(arrival) <= Sys.Date()]
    arrival[, Expect := strsplit(Expect, "/") %>%
              lapply(., function(k) { paste(k[2], k[3], sep = "/")}) %>% unlist]
    arrival[is.na(spec), spec := ""]
    
    # transfer the PreorderingHint into date format
    preordering.hint <- str_extract_all(mutatedata$預購提示, "\\d*\\/\\d*")
    preordering.hint <- sapply(preordering.hint, function(k) { k[1][[1]] })
    mutatedata[, expectedarrival := preordering.hint]
    
    # Launching filtering process
    detectlist <- lapply(1 : nrow(arrival), function(k) {
      grepl(arrival$itemname[k], mutatedata$商品名稱, fixed = TRUE) &
      grepl(arrival$spec[k], mutatedata$選項, ignore.case = TRUE) &
      grepl(arrival$Expect[k], mutatedata$expectedarrival)
    })
    for (i in 1 : length(detectlist)) {
      mutatedata[detectlist[[i]], detection := TRUE]
    }
    mutatedata[is.na(detection), detection := FALSE]
    tmpfile <- mutatedata[, all(detection), 訂單號碼]
    bookingID <- tmpfile[V1 == TRUE, 訂單號碼]
    ReadyToGo <- c(bookingID, Instocknum)
    salesdata[訂單號碼 %in% ReadyToGo] %>% return
  })
  
  # Design delivery filtering UI
  output$AdditionalSelect1 <- renderUI({
    manipulation <- zoopy()
    deliverymethod <- manipulation$送貨方式 %>% as.factor %>% levels %>% 
      c("", .)
    selectInput("delivery", "送貨方式", choices = deliverymethod)
  })
  
  # Design payment status filtering UI
  output$AdditionalSelect2 <- renderUI({
    manipulation <- zoopy()
    paymentstatus <- manipulation$付款狀態 %>% as.factor %>% levels %>%
      c("", .)
    selectInput("payment", "付款狀態", choices = paymentstatus)
  })
  
  # Design order status filtering UI
  output$AdditionalSelect3 <- renderUI({
    manipulation <- zoopy()
    orderstatus <- manipulation$訂單狀態 %>% as.factor %>% levels %>%
      c("", .)
    selectInput("order", "訂單狀態", choices = orderstatus)
  })
  
  # Filtering the column designed above
  zookeeper <- reactive({
    issac <- zoopy()
    delivery <- ifelse(input$delivery == "", "", "送貨方式 == input$delivery")
    payment <- ifelse(input$payment == "", "", "付款狀態 == input$payment")
    order <- ifelse(input$order == "", "", "訂單狀態 == input$order")
    if (delivery == "" & payment == "" & order == "") {
      return (issac)
    } else {
      dvec <- c(delivery, payment, order) %>% .[!. == ""] %>%
        paste(collapse = "&")
      issac[parse(text = dvec) %>% eval] %>% return
    }
  })
  
  # Filtering total value of every ordering
  desertcamal <- reactive({
    skt1bengi <- zookeeper()
    relation1 <- paste("合計", input$relationship1, input$value1)
    if (input$relationship1 != "" & input$value1 != "") {
      skt1bengi[, 合計 := first(合計), 訂單號碼]
      skt1bengi[parse(text = relation1) %>% eval] %>% return
    } else {
      return (skt1bengi)
    }
  })
  
  # Filtering ordering item number
  milktea <- reactive({
    faker <- desertcamal()
    relation2 <- paste(".N", input$relationship2, input$value2)
    if (input$relationship2 != "" & input$value2 != "") {
      tempnumber<- faker[, parse(text = relation2) %>% eval, 訂單號碼] %>% 
        .[V1 == TRUE, 訂單號碼]
      faker[訂單號碼 %in% tempnumber] %>% return
    } else {
      return (faker)
    }  
  })
  
  # Filtering itemname and specifications
  blackmonkey <- reactive({
    greentea <- milktea()
    if (input$value3 != "" | input$value4 != "") {
      blacktea <- greentea[, (grepl(input$value3, 商品名稱) & 
                              grepl(input$value4, 選項)) %>% any, 訂單號碼] 
      greentea[訂單號碼 %in% blacktea[V1 == TRUE, 訂單號碼]] %>% return
    } else {
      return (greentea)
    } 
  })
  
  # Reseting parameter by reset function with shinyjs package
  observeEvent(input$resetparameter, {
    reset("filtering")
    reset("delivery")
    reset("order")
    reset("payment")
  })
  
  # Deciding output form
  hokaido <- reactive({
    if (input$elaboration == "簡易輸出") {
      outputselect <- blackmonkey()
      bookingidvector <- unique(outputselect$訂單號碼)
      data.table(ID = seq_along(bookingidvector),
                 BookingID = bookingidvector) %>% return
    } else {
      gugudan <- blackmonkey()
      gugudan <- gugudan[, .SD[1], 訂單號碼]
      gugudan[, c("訂單號碼", "訂單日期", "電郵", "電話號碼", "送貨方式",
                  "送貨狀態","付款狀態", "合計", "收件人", "地址.1", "地址.2",
                  "城市", "訂單備註", "郵政編號.如適用.", "門市名稱", 
                  "全家服務編號...7.11.店號"), with = FALSE] %>% return
    }
  })
  
  # Output final result
  output$pikachu <- renderDataTable({
    hokaido()
  })
  
  # Handling download file
  output$Gotit <- downloadHandler(
    filename = function() { 
      paste('CompletedOrder', 'csv', sep='.') 
    },
    content = function(file) {
      write.csv(hokaido(), file, row.names = FALSE, fileEncoding = "big5")
    }
  )
})
