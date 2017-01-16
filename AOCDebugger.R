  AOCDebugger <- function(SalesData, BookingID, ArrivalData) {
    # This function aims to help AOC maintainer to look into the problem deeply
    # and fast. Since discovering the specific bug is time-consuming, this 
    # function is able to handle this dirty job.
    #
    # Args :
    #   SalesData : The main file which stores in global environment
    #   BookingID : BookingID of the specific problematic order
    #   ArrivalData : The support file which is necessary to operate AOC 
    #                 function. This should stores in global environment as well
    #
    # Return :
    #   Pretty clear output of the problematic order which enable user to see
    #   which item arouse this issue.
    SalesData <- SalesData[訂單號碼 %in% BookingID]
    SalesData[! (grepl("預購", 商品名稱) | grepl("預購", 選項)), detect := TRUE]
    
    ArrivalData[, itemname := gsub("?", "", itemname, fixed = TRUE)]
    ArrivalData <- ArrivalData[as.Date(arrival) <= Sys.Date()]
    ArrivalData[, Expect := strsplit(Expect, "/") %>%
              lapply(., function(k) { paste(k[2], k[3], sep = "/")}) %>% unlist]
    ArrivalData[is.na(spec), spec := ""]
    
    preordering.hint <- str_extract_all(SalesData$預購提示, "\\d*\\/\\d*")
    preordering.hint <- sapply(preordering.hint, function(k) { k[1][[1]] })
    SalesData[, expectedarrival := preordering.hint]
    
    detectlist <- lapply(1 : nrow(ArrivalData), function(k) {
      grepl(ArrivalData$itemname[k], SalesData$商品名稱, fixed = TRUE) &
      grepl(ArrivalData$spec[k], SalesData$選項, ignore.case = TRUE) &
      grepl(ArrivalData$Expect[k], SalesData$expectedarrival)
    })
    
    for (i in 1 : length(detectlist)) {
      SalesData[detectlist[[i]], detect := TRUE]
    }
    SalesData[is.na(detect), detect := FALSE]
    
    
    FinalData <<- SalesData
  }