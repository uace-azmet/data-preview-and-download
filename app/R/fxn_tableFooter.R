#' `fxn_tableFooter.R` - Build text for footer of preview table
#' 
#' @param inData - Downloaded AZMet hourly or daily data from `fxnAZMetDataELT.R`
#' @param timeStep - AZMet data time step
#' @return `tableFooter` - Text for footer of preview table


fxn_tableFooter <- function(inData, timeStep) {
  if (timeStep == "Daily") { # For daily data
    startDate <- 
      gsub(" 0", " ", format(lubridate::date(min(inData$datetime)), format = "%B %d, %Y"))
    
    endDate <- 
      gsub(" 0", " ", format(lubridate::date(max(inData$datetime)), format = "%B %d, %Y"))
    
    tableFooter <- 
      htmltools::p(
        htmltools::HTML(
          paste0(
            "Data are from ", startDate, " through ", endDate, "."
          )
        ), 
        
        class = "table-footer"
      )
  } else if (timeStep == "Hourly") { # For hourly data
    startDate <- 
      gsub(" 0", " ", format(lubridate::date(min(inData$date_datetime)), format = "%B %d, %Y"))
    
    endDate <- 
      gsub(" 0", " ", format(lubridate::date(max(inData$date_datetime)), format = "%B %d, %Y"))
    
    startTime <- 
      paste(format(as.POSIXct(min(inData$date_datetime)), format = "%H:%M:%S"), "MST", sep = " ")
    
    endTime <- 
      paste(format(as.POSIXct(max(inData$date_datetime)), format = "%H:%M:%S"), "MST", sep = " ")
    
    startDateTime <- paste(startDate, startTime, sep = " ")
    
    endDateTime <- paste(endDate, endTime, sep = " ")
    
    tableFooter <- 
      htmltools::p(
        htmltools::HTML(
          paste0(
            "Data are from ", startDateTime, " through ", endDateTime, "."
          )
        ), 
        
        class = "table-footer"
      )
  }
  
  return(tableFooter)
}
