#' `fxnCardFooterText.R` - Build text for card footer of data table
#' 
#' @param inData - Downloaded AZMet hourly or daily data from `fxnAZMetDataELT.R`
#' @param timeStep - AZMet data time step
#' @return `cardFooterText` - Text for card footer of data table


fxnCardFooterText <- function(
    inData,
    timeStep
  ) {
  
  if (timeStep == "Daily") {
    startDate <- gsub(" 0", " ", format(lubridate::date(min(inData$date_datetime)), format = "%B %d, %Y"))
    endDate <- gsub(" 0", " ", format(lubridate::date(max(inData$date_datetime)), format = "%B %d, %Y"))
    
    cardFooterText <- 
      htmltools::p(
        htmltools::HTML(
          paste0(
            "Data are from ", startDate, " through ", endDate, "."
          )
        ), 
        
        class = "card-footer-text"
      )
  } else if (timeStep == "Hourly") {
    startDate <- gsub(" 0", " ", format(lubridate::date(min(inData$date_datetime)), format = "%B %d, %Y"))
    endDate <- gsub(" 0", " ", format(lubridate::date(max(inData$date_datetime)), format = "%B %d, %Y"))
    
    startTime <- paste(format(min(inData$date_datetime), format = "%H:%M:%S"), "MST", sep = " ")
    endTime <- paste(format(max(inData$date_datetime), format = "%H:%M:%S"), "MST", sep = " ")
    
    startDateTime <- paste(startDate, startTime, sep = " ")
    endDateTime <- paste(endDate, endTime, sep = " ")
    
    cardFooterText <- 
      htmltools::p(
        htmltools::HTML(
          paste0(
            "Data are from ", startDateTime, " through ", endDateTime, "."
          )
        ), 
        
        class = "card-footer-text"
      )
  }
  
  return(cardFooterText)
}
