#' `fxn_tableFooter.R` - Build text for footer of preview table
#' 
#' @param inData - Downloaded AZMet hourly or daily data from `fxnAZMetDataELT.R`
#' @param azmetStation - AZMet station name
#' @param timeStep - AZMet data time step
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `tableFooter` - Text for footer of preview table


fxn_tableFooter <- function(inData, azmetStation, timeStep, startDate, endDate) {
  if (timeStep == "Daily") { # For daily data
    if (nrow(inData) == 0) {
      startDate <- gsub(" 0", " ", format(startDate, format = "%B %d, %Y"))
      endDate <- gsub(" 0", " ", format(endDate, format = "%B %d, %Y"))
      
      tableFooter <- 
        htmltools::p(
          htmltools::HTML(
            paste0(
              "No data are available from ", startDate, " through ", endDate, ", a period when the AZMet ", azmetStation, " station was not in operation."
            )
          ), 
          
          class = "table-footer"
        )
    } else {
      startDate <- 
        gsub(" 0", " ", format(lubridate::date(min(inData$datetime)), format = "%B %d, %Y"))
      
      endDate <- 
        gsub(" 0", " ", format(lubridate::date(max(inData$datetime)), format = "%B %d, %Y"))
      
      tableFooter <- 
        htmltools::p(
          htmltools::HTML(
            paste0(
              "Data are from ", startDate, " through ", endDate, ". Empty cells denote no data."
            )
          ), 
          
          class = "table-footer"
        )
    }
  } else if (timeStep == "Hourly") { # For hourly data
    if (nrow(inData) == 0) {
      startDate <- gsub(" 0", " ", format(startDate, format = "%B %d, %Y"))
      endDate <- gsub(" 0", " ", format(endDate, format = "%B %d, %Y"))
      
      startTime <- paste("01:00:00", "MST", sep = " ")
      endTime <- paste("23:59:59", "MST", sep = " ")
      
      startDateTime <- paste(startDate, startTime, sep = " ")
      endDateTime <- paste(endDate, endTime, sep = " ")
      
      tableFooter <- 
        htmltools::p(
          htmltools::HTML(
            paste0(
              "No data are available from ", startDateTime, " through ", endDateTime, ", a period when the AZMet ", azmetStation, " station was not in operation."
            )
          ), 
          
          class = "table-footer"
        )
    } else {
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
              "Data are from ", startDateTime, " through ", endDateTime, ". Empty cells denote no data."
            )
          ), 
          
          class = "table-footer"
        )
    }
  }
  
  return(tableFooter)
}
