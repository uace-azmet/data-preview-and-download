#' `fxnTableTitle.R` - Build title for HTML table based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param timeStep - AZMet data time step
#' @return `tableTitle` - Table title for HTML table based on user input


fxnTableTitle <- function(azmetStation, timeStep) {
  tableTitle <- 
    htmltools::h4(
      htmltools::HTML(
        paste(
          "Preview of", timeStep, "Data from the AZMet", azmetStation, "station", 
          sep = " "
        ),
      ),
      
      class = "table-title"
    )
  
  return(tableTitle)
}
