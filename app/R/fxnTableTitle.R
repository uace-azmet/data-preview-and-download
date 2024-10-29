#' `fxnTableTitle.R` - Build title for data table based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param timeStep - AZMet data time step selection by user
#' @return `tableTitle` - Title for data table based on user input


fxnTableTitle <- function(
    azmetStation, 
    timeStep
  ) {
  
  tableTitle <- 
    htmltools::p(
      htmltools::HTML(
        paste(
          bsicons::bs_icon("table"), 
          htmltools::HTML("&nbsp;"), 
          timeStep, "Data from the AZMet", azmetStation, "station", 
          sep = " "
        ),
      ),
      
      class = "table-title"
    )
  
  return(tableTitle)
}
