#' `fxn_cardHeaderTitle.R` - Build title for card header of data table based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param timeStep - AZMet data time step selection by user
#' @return `cardHeaderTitle` - Title for card header of data table based on user input


fxn_cardHeaderTitle <- function(
    azmetStation, 
    timeStep
  ) {
  
  cardHeaderTitle <- 
    htmltools::p(
      htmltools::HTML(
        paste(
          bsicons::bs_icon("table"), 
          htmltools::HTML("&nbsp;"), 
          timeStep, "Data from the AZMet", azmetStation, "station", 
          sep = " "
        ),
      ),
      
      class = "card-header-title"
    )
  
  return(cardHeaderTitle)
}
