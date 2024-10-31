#' `fxn_tableHelpText.R` - Build help text for data table
#' 
#' @return `tableHelpText` - Help text for data table


fxn_tableHelpText <- function() {
  tableHelpText <- 
    htmltools::p(
      htmltools::HTML(
        "Scroll over the table to view additional rows and columns."
      ), 
      
      class = "table-help-text"
    )
  
  return(tableHelpText)
}
