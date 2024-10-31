#' `fxnTableHelpText.R` - Build help text for data table
#' 
#' @return `tableHelpText` - Help text for data table


fxnTableHelpText <- function() {
  tableHelpText <- 
    htmltools::p(
      htmltools::HTML(
        "Scroll over the table to view additional rows and columns."
      ), 
      
      class = "table-help-text"
    )
  
  return(tableHelpText)
}
