#' `fxnTableHelpText.R` - Build help text for HTML table
#' 
#' @return `tableHelpText` - Help text for HTML table


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
