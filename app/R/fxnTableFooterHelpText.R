#' `fxnTableFooterHelpText.R` - Build help text for HTML table footer
#' 
#' @return `tableFooterHelpText` - Help text for HTML table footer


fxnTableFooterHelpText <- function() {
  tableFooterHelpText <- 
    htmltools::p(
      htmltools::HTML(
        "Scroll down over the following text to view additional information. This feature appears with certain device and browser settings."
      ), 
      
      class = "table-footer-help-text"
    )
  
  return(tableFooterHelpText)
}
