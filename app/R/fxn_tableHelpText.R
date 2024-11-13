#' `fxn_tableHelpText.R` - Build help text for preview table
#' 
#' @return `tableHelpText` - Help text for preview table


fxn_tableHelpText <- function() {
  tableHelpText <- 
    htmltools::p(
      htmltools::HTML(
        "Scroll over the table to view additional rows and columns. Click or tap on column headers to sort corresponding data by ascending or descending values."
      ), 
      
      class = "table-help-text"
    )
  
  return(tableHelpText)
}
