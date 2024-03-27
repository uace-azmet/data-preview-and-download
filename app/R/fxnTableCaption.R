#' `fxnTableCaption.R` - Build caption for HTML table
#' 
#' @return `tableCaption` - Caption for HTML table


fxnTableCaption <- function() {
  tableCaption <- 
    htmltools::p(
      htmltools::HTML(
        "Click or tap the button below to download a file of the above data with tab-separated values."
      ), 
      
      class = "table-caption"
    )
  
  return(tableCaption)
}
