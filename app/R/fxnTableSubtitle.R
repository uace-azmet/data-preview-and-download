#' `fxnTableSubtitle.R` - Build subtitle for HTML table based on current date
#' 
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `tableSubtitle` - Subtitle for HTML table based on current date


fxnTableSubtitle <- function(startDate, endDate) {
  tableSubtitle <- 
    htmltools::p(
      htmltools::HTML(
        paste(
          "From", gsub(" 0", " ", format(startDate, "%B %d, %Y")), "through", gsub(" 0", " ", format(endDate, "%B %d, %Y")), 
          sep = " "
        )
      ), 
      
      class = "table-subtitle"
    )
  
  return(tableSubtitle)
}
