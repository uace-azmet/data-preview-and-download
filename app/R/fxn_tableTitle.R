#' `fxn_tableTitle.R` - Build title for preview table based on user input
#' 
#' @param azmetStation - AZMet station selection by user
#' @param timeStep - AZMet data time step selection by user
#' @return `tableTitle` - Title for preview table based on user input


fxn_tableTitle <- function(azmetStation, timeStep) {
  tableTitle <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          bsicons::bs_icon("table"), 
          htmltools::HTML("&nbsp;&nbsp;"), 
          toupper(
            paste0(
              timeStep, " Data from the AZMet ", azmetStation, " station"
            ) 
          ),
          htmltools::HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "Scroll or swipe over the table to view additional rows and columns. Click or tap on column headers to sort corresponding data by ascending or descending values.",
            id = "infoFigureTitle",
            placement = "right"
          )
        ),
      ),
      
      class = "table-title"
    )
  
  return(tableTitle)
}
