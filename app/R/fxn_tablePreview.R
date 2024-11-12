#' `fxn_tablePreview.R` - Build preview table for formatted AZMet hourly or daily data
#' 
#' @param inData - Formatted AZMet hourly or daily data from `fxn_dataFormat.R`
#' @return `tablePreview` - preview table of formatted AZMet data


fxn_tablePreview <- function(inData) {
  tablePreview <- inData |>
    DT::datatable(
      extensions = "FixedColumns",
      options = list(
        cellBorder = TRUE,
        deferRender = TRUE,
        fixedColumns = list(
          left = 1
        ),
        orderClasses = TRUE,
        ordering = TRUE,
        paging = FALSE,
        scrollCollapse = TRUE,
        scroller = TRUE,
        scrollX = TRUE,
        scrollY = "400px",
        searching = FALSE
      ),
      rownames = FALSE,
      selection = "none"
    ) |>
    DT::formatStyle(
      border = "1px solid #dee2e6",
      columns = colnames(inData)
    )
  
  return(tablePreview)
}
  