#' `fxnDownloadButtonHelpText.R` - Build help text for download .tsv button
#' 
#' @return `downloadButtonHelpText` - Help text for download .tsv button


fxnDownloadButtonHelpText <- function() {
  downloadButtonHelpText <- 
    htmltools::p(
      htmltools::HTML(
        "Click or tap the button below to download a file of the above data with tab-separated values."
      ), 
      
      class = "download-button-help-text"
    )
  
  return(downloadButtonHelpText)
}
