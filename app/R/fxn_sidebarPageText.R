#' `fxn_sidebarPageText.R` - Build supporting text for data tool based on user input
#' 
#' @param timeStep - AZMet data time step
#' @return `sidebarPageText` - Supporting text for data tool based on user input


fxn_sidebarPageText <- function(timeStep) {
  # Define inputs
  todayDate <- gsub(" 0", " ", format(lubridate::today(), "%B %d, %Y"))
  
  todayYear <- lubridate::year(lubridate::today())
  
  azmetrURL <- a(
    "azmetr", 
    href="https://uace-azmet.github.io/azmetr/",
    target="_blank"
  )
  
  webpageAZMet <- a(
    "AZMet website", 
    href="https://azmet.arizona.edu/", 
    target="_blank"
  )
  
  webpageCode <- a(
    "GitHub page", 
    href="https://github.com/uace-azmet/data-preview-and-download", 
    target="_blank"
  )
  
  webpageDataVariables <- a(
    "data variables", 
    href="https://azmet.arizona.edu/about/data-variables", 
    target="_blank"
  )
  
  webpageNetworkMap <- a(
    "station locations", 
    href="https://azmet.arizona.edu/about/network-map", 
    target="_blank"
  )
  
  webpageStationMetadata <- a(
    "station metadata", 
    href="https://azmet.arizona.edu/about/station-metadata", 
    target="_blank"
  )
  
  if (timeStep == "Hourly") {
    apiURL <- a(
      "api.azmet.arizona.edu", 
      href="https://api.azmet.arizona.edu/v1/observations/hourly", # Hourly data
      target="_blank"
    )
  } else {
    apiURL <- a(
      "api.azmet.arizona.edu", 
      href="https://api.azmet.arizona.edu/v1/observations/daily", # Daily data
      target="_blank"
    )
  }
  
  # Build text
  sidebarPageText <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          timeStep, " AZMet data are from ", apiURL, " and accessed using the ", azmetrURL, " R package. Table values from recent dates may be based on provisional data. More information about ", webpageDataVariables, ", ", webpageNetworkMap, ", and ", webpageStationMetadata, " is available on the ", webpageAZMet, ". Users of AZMet data and related information assume all risks of its use.",
          htmltools::br(), htmltools::br(),
          "To cite the above AZMet data, please use: 'Arizona Meteorological Network (", todayYear, ") Arizona Meteorological Network (AZMet) Data. https:://azmet.arizona.edu. Accessed ", todayDate, "'.",
          htmltools::br(), htmltools::br(),
          "For information on how this webpage is put together, please visit the ", webpageCode, " for this tool."
        )
      ),
      
      class = "sidebar-page-text"
    )
  
  return(sidebarPageText)
}
