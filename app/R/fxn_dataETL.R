#' `fxn_dataETL.R` Download AZMet hourly or daily data from API-based database
#' 
#' @param azmetStation - AZMet station name
#' @param timeStep - AZMet data time step
#' @param startDate - Start date of period of interest
#' @param endDate - End date of period of interest
#' @return `dataETL` - Downloaded data table


fxn_dataETL <- function(azmetStation, timeStep, startDate, endDate) {
  
  # Hourly data -----
  
  if (timeStep == "Hourly") {
    start_date_time = paste(startDate, "01", sep = " ")
    
    if (endDate == lubridate::today(tzone = "America/Phoenix")) {
      end_date_time = NULL
    } else {
      end_date_time = paste(endDate, "24", sep = " ")
    }
    
    dataETL <- azmetr::az_hourly(
      station_id = 
        dplyr::filter(azmetStationMetadata, meta_station_name == azmetStation)$meta_station_id,
      start_date_time = start_date_time,
      end_date_time = end_date_time
    )
    
    # For case of empty data return
    if (nrow(dataETL) == 0) {
      dataETL <- data.frame(matrix(
        # data = NA,
        nrow = 0, 
        ncol = length(c(hourlyVarsID, hourlyVarsMeasured, hourlyVarsDerived))
      ))
      
      colnames(dataETL) <- c(hourlyVarsID, hourlyVarsMeasured, hourlyVarsDerived)
      
      dataETL <- dataETL %>% 
        dplyr::select(dplyr::all_of(c(hourlyVarsID, hourlyVarsMeasured, hourlyVarsDerived))) %>%
        dplyr::select(sort(names(.)))
    } else {
      # Tidy data
      dataETL <- dataETL %>%
        dplyr::select(dplyr::all_of(c(hourlyVarsID, hourlyVarsMeasured, hourlyVarsDerived))) %>%
        dplyr::select(sort(names(.))) %>% 
        dplyr::mutate(
          dplyr::across(c("date_datetime", "wind_2min_timestamp"), as.character)
        )
    } 
  }
  
  # Daily data -----
  
  if (timeStep == "Daily") {
    dataETL <- azmetr::az_daily(
      station_id = 
        dplyr::filter(azmetStationMetadata, meta_station_name == azmetStation)$meta_station_id,
      start = startDate, 
      end = endDate
    )
    
    # For case of empty data return
    if (nrow(dataETL) == 0) {
      dataETL <- data.frame(matrix(
        # data = NA,
        nrow = 0, 
        ncol = length(c(dailyVarsID, dailyVarsMeasured, dailyVarsDerived))
      ))
      
      colnames(dataETL) <- c(dailyVarsID, dailyVarsMeasured, dailyVarsDerived)
      
      dataETL <- dataETL %>% 
        dplyr::select(dplyr::all_of(c(dailyVarsID, dailyVarsMeasured, dailyVarsDerived))) %>%
        dplyr::select(sort(names(.)))
    } else {
      # Tidy data
      dataETL <- dataETL %>%
        dplyr::select(dplyr::all_of(c(dailyVarsID, dailyVarsMeasured, dailyVarsDerived))) %>%
        dplyr::select(sort(names(.))) %>% 
        dplyr::mutate(
          dplyr::across("wind_2min_timestamp", as.character)
        )
    }
  }
  
  return(dataETL)
}
