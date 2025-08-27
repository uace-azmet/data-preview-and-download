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
    
    # Set identification variables of interest from the following hourly data variables: 
    # c("date_datetime", "date_doy", "date_hour", "date_year", "meta_needs_review", "meta_station_id", "meta_station_name", "meta_version")
    varsID <- c(
      "meta_needs_review", "meta_station_id", "meta_station_name", "meta_version", "date_datetime", "date_doy", "date_hour", "date_year"
    )
    
    # Set hourly measured variables of interest from the following:
    # c("dwpt", "dwptF", "eto_azmet", "eto_azmet_in", "heatstress_cottonC", "heatstress_cottonF", "meta_bat_volt", "precip_total", "precip_total_in", "relative_humidity", "sol_rad_total", "sol_rad_total_ly", "temp_airC", "temp_airF", "temp_soil_10cmC", "temp_soil_10cmF", "temp_soil_50cmC", "temp_soil_50cmF", "vp_actual", "vp_deficit", "wind_2min_spd_max_mph", "wind_2min_spd_max_mps", "wind_2min_spd_mean_mph", "wind_2min_spd_mean_mps", "wind_2min_timestamp", "wind_2min_vector_dir", "wind_spd_max_mph", "wind_spd_max_mps", "wind_spd_mph", "wind_spd_mps", "wind_vector_dir", "wind_vector_dir_stand_dev", "wind_vector_magnitude", "wind_vector_magnitude_mph")
    varsMeasure <- c(
      "dwpt", "dwptF", "eto_azmet", "eto_azmet_in", "heatstress_cottonC", "heatstress_cottonF", "meta_bat_volt", "precip_total", "precip_total_in", "relative_humidity", "sol_rad_total", "sol_rad_total_ly", "temp_airC", "temp_airF", "temp_soil_10cmC", "temp_soil_10cmF", "temp_soil_50cmC", "temp_soil_50cmF", "vp_actual", "vp_deficit", "wind_2min_spd_max_mph", "wind_2min_spd_max_mps", "wind_2min_spd_mean_mph", "wind_2min_spd_mean_mps", "wind_2min_timestamp", "wind_2min_vector_dir", "wind_spd_max_mph", "wind_spd_max_mps", "wind_spd_mph", "wind_spd_mps", "wind_vector_dir", "wind_vector_dir_stand_dev", "wind_vector_magnitude", "wind_vector_magnitude_mph"
    )
   
    # For case of empty data return
    if (nrow(dataETL) == 0) {
      dataETL <- data.frame(matrix(
        data = NA,
        nrow = 0, 
        ncol = length(c(varsID, varsMeasure))
      ))
      
      colnames(dataETL) <- c(varsID, varsMeasure)
    } else {
      # Tidy data
      dataETL <- dataETL %>%
        dplyr::select(all_of(c(varsID, varsMeasure))) %>%
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
        data = 1,
        nrow = 1, 
        ncol = length(c(dailyVarsID, dailyVarsMeasured, dailyVarsDerived))
      ))
      
      colnames(dataETL) <- c(dailyVarsID, dailyVarsMeasured, dailyVarsDerived)
      
      dataETL <- dataETL %>% 
        dplyr::select(dplyr::all_of(c(dailyVarsID, dailyVarsMeasured, dailyVarsDerived))) %>%
        dplyr::select(sort(names(.))) %>% 
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ na_if(., 1)))
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
