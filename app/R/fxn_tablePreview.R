#' `fxn_tablePreview.R` - Build preview table for formatted AZMet hourly or daily data
#' 
#' @param inData - Formatted AZMet hourly or daily data from `fxn_dataFormat.R`
#' @param timeStep - AZMet data time step
#' @return `tablePreview` - preview table of formatted AZMet data


fxn_tablePreview <- function(inData, timeStep) {
  # `inData` has character values for .tsv download
  if (timeStep == "Daily") {
    varsCharacter <- 
      c("meta_station_id", "meta_station_name", "datetime", "wind_2min_timestamp")
  } else if (timeStep == "Hourly") {
    varsCharacter <- 
      c("meta_station_id", "meta_station_name", "date_datetime", "date_hour", "wind_2min_timestamp")
  }
  
  inData <- inData |>
      dplyr::mutate(dplyr::across(!dplyr::all_of(varsCharacter), as.numeric))
  
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
      columns = colnames(inData)#,
      #fontFamily = "Monospace",
      #fontSize = "0.8rem"
    )
  
  if (timeStep == "Daily") {
    tablePreview <- tablePreview |>
      DT::formatRound(
        columns = c("chill_hours_0C", "chill_hours_20C", "chill_hours_32F", "chill_hours_45F", "chill_hours_68F", "chill_hours_7C", "date_doy", "date_year", "meta_needs_review", "meta_version", "relative_humidity_max", "relative_humidity_mean", "relative_humidity_min", "wind_2min_vector_dir", "wind_vector_dir", "wind_vector_dir_stand_dev"),
        digits = 0,
        interval = NULL
      ) |>
      DT::formatRound(
        columns = c("dwpt_mean", "dwpt_meanF", "eto_azmet", "eto_pen_mon", "heat_units_10C", "heat_units_13C", "heat_units_3413C", "heat_units_45F", "heat_units_50F", "heat_units_55F", "heat_units_7C", "heat_units_9455F", "heatstress_cotton_meanC", "heatstress_cotton_meanF", "precip_total_mm", "temp_air_maxC", "temp_air_maxF", "temp_air_meanC", "temp_air_meanF", "temp_air_minC", "temp_air_minF", "temp_soil_10cm_maxC", "temp_soil_10cm_maxF", "temp_soil_10cm_meanC",  "temp_soil_10cm_meanF", "temp_soil_10cm_minC", "temp_soil_10cm_minF", "temp_soil_50cm_maxC", "temp_soil_50cm_maxF", "temp_soil_50cm_meanC", "temp_soil_50cm_meanF", "temp_soil_50cm_minC", "temp_soil_50cm_minF", "wind_2min_spd_max_mph", "wind_2min_spd_max_mps", "wind_2min_spd_mean_mph", "wind_2min_spd_mean_mps", "wind_spd_max_mph", "wind_spd_max_mps", "wind_spd_mean_mph", "wind_spd_mean_mps", "wind_vector_magnitude", "wind_vector_magnitude_mph"),
        digits = 1,
        interval = NULL
      ) |>
      DT::formatRound(
        columns = c("eto_azmet_in", "eto_pen_mon_in", "meta_bat_volt_max", "meta_bat_volt_mean", "meta_bat_volt_min", "precip_total_in", "sol_rad_total", "sol_rad_total_ly", "vp_actual_max", "vp_actual_mean", "vp_actual_min", "vp_deficit_mean"),
        digits = 2,
        interval = NULL
      )
  } else if (timeStep == "Hourly") {
    tablePreview <- tablePreview |>
      DT::formatRound(
        columns = c("date_doy", "date_year", "meta_needs_review", "meta_version", "relative_humidity", "wind_2min_vector_dir", "wind_vector_dir", "wind_vector_dir_stand_dev"),
        digits = 0,
        interval = NULL
      ) |>
      DT::formatRound(
        columns = c("dwpt", "dwptF", "eto_azmet", "heatstress_cottonC", "heatstress_cottonF", "precip_total", "temp_airC", "temp_airF", "temp_soil_10cmC", "temp_soil_10cmF", "temp_soil_50cmC", "temp_soil_50cmF", "wind_2min_spd_max_mph", "wind_2min_spd_max_mps", "wind_2min_spd_mean_mph", "wind_2min_spd_mean_mps", "wind_spd_max_mph", "wind_spd_max_mps", "wind_spd_mph", "wind_spd_mps", "wind_vector_magnitude", "wind_vector_magnitude_mph"),
        digits = 1,
        interval = NULL
      ) |>
      DT::formatRound(
        columns = c("eto_azmet_in", "meta_bat_volt", "precip_total_in", "sol_rad_total", "sol_rad_total_ly", "vp_actual", "vp_deficit"),
        digits = 2,
        interval = NULL,
        zero.print = "%#.2f"
      )
  }
  
  return(tablePreview)
}
  