# Libraries --------------------

library(azmetr)
library(bsicons)
library(bslib)
library(dplyr)
library(DT)
library(htmltools)
library(lubridate)
library(shiny)
library(vroom)


# Files --------------------

# Functions 
#source("./R/fxn_ABC.R", local = TRUE)

# Scripts
#source("./R/scr##_DEF.R", local = TRUE)


# Variables --------------------


# `fxn_dataETL` -----

# Derived (after data retrieved from station) variables
dailyVarsDerived <- 
  c(
    "chill_hours_0C",
    "chill_hours_20C",
    "chill_hours_32F",
    "chill_hours_45F",
    "chill_hours_68F",
    "chill_hours_7C",
    "dwpt_mean",
    "dwpt_meanF",
    "eto_azmet",
    "eto_azmet_in", 
    "eto_pen_mon", 
    "eto_pen_mon_in", 
    "heat_units_10C",
    "heat_units_13C",
    "heat_units_3413C",
    "heat_units_45F",
    "heat_units_50F",
    "heat_units_55F",
    "heat_units_7C",
    "heat_units_9455F",
    "heatstress_cotton_meanC",
    "heatstress_cotton_meanF",
    "precip_total_in", 
    "sol_rad_total_ly",
    "temp_air_maxF",
    "temp_air_meanF",
    "temp_air_minF",
    "temp_soil_10cm_maxF",
    "temp_soil_10cm_meanF",
    "temp_soil_10cm_minF",
    "temp_soil_50cm_maxF",
    "temp_soil_50cm_meanF",
    "temp_soil_50cm_minF",
    "wind_2min_spd_max_mph",
    "wind_2min_spd_mean_mph",
    "wind_2min_timestamp",
    "wind_spd_max_mph",
    "wind_spd_mean_mph",
    "wind_vector_magnitude_mph"
  )

# Identification and date variables
dailyVarsID <- 
  c(
    "date_doy", 
    "date_year", 
    "datetime", 
    "meta_needs_review", 
    "meta_station_id", 
    "meta_station_name", 
    "meta_version"
  )

# Measured (or dervied at station datalogger) variables
dailyVarsMeasured <- 
  c(
    "meta_bat_volt_max",
    "meta_bat_volt_mean",
    "meta_bat_volt_min",
    "precip_total_mm", 
    "relative_humidity_max",
    "relative_humidity_mean",
    "relative_humidity_min",
    "sol_rad_total",
    "temp_air_maxC",
    "temp_air_meanC",
    "temp_air_minC",
    "temp_soil_10cm_maxC",
    "temp_soil_10cm_meanC",
    "temp_soil_10cm_minC",
    "temp_soil_50cm_maxC",
    "temp_soil_50cm_meanC",
    "temp_soil_50cm_minC",
    "vp_actual_max",
    "vp_actual_mean",
    "vp_actual_min",
    "vp_deficit_mean",
    "wind_2min_spd_max_mps",
    "wind_2min_spd_mean_mps",
    "wind_2min_timestamp",
    "wind_2min_vector_dir",
    "wind_spd_max_mps",
    "wind_spd_mean_mps",
    "wind_vector_dir",
    "wind_vector_dir_stand_dev",
    "wind_vector_magnitude"
  )

# Derived (after data retrieved from station) variables
hourlyVarsDerived <-
  c(
    "dwptF", 
    "eto_azmet", 
    "eto_azmet_in", 
    "heatstress_cottonC", 
    "heatstress_cottonF", 
    "precip_total_in", 
    "sol_rad_total_ly", 
    "temp_airF", 
    "temp_soil_10cmF", 
    "temp_soil_50cmF", 
    "wind_2min_spd_max_mph", 
    "wind_2min_spd_mean_mph", 
    "wind_spd_max_mph", 
    "wind_spd_mph", 
    "wind_vector_magnitude_mph"
  )

# Identification and date variables
hourlyVarsID <- 
  c(
    "meta_needs_review", 
    "meta_station_id", 
    "meta_station_name", 
    "meta_version", 
    "date_datetime", 
    "date_doy", 
    "date_hour", 
    "date_year"
  )

# Measured (or dervied at station datalogger) variables
hourlyVarsMeasured <- 
  c(
    "dwpt", 
    "meta_bat_volt", 
    "precip_total", 
    "relative_humidity", 
    "sol_rad_total", 
    "temp_airC", 
    "temp_soil_10cmC", 
    "temp_soil_50cmC", 
    "vp_actual", 
    "vp_deficit", 
    "wind_2min_spd_max_mps", 
    "wind_2min_spd_mean_mps", 
    "wind_2min_timestamp", 
    "wind_2min_vector_dir", 
    "wind_spd_max_mps", 
    "wind_spd_mps", 
    "wind_vector_dir", 
    "wind_vector_dir_stand_dev", 
    "wind_vector_magnitude"
  )


# Sidebar -----

tzone <- "America/Phoenix"

newDayThresholdTime <- paste(lubridate::today(tzone = tzone), "01:30:00", sep = " ")

# Maximum hourly and daily end dates for active stations
if (lubridate::now(tzone = tzone) > lubridate::as_datetime(newDayThresholdTime)) {
  endDateMaxHourly <- lubridate::today(tzone = tzone)
} else {
  endDateMaxHourly <- lubridate::today(tzone = tzone) - 1
}

endDateMaxDaily <- endDateMaxHourly - 1

azmetStationMetadata <- azmetr::station_info |>
  dplyr::mutate(end_date = NA) |> # Placeholder until inactive stations are in API and `azmetr`
  dplyr::mutate(
    end_date_daily = dplyr::if_else(
      status == "active",
      endDateMaxDaily,
      end_date
    )
  ) |>
  dplyr::mutate(
    end_date_hourly = dplyr::if_else(
      status == "active",
      endDateMaxHourly,
      end_date
    )
  ) |>
  dplyr::filter(!meta_station_name %in% c("Test"))

activeStations <-
  dplyr::filter(
    azmetStationMetadata,
    status == "active"
  )

initialStation <-
  dplyr::filter(
    activeStations,
    meta_station_name == azmetStationMetadata[order(azmetStationMetadata$meta_station_name), ]$meta_station_name[1]
  )$meta_station_name

initialDateMinimum <- 
  dplyr::filter(activeStations, meta_station_name == initialStation)$start_date

timeSteps <- c("Hourly", "Daily")
