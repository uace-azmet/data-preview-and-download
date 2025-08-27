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
