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

# apiStartDate <- as.Date("2021-01-01")
# 
# azmetStations <- vroom::vroom(
#   file = "aux-files/azmet-stations-api-db.csv",
#   delim = ",",
#   col_names = TRUE,
#   show_col_types = FALSE
# )

# Initial input dates for sidebar based on `hourly` time step and an active station, `initialEndDate` also used as a value in `azmetStationMetadata`
if (
  lubridate::now(tzone = "America/Phoenix") > 
  lubridate::as_datetime(
    paste(
      lubridate::today(tzone = "America/Phoenix"), 
      "01:30:00", sep = " "
    ),
    tz = "America/Phoenix"
  )
) {
  initialEndDate <- lubridate::today(tzone = "America/Phoenix")
  initialStartDate <- initialEndDate - 7
} else {
  initialEndDate <- lubridate::today(tzone = "America/Phoenix") - 1
  initialStartDate <- initialEndDate - 7
}



tzone <- "America/Phoenix"
newDayThresholdTime <- paste(lubridate::today(tzone = tzone), "01:30:00", sep = " ")

# Maximum hourly and daily end dates for active stations
if (lubridate::now(tzone = tzone) > lubridate::as_datetime(newDayThresholdTime)) {
  endDateHourly <- lubridate::today(tzone = tzone)
} else {
  endDateHourly <- lubridate::today(tzone = tzone) - 1
}

endDateDaily <- endDateHourly - 1


azmetStationMetadata <- azmetr::station_info |>
  dplyr::mutate(end_date = NA) |> # Placeholder until inactive stations are in API and `azmetr`
  dplyr::mutate(
    end_date_daily = dplyr::if_else(
      status == "active",
      endDateDaily,
      end_date
    )
  ) |>
  dplyr::mutate(
    end_date_hourly = dplyr::if_else(
      status == "active",
      endDateHourly,
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
