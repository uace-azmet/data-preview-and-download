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

azmetStationMetadata <- azmetr::station_info |>
  dplyr::mutate(end_date = NA) |> # Placeholder until inactive stations are in API and `azmetr`
  dplyr::mutate(
    end_date = dplyr::if_else(
      status == "active",
      lubridate::today(tzone = "America/Phoenix") - 1,
      end_date
    )
  ) |>
  dplyr::mutate(
    start_date = dplyr::if_else(
      meta_station_name == "Mohave ETo",
      lubridate::date("2024-06-20"), # When solar radiation measurements started at MOE
      start_date
    )
  ) |>
  dplyr::filter(!meta_station_name %in% c("Test"))

# Initial input date, based on `hourly` time step
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
  initialInputDate <- lubridate::today(tzone = "America/Phoenix")
} else {
  initialInputDate <- lubridate::today(tzone = "America/Phoenix") - 1
}

timeSteps <- c("Hourly", "Daily")
