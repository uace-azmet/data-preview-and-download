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

apiStartDate <- as.Date("2021-01-01")

azmetStations <- vroom::vroom(
  file = "aux-files/azmet-stations-api-db.csv", 
  delim = ",", 
  col_names = TRUE, 
  show_col_types = FALSE
)

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
