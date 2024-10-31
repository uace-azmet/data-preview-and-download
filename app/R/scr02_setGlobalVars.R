apiStartDate <- as.Date("2021-01-01")

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
