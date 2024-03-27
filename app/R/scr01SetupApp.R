apiStartDate <- as.Date("2021-01-01")

# Load auxiliary files
stns <- vroom::vroom(
  file = "aux-files/azmet-stations-api-db.csv", 
  delim = ",", 
  col_names = TRUE, 
  show_col_types = FALSE
)

timeSteps <- c("Hourly", "Daily")
