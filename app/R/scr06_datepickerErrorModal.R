datepickerErrorModal <- 
  shiny::modalDialog(
    shiny::em(
      "Please select a 'Start Date' that is earlier than or the same as the 'End Date'."
    ),
    easyClose = FALSE,
    fade = FALSE,
    footer = shiny::modalButton("CLOSE"),
    size = "s",
    title = htmltools::p(
      id = "datepickerModal",
      bsicons::bs_icon("sliders"), 
      htmltools::HTML("&nbsp;"),
      "DATA OPTIONS"
    )
  )
