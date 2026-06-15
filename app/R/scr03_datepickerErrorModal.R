datepickerErrorModal <- 
  shiny::modalDialog(
    htmltools::HTML(
      "<em>Please select a <b>Start Date</b> that is earlier than or the same as the <b>End Date</b>.</em>"
    ),
    easyClose = FALSE,
    fade = FALSE,
    footer = shiny::modalButton("CLOSE"),
    size = "s",
    title = 
      htmltools::p(
        id = "datepickerModal",
        bsicons::bs_icon("sliders", class = "bolder-icon"), 
        htmltools::HTML("&nbsp;<b>DATA OPTIONS</b>")
      )
  )
