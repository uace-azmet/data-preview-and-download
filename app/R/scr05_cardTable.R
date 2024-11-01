cardTable <- bslib::card(
  full_screen = TRUE,
  max_height = 600,
  
  bslib::card_header(
    shiny::htmlOutput(outputId = "cardHeaderTitle"),
    shiny::htmlOutput(outputId = "tableHelpText")
  ),
  
  bslib::card_body(
    reactable::reactableOutput("cardTable")
  ),
  
  bslib::card_footer(
    shiny::htmlOutput(outputId = "cardFooterText")
  )
) |>
  htmltools::tagAppendAttributes(
    #https://getbootstrap.com/docs/5.0/utilities/api/
    class = "border-0 rounded-0 shadow-none"
  )
