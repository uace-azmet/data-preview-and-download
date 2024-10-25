cardDatatable <- bslib::card(
  max_height = 800,
  full_screen = TRUE,
  
  bslib::card_header(
    #class = "bg-light",
    shiny::htmlOutput(outputId = "tableTitle")
  ),
  
  bslib::card_body(
    class = "p-0",
    gt::gt_output(outputId = "gt_tbl")
  ),
  
  bslib::card_footer(
    class = "fs-6",
    lorem::ipsum(paragraphs = 1, sentences = 1)
  )
) |>
  htmltools::tagAppendAttributes(
    #https://getbootstrap.com/docs/5.0/utilities/api/
    class = "border-0 rounded-0 shadow-none"
  )
