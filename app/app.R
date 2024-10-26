# Preview and download hourly and daily data from API database by specified stations and date ranges

# Libraries
library(azmetr)
library(bsicons)
library(bslib)
library(dplyr)
library(gt)
library(htmltools)
library(lubridate)
library(shiny)
library(vroom)

# Functions 
#source("./R/fxnABC.R", local = TRUE)

# Scripts
#source("./R/scr##DEF.R", local = TRUE)


# UI --------------------

ui <- htmltools::htmlTemplate(
  
  "azmet-shiny-template.html",
  
  pageSidebar = bslib::page_sidebar(
    title = NULL,
    sidebar = sidebar, # `scr04_sidebar.R`
    fillable = TRUE,
    fillable_mobile = FALSE,
    theme = theme, # `scr03_theme.R`
    lang = NULL,
    window_title = NA,
    
    #shiny::htmlOutput(outputId = "tableTitle"),
    #shiny::htmlOutput(outputId = "tableHelpText"),
    
    cardDatatable, # `scr05_cardDatatable.R`
    
    #shiny::tableOutput(outputId = "dataTablePreview"),
    htmltools::br(), 
    htmltools::br(),
    shiny::htmlOutput(outputId = "tableCaption"),
    shiny::uiOutput(outputId = "downloadButtonTSV"),
    htmltools::br(), 
    htmltools::br(),
    htmltools::br(),
    shiny::htmlOutput(outputId = "tableFooterHelpText"),
    shiny::htmlOutput(outputId = "tableFooter")
  )
)


# Server --------------------

server <- function(input, output, session) {
  
  # Observables -----
  
  # Update maximum calendar date based on time step 
  shiny::observeEvent(input$timeStep, {
    if (input$timeStep == "Daily") {
      if (input$startDate == lubridate::today(tzone = "America/Phoenix")) {
        startDateAdjusted <- lubridate::today(tzone = "America/Phoenix") - 1
      } else {
        startDateAdjusted <- input$startDate
      }
      
      if (input$endDate == lubridate::today(tzone = "America/Phoenix")) {
        endDateAdjusted <- lubridate::today(tzone = "America/Phoenix") - 1
      } else {
        endDateAdjusted <- input$endDate
      }
      
      shiny::updateDateInput(
        session = session, 
        inputId = "startDate",
        label = "Start Date",
        value = startDateAdjusted,
        min = apiStartDate,
        max = lubridate::today(tzone = "America/Phoenix") - 1
      )
      
      shiny::updateDateInput(
        session = session, 
        inputId = "endDate",
        label = "End Date",
        value = endDateAdjusted,
        min = apiStartDate,
        max = lubridate::today(tzone = "America/Phoenix") - 1
      )
    } else if (input$timeStep == "Hourly") {
      shiny::updateDateInput(
        session = session, 
        inputId = "startDate",
        label = "Start Date",
        value = input$startDate,
        min = apiStartDate,
        max = lubridate::today(tzone = "America/Phoenix")
      )
      
      shiny::updateDateInput(
        session = session, 
        inputId = "endDate",
        label = "End Date",
        value = input$endDate,
        min = apiStartDate,
        max = lubridate::today(tzone = "America/Phoenix")
      )
    }
  })
  
  # Date error notification
  shiny::observeEvent(input$previewData, {
    if (input$startDate > input$endDate) {
      shiny::showModal(datepickerErrorModal) # `scr06_datepickerErrorModal.R`
    }
  })
  
  # Reactives -----
  
  # Download AZMet data
  dfAZMetData <- shiny::eventReactive(input$previewData, {
    shiny::validate(
      shiny::need(
        expr = input$startDate <= input$endDate, 
        message = FALSE
      )
    )
    
    idPreview <- shiny::showNotification(
      ui = "Preparing data preview . . .", 
      action = NULL, 
      duration = NULL, 
      closeButton = FALSE,
      id = "idPreview",
      type = "message"
    )
    
    on.exit(shiny::removeNotification(id = idPreview), add = TRUE)
    
    fxnAZMetDataELT(
      azmetStation = input$azmetStation, 
      timeStep = input$timeStep, 
      startDate = input$startDate, 
      endDate = input$endDate
    )
  })
  
  # Format AZMet data for table preview
  dfAZMetDataPreview <- shiny::eventReactive(dfAZMetData(), {
    fxnAZMetDataPreview(
      inData = dfAZMetData(), 
      timeStep = input$timeStep
    )
  })
  
  # Build table caption
  tableCaption <- shiny::eventReactive(dfAZMetData(), {
    fxnTableCaption()
  })
  
  # Build table footer
  tableFooter <- shiny::eventReactive(dfAZMetData(), {
    fxnTableFooter(timeStep = input$timeStep)
  })
  
  # Build table footer help text
  tableFooterHelpText <- shiny::eventReactive(dfAZMetData(), {
    fxnTableFooterHelpText()
  })
  
  # Build table help text
  tableHelpText <- shiny::eventReactive(dfAZMetData(), {
    fxnTableHelpText()
  })
  
  # Build table subtitle
  tableSubtitle <- shiny::eventReactive(dfAZMetData(), {
    fxnTableSubtitle(
      startDate = input$startDate, 
      endDate = input$endDate
    )
  })
  
  # Build table title
  tableTitle <- shiny::eventReactive(dfAZMetData(), {
    fxnTableTitle(
      azmetStation = input$azmetStation,
      timeStep = input$timeStep
    )
  })
  
  # Outputs -----
  
  output$gt_tbl <- gt::render_gt({
    expr = dfAZMetDataPreview()
  })
  
  output$dataTablePreview <- renderTable(
    expr = dfAZMetDataPreview(), 
    striped = TRUE, 
    hover = TRUE, 
    bordered = FALSE, 
    spacing = "xs", 
    width = "auto", 
    align = "c", 
    rownames = FALSE, 
    colnames = TRUE, 
    digits = NULL, 
    na = "na"
  )
  
  output$downloadButtonTSV <- renderUI({
    req(dfAZMetData())
    downloadButton(
      "downloadTSV", 
      label = "Download .tsv", 
      class = "btn btn-default btn-blue", 
      type = "button"
    )
  })
  
  output$downloadTSV <- downloadHandler(
    filename = function() {
      paste0(
        "AZMet ", input$azmetStation, " ", input$timeStep, " Data ", input$startDate, " to ", input$endDate, ".tsv"
      )
    },
    content = function(file) {
      vroom::vroom_write(x = dfAZMetData(), file = file, delim = "\t")
    }
  )
  
  output$tableCaption <- renderUI({
    tableCaption()
  })
  
  output$tableFooter <- renderUI({
    tableFooter()
  })
  
  output$tableFooterHelpText <- renderUI({
    tableFooterHelpText()
  })
  
  output$tableHelpText <- renderUI({
    tableHelpText()
  })
  
  output$tableSubtitle <- renderUI({
    tableSubtitle()
  })
  
  output$tableTitle <- renderUI({
    tableTitle()
  })
}


# Run --------------------

shinyApp(ui, server)
