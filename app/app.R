# Preview and download hourly and daily data from API database by specified stations and date ranges

# Libraries
library(azmetr)
library(bsicons)
library(bslib)
library(dplyr)
library(DT)
library(htmltools)
library(lubridate)
library(shiny)
library(vroom)

# Functions 
#source("./R/fxn_ABC.R", local = TRUE)

# Scripts
#source("./R/scr##_DEF.R", local = TRUE)


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
    
    shiny::htmlOutput(outputId = "tableTitle"),
    # shiny::htmlOutput(outputId = "tableHelpText"),
    DT::dataTableOutput("tablePreview"),
    shiny::htmlOutput(outputId = "tableFooter"),
    shiny::htmlOutput(outputId = "downloadButtonHelpText"),
    shiny::uiOutput(outputId = "downloadButtonCSV"),
    shiny::uiOutput(outputId = "downloadButtonTSV"),
    shiny::htmlOutput(outputId = "sidebarPageText")
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
  
  shiny::observeEvent(input$previewData, {
    if (input$startDate > input$endDate) {
      shiny::showModal(datepickerErrorModal) # `scr05_datepickerErrorModal.R`
    }
  })
  
  # Reactives -----
  
  dataETL <- shiny::eventReactive(input$previewData, {
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
    
    fxn_dataETL(
      azmetStation = input$azmetStation, 
      timeStep = input$timeStep, 
      startDate = input$startDate, 
      endDate = input$endDate
    )
  })
  
  dataFormat <- shiny::eventReactive(dataETL(), {
    fxn_dataFormat(
      inData = dataETL(), 
      timeStep = input$timeStep
    )
  })
  
  downloadButtonHelpText <- shiny::eventReactive(dataETL(), {
    fxn_downloadButtonHelpText()
  })
  
  sidebarPageText <- shiny::eventReactive(dataETL(), {
    fxn_sidebarPageText(timeStep = input$timeStep)
  })
  
  tableFooter <- shiny::eventReactive(dataETL(), {
    fxn_tableFooter(
      inData = dataETL(),
      timeStep = input$timeStep
    )
  })
  
  tablePreview <- shiny::eventReactive(dataETL(), {
    fxn_tablePreview(
      inData = dataFormat(),
      timeStep = input$timeStep
    )
  })
  
  tableTitle <- shiny::eventReactive(dataETL(), {
    fxn_tableTitle(
      azmetStation = input$azmetStation,
      timeStep = input$timeStep
    )
  })
  
  
  # Outputs -----
  
  output$downloadButtonCSV <- renderUI({
    req(dataETL())
    downloadButton(
      "downloadCSV", 
      label = "Download .csv", 
      class = "btn btn-default btn-blue", 
      type = "button"
    )
  })
  
  output$downloadButtonHelpText <- renderUI({
    downloadButtonHelpText()
  })
  
  output$downloadButtonTSV <- renderUI({
    req(dataETL())
    downloadButton(
      "downloadTSV", 
      label = "Download .tsv", 
      class = "btn btn-default btn-blue", 
      type = "button"
    )
  })
  
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste0(
        "AZMet-", input$azmetStation, "-", input$timeStep, "-Data-", input$startDate, "-to-", input$endDate, ".csv"
      )
    },
    
    content = function(file) {
      vroom::vroom_write(
        x = dataFormat(), 
        file = file, 
        delim = ","
      )
    }
  )
  
  output$downloadTSV <- downloadHandler(
    filename = function() {
      paste0(
        "AZMet-", input$azmetStation, "-", input$timeStep, "-Data-", input$startDate, "-to-", input$endDate, ".tsv"
      )
    },
    
    content = function(file) {
      vroom::vroom_write(
        x = dataFormat(), 
        file = file, 
        delim = "\t"
      )
    }
  )
  
  output$sidebarPageText <- renderUI({
    sidebarPageText()
  })
  
  output$tableFooter <- renderUI({
    tableFooter()
  })
  
  output$tablePreview <- DT::renderDataTable({
    tablePreview()
  })
  
  output$tableTitle <- renderUI({
    tableTitle()
  })
}


# Run --------------------

shinyApp(ui, server)
