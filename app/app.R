# Preview and download hourly and daily data by specified stations and date ranges from API database

# Libraries
library(azmetr)
library(bsicons)
library(bslib)
library(dplyr)
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
    #theme = theme, # `scr03_theme.R`
    lang = NULL,
    window_title = NA,
    
    shiny::htmlOutput(outputId = "tableTitle"),
    shiny::htmlOutput(outputId = "tableTitle"),
    shiny::htmlOutput(outputId = "tableHelpText"),
    #navsetCardTab, # `scr05_navsetCardTab.R`
    shiny::tableOutput(outputId = "dataTablePreview"),
    shiny::htmlOutput(outputId = "tableCaption"),
    shiny::uiOutput(outputId = "downloadButtonTSV"),
    br(), 
    br(),
    br(),
    shiny::htmlOutput(outputId = "tableFooterHelpText"),
    shiny::htmlOutput(outputId = "tableFooter")
  )
) # htmltools::htmlTemplate()


# Server --------------------

server <- function(input, output, session) {
  
  # Reactive events -----
  
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
  
  # AZMet data ELT
  dfAZMetData <- eventReactive(input$previewData, {
    validate(
      need(expr = input$startDate <= input$endDate, message = FALSE)
    )
    
    idPreview <- showNotification(
      ui = "Preparing data preview . . .", 
      action = NULL, 
      duration = NULL, 
      closeButton = FALSE,
      id = "idPreview",
      type = "message"
    )
    
    on.exit(removeNotification(id = idPreview), add = TRUE)
    
    fxnAZMetDataELT(
      azmetStation = input$azmetStation, 
      timeStep = input$timeStep, 
      startDate = input$startDate, 
      endDate = input$endDate
    )
  })
  
  # Format AZMet data for HTML table preview
  dfAZMetDataPreview <- eventReactive(dfAZMetData(), {
    fxnAZMetDataPreview(
      inData = dfAZMetData(), 
      timeStep = input$timeStep
    )
  })
  
  # Build table caption
  tableCaption <- eventReactive(dfAZMetData(), {
    fxnTableCaption()
  })
  
  # Build table footer
  tableFooter <- eventReactive(dfAZMetData(), {
    fxnTableFooter(timeStep = input$timeStep)
  })
  
  # Build table footer help text
  tableFooterHelpText <- eventReactive(dfAZMetData(), {
    fxnTableFooterHelpText()
  })
  
  # Build table help text
  tableHelpText <- eventReactive(dfAZMetData(), {
    fxnTableHelpText()
  })
  
  # Build table subtitle
  tableSubtitle <- eventReactive(dfAZMetData(), {
    fxnTableSubtitle(
      startDate = input$startDate, 
      endDate = input$endDate
    )
  })
  
  # Build table title
  tableTitle <- eventReactive(input$previewData, {
    validate(
      need(
        expr = input$startDate <= input$endDate, 
        message = "Please select a 'Start Date' that is earlier than or the same as the 'End Date'."
      ),
      errorClass = "datepicker"
    )
    
    fxnTableTitle(
      azmetStation = input$azmetStation,
      timeStep = input$timeStep
    )
  })
  
  # Outputs -----
  
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
