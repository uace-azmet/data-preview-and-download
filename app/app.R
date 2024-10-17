

# Preview and download hourly and daily data by specified stations and date ranges from API database


# Libraries
library(azmetr)
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
  
  sidebarLayout = sidebarLayout(
    position = "left",
    
    sidebarPanel(
      id = "sidebarPanel",
      width = 4,
      
      verticalLayout(
        helpText(em(
          "Select an AZMet station, specify the time step, and set dates for the period of interest. Then, click or tap 'PREVIEW DATA'."
        )),
        
        br(),
        selectInput(
          inputId = "azmetStation", 
          label = "AZMet Station",
          choices = stns[order(stns$stationName), ]$stationName,
          selected = "Aguila"
        ),
        
        selectInput(
          inputId = "timeStep", 
          label = "Time Step",
          choices = timeSteps,
          selected = "Hourly"
        ),
        
        dateInput(
          inputId = "startDate",
          label = "Start Date",
          value = lubridate::today(tzone = "America/Phoenix") - 1,
          min = apiStartDate,
          max = lubridate::today(tzone = "America/Phoenix"), # Initial timeStep is 'Hourly'
          format = "MM d, yyyy",
          startview = "month",
          weekstart = 0, # Sunday
          width = "100%",
          autoclose = TRUE
        ),
        
        dateInput(
          inputId = "endDate",
          label = "End Date",
          value = lubridate::today(tzone = "America/Phoenix"),
          min = apiStartDate,
          max = lubridate::today(tzone = "America/Phoenix"),  # Initial timeStep is 'Hourly'
          format = "MM d, yyyy",
          startview = "month",
          weekstart = 0, # Sunday
          width = "100%",
          autoclose = TRUE
        ),
        
        br(),
        actionButton(
          inputId = "previewData", 
          label = "Preview Data",
          class = "btn btn-block btn-blue"
        )
      )
    ), # sidebarPanel()
    
    mainPanel(
      id = "mainPanel",
      width = 8,
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "tableTitle"))
      ), 
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "tableSubtitle"))
      ),
      
      br(),
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "tableHelpText"))
      ),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, tableOutput(outputId = "dataTablePreview"))
      ), 
      
      br(), br(),
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "tableCaption"))
      ),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, uiOutput(outputId = "downloadButtonTSV"))
      ),
      
      br(), br(),
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "tableFooterHelpText"))
      ),
      
      fluidRow(
        column(width = 11, align = "left", offset = 1, htmlOutput(outputId = "tableFooter"))
      ),
      br()
    ) # mainPanel()
  ) # sidebarLayout()
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
