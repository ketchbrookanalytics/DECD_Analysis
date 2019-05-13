library(shiny)

mapPanel <- shiny::tabPanel(
  title = "Map Page", 
  shiny::fluidRow(
    column(
      width = 3, 
      shinyWidgets::pickerInput(
        inputId = "MapYearSelection", 
        label = "Select a Year", 
        choices = sort(unique(DECD.data$fiscal_year), 
                       decreasing = TRUE), 
        selected = "2018", 
        options = list(`actions-box` = TRUE), 
        multiple = T
      ), 
      shiny::selectInput(
        inputId = "MapFundingSourceSelection", 
        label = "Select a Funding Source", 
        choices = sort(unique(DECD.data$funding_source)), 
        multiple = TRUE
      ), 
      shiny::selectInput(
        inputId = "MapRadiusMeasureSelection", 
        label = "Select a variable to use for the circle radius scale", 
        choices = list("amount_leveraged", 
                       "grant_amount", 
                       "total_assistance", 
                       "total_project_cost")
      ), 
      shiny::textOutput(
        outputId = "DisplayLengthYearSelection"
      )
      ), 
    column(
      width = 9, 
      leaflet::leafletOutput(
        outputId = "Map"
      )
    )
  )
)

histPanel <- shiny::tabPanel(
  title = "Hist Page", 
  shiny::fluidRow(
    column(
      width = 3, 
      shiny::selectInput(
        inputId = "VariableSelection", 
        label = "Choose a variable", 
        choices = list("grant_amount", 
                       "amount_leveraged")
        ), 
      shiny::selectInput(
        inputId = "TownSelection", 
        label = "Choose a Town", 
        choices = sort(unique(DECD.data$municipality))
      )
      ), 
    column(
      width = 9, 
      shiny::plotOutput(
        outputId = "AmountsHist"
      )
    )
    )
  )

shiny::navbarPage(
  title = "DECD Analysis", 
  selected = "Map Page", 
  histPanel, 
  mapPanel
)