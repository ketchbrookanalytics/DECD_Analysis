
# Load R Packages ---------------------------------------------------------

# These are all the packages we'll need to build our app
# You'll need to install any of these that you don't yet have installed
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(shinyWidgets)
library(scales)
library(RSocrata)

options(scipen = 999)


# Data Prep ---------------------------------------------------------------

# # Grab data from data.ct.gov API endpoint specific to this dataset
# url <- "https://data.ct.gov/resource/a8k4-9euq.json"
# 
# cols <- c(
#   "amount_leveraged", 
#   "company", 
#   "county_1", 
#   "fiscal_year", 
#   "funding_source", 
#   "grant_amount", 
#   "loan_amount", 
#   "industry", 
#   "municipality", 
#   "status", 
#   "total_assistance", 
#   "total_project_cost", 
#   "longitude", 
#   "latitude"
# )
# 
# query <- paste0(
#   url, "?",
#   "$query=SELECT ", paste(cols, collapse = ", "), 
#   " WHERE state = 'CT'"
# )
# 
# DECD_data <- RSocrata::read.socrata(
#   "https://data.ct.gov/resource/a8k4-9euq.json?state=CT"
# )

# Format some of the variable types to 'numeric', and 
# correct spelling error in "East WIndsor" observations 
DECD_data <- readRDS("data.RDS") |> 
  dplyr::mutate(
    dplyr::across(
      .cols = grant_amount:amount_leveraged,
      .fns = function(x) as.integer(x)
    )
  ) |> 
  dplyr::select(
    company = company_name, 
    fiscal_year, 
    funding_source, 
    status = job_obligation_status, 
    grant_amount, 
    loan_amount, 
    total_assistance, 
    total_project_cost, 
    amount_leveraged, 
    industry = business_industry, 
    municipality, 
    county_1, 
    geocoded_location.coordinates
  ) |> 
  dplyr::mutate(
    municipality = dplyr::if_else(
      municipality == "East WIndsor", 
      "East Windsor", 
      municipality
    ), 
    funding_source = dplyr::case_when(
      grepl("Manufacturing Assistance Act", funding_source) == TRUE ~ "Manufacturing Assistance Act", 
      funding_source == "Small Business Express Program" ~ "Small Business Express Program", 
      TRUE ~ "Other"
    )
  ) |> 
  tidyr::hoist(
    geocoded_location.coordinates,
    latitude = list(2L),
    longitude = list(1L)
  ) |> 
  dplyr::select(-geocoded_location.coordinates)


# Build the UI ------------------------------------------------------------

# Construct the sidebar
sidebar <- shiny::div(id = "Sidebar",
                      shiny::sidebarPanel( 
                        shiny::h1("CT DECD ANALYZER"), 
                        hr(), 
                        # Create input selection for the 'fiscal_year'
                        shinyWidgets::pickerInput(
                          inputId = "MapYearSelection", 
                          label = "Select a Year", 
                          choices = sort(unique(DECD_data$fiscal_year), 
                                         decreasing = TRUE), 
                          selected = unique(DECD_data$fiscal_year), 
                          options = list(`actions-box` = TRUE), 
                          multiple = T
                        ), 
                        # Create input selection for 'funding_source'
                        shinyWidgets::pickerInput(
                          inputId = "MapFundingSourceSelection", 
                          label = "Select a Funding Source", 
                          choices = sort(unique(DECD_data$funding_source)), 
                          selected = "Manufacturing Assistance Act", 
                          options = list(`actions-box` = TRUE),
                          multiple = TRUE
                        ), 
                        # Create input selection for 'municipality'
                        shinyWidgets::pickerInput(
                          inputId = "CitySelection",
                          label = "Select a City",
                          choices = sort(unique(DECD_data$municipality)),
                          selected = unique(DECD_data$municipality),
                          options = list(`actions-box` = TRUE),
                          multiple = TRUE
                        ), 
                        hr(), 
                        br(), 
                        # Add text to the sidebar below the input selectors
                        shiny::h5("This interactive dashboard displays data reported by the Connecticut Department of Economic and Community Development."), 
                        shiny::h5("The DECD provides support for Connecticut business in a variety of ways, including grants and loans."), 
                        shiny::h5("This dashboard shows the businesses in Connecticut that received assistance from the DECD in the last decade."), 
                        shiny::h5("To learn more about the DECD or see the public dataset used, click on the respsective links below:"), 
                        shiny::uiOutput("DECDurl"), 
                        shiny::uiOutput("DECDdataset"), 
                        hr(), 
                        br(), 
                        br(), 
                        shiny::h6("This dashboard was developed by Mike Thomas using the R programming language and the Shiny development framework."), 
                        shiny::h6("This is an open project, and all of the app code can be found in the following Github repository:"), 
                        shiny::uiOutput("GithubLink"),
                        width = 3
                      )
)

# Construct the main dashboard
maindash <- shiny::mainPanel( 
  # Create a "toggle sidebar" button to show/hide the sidebar
  shiny::actionButton("toggleSidebar", "Toggle sidebar", icon = icon("list-alt")), 
  br(), 
  hr(), 
  # Create the first "row" of the dashboard containing the Map and Chart
  shiny::fluidRow(
    shiny::tabsetPanel(
      shiny::tabPanel(
        title = "Map", 
        leaflet::leafletOutput(
          outputId = "Map"
        )
      ), 
      shiny::tabPanel(
        title = "Chart", 
        shiny::plotOutput(
          outputId = "BarChart"
        )
      )
    )
  ), 
  hr(), 
  # Create the second "row" of the dashboard containing the Table
  shiny::fluidRow(
    DT::dataTableOutput(
      outputId = "Table"
    )
  )
)

# Piece the UI together
ui <- shiny::fluidPage( 
  # Format the dashboard theme/color
  theme = shinythemes::shinytheme(theme = "sandstone"), 
  shinyWidgets::setBackgroundColor(color = "lightgrey", 
                                   gradient = c("linear", 
                                                "radial"), 
                                   direction = c("bottom", 
                                                 "top", 
                                                 "right", 
                                                 "left")), 
  shinyjs::useShinyjs(), 
  shiny::sidebarLayout( 
    # Add the pieces of the UI constructed earlier to the final "UI" object
    sidebarPanel = sidebar, 
    mainPanel = maindash
  )
)

# Build the Server --------------------------------------------------------

server <- shiny::shinyServer(function(input, output, session) {
  
  # Create toggle sidebar functionality
  shiny::observeEvent(input$toggleSidebar, {
    shinyjs::toggle(id = "Sidebar")
  })
  
  # Create hyperlink to DECD homepage
  output$DECDurl <- shiny::renderUI({
    shiny::tagList(a("DECD Homepage", href = "https://portal.ct.gov/DECD"))
  })
  
  # Create hyperlink to data.ct.gov DECD dataset
  output$DECDdataset <- shiny::renderUI({
    shiny::tagList(a("DECD Dataset", href = "https://data.ct.gov/Business/Department-of-Economic-and-Community-Development-D/xnw3-nytd"))
  })
  
  # Create hyperlink to Github repo
  output$GithubLink <- shiny::renderUI({
    shiny::tagList(a("Github Repo", href = "https://github.com/thomasmj92/DECD_Analysis"))
  })
  
  # Create leaflet map
  output$Map <- leaflet::renderLeaflet({
    leaflet::leaflet() |> 
      leaflet::addProviderTiles(provider = providers$Esri) |> 
      leaflet::addCircleMarkers(data = DECD_data |> 
                                  dplyr::filter(fiscal_year %in% input$MapYearSelection) |> 
                                  dplyr::filter(funding_source %in% input$MapFundingSourceSelection) |> 
                                  dplyr::filter(municipality %in% input$CitySelection), 
                                lng = ~longitude, 
                                lat = ~latitude, 
                                weight = 10, 
                                radius = 5,
                                popup = ~company)
  })
  
  # Create bar chart
  output$BarChart <- shiny::renderPlot({
    ggplot2::ggplot(DECD_data |>
             dplyr::filter(fiscal_year %in% input$MapYearSelection) |>
             dplyr::filter(funding_source %in% input$MapFundingSourceSelection) |>
             dplyr::filter(municipality %in% input$CitySelection) |> 
             dplyr::rename(City = municipality) |> 
             dplyr::select(fiscal_year, total_assistance, City),
           ggplot2::aes(x = fiscal_year,
               y = total_assistance, 
               fill = City)) +
      ggplot2::geom_bar(stat = "identity") + 
      ggplot2::xlab("Year") + 
      ggplot2::ylab("Sum of Total Assistance") + 
      ggplot2::scale_y_continuous(labels = scales::comma)
    
  })
  
  # Create data table
  output$Table <- DT::renderDataTable({
    DECD_data |>
      dplyr::filter(fiscal_year %in% input$MapYearSelection) |>
      dplyr::filter(funding_source %in% input$MapFundingSourceSelection) |>
      dplyr::filter(municipality %in% input$CitySelection) |>
      dplyr::mutate(Company = company,
                    County = county_1,
                    City = municipality,
                    Year = fiscal_year,
                    Funding_Source = funding_source,
                    Industry = industry,
                    Grant_Amt = formatC(round(grant_amount), format = "f", big.mark = ",", drop0trailing = TRUE), 
                    Loan_Amt = formatC(round(loan_amount), format = "f", big.mark = ",", drop0trailing = TRUE),
                    Total_Amt = formatC(round(total_assistance), format = "f", big.mark = ",", drop0trailing = TRUE),
                    Amt_Leveraged = formatC(round(amount_leveraged), format = "f", big.mark = ",", drop0trailing = TRUE),
                    Status = status) |>
      dplyr::select(Company,
                    County,
                    City,
                    Year,
                    Funding_Source,
                    Industry,
                    Grant_Amt,
                    Loan_Amt,
                    Total_Amt,
                    Amt_Leveraged,
                    Status)
  }, rownames = FALSE)
  
})


# Run the App -------------------------------------------------------------

shiny::shinyApp(ui = ui, server = server)



