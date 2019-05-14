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

DECD_data <- RSocrata::read.socrata("https://data.ct.gov/resource/a8k4-9euq.json")

DECD_data <- DECD_data %>% 
  dplyr::mutate_at(.vars = c("amount_leveraged", 
                             "grant_amount", 
                             "loan_amount", 
                             "total_assistance", 
                             "total_project_cost" 
  ), 
  .funs = as.numeric) %>% 
  dplyr::mutate(municipality = ifelse(municipality == "East WIndsor", 
                                      "East Windsor", 
                                      municipality))

DECD_data$longitude <- c(rep(NA, nrow(DECD_data)))

DECD_data$latitude <- c(rep(NA, nrow(DECD_data)))

for (i in 1:nrow(DECD_data)) { 
  
  DECD_data$longitude[i] <- ifelse(is.null(DECD_data$geocoded_location.coordinates[[i]]) == TRUE, 
                                   NA, 
                                   DECD_data$geocoded_location.coordinates[[i]][1])
  
  DECD_data$latitude[i] <- ifelse(is.null(DECD_data$geocoded_location.coordinates[[i]]) == TRUE, 
                                  NA, 
                                  DECD_data$geocoded_location.coordinates[[i]][2])
  
}

DECD_data <- DECD_data %>% 
  dplyr::filter(state == "CT") %>%
  dplyr::select(amount_leveraged, 
                company, 
                county_1, 
                fiscal_year, 
                funding_source, 
                grant_amount, 
                loan_amount, 
                industry, 
                municipality, 
                status, 
                total_assistance, 
                total_project_cost, 
                longitude, 
                latitude) %>% 
  dplyr::mutate(funding_source = dplyr::case_when(
    grepl("Manufacturing Assistance Act", funding_source) == TRUE ~ "Manufacturing Assistance Act", 
    funding_source == "Small Business Express Program" ~ "Small Business Express Program", 
    TRUE ~ "Other"
  ))


# Build the UI ------------------------------------------------------------

# Construct the sidebar
sidebar <- shiny::div(id = "Sidebar",
                      shiny::sidebarPanel( 
    shiny::h1("CT DECD ANALYZER"), 
    hr(), 
    shinyWidgets::pickerInput(
      inputId = "MapYearSelection", 
      label = "Select a Year", 
      choices = sort(unique(DECD_data$fiscal_year), 
                     decreasing = TRUE), 
      selected = unique(DECD_data$fiscal_year), 
      options = list(`actions-box` = TRUE), 
      multiple = T
    ), 
    shinyWidgets::pickerInput(
      inputId = "MapFundingSourceSelection", 
      label = "Select a Funding Source", 
      choices = sort(unique(DECD_data$funding_source)), 
      selected = "Manufacturing Assistance Act", 
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    ), 
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
    width = 3
)
)

# Construct the main dashboard
maindash <- shiny::mainPanel( 
  shiny::actionButton("toggleSidebar", "Toggle sidebar", icon = icon("list-alt")), 
  br(), 
  hr(), 
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
  shiny::fluidRow(
    DT::dataTableOutput(
      outputId = "Table"
    )
  )
)
  
# Piece the UI together
ui <- shiny::fluidPage( 
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
  
  # Create leaflet map
  output$Map <- leaflet::renderLeaflet({
    leaflet() %>% 
      leaflet::addProviderTiles(provider = providers$Esri) %>% 
      leaflet::addCircleMarkers(data = DECD_data %>% 
                                  dplyr::filter(fiscal_year %in% input$MapYearSelection) %>% 
                                  dplyr::filter(funding_source %in% input$MapFundingSourceSelection) %>% 
                                  dplyr::filter(municipality %in% input$CitySelection), 
                                lng = ~longitude, 
                                lat = ~latitude, 
                                weight = 10, 
                                radius = 5,
                                popup = ~company)
  })
  
  # Create bar chart
  output$BarChart <- shiny::renderPlot({
   ggplot(DECD_data %>%
            dplyr::filter(fiscal_year %in% input$MapYearSelection) %>%
            dplyr::filter(funding_source %in% input$MapFundingSourceSelection) %>%
            dplyr::filter(municipality %in% input$CitySelection) %>% 
            dplyr::rename(City = municipality) %>% 
            dplyr::select(fiscal_year, total_assistance, City),
            aes(x = fiscal_year,
                y = total_assistance, 
                fill = City)) +
     ggplot2::geom_bar(stat = "identity") + 
     ggplot2::xlab("Year") + 
     ggplot2::ylab("Sum of Total Assistance") + 
     ggplot2::scale_y_continuous(labels = scales::comma)
     
 })

  # Create data table
  output$Table <- DT::renderDataTable({

   DECD_data %>%
     dplyr::filter(fiscal_year %in% input$MapYearSelection) %>%
     dplyr::filter(funding_source %in% input$MapFundingSourceSelection) %>%
     dplyr::filter(municipality %in% input$CitySelection) %>%
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
                   Status = status) %>%
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



