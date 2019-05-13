library(shiny)
library(tidyverse)
library(leaflet)
library(shinyWidgets)

options(scipen = 999)


# Data Prep ---------------------------------------------------------------

DECD.data <- readRDS("DECD_data.Rds")

DECD.data <- DECD.data %>% 
  dplyr::mutate_at(.vars = c("amount_leveraged", 
                             "grant_amount", 
                             "loan_amount", 
                             "total_assistance", 
                             "total_project_cost" 
                             ), 
                   .funs = as.numeric)

DECD.data$longitude <- c(rep(NA, nrow(DECD.data)))

DECD.data$latitude <- c(rep(NA, nrow(DECD.data)))

for (i in 1:nrow(DECD.data)) { 
  
  DECD.data$longitude[i] <- ifelse(is.null(DECD.data$geocoded_location.coordinates[[i]]) == TRUE, 
                                   NA, 
                                   DECD.data$geocoded_location.coordinates[[i]][1])
  
  DECD.data$latitude[i] <- ifelse(is.null(DECD.data$geocoded_location.coordinates[[i]]) == TRUE, 
                                  NA, 
                                  DECD.data$geocoded_location.coordinates[[i]][2])
  
}

Map.DECD.data <- DECD.data %>% 
  dplyr::filter(state == "CT") %>%
  dplyr::select(amount_leveraged, 
                company, 
                county_1, 
                fiscal_year, 
                funding_source, 
                grant_amount, 
                industry, 
                municipality, 
                status, 
                total_assistance, 
                total_project_cost, 
                longitude, 
                latitude)


# Build the Server --------------------------------------------------------

shiny::shinyServer(function(input, output, session) {
  
  
  
  output$Map <- leaflet::renderLeaflet(
    leaflet() %>% 
      leaflet::addProviderTiles(provider = providers$Esri) %>% 
      leaflet::addCircleMarkers(data = Map.DECD.data %>% 
                            dplyr::filter(fiscal_year %in% input$MapYearSelection), # %>% 
                            #dplyr::filter(funding_source %in% input$MapFundingSourceSelection), 
                          lng = ~longitude, 
                          lat = ~latitude, 
                          weight = 10, 
                          radius = 5,
                          popup = ~company)
  )
  
  output$DisplayLengthYearSelection <- shiny::renderText({
    length(input$MapYearSelection)
  })
  
  output$AmountsHist <- shiny::renderPlot({
    ggplot(DECD.data %>% 
             dplyr::filter(municipality == input$TownSelection), 
           aes_string(input$VariableSelection)) + 
      ggplot2::geom_histogram()
  })
  
  
  
  
})