# Shiny App ---------------------------------------------------------------
files_located <- "../DATA/OUTPUT/"
filenames <- paste0(files_located,list.files(files_located))

Countries <- readRDS(filenames[1])
forecasts_models <- readRDS(filenames[2])
models_list <- readRDS(filenames[3])

library(stargazer)
library(forecast)
library(dplyr)
library(tidyverse)
library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(shinythemes)
library(shinydashboard)
  

# UI
ui <- fluidPage(
  titlePanel("🌽 Corn: Area Harvested Forecast"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select a country:", choices = Countries, selected = "Argentina", multiple = FALSE, selectize = TRUE),
      actionButton("submit", "Generate Plot", class = "btn-primary btn-lg"),
      br(),
      h4("Download Forecast Data"),

      downloadButton("downloadButton", "Download Excel", class = "btn-warning"),
      
      br(), br(),
      div(style = "text-align: center; font-size: 14px; color: gray;", "Developed by Marcelo Katogui")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Forecast Dashboard", 
                 fluidRow(
                   column(8, plotlyOutput("plot")),
                   column(4,
                          h4("📊 Forecasting Details"),
                          p("This tool applies ARIMA, ETS, NNETAR, and TBATS models to predict the area to be harvested in hectares."),
                          h4("Forecast Accuracy"),
                          htmlOutput("table"),
                          h4("Forecasts (Equal Weighted)"),
                          htmlOutput("forecasted_years")
                   )
                 )
        ),
        tabPanel("Forecast Data", 
                 fluidRow(
                   column(12, DTOutput("forecasts"))
                 )
        )
      )
    )
  )
)

# Define the server
server <- function(input, output) {
  plot_data <- reactive({
    Country <- input$country
      
    plot_country <- autoplot(forecasts_models[[Country]]) + 
      ggtitle(label = Country) + labs(y = "Area Harvested")

    max_value <- max(forecasts_models[[Country]][,"real_data"],na.rm = T)  
    if (max_value >= 1000000) {
      plot_country <- plot_country + scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))
    } else {
      plot_country <- plot_country + scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3))
    }
      
    plotly_object <- ggplotly(plot_country)

    return(list(plotly_object, Country))
  })
  
  accuracy_data <- reactive({
    Country <- input$country
    data.frame(
      cbind(
        ARIMA = round(100-accuracy(models_list[[Country]]$arima_model)[5],digits = 0),
        ETS = round(100-accuracy(models_list[[Country]]$ets_model)[5],digits = 0),
        TBATS = round(100-accuracy(models_list[[Country]]$tbats_model)[5],digits = 0),
        NNTAR = round(100-accuracy(models_list[[Country]]$nnetar_model)[5],digits = 0)
      ),
      row.names = "Accuracy (%)"
    )
  })

  forecasts_data <- reactive({
    Country <- input$country
    Values <- round(forecasts_models[[Country]])
    names_to_change <- colnames(Values)
    names_to_change <- stringr::str_remove(names_to_change,"Values.")
    colnames(Values) <- names_to_change

    data.frame(Year = c(time(forecasts_models[[Country]])),Values)
  })
  

  output$plot <- renderPlotly({
    if(input$submit > 0) {
      # Call the reactive expression to generate the plot data
      plot_data()[[1]]
    }
  })
  
  output$table <- renderText({stargazer(t(accuracy_data()),type = "html")})
  
  output$forecasted_years <- renderTable({
    startEnd <- 2021:2025
    Year <- ((forecasts_data()%>%filter(Year %in% startEnd))$Year)
    Area_Harvested <- (forecasts_data()%>%filter(Year %in% startEnd))[,2]
    Area_Harvested_Forecasted <- (rowMeans((forecasts_data()%>%filter(Year %in% startEnd))[,3:6]))

    data.frame(
      Year = format(Year,digits = 1),
      cbind(Area_Harvested=format(Area_Harvested,big.mark = ","), Area_Harvested_Forecasted=format(Area_Harvested_Forecasted,big.mark = ","))
    )
  })
  
  output$forecasts <- renderDataTable({
    datatable(
      forecasts_data() %>%
        arrange(desc(Year)),  # Sort the data by Year, descending
      rownames = FALSE,
      options = list(
        pageLength = 15
      )
    )
  })
  
  
  
  output$downloadButton <- downloadHandler(
    filename = function() {paste("data-", Sys.Date(), ".xlsx", sep="")},
    content = function(file) {openxlsx::write.xlsx(forecasts_data(), file)}
  )
}

# Run the app
shinyApp(ui = ui, server = server)
