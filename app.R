# Shiny App ---------------------------------------------------------------
files_located <- "DATA/OUTPUT/"
filenames <- paste0(files_located,list.files(files_located))

Countries <- readRDS(filenames[1])
forecasts_models <- readRDS(filenames[2])
models_list <- readRDS(filenames[3])

library(dplyr)
library(tidyverse)
library(shiny)
library(forecast)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(stargazer)
library(forecast)
library(shinythemes)
library(shinydashboard)
  

ui <- dashboardPage(
  
  dashboardHeader(
    title="Corn: Area Harvested Forecasted",titleWidth = 400),
  
  dashboardSidebar(
    selectInput(inputId = "country",
                label = "Select a country:",
                choices = Countries,selected = "Argentina"),
    actionButton(inputId = "submit",
                 label = "Generate Plot")
  ),
  
  dashboardBody(
    fluidRow( 
      box(plotlyOutput(outputId = "plot"), 
          title = "Forecasted values", 
          height = 500, 
          solidHeader = TRUE, 
          status = "primary"),
      
      box(
        title = "Results",
        p("This tool uses forecasting techniques such as ARIMA, ETS, NNETAR, and TBATS to predict the area to be harvested in hectares."),
          
        htmlOutput("table"),
        br(),
        h3("Forecasts: equal weighted"),
        htmlOutput("forecasted_years"),
        height = NULL, solidHeader = TRUE, 
        status = "info",
        footer ="Accuracy is the result of 100-MAPE:"
      ),
    
      
      box(DTOutput("forecasts"), 
          downloadButton(
            outputId = "downloadButton",
            label = "Download Data"
          ),
          title = "Data", 
          height = NULL, 
          solidHeader = TRUE, 
          status = "success"),
    ), # end fluidrow
    
    tags$div(style = "position:fixed;bottom:0;width:100%;background-color:#f5f5f5;text-align:center;padding:5px;", "Developed by Marcelo Katogui")
    
  ), # end dashboardBody
  
) # end dashboardPage


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
    Year <- ((forecasts_data()%>%filter(Year %in% c(2021:2023)))$Year)
    Area_Harvested <- (forecasts_data()%>%filter(Year %in% c(2021:2023)))[,2]
    Area_Harvested_Forecasted <- (rowMeans((forecasts_data()%>%filter(Year %in% c(2021:2023)))[,3:6]))

    data.frame(
      Year = format(Year,digits = 1),
      cbind(Area_Harvested=format(Area_Harvested,big.mark = ","), Area_Harvested_Forecasted=format(Area_Harvested_Forecasted,big.mark = ","))
    )
  })

  output$forecasts <- renderDataTable({
    datatable(forecasts_data() %>% arrange(desc(Year)), rownames = FALSE)
  })
  
  output$downloadButton <- downloadHandler(
    filename = function() {paste("data-", Sys.Date(), ".xlsx", sep="")},
    content = function(file) {openxlsx::write.xlsx(forecasts_data(), file)}
  )
}

# Run the app
shinyApp(ui = ui, server = server)
