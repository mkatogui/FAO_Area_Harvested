library(dplyr)
library(stringr)
library(tidyverse)
library(purrr)
library(forecast)
library(ggplot2)

# data is located
DATA_EXTRACTED <- "../DATA/EXTRACTED/"

# Output
DATA_OUTPUT <- "../DATA/OUTPUT"
dir.create(DATA_OUTPUT,recursive = T)

# files to read
files <- paste0(DATA_EXTRACTED,list.files(DATA_EXTRACTED))
nav_list <- list.files(DATA_EXTRACTED)[grepl("Production_Crops",list.files(DATA_EXTRACTED))]

# loading the files
data <- lapply(files[(grepl("Production_Crops",files))],read.csv)
names(data) <- nav_list

# Exploring headers
lapply(data,head)

input1 <- "corn"

# Looking for similarities
library(stringi)

Show_Filtered <- (data$Production_Crops_Livestock_E_ItemCodes %>% 
                    filter(stri_detect_fixed(Item, input1, opts_fixed = list(case_insensitive = TRUE))))


input2 <- 2 # the list to select is from Show_Filtered[,1]
selected_code <- Show_Filtered[input2,1]

       
Crops <- (data$`Production_Crops_Livestock_E_All_Data_(Normalized).csv` %>% 
            filter(Item.Code == selected_code))

Crops$Unit <- str_replace(Crops$Unit,pattern = "/",replacement = "_")
Crops$Element <- str_replace(Crops$Element,pattern = " ",replacement = "_")

Crops <- Crops %>% filter(Area %in%  (Crops %>% 
                                        group_by(Area,Element) %>% 
                                        summarise(N=n_distinct(Year)) %>% 
                                        select(Area,N) %>% 
                                        unique() %>% 
                                        filter(N>10))$Area)

# Create a list of time series objects by country
Crops_by_Country <- map(unique(Crops$Area), function(i) {
  ts_serie <- Crops %>% 
    filter(Area == i) %>% 
    select(Element, Unit, Year, Value) %>% 
    pivot_wider(names_from = c(Element, Unit), values_from = Value) %>% 
    arrange(Year)
  ts(ts_serie[,-1], start = c(ts_serie[1,1], 1), frequency = 1)
})


# deleting directories ----------------------------------------------------
unlink("DATA/EXTRACTED/", recursive = TRUE)

# Forecasting -------------------------------------------------------------

# Define a function to fit various time series models to a time series
fit_models <- function(ts_obj) {
  # Fit ARIMA model
  arima_model <- auto.arima(y = ts_obj[, "Area_harvested_ha"]#, 
                            # approximation = F,
                            # stepwise = F
                            )
  
  # Fit ETS model
  ets_model <- ets(ts_obj[, "Area_harvested_ha"],na.action="na.contiguous")
  
  # Fit tbats model
  tbats_model <- tbats(ts_obj[, "Area_harvested_ha"])
  
  # Fit nnetar model
  nnetar_model <- nnetar(ts_obj[, "Area_harvested_ha"])
  
  # Return a list containing all the models
  return(list(arima_model = arima_model,
              ets_model = ets_model,
              tbats_model = tbats_model,
              nnetar_model = nnetar_model))
}

# Apply the fit_models function to each time series in Crops_by_Country
models_list <- map(Crops_by_Country, fit_models)

# Generate forecast plots for each model and time series
forecasts_models <- map(models_list, function(model_list) {
  # Extract the last year of the original time series
  real_data <- tail(time(model_list$arima_model$x), 1)
  
  # Generate forecasts for each model
  arima_forecast <- append(model_list$arima_model$fitted,forecast(model_list$arima_model, h = 10)$mean)
  ets_forecast <-  append(model_list$ets_model$fitted,forecast(model_list$ets_model, h = 10)$mean)
  tbats_forecast <-  append(model_list$tbats_model$fitted,forecast(model_list$tbats_model, h = 10)$mean)
  nnetar_forecast <-  append(model_list$nnetar_model$fitted,forecast(model_list$nnetar_model, h = 10)$mean)
  
  # Combine the forecasts into a list
  forecast_list <- 
    ts.union(ts(
    data.frame(
        real_data = c(model_list$arima_model$x,rep(NA,10)),
        arima_forecast=arima_forecast,
        ets_forecast=ets_forecast, 
        tbats_forecast=tbats_forecast,
        nnetar_forecast=nnetar_forecast
        ),
    start = start(model_list$arima_model$x)[1],frequency=1))
  }
  )


# Countries forecasted
Countries <- unique(Crops$Area)
Countries[which(Countries=="T\xfcrkiye")] <- "Turkey"
Countries[which(Countries=="C\xf4te d'Ivoire")] <- "Ivory Coast"
Countries[which(Countries=="R\xe9union")] <- "Reunion"

# Renaming objects
names(forecasts_models) <- Countries
names(models_list) <- Countries


# Files for output
#Sys.setlocale(category = "LC_ALL", locale = "UTF-8") 
write_rds(Countries,paste0(DATA_OUTPUT,"/Countries.rds"))
write_rds(forecasts_models,paste0(DATA_OUTPUT,"/forecasts_models.rds"))
write_rds(models_list,paste0(DATA_OUTPUT,"/models_list.rds"))

