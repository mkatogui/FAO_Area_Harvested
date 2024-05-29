# Working with data from FAO

## Scraping FAO data and forecasting Area Harvested
## Project Description

This project involves scraping data from a website, followed by transforming and generating models to make predictions. The output is presented in a shiny app, which allows the user to interact with the data by selecting the desired country.

The project is divided into four scripts:

1. Script 1: Downloading Data
   - This script is responsible for downloading the required data.

2. Script 2: Transforming Data
   - This script takes the downloaded data and performs necessary transformations.

3. Script 3: Modeling and Forecasting
   - In this script, the transformed data is used to build models and generate forecasts.

4. Shiny App Integration
   - After completing the above steps, the output is saved as an RDS file and then loaded into a shiny app. The shiny app provides an interactive interface for the end user to explore the data by selecting a specific country.

By following these steps, users can easily scrape, transform, model, and visualize data using the provided shiny app.

To run the whole project please execute the ***main.R*** script.

Link to the Shiny app: https://m-katogui.shinyapps.io/forecasting_corn_area_harvested/

![image](https://github.com/mkatogui/FAO_Area_Harvested/assets/60530366/ce5cd784-ca41-4cf7-872c-d07f7a1ceb97)
