#' TODO this will not work without a bit of organisation of the underlying 
#' functions used. The most repeatable way would be to make the generation of 
#' data and  plotting be part of a package which then could be loaded.

#generate master data once
library(JHCovidHelpers)
library(tidyverse)
library(lubridate)
library(plotly)
options(digits = 3)   # report 3 significant digits
options(scipen = -2)

#get data from web
master_data <- getDataFromWeb(#destfolder = "./Data"
                              )

#TODO the population adding seemt not to happen!
master_data <- master_data %>%
  addPopulationData()

master_data <- master_data %>%
  addCalculatedVariables()

#list of unique countries to pick from
countries_unique <- unique(master_data$Country_Region)
countries_start_selection <- c("France","United Kingdom","Germany","Spain","Italy")

#list of variables to pick from
#Totals
var_unique_totals <- c(
  "Deaths", "Weighted_Deaths",
  "Confirmed", "Weighted_Confirmed",
  "Recovered", 
  "Doses_admin", 
  "People_partially_vaccinated","People_fully_vaccinated", "People_fully_vaccinated_Perc", "People_partially_vaccinated_Perc", 
  "Population"
)
#Increases
var_unique_increases <- c("Increase_Deaths", "Increase_Deaths_Avg", "Increase_Deaths_Avg_Avg",
                          "Increase_Weighted_Deaths","Increase_Weighted_Deaths_Avg", "Increase_Weighted_Deaths_Avg_Avg", 
                          "Increase_Confirmed", "Increase_Confirmed_Avg", "Increase_Confirmed_Avg_Avg",
                          "Increase_Weighted_Confirmed", "Increase_Weighted_Confirmed_Avg","Increase_Weighted_Confirmed_Avg_Avg", 
                          "Increase_Increase_Confirmed","Increase_Increase_Deaths", "Rate_Increase_Deaths", "Rate_Increase_Confirmed"
)

var_unique <- c(var_unique_totals,var_unique_increases)
var_start_selection <- c("Increase_Deaths_Avg_Avg", "Increase_Confirmed_Avg_Avg")

#min and max dates availablw
minDate <- min(master_data$Date)
maxDate <- max(master_data$Date)
startDate_start_selection <- maxDate - lubridate::dmonths(3)

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Covid Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId = "SelectedCountries",
                           choices = countries_unique,
                           selected = countries_start_selection,
                           label = "Select Countries",
                           multiple = TRUE
                           ) ,
            selectizeInput(inputId = "SelectedVariables",
                           choices = var_unique,
                           selected = var_start_selection,
                           label = "Select Variables",
                           multiple = TRUE),
            
            dateRangeInput(inputId = "SelectedDates",
                           label = "Select Date range:",
                           min = minDate,
                           max = maxDate,
                           start = startDate_start_selection,
                           end = maxDate)
        ),

        
        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput(outputId = "covidPlot",height = "600px" #"auto"
                        )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$covidPlot <- renderPlotly({
      theplot <- JHGetplot_CountryLevel_MultipleVar(
        JH_Data = (master_data %>%
                     filter(Date >= input$SelectedDates[1] & Date <= input$SelectedDates[2])),
        CountryList = input$SelectedCountries,
        VarNames = input$SelectedVariables)
      
      # 
      # theplot <- JHGetplot_CountryLevel(
      #   JH_Data = (master_data %>%
      #                filter(Date >= input$SelectedDates[1] & Date <= input$SelectedDates[2])),
      #   CountryList = input$SelectedCountries,
      #   VarName = input$SelectedVariables[1])
      ggplotly(theplot,dynamicTicks = TRUE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
