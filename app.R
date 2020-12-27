# Title: Final Map
# Name: Ben Edmonds

if (!require(albersusa)) install.packages('alberusa')
if (!require(plotly)) install.packages('plotly')
if (!require(shiny)) install.packages('shiny')
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
library(albersusa)
library(plotly)
library(shiny)
library(readxl)

my_map_theme <- function(){
    theme(panel.background=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank())
}

us_states <- usa_sf("laea")

us_states_covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", col_types = cols(date = col_date(format = "%Y-%m-%d")))

state_pop_millions <- read_excel("statistic_id183497_population-in-the-states-of-the-us-2019.xlsx", sheet = "Data", col_names = FALSE, skip = 5)

state_pop_millions <- state_pop_millions %>%
    rename(`state` = ...1,
           `population_millions` = ...2)

covid_data <- us_states_covid %>%
    left_join(state_pop_millions, c("state" = "state")) %>%
    mutate(case_rate = (cases/population_millions)) %>%
    mutate(death_rate = (deaths/population_millions))

fullmap <- function(myrate, mydate){
    if(myrate == "Cases"){
        testday <- covid_data %>%
            filter(date == as.Date(mydate))
        
        map <- us_states %>%
            left_join(testday, c("name"="state"))
        
        covid_map <- ggplot(map) +
            geom_sf(aes(fill=case_rate)) +
            scale_fill_continuous(low="yellow", high ="red", name = "cases per million people") 
        
        covid_map2 <- map %>%
            mutate(text = paste("<b>",name,"</b>\n", round(case_rate, digits = 2))) %>%
            ggplot() +
            geom_sf(aes(fill=case_rate, text=text), color="black") +
            scale_fill_continuous(low="yellow", high="red", name = "Cases per Million") +
            my_map_theme()
        
        
        covid_map3 <- ggplotly(covid_map2,tooltip = "text") %>%
            style(hoveron = "fills")
        
        covid_map3
    }
    else {
        testday <- covid_data %>%
            filter(date == as.Date(mydate))
        
        map <- us_states %>%
            left_join(testday, c("name"="state"))
        
        covid_map <- ggplot(map) +
            geom_sf(aes(fill=death_rate)) +
            scale_fill_continuous(low="yellow", high ="red", name = "Deaths per Million")
        
        covid_map2 <- map %>%
            mutate(text = paste("<b>",name,"</b>\n", round(death_rate, digits = 2))) %>%
            ggplot() +
            geom_sf(aes(fill=death_rate, text=text), color="black") +
            scale_fill_continuous(low="yellow", high="red", name = "Deaths per Million") +
            my_map_theme()
        
        
        covid_map3 <- ggplotly(covid_map2,tooltip = "text") %>%
            style(hoveron = "fills")
        
        covid_map3
    }
}

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("US States Covid Map"),
    

    # Sidebar with a slider input for date
    sidebarLayout(
        sidebarPanel(
            selectInput("choosenstat",
                        "Select a statistic to display:",
                        choices = list("Cases" = "Cases",
                                       "Deaths" = "Deaths"), 
                        selected = "Cases"),
            sliderInput("choosendate",
                        "Select a date to display:",
                        min = as.Date("2020-01-21","%Y-%m-%d"),
                        value = as.Date("2020-11-01","%Y-%m-%d"),
                        max = as.Date(format(Sys.Date()-2,"%Y-%m-%d")),
                        timeFormat = "%m-%d")
        ),

        # Show map
        mainPanel(
            h3(textOutput("TitleText")),
           plotlyOutput("map"),
           h5("Data source:", 
              tags$a(href="https://github.com/nytimes/covid-19-data", 
                     "Coronavirus (Covid-19) Data in the United States"))
        )
    )
)

# Define server logic required to draw map
server <- function(input, output) {
    
    output$TitleText <- renderText(paste("Cumulative Covid ",input$choosenstat, "per Million as of ", input$choosendate))

    output$map <- renderPlotly({
       fullmap(input$choosenstat,input$choosendate)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
