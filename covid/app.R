#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(tidyverse)
library(lubridate)
link <- c("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
link %>% map(read_csv)->data
rm(link)

perevod <- function(var, vec) {
    (var-min(vec, na.rm = T))/(max(vec, na.rm = T)-min(vec, na.rm = T))
}

data[[1]] %>% 
    pivot_longer(matches("^\\d"), values_to = "cases") %>% 
    left_join(data[[2]] %>% pivot_longer(matches("^\\d"), values_to = "death")) %>% 
    left_join(data[[3]] %>% pivot_longer(matches("^\\d"), values_to = "recover")) %>%
    rename("date" = "name") %>% 
    mutate(date = mdy(date)) %>% 
    janitor::clean_names() %>% 
    mutate(country = str_to_lower(country_region)) %>% 
    group_by(country,date) %>% 
    summarise(lat = mean(lat), 
              long = mean(long), 
              cases = sum(cases), 
              death = sum(death), 
              recover = sum(recover)) %>% 
    ungroup() %>% 
    mutate(razcases = cases - lag(cases),
           razdeath = death - lag(death),
           razrecover = recover - lag(recover)) %>%
    mutate_at(vars(matches("^raz")), ~if_else(is.na(.)|.<0,0,.)) %>% 
    mutate(raz2cases = razcases - lag(razcases),
           raz2death = razdeath - lag(razdeath),
           raz2recover = razrecover - lag(razrecover)) %>%
    mutate_at(vars(matches("^raz")), ~if_else(is.na(.),0,.)) -> data

m <- 14

data %>% 
    mutate(country = if_else(country == "us", "usa", country)) %>% 
    mutate(country = if_else(country == "united kingdom", "uk", country)) %>% 
    mutate(cases_skol = (razcases+lag(razcases, 1)+lag(razcases, 2)+lag(razcases, 3)+lag(razcases, 4)+lag(razcases, 5))/6) %>% 
    mutate(cases2_skol = (raz2cases+lag(raz2cases, 1)+lag(raz2cases, 2)+lag(raz2cases, 3)+lag(raz2cases, 4)+lag(raz2cases, 5))/6) %>% 
    filter(between(date, today("GMT")-(m+1), today("GMT")-1)) %>% 
    group_by(country) %>% 
    summarise(casesmod = lm(cases_skol~date) %>% broom::tidy(conf.int = T) %>% list(),
              cases_skol = cases_skol[n()],
              cases2_skol = cases2_skol[n()],
              cases = cases[n()]) %>% 
    unnest(casesmod) %>% 
    filter(term == "date") %>% 
    mutate(estimate = round(estimate, 1)) %>% 
    mutate(cases2_skol = round(cases2_skol, 1)) %>% 
    right_join(map_data("world") %>% 
    mutate(country =str_to_lower(region))) -> datamap

library(shiny)
library(plotly)
library(maps)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Covid19 cases by country"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("country", "Country:",
                        choices = data$country %>% unique()
                        ),
            selectInput("stat", "Statistics:",
                        choices = c("cases", "death", "recover")
                        )
        ),
            # Show a plot of the generated distribution
            mainPanel("Overall",
                      fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                           plotlyOutput("distPlot"),
                                           plotlyOutput("distPlot2"))),
                      fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                           plotlyOutput("distPlot3"),
                                           plotlyOutput("distPlot4")))
                      
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlotly({
        # generate bins based on input$bins from ui.R
        
        data %>% 
            filter(country == input$country) %>% 
            ggplot(aes(x = date, y = case_when(input$stat == "death" ~ death,
                                               input$stat == "recover" ~ recover,
                                               TRUE ~ cases)))+
            labs(x = "time", y = input$stat)+
            geom_smooth(alpha = 0.5)+
            geom_point(show.legend = F)+
            theme_minimal()-> p
        ggplotly(p)
            # geom_smooth()+
            # scale_y_log10()
        
        # draw the histogram with the specified number of bins
        
    })
    output$distPlot2 <- renderPlotly({
        
        data %>% 
            filter(country == input$country) %>% 
            ggplot(aes(x = date, y = case_when(input$stat == "death" ~ razdeath,
                                               input$stat == "recover" ~ razrecover,
                                               TRUE ~ razcases)))+
            labs(title = "new cases", x = "time", y = input$stat)+
            geom_smooth(alpha = 0.5)+
            geom_col(show.legend = F)+
            theme_minimal()-> m
        ggplotly(m)
        
        
    })
    output$distPlot3 <- renderPlotly({
        
        
        datamap %>% 
            ggplot(aes(x = long, y = lat, group = group, fill = cases2_skol)) +
            geom_polygon(colour = "white", show.legend = F)+
            scale_fill_gradientn(
                colours = c( "darkred", "red", "white", "green", "darkgreen" ),
                values = c( 1, perevod(20, datamap$cases2_skol), perevod(0, datamap$cases2_skol), perevod(-20, datamap$cases2_skol), 0)) -> n
        ggplotly(n)
        
        
    })
    output$distPlot4 <- renderPlotly({
        
        
        datamap %>% 
            ggplot(aes(x = long, y = lat, group = group, fill = estimate)) +
            geom_polygon(colour = "white", show.legend = F)+
            scale_fill_gradientn(
                colours = c( "darkred", "red", "white", "green", "darkgreen" ),
                values = c( 1, perevod(20, datamap$estimate), perevod(0, datamap$estimate), perevod(-20, datamap$estimate), 0)) -> n
        ggplotly(n)
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
