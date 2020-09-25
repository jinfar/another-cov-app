#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


require(maps)
library(tidyverse)
library(lubridate)
library(shiny)
library(plotly)
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
    summarise(lat = mean(lat, na.rm = TRUE), 
              long = mean(long, na.rm = TRUE), 
              cases = sum(cases,na.rm = TRUE), 
              death = sum(death, na.rm = TRUE), 
              recover = sum(recover, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(razcases = cases - lag(cases),
           razdeath = death - lag(death),
           razrecover = recover - lag(recover)) %>%
    mutate_at(vars(matches("^raz")), ~if_else(is.na(.)|.<0,0,.)) %>% 
    mutate(raz2cases = razcases - lag(razcases),
           raz2death = razdeath - lag(razdeath),
           raz2recover = razrecover - lag(razrecover)) %>%
    mutate_at(vars(matches("^raz2")), ~if_else(is.na(.),0,.)) %>%
    mutate_at(vars(matches("^raz2")), ~if_else(date == "2020-01-22"&.<0,0,.)) %>% 
    mutate(cases_skol = (razcases+lag(razcases, 1)+lag(razcases, 2)+lag(razcases, 3)+lag(razcases, 4)+lag(razcases, 5))/6) %>% 
    mutate(cases2_skol = (raz2cases+lag(raz2cases, 1)+lag(raz2cases, 2)+lag(raz2cases, 3)+lag(raz2cases, 4)+lag(raz2cases, 5))/6) %>% 
    mutate(country = if_else(country == "us", "usa", country)) %>% 
    filter(date >= "2020-03-01") %>% 
    mutate(country = if_else(country == "united kingdom", "uk", country)) -> data

data %>% 
    group_by(date) %>% 
    summarise_if(is.double, ~sum(., na.rm = TRUE)) %>% 
    mutate(country = "world") %>% 
    mutate(lat = 0) %>% 
    mutate(long = 0) %>% 
    select(country, everything()) %>% 
    rbind(data) -> data
    

m <- 14

data %>% 
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
    right_join(map_data('world') %>% mutate(country = if_else(region == "Antarctica", "world", str_to_lower(region)))) %>% 
    select(-region, -subregion)-> datamap


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Covid19 cases by country"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 1,
            selectInput("country", "Country:",
                        choices = data$country %>% unique()
                        ),
            selectInput("stat", "Statistics:",
                        choices = c("cases", "death", "recover")
                        )
        ),
            # Show a plot of the generated distribution
            mainPanel("Overall", width = 11,
                      fluidRow(splitLayout(plotlyOutput("distPlot"),
                                           plotlyOutput("distPlot2"),
                                           plotlyOutput("distPlot5"))),
                      fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                           plotlyOutput("distPlot3"),
                                           plotlyOutput("distPlot4")))
                      
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlotly({
        
        data %>% 
            filter(country == input$country) %>% 
            ggplot(aes(x = date, 
                       y = case_when(input$stat == "death" ~ death,
                                     input$stat == "recover" ~ recover,
                                     TRUE ~ cases),
                       text = case_when(input$stat == "death" ~ paste0("Smerti:", death),
                                        input$stat == "recover" ~ paste0("Vizdrovivshie:", recover),
                                        TRUE ~ paste0("Sluchaev zarazheniya:", cases))))+
            labs(x = "time", y = input$stat)+
            geom_smooth(alpha = 0.5)+
            geom_point(show.legend = F)+
            theme_minimal()-> p
        ggplotly(p, tooltip = c("x", "text"))
            # geom_smooth()+
            # scale_y_log10()
        
        # draw the histogram with the specified number of bins
        
    })
    output$distPlot2 <- renderPlotly({
        
        data %>% 
            filter(country == input$country) %>% 
            ggplot(aes(x = date, 
                       y = case_when(input$stat == "death" ~ razdeath,
                                     input$stat == "recover" ~ razrecover,
                                     TRUE ~ razcases),
                       text = case_when(input$stat == "death" ~ paste0("Smerti:", razdeath),
                                        input$stat == "recover" ~ paste0("Vizdrovivshie:", razrecover),
                                        TRUE ~ paste0("Sluchaev zarazheniya:", razcases))))+
            labs(title = "new cases", x = "time", y = input$stat)+
            geom_smooth(alpha = 0.5)+
            geom_col(show.legend = F)+
            theme_minimal()-> m1
        ggplotly(m1, tooltip = c("x", "text"))
        
        
    })
    output$distPlot5 <- renderPlotly({
        
        data %>% 
            filter(country == input$country) %>% 
            ggplot(aes(x = date, 
                       y = case_when(input$stat == "death" ~ raz2death,
                                     input$stat == "recover" ~ raz2recover,
                                     TRUE ~ raz2cases),
                       text = case_when(input$stat == "death" ~ paste0("Smerti:", raz2death),
                                        input$stat == "recover" ~ paste0("Vizdrovivshie:", raz2recover),
                                        TRUE ~ paste0("Sluchaev zarazheniya:", raz2cases))))+
            labs(title = "changes in new cases", x = "time", y = input$stat)+
            geom_smooth(alpha = 0.5)+
            geom_col(show.legend = F)+
            theme_minimal()-> m2
        ggplotly(m2, tooltip = c("x", "text"))
        
        
    })
    
    output$distPlot3 <- renderPlotly({
        
        
        datamap %>% 
            ggplot(aes(x = long, y = lat, group = group, 
                       fill = cases2_skol, text = paste0("Ozhidanie:", cases2_skol))) +
            geom_polygon(colour = "white", show.legend = F)+
            labs(title  = "avarage of changes in new cases for last 6 days")+
            scale_fill_gradientn(
                colours = c( "darkred", "red", "white", "green", "darkgreen" ),
                values = c( 1, perevod(20, datamap$cases2_skol), perevod(0, datamap$cases2_skol), perevod(-20, datamap$cases2_skol), 0)) -> n
        ggplotly(n, tooltip = "text")
        
        
    })
    output$distPlot4 <- renderPlotly({
        
        
        datamap %>% 
            ggplot(aes(x = long, y = lat, group = group, fill = estimate,
                       text = paste0("Ozhidanie:", estimate))) +
            geom_polygon(colour = "white", show.legend = F)+
            labs(title  = "estimate of a slope of linreg for 14 last days")+
            scale_fill_gradientn(
                colours = c( "darkred", "red", "white", "green", "darkgreen" ),
                values = c( 1, perevod(20, datamap$estimate), perevod(0, datamap$estimate), perevod(-20, datamap$estimate), 0)) -> n
        ggplotly(n, tooltip = "text")
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
