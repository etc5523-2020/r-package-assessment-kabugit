library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(lubridate)

india <- read.csv(here::here("data", "India_states.csv"))

india1 <- india %>% pivot_longer(cols = 3:41, names_to = "state", values_to = "value")

india2 <- india1 %>% pivot_wider(names_from = Status, values_from = value)

india2 <- india2 %>% rename(State_code = state)

isc <- read.csv(here::here("data", "India_state_codes.csv"))

india3 <- india2 %>% left_join(isc, by = "State_code")

US <- read.csv(here::here("data", "US_states.csv"))

brazil <- readxl::read_xlsx(here::here("data", "Brazil_states.xlsx"))

aus_cases <- read.csv(here::here("data", "Aus_cases.csv"))

aus_cases1 <- aus_cases %>% pivot_longer(cols = 2:9, names_to = "state", values_to = "new_cases")
aus_cases1 <- aus_cases1 %>% rename(date = `ï..Date`)

aus_deaths <- read.csv(here::here("data", "Aus_deaths.csv"))

aus_deaths1 <- aus_deaths %>% pivot_longer(cols = 2:9, names_to = "state", values_to = "new_deaths")
aus_deaths1 <- aus_deaths1 %>% rename(date = `ï..Date`)

aus_join <- aus_cases1 %>% left_join(aus_deaths1, by = c("date" = "date", "state" = "state"))

aus_join[is.na(aus_join)] = 0

aus_join$date <- paste0(aus_join$date, "-2020")

aus_join$date <- dmy(aus_join$date)
aus_join <- aus_join %>% rename(State_code = state)

aus_ll <- readxl::read_xlsx(here::here("data", "Aus_state_codes.xlsx"))
aus_ll <- aus_ll %>% rename(state = State)

aus_final <- aus_join %>% left_join(aus_ll, by = "State_code")

india3$Date <- dmy(india3$Date)

india_ll <- readxl::read_xlsx(here::here("data", "India_latlong.xlsx"))

india_final <- india3 %>% left_join(india_ll, by = "State")

india_final <- india3 %>% filter(State != "State Unassigned") %>% filter(State != "Total")

us_ll <- read.csv(here::here("data", "us_state_codes.csv"))

US <- US %>% rename(State_code = state)

us_final <- US %>% left_join(us_ll, by = "State_code")

us_final$submission_date <- mdy(us_final$submission_date)

us_final <- us_final[!is.na(us_final$Latitude), ]

brazil1 <- brazil %>% filter(state != "TOTAL")

brazil_ll <- readxl::read_xlsx(here::here("data", "Brazil_state_codes.xlsx"))

brazil1 <- brazil1 %>% rename(State_codes = state)

brazil2 <- brazil1 %>% left_join(brazil_ll, by = "State_codes")

brazil_final <- brazil2 %>% select(date, country, State_codes, newDeaths, newCases, totalCases, state)

brazil_final <- brazil_final %>% rename(State_code = State_codes)
brazil_final1 <- brazil_final %>% select(date, State_code, state, country, newCases, newDeaths)

aus_final$country <- "Australia"

aus_final <- aus_final %>% rename(newCases = new_cases, newDeaths = new_deaths)
aus_final1 <- aus_final %>% select(date, State_code, state, country, newCases, newDeaths)

us_final$country <- "USA"

us_final <- us_final %>% rename(date = submission_date, state = State, newCases = new_case, newDeaths = new_death)
us_final1 <- us_final %>% select(date, State_code, state, country, newCases, newDeaths)

india_final$country <- "India"

india_final <- india_final %>% rename(date = Date, newCases = Confirmed, newDeaths = Deceased, state = State)
india_final1 <- india_final %>% select(date, State_code, state, country, newCases, newDeaths)

final <- rbind(us_final1, india_final1, brazil_final1, aus_final1)

final <- final

brazil_states <- geojsonio::geojson_read("https://raw.githubusercontent.com/codeforamerica/click_that_hood/master/public/data/brazil-states.geojson", what = "sp")

brazil_states1 <- st_as_sf(brazil_states)

brazil_states1 <- brazil_states1 %>% rename(state = name)

brazil_states1 <- brazil_states1 %>% rename(State_code = sigla)

brazil_states2 <- brazil_states1 %>% select(State_code, geometry)

brazil_join <- brazil_final1 %>% left_join(brazil_states2, by = "State_code")

us_states <- geojsonio::geojson_read("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", what = "sp")

us_states1 <- st_as_sf(us_states)

us_states1 <- us_states1 %>% rename(state = name)

us_states2 <- us_states1 %>% select(state, geometry)

us_join <- us_final1 %>% left_join(us_states2, by = "state")

india_states <- geojsonio::geojson_read("https://raw.githubusercontent.com/geohacker/india/master/state/india_telengana.geojson", what = "sp")

india_states1 <- st_as_sf(india_states)

india_sort <- india_final1 %>% select(State_code, state)

india_sort1 <- india_sort %>% mutate(ID = row_number())

india_sort1 <- india_sort1 %>% filter(ID <= 36)

india_states2 <- india_states1 %>% rename(ID = ID_1)

india_sort3 <- india_sort1 %>% left_join(india_final1, by = "state")

india_states2 <- india_states2 %>% select(ID, geometry)

india_final2 <- india_sort3 %>% left_join(india_states2, by = "ID")

india_final2 <- india_final2 %>% select(date, State_code.y, state, country, newCases, newDeaths, geometry)
india_final2 <- india_final2 %>% rename(State_code = State_code.y)

australia_states <- geojsonio::geojson_read("https://raw.githubusercontent.com/rowanhogan/australian-states/master/states.geojson", what = "sp")

australia_states1 <- st_as_sf(australia_states)

australia_states1 <- australia_states1 %>% rename(state = STATE_NAME)

australia_states1 <- australia_states1 %>% select(state, geometry)

australia_join <- aus_final1 %>% left_join(australia_states1, by = "state")

final <- rbind(us_join, india_final2, brazil_join, australia_join)
final <- st_as_sf(final)

final <- final %>% filter(date <= "2020-09-25")

final$newCases <- str_remove_all(final$newCases, "-")
final$newCases <- as.double(final$newCases)

final$newDeaths <- str_remove_all(final$newDeaths, "-")
final$newDeaths <- as.double(final$newDeaths)


cd <- final

cd <- cd %>% mutate(month = substr(date, 6, 7))

cd <- cd %>% group_by(state, month) %>% mutate(m_case = sum(newCases)) %>% mutate(m_death = sum(newDeaths))

cd <- cd %>% mutate(cbd = (m_death/m_case)*100)

cd$cbd[is.nan(cd$cbd)]<-0

cd$cbd <- round(cd$cbd, 2)

cd <- cd %>% group_by(country, month) %>% mutate(countr_mcase = sum(newCases)) %>% mutate(countr_mdeath = sum(newDeaths))

cd <- cd %>% mutate(countr_cbd = (countr_mdeath/countr_mcase)*100)

cd$countr_cbd <- round(cd$countr_cbd, 2)

stringent <- read.csv(here::here("data", "covid-stringency-index.csv"))

stringent <- stringent %>% filter(Date >= "2020-01-22" & Date <= "2020-09-25")

stringent <- stringent %>% filter(Code %in% c("USA", "AUS", "BRA", "IND"))

stringent <- stringent %>% mutate(month = substr(Date, 6, 7)) %>%
    group_by(month, Entity) %>%
    mutate(m_string = mean(Stringency.Index..OxBSG.))

stringent$m_string <- round(stringent$m_string, 2)

stringent <- stringent %>% rename(country = Entity)

stringent <- stringent %>% select(country, month, m_string)

stringent$country[stringent$country == "United States"] <- "USA"

stringent <- distinct(stringent)
cd <- cd %>% left_join(stringent)

cd$month <- as.numeric(cd$month)

cd1 <- cd %>% select(month, country, state, m_case, m_death, m_string)
cd1 <- as.data.frame(cd1)
cd1 <- distinct(cd1)

cd2 <- cd %>% select(month, country, state, cbd)
cd2 <- as.data.frame(cd2)

# Lets create some slick functions!

widthFun <- function(width, box.width, command) {
    fluidRow(width = 6,
             box(width = NULL,
                 command))
}

inputFun <- function(id, label){
    selectInput(id, label, choices = unique(final$country), selected = unique(final$country)[1])
}

slideTime <- function(id, label) {
    sliderInput(id, label, min(final$date), max(final$date), max(final$date))
}

ui <-dashboardPage(
    dashboardHeader(title = h6("Countries most affected by COVID-19 and Australia")),
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName = "overview"),
            menuItem("New cases", tabName = "cases",
                     menuSubItem("User guide", tabName = "cases-h"),
                     menuSubItem("Analysis", tabName = "cases-a")),
            menuItem("New deaths", tabName = "deaths",
                     menuSubItem("User guide", tabName = "deaths-h"),
                     menuSubItem("Analysis", tabName = "deaths-a")),
            menuItem("Death rate and stringency", tabName = "cd",
                     menuSubItem("User guide", tabName = "drs-h"),
                     menuSubItem("Analysis", tabName = "drs-a"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "overview", h2("A quick overview"),
                    fluidRow(box(width = 11,
                                 h4("Author - Ketan Kabu"),
                                 br(),
                                 h4("This app was created to understand and share different angles of the pandemic, such as stringency index, death rate per case and to enjoy a palatable visualisation of a horrific pandemic. There is also a need in Australia's current political environment to understand and compare the nation's economic and public health situation to most direly affected countries - USA, India and Brazil. I sincerely hope you enjoy sifting through the tabs and playing around with the visualisation.")))
            ),
            tabItem(tabName = "cases-a", h2("Cases By Country"),
                    br(),
                    
                    widthFun(6, NULL, inputFun("countryselc", h4("Select country"))), #selectInput("countryselc", h4("Select country"), choices = unique(final$country), selected = unique(final$country)[1])), functioned inside "inputFun".
                    # fluidRow(width = 6,
                    #          box(width = NULL,
                    #              selectInput("countryselc", h4("Select country"), choices = unique(final$country), selected = unique(final$country)[1]))), functioned inside "widthFun".
                    widthFun(6, NULL, leafletOutput("plotcase")),
                    
                    # fluidRow(width = 6,
                    #          box(width = NULL,
                    #              leafletOutput("plotcase"))),
                    
                    widthFun(6, NULL, slideTime("timerc", "Time"))), #sliderInput("timerc", "Time", min(final$date), max(final$date), max(final$date)), functioned inside "slideTime".
            
            # fluidRow(width = 6,
            #          box(width = NULL,
            #              slideTime("timerc", "Time")))),
            #sliderInput("timerc", "Time", min(final$date), max(final$date), max(final$date))))),
            tabItem(tabName="cases-h", h3("Guidelines to use the Dashboard", style = "font-family:'Cambria';"),
                    
                    fluidRow(width = 12,
                             box(width = NULL,
                                 h4("Cases by country - Guide"),
                                 p("This section focuses on how new positive COVID 19 cases have progressed since January. The layout of this visualisation will help you understand the countries and states that have been worst hit, and also the ones that have recovered well over the last few months."),
                                 p("Some examples of questions that can be addressed through this section are:"),
                                 p("1. Which states in the United States have progressively worsened with time during the pandemic?"),
                                 p("2. Which states in the United States have recovered well after being badly hit?"),
                                 br(),
                                 p("To answer the first question, one would go about it the following way:"),
                                 p("1. Select USA from the drop down menu at the top."),
                                 p("2. Slide the time button at the bottom to view changes in the COVID 19 situation in the United States."),
                                 p("3. Hover your mouse over the map in the middle to see state names and case numbers for better understanding."),
                                 br(),
                                 img(src = here::here("data","New cases.png"), height = "500px", width = "100%")
                             ))),
            
            tabItem(tabName = "deaths-a", h2("Deaths By Country"),
                    br(),
                    widthFun(6, NULL, inputFun("countryseld", h4("Select country"))), #selectInput("countryseld", h4("Select country"), choices = unique(final$country), selected = unique(final$country)[1])),
                    
                    # fluidRow(width = 6,
                    #          box(width = NULL,
                    #              selectInput("countryseld", h4("Select country"), choices = unique(final$country), selected = unique(final$country)[1]))),
                    
                    widthFun(6, NULL, leafletOutput("plotdeath")),
                    
                    # fluidRow(width = 6,
                    #          box(width = NULL,
                    #              leafletOutput("plotdeath"))),
                    
                    widthFun(6, NULL, slideTime("timerd", "Time"))),
            
            # fluidRow(width = 6,
            #          box(width = NULL,
            #              slideTime("timerd", "Time")))), #Function to call sliderInput
            #sliderInput("timerd", "Time", min(final$date), max(final$date), max(final$date))))),
            tabItem(tabName="deaths-h", h3("Guidelines to use the Dashboard", style = "font-family:'Cambria';"),
                    fluidRow(width = 12,
                             box(width = NULL,
                                 h4("Deaths by country - Guide"),
                                 p("This section focuses on how deaths due to COVID 19 have trended since January. The layout of this visualisation will be similar to the previous tab, to get some uniformity and understaning the visualisation."),
                                 p("Some examples of questions that can be addressed through this section are:"),
                                 p("1. Which states in the Brazil have higher number of deaths with time during the pandemic?"),
                                 p("2. Which states in Brazil have managed to reduce death numbers after being badly hit?"),
                                 br(),
                                 p("To answer the first question, one would go about it the following way:"),
                                 p("1. Select Brazil from the drop down menu at the top."),
                                 p("2. Slide the time button at the bottom to view changes in COVID 19 deaths in the Brazil."),
                                 p("3. Hover your mouse over the map in the middle to see state names and case numbers for better understanding."),
                                 br(),
                                 img(src = "New deaths.png", height = "500px", width = "100%")
                             ))),
            
            tabItem(tabName = "drs-a", h2("Death Rate Per Case For Countries And States"),
                    br(),
                    
                    widthFun(6, NULL, plotlyOutput("plotcd")),
                    # fluidRow(width = 6,
                    #          box(width = NULL,
                    #              plotlyOutput("plotcd"))),
                    fluidRow(column(width = 6,
                                    box(width = NULL,
                                        DT::DTOutput("clicker"))),
                             column(width = 6,
                                    box(width = NULL,
                                        plotOutput("clicker2")))),
                    fluidRow(box(width = NULL,
                                 sliderInput("timerstr", "Slide for monthly change", min(cd$month), max(cd$month), max(cd$month))))),
            
            tabItem(tabName="drs-h", h3("Guidelines to use the Dashboard", style = "font-family:'Cambria';"),
                    fluidRow(width = 12,
                             box(width = NULL,
                                 h4("Deaths Rate and Stringency - Guide"),
                                 p("This section covers deaths % and government stringency index, and understanding how that has affected death numbers. It gives us a decent idea regarding the government's hammering down of restrictions and how their (or lack of their) effects have impacted the death toll in their country."),
                                 p("Some examples of questions that can be addressed through this section are:"),
                                 p("1. Which country has experienced the highest death to case ratio (or death rate%)?"),
                                 p("2. Which states have been hit the worst due to a drop in stringency index?"),
                                 br(),
                                 p("To answer these questions, one would go about it the following way:"),
                                 p("1. Select one of the countries (lines) from the graph at the top."),
                                 p("2. Slide the time button at the bottom to view changes in deaths with respect to stringency index (bottom left) and top states that have high death rate % for the country selected (bottom right)."),
                                 p("Please note that both bottom graphs are reactive to the country chosen from the top graph as well as the bottom time slider. Numbers are all monthly, with average and sums taken. As seen below in the first image, only the top graph is visible at first. Upon clicking on one of the country lines, the bottom two visualisations are then visible."),
                                 p("For your reference - table headers:"),
                                 p("1. m_case - Monthly cases"),
                                 p("2. m_death - Monthly deaths"),
                                 p("3. m_string - Stringency index in that month in that country"),
                                 br(),
                                 img(src = "Death rate null.png", height = "500px", width = "100%"),
                             )))
            
        )))

server <- function(input, output) {
    output$plotcase <- renderLeaflet({
        final_filt <- final %>% 
            filter(country == input$countryselc & date == input$timerc)
        
        bins <- c(0, 50, 100, 200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000, Inf)
        beaut <- colorBin("YlOrRd", domain = final_filt$newCases, bins = bins)
        
        b <- leaflet(final_filt) %>%
            setView(-52, -15, 3.5) %>%
            addTiles("MapBox")
        
        
        u <- leaflet(final_filt) %>% 
            setView(-110, 52.8, 2.5) %>% 
            addTiles("MapBox")
        
        
        i <- leaflet(final_filt) %>% 
            setView(79, 21, 4.4) %>% 
            addTiles("MapBox")
        
        
        a <- leaflet(final_filt) %>% 
            setView(134, -27, 4) %>% 
            addTiles("MapBox")
        
        leafSel <- function(country){
            country = input$countryselc
            if(country == "USA") {
                return(u)
            }
            if(country == "Brazil"){
                return(b)
            }
            if(country == "India"){
                return(i)
            }
            if(country == "Australia"){
                return(a)
            }
        }
        
        labels <- sprintf("<strong>%s</strong><br/>%g new cases",
                          final_filt$state, final_filt$newCases) %>% lapply(htmltools::HTML)
        
        pcase <-  leafSel(input$countryselc) %>%
            addPolygons(fillColor = ~beaut(final_filt$newCases),
                        weight = 2,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 1,
                        highlight = highlightOptions(
                            weight = 5,
                            color = "#600",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE),
                        label = labels)%>% addLegend(pal = beaut, values = ~newCases, opacity = 0.7, title = NULL,
                                                     position = "topright")
    })
    output$plotdeath <- renderLeaflet({
        final_filt <- final %>% 
            filter(country == input$countryseld & date == input$timerd)
        
        bins <- c(0, 10,20,30,40,50,75,100,125,150,175,200, Inf)
        beaut <- colorBin("YlOrRd", domain = final_filt$newDeaths, bins = bins)
        
        b <- leaflet(final_filt) %>%
            setView(-52, -15, 3.5) %>%
            addTiles("MapBox")
        
        
        u <- leaflet(final_filt) %>% 
            setView(-110, 52.8, 2.5) %>% 
            addTiles("MapBox")
        
        
        i <- leaflet(final_filt) %>% 
            setView(79, 21, 4.4) %>% 
            addTiles("MapBox")
        
        
        a <- leaflet(final_filt) %>% 
            setView(134, -27, 4) %>% 
            addTiles("MapBox")
        
        leafSel <- function(country){
            country = input$countryseld
            if(country == "USA") {
                return(u)
            }
            if(country == "Brazil"){
                return(b)
            }
            if(country == "India"){
                return(i)
            }
            if(country == "Australia"){
                return(a)
            }
        }
        
        labels <- sprintf("<strong>%s</strong><br/>%g new deaths",
                          final_filt$state, final_filt$newDeaths) %>% lapply(htmltools::HTML)
        
        pcase <-  leafSel(input$countryseld) %>%
            addPolygons(fillColor = ~beaut(final_filt$newDeaths),
                        weight = 2,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 1,
                        highlight = highlightOptions(
                            weight = 5,
                            color = "#600",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE),
                        label = labels) %>% addLegend(pal = beaut, values = ~newDeaths, opacity = 0.7, title = NULL,
                                                      position = "topright")
    })
    output$plotcd <- renderPlotly({
        plota <- cd %>%
            ggplot(aes(x = as.factor(month), y = countr_cbd, text = paste0("Country: ", country, ",  Death rate: ", cbd, "  Month: ", month))) +
            geom_line(aes(color = country, group = country)) +
            labs(x = "Months",
                 y = "Death rate per monthly cases") +
            theme_bw() +
            theme(legend.title = element_blank())
        
        ggplotly(plota, tooltip = "text")
    })
    
    eventFun <- function(df){
        eve <- event_data("plotly_click")
        if (is.null(eve)) return(NULL)
        
        if(eve$curveNumber == 0){
            temp <<- df %>% filter(country == "Australia")
        }
        if(eve$curveNumber == 1){
            temp <<- df %>% filter(country == "Brazil")
        }
        if(eve$curveNumber == 2){
            temp <<- df %>% filter(country =="India")
        }
        if(eve$curveNumber == 3){
            temp <<- df %>% filter(country =="USA")
        }
    }
    
    output$clicker <- DT::renderDataTable({
        eventFun(cd1)
        
        # eve <- event_data("plotly_click")
        # if (is.null(eve)) return(NULL)
        # 
        # if(eve$curveNumber == 0){
        #     temp <- cd1 %>% filter(country == "Australia")
        # }
        # if(eve$curveNumber == 1){
        #     temp <- cd1 %>% filter(country == "Brazil")
        # }
        # if(eve$curveNumber == 2){
        #     temp <- cd1 %>% filter(country =="India")
        # }
        # if(eve$curveNumber == 3){
        #     temp <- cd1 %>% filter(country =="USA")
        # }
        # Above commented code functioned into "eventFun()"
        
        temp %>%
            filter(month == input$timerstr) %>% 
            select(state, m_case, m_death, m_string) %>% 
            arrange(desc(m_death))
        
        
    })
    output$clicker2 <- renderPlot({
        eventFun(cd2)
        
        # eve <- event_data("plotly_click")
        # if (is.null(eve)) return(NULL)
        # 
        # if(eve$curveNumber == 0){
        #     temp <- cd2 %>% filter(country == "Australia")
        # }
        # if(eve$curveNumber == 1){
        #     temp <- cd2 %>% filter(country == "Brazil")
        # }
        # if(eve$curveNumber == 2){
        #     temp <- cd2 %>% filter(country =="India")
        # }
        # if(eve$curveNumber == 3){
        #     temp <- cd2 %>% filter(country =="USA")
        # }
        
        temp %>%
            arrange(desc(cbd)) %>% 
            head(10) %>% 
            filter(month == input$timerstr) %>% 
            ggplot(aes(x = state, y = cbd)) + 
            geom_bar(stat = "identity") +
            coord_flip() +
            labs(x = "Death rate",
                 y = "State") +
            theme_bw()
        
    })
    
}

shinyApp(ui = ui, server = server)