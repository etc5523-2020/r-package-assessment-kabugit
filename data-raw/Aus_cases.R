library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(lubridate)

Aus_cases <- system.file("ext", "Aus_cases.csv", package="covtrack")
Aus_deaths <- system.file("ext", "Aus_deaths.csv", package="covtrack")
Aus_deaths <- system.file("ext", "Aus_state_codes.xlsx", package="covtrack")
Aus_deaths <- system.file("ext", "Aus_deaths.csv", package="covtrack")
Aus_state_codes <- system.file("ext", "Aus_state_codes.xlsx", package="covtrack")

# aus_cases <- read.csv(here::here("data", "Aus_cases.csv"))
aus_cases <- read.csv(Aus_cases)

aus_cases1 <- aus_cases %>% pivot_longer(cols = 2:9, names_to = "state", values_to = "new_cases")
aus_cases1 <- aus_cases1 %>% rename(date = `ï..Date`)

# aus_deaths <- read.csv(here::here("data", "Aus_deaths.csv"))
aus_deaths <- read.csv(Aus_deaths)

aus_deaths1 <- aus_deaths %>% pivot_longer(cols = 2:9, names_to = "state", values_to = "new_deaths")
aus_deaths1 <- aus_deaths1 %>% rename(date = `ï..Date`)

aus_join <- aus_cases1 %>% left_join(aus_deaths1, by = c("date" = "date", "state" = "state"))

aus_join[is.na(aus_join)] = 0

aus_join$date <- paste0(aus_join$date, "-2020")

aus_join$date <- dmy(aus_join$date)
aus_join <- aus_join %>% rename(State_code = state)

# aus_ll <- readxl::read_xlsx(here::here("data", "Aus_state_codes.xlsx"))
aus_ll <- readxl::read_xlsx(Aus_state_codes)

aus_ll <- aus_ll %>% rename(state = State)

aus_final <- aus_join %>% left_join(aus_ll, by = "State_code")

aus_final$country <- "Australia"

aus_final <- aus_final %>% rename(newCases = new_cases, newDeaths = new_deaths)
aus_final1 <- aus_final %>% select(date, State_code, state, country, newCases, newDeaths)

australia_states <- geojsonio::geojson_read("https://raw.githubusercontent.com/rowanhogan/australian-states/master/states.geojson", what = "sp")

australia_states1 <- st_as_sf(australia_states)

australia_states1 <- australia_states1 %>% rename(state = STATE_NAME)

australia_states1 <- australia_states1 %>% select(state, geometry)

australia_join <- aus_final1 %>% left_join(australia_states1, by = "state")

usethis::use_data(aus_final1, overwrite = TRUE)
