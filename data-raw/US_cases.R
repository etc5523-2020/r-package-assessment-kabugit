library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(lubridate)

us_state_codes <- system.file("ext", "us_state_codes.csv", package="covtrack")
US_states <- system.file("ext", "US_states.csv", package="covtrack")

US <- read.csv(US_states)

# us_ll <- read.csv(here::here("data", "us_state_codes.csv"))
us_ll <- read.csv(us_state_codes)


US <- US %>% rename(State_code = state)

us_final <- US %>% left_join(us_ll, by = "State_code")

us_final$submission_date <- mdy(us_final$submission_date)

us_final <- us_final[!is.na(us_final$Latitude), ]

us_final$country <- "USA"

us_final <- us_final %>% rename(date = submission_date, state = State, newCases = new_case, newDeaths = new_death)
us_final1 <- us_final %>% select(date, State_code, state, country, newCases, newDeaths)

us_states <- geojsonio::geojson_read("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", what = "sp")

us_states1 <- st_as_sf(us_states)

us_states1 <- us_states1 %>% rename(state = name)

us_states2 <- us_states1 %>% select(state, geometry)

us_join <- us_final1 %>% left_join(us_states2, by = "state")

usethis::use_data(us_final1, overwrite = TRUE)
