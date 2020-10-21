library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(lubridate)

Brazil_state_codes <- system.file("ext", "Brazil_state_codes.xlsx", package="covtrack")
Brazil_states <- system.file("ext", "Brazil_states.xlsx", package="covtrack")

brazil <- readxl::read_xlsx(Brazil_states)

brazil1 <- brazil %>% filter(state != "TOTAL")

# brazil_ll <- readxl::read_xlsx(here::here("data", "Brazil_state_codes.xlsx"))
brazil_ll <- readxl::read_xlsx(Brazil_state_codes)

brazil1 <- brazil1 %>% rename(State_codes = state)

brazil2 <- brazil1 %>% left_join(brazil_ll, by = "State_codes")

brazil_final <- brazil2 %>% select(date, country, State_codes, newDeaths, newCases, totalCases, state)

brazil_final <- brazil_final %>% rename(State_code = State_codes)
brazil_final1 <- brazil_final %>% select(date, State_code, state, country, newCases, newDeaths)

brazil_states <- geojsonio::geojson_read("https://raw.githubusercontent.com/codeforamerica/click_that_hood/master/public/data/brazil-states.geojson", what = "sp")


brazil_states1 <- st_as_sf(brazil_states)

brazil_states1 <- brazil_states1 %>% rename(state = name)

brazil_states1 <- brazil_states1 %>% rename(State_code = sigla)

brazil_states2 <- brazil_states1 %>% select(State_code, geometry)

brazil_join <- brazil_final1 %>% left_join(brazil_states2, by = "State_code")

usethis::use_data(brazil_final1, overwrite = TRUE)
