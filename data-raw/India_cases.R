library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(lubridate)

India_latlong <- system.file("ext", "India_latlong.xlsx", package="covtrack")
India_state_codes <- system.file("ext", "India_state_codes.csv", package="covtrack")
India_states <- system.file("ext", "India_states.csv", package="covtrack")

# india <- read.csv(here::here("data", "India_states.csv"))
india <- read.csv(India_states)

india1 <- india %>% pivot_longer(cols = 3:41, names_to = "state", values_to = "value")

india2 <- india1 %>% pivot_wider(names_from = Status, values_from = value)

india2 <- india2 %>% rename(State_code = state)

# isc <- read.csv(here::here("data", "India_state_codes.csv"))
isc <- read.csv(India_state_codes)

india3 <- india2 %>% left_join(isc, by = "State_code")

india3$Date <- dmy(india3$Date)

# india_ll <- readxl::read_xlsx(here::here("data", "India_latlong.xlsx"))
india_ll <- readxl::read_xlsx(India_latlong)


india_final <- india3 %>% left_join(india_ll, by = "State")

india_final <- india3 %>% filter(State != "State Unassigned") %>% filter(State != "Total")

india_final$country <- "India"

india_final <- india_final %>% rename(date = Date, newCases = Confirmed, newDeaths = Deceased, state = State)
india_final1 <- india_final %>% select(date, State_code, state, country, newCases, newDeaths)

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

usethis::use_data(india_final1, overwrite = TRUE)
