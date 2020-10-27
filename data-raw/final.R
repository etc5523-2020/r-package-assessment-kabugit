library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(lubridate)

final <- rbind(us_join, india_final2, brazil_join, australia_join)
final <- st_as_sf(final)

final <- final %>% filter(date <= "2020-09-25")

final$newCases <- str_remove_all(final$newCases, "-")
final$newCases <- as.double(final$newCases)

final$newDeaths <- str_remove_all(final$newDeaths, "-")
final$newDeaths <- as.double(final$newDeaths)

usethis::use_data(final, overwrite = TRUE)
