library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(lubridate)

covid_stringency_index <- system.file("ext", "covid-stringency-index.csv", package="covtrack")

cd <- final

cd <- cd %>% mutate(month = substr(date, 6, 7))

cd <- cd %>% group_by(state, month) %>% mutate(m_case = sum(newCases)) %>% mutate(m_death = sum(newDeaths))

cd <- cd %>% mutate(cbd = (m_death/m_case)*100)

cd$cbd[is.nan(cd$cbd)]<-0

cd$cbd <- round(cd$cbd, 2)

cd <- cd %>% group_by(country, month) %>% mutate(countr_mcase = sum(newCases)) %>% mutate(countr_mdeath = sum(newDeaths))

cd <- cd %>% mutate(countr_cbd = (countr_mdeath/countr_mcase)*100)

cd$countr_cbd <- round(cd$countr_cbd, 2)

# stringent <- read.csv(here::here("data", "covid-stringency-index.csv"))
stringent <- read.csv(covid_stringency_index)

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
usethis::use_data(cd, overwrite = TRUE)
