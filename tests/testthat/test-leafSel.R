library(leaflet)
library(shiny)

testthat::test_that("leaflet selector", {
  expect_equal(leafSel("Australia", aus_final1, session),
               (leaflet(aus_final1) %>% 
                  setView(134, -27, 4) %>% 
                  addTiles("MapBox")))
})
