testthat::test_that("inputfunc", {
  expect_equal(inputFun("countrysel", "Select your country"),
               shiny::selectInput("countrysel", "Select your country", choices = c("USA, India, Brazil, Australia"), selected = "USA"))
})
