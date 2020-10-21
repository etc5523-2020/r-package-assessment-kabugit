testthat::test_that("inputfunc", {
  expect_equal(inputFun("countrysel", "Select your country"),
               selectInput("countrysel", "Select your country", choices = unique(final$country), selected = unique(final$country)[1]))
})
