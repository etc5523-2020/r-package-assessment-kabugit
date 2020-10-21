#' View a leaflet map.
#' 
#' This function will display a leaflet map for cases/deaths for the country chosen by the user.
#' 
#' @author Ketan Kabu
#' 
#' @param country A country that the user will select to display country specific visualisation.
#'
#' @export

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