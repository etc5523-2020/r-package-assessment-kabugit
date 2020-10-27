#' View a leaflet map.
#' 
#' This function will display a leaflet map for cases/deaths for the country chosen by the user.
#' 
#' @author Ketan Kabu
#' 
#' @param country A country that the user will select to display country specific visualisation.
#' @param df Dataframe that will be used to display a map based on country selected by user.
#' @param session Link to shiny app server.
#'
#' @export

leafSel <- function(country, df, session){
  

  b <- leaflet(df) %>%
    setView(-52, -15, 3.5) %>%
    addTiles("MapBox")
  
  
  u <- leaflet(df) %>% 
    setView(-110, 52.8, 2.5) %>% 
    addTiles("MapBox")
  
  
  i <- leaflet(df) %>% 
    setView(79, 21, 4.4) %>% 
    addTiles("MapBox")
  
  
  a <- leaflet(df) %>% 
    setView(134, -27, 4) %>% 
    addTiles("MapBox")
  
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