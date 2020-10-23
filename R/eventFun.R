#' View the state-wise details.
#' 
#' This function will let the user select the country of preference to display state-wise details of cases/deaths.
#' 
#' @author Ketan Kabu
#' 
#' @param df A data frame that contains information of all countries, out of which one would be selected by user on the line graph.
#' @param session Link to shiny app server.
#' @export

eventFun <- function(df, session){
  eve <- event_data("plotly_click")
  if (is.null(eve)) return(NULL)
  
  if(eve$curveNumber == 0){
    df %>% filter(country == "Australia")
  }
  if(eve$curveNumber == 1){
    df %>% filter(country == "Brazil")
  }
  if(eve$curveNumber == 2){
    temp <<- df %>% filter(country =="India")
  }
  if(eve$curveNumber == 3){
    temp <<- df %>% filter(country =="USA")
  }
}