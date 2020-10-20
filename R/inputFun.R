#' Add a drop down set of options for user to select.
#' 
#' This function will create drop down selection options in app UI for user to visualise a specific country.
#' 
#' @author Ketan Kabu
#' 
#' @param id: An identification name for input from user, will later be used in server section to generate a plot based of user's choice.
#' @param label: What the user sees when asked for input.
#'
#' @export

inputFun <- function(id, label){
  selectInput(id, label, choices = unique(final$country), selected = unique(final$country)[1])
}