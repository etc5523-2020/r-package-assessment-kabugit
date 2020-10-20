#' Add a time slider.
#' 
#' This function will add a time slider that the user can move to select a specific day for the country's casee/deaths to be visualised.
#' 
#' @author Ketan Kabu
#' 
#' @param id: An identification name for input from user, will later be used in server section to generate a plot based on user's choice of time frame (day).
#' @param label: What the user sees when asked for day input needed for visualsation.
#'
#' @export

slideTime <- function(id, label) {
  sliderInput(id, label, min(final$date), max(final$date), max(final$date))
}