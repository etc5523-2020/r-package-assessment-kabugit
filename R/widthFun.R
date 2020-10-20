#' Add width settings.
#' 
#' This function will create a set UI display of width specified by user for 2 levels - row and box.
#' 
#' @author Ketan Kabu
#' 
#' @param width: Width setting for rows, personal preference is 6.
#' @param box.width: Width setting for box, personal preference is NULL.
#' @param command: The expression for UI input
#' 
#' @export

widthFun <- function(width, box.width, command) {
  fluidRow(width = 6,
           box(width = NULL,
               command))
}