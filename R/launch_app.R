#' Just launch the app!
#' 
#' This function launches the shiny app embedded in this package. Use this function to quickly start using the app. 
#' @export
launch_app <- function(){
  shiny::runApp(system.file("app", package = "covtrack"))
}

"launch_app"