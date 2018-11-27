#' Start TAShiny
#' @title Launch TAShiny Interface
#' @return Nothing
#' @description TAShiny() loads interactive user interface built using R shiny.
#' @details The interactive user interface is to provide an easy way for people who are learning Text Mining and Analytics. Includes example data for testing out a few example analysis.
#' @keywords TAShiny
#' @examples
#' \dontrun{
#' library(shiny)
#' TAShiny()
#' }

 TAShiny <- function() {


  shiny::runApp(appDir = system.file("shiny-examples", "myapp", package = "TAShiny"))
  Sys.setenv("R_TESTS" = "")
}
