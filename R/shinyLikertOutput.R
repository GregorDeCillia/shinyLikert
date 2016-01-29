#' plot shiny likert output
#'
#' creates the app in a fluidpage.
#' @param x     object to be printed
#' @param ...   currently unused
#'
#' @export
plot.shinyLikertOutput = function( x, ... )
{
  shiny::fluidPage( x )
}
