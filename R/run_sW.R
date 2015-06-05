
#' @title Run statnetWeb Application
#'
#' @description Runs the statnetWeb shiny application, a GUI for the statnet
#' suite of network analysis packages.
#'
#' @keywords GUI, shiny
#' @export
#'
#' @examples
#' \dontrun{
#' run_sW()
#' }
#'
run_sW <- function() {
  shiny::runApp(system.file("shiny", package = "statnetWeb"))
}