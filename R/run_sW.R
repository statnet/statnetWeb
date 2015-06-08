
#' @title Run statnetWeb
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
#' @import ergm
#' @importFrom RColorBrewer brewer.pal
#'
run_sW <- function() {
  shiny::runApp(system.file("shiny", package = "statnetWeb"))
}