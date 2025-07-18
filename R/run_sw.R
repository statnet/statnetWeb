#' @title Run statnetWeb
#'
#' @description Runs the statnetWeb shiny application, a GUI for the statnet
#' suite of network analysis packages.
#'
#' @keywords graphs datagen models
#' @concept networks
#' @concept GUI
#' @concept shiny
#' @concept ergm
#' @export
#' @examples
#' \dontrun{
#' run_sw()
#' }
#'
#'
run_sw <- function() {
  shiny::runApp(system.file("shiny/statnetWeb", package = "statnetWeb"))
}

tame_rcmd_check <- function() {
  # Let R CMD check find a instance of using DT. The shiny app code is in
  # `inst` so invisible to the check.
  DT::DTOutput()
  stop("don't call `tame_rcmd_check()`")
  NULL
}