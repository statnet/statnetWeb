
#' @title Run statnetWeb
#'
#' @description Runs the one of the statnetWeb shiny applications,
#' which serve as GUIs for the statnet suite of network analysis packages.
#'
#' @param app
#'
#' @keywords GUI, shiny
#' @export
#'
#' @examples
#' \dontrun{
#' run_sw()
#' }
#'
#'
run_sw <- function(app = "ergm") {
  if(!(app %in% c("ergm", "stergm"))){
    stop("app must be one of ergm or stergm")
  }
  shiny::runApp(system.file(paste0("inst/shiny/", app),
                            package = "statnetWeb"))
}