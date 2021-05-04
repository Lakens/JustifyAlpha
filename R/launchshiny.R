#' Launch the Justify your alpha shiny app.
#' @export
runApp <- function() {
  appDir <- system.file("shiny-examples", "shiny.R", package = "justifieR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `justifieR`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
