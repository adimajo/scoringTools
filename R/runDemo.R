#' Launch the Shiny demo app.
runDemo <- function() {
  appDir <- system.file("shiny", "demo", package = "scoringTools")
  if (appDir == "") {
    stop("Could not find demo directory. Try re-installing `scoringTools`.", call. = FALSE)
  }

  shiny::runApp(appDir, launch.browser = TRUE, display.mode = "normal")
}
