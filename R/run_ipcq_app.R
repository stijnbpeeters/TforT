#' Run the iPCQ Shiny calculator
#'
#' Launches the Shiny application included with TforT in the local browser.
#'
#' @param launch.browser Passed to [shiny::runApp()].
#' @param ... Additional arguments passed to [shiny::runApp()].
#' @return The value returned invisibly by [shiny::runApp()].
#' @export
run_ipcq_app <- function(launch.browser = TRUE, ...) {
  app_dir <- system.file("shiny", "ipcq", package = "TforT")
  if (!nzchar(app_dir)) {
    stop("The packaged iPCQ Shiny application could not be found.", call. = FALSE)
  }
  shiny::runApp(app_dir, launch.browser = launch.browser, ...)
}
