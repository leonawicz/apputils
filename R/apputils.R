#' Initialize apputils
#'
#' Call this function once near the top of the Shiny UI definition.
#'
#' Calling \code{use_apputils} loads package css into the head of a shiny app.
#'
#' @return HTML tags placed in the head of the app UI HTML file.
#' @export
#'
#' @examples
#' # See usage.
use_apputils <- function() {
  addResourcePath(prefix = 'apputils', directoryPath = system.file('apputils', package='apputils'))
  tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'apputils/apputils.css'))
}
