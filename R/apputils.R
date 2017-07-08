#' Initialize apputils
#'
#' Call this function once near the top of the Shiny UI definition.
#'
#' Calling \code{use_apputils} loads package css into the head of a Shiny app.
#' Optionally calls \code{introjsUI()} and/or \code{useToastr()}, in which case the Shiny app must have these packages loaded.
#'
#' @param use_rintrojs logical, also loads \code{rintrojs} package UI components via \code{introjsUI}.
#' @param use_shinytoastr logical, also loads \code{shinytoastr} package UI components via \code{useToastr}.
#'
#' @return HTML tags placed in the head of the app UI HTML file.
#' @export
#'
#' @examples
#' #not run
use_apputils <- function(use_rintrojs=FALSE, use_shinytoastr=FALSE) {
  shiny::addResourcePath("resources", system.file("resources", package = "apputils"))
  x <- shiny::tags$head(shiny::tags$link(rel = 'stylesheet', type = 'text/css', href = 'resources/apputils.css'))
  if(!use_rintrojs & !use_shinytoastr) return(x)
  if(use_rintrojs & use_shinytoastr)
    return(shiny::tagList(x, shinytoastr::useToastr(), rintrojs::introjsUI()))
  if(use_rintrojs)
    return(shiny::tagList(x, rintrojs::introjsUI()))
  if(use_shinytoastr)
    return(shiny::tagList(x, shinytoastr::useToastr()))
}
