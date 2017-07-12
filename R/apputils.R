globalVariables(c(".", "%>%", "Model", "RCP", "Year", "Var", "Val", "Decade", "Decadal_mean"))

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

#' Update shinytoastr css
#'
#' Update toast css from shinytoastr package.
#'
#' \code{apputils} already contains some toastr css overrides (loaded via \code{use_apputils}).
#' This function allows for injecting additonal or different css overrides for a specific toast container
#' that may not already be as specified by \code{apputils}. This is typically used to adjust the app intro toast,
#' hence the default for \code{position} is \code{"top-center"}.
#'
#' Note that list names and values ay be quoted if necessary. See example.
#' Should be familiar with source toastr css in addition to running the example
#' in order to understand which elements apply to \code{container} vs. \code{toast}.
#'
#' @param container list of style arguments for the container div. See details and example.
#' @param toast list of style arguments for the toast. See details and example.
#' @param position character, defaults to \code{"top-center"}.
#'
#' @return an html style tag.
#' @export
#'
#' @examples
#' update_toastr_css(
#'   list('overflow-y'='auto', width='70%', height='700px'),
#'   list(top='100px', margin='0 auto', left='115px')
#' )
update_toastr_css <- function(container=NULL, toast=NULL, position="top-center"){
  if(is.null(container) & is.null(toast))
    stop("Must provide valid arguments to at least one of container or toast.")
  if(!is.null(container))
    container <- paste0('#toast-container.toast-', position, ' > div {\n  ',
      paste0(paste(names(container), container, sep=":", collapse=";\n  "), ";\n}"))
  if(!is.null(toast))
    toast <- paste0('.toast-', position, ' {\n  ',
      paste0(paste(names(toast), toast, sep=":", collapse=";\n  "), ";\n}"))
  shiny::tags$style(shiny::HTML(paste(container, toast, sep="\n")))
}

#' Require non-null inputs in app UI
#'
#' Generate quoted javascript logic for non-null inputs.
#'
#' @param inputs a character vector of inputs.
#'
#' @return a character string of the javascript condition.
#' @export
#'
#' @examples
#' inputs_not_null(c("input1", "input2"))
inputs_not_null <- function(inputs){
  paste(paste0("input.", inputs), "!== null", collapse=" & ")
}
