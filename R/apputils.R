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
    return(shiny::tagList(shinytoastr::useToastr(), rintrojs::introjsUI(), x))
  if(use_rintrojs)
    return(shiny::tagList(rintrojs::introjsUI(), x))
  if(use_shinytoastr)
    return(shiny::tagList(shinytoastr::useToastr(), x))
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
#' If wanting to keep text fully opaque in the toast while using semi-transparency,
#' especially useful when adding a background image, use css rgba instead of opacity.
#' \code{rgba} and\code{hover.rgba} nullify opacity arguments if both are provided, respectively.
#'
#' @param container list of style arguments for the container div. See details and example.
#' @param toast list of style arguments for the toast. See details and example.
#' @param position character, defaults to \code{"top-center"}.
#' @param rgba numeric, vector of four css rgba property values for background color, e.g., \code{c(0,0,0,0.5)}. See details.
#' @param hover.rgba numeric, vector of four css rgba property values for background color on mouse hover. See details.
#' @param opacity numeric, toast opacity. Appended to \code{container}.
#' @param hover.opacity numeric, toast opacity on mouse hover.
#' @param radius character, border radius, e.g., \code{"0px"}.
#'
#' @return an html style tag.
#' @export
#'
#' @examples
#' update_toastr_css(
#'   list('overflow-y'='auto', width='70%', height='700px'),
#'   list(top='100px', margin='0 auto', left='115px')
#' )
update_toastr_css <- function(container=NULL, toast=NULL, rgba=NULL, hover.rgba=NULL,
                              opacity=NULL, hover.opacity=NULL, radius=NULL, position="top-center"){
  if(is.null(container) & is.null(toast))
    stop("Must provide valid arguments to at least one of container or toast.")
  set_op <- function(op){
    list(opacity=op,
         '-ms-filter'=paste0("progid:DXImageTransform.Microsoft.Alpha(Opacity=", 100*op, ")"),
         filter=paste0("alpha(", 100*op, ")"))
  }

  if(!is.null(opacity) && is.null(rgba)){
    op <- set_op(opacity)
    container <- c(container, op)
  }
  if(!is.null(hover.opacity) & is.null(hover.rgba)){
    hov.op <- set_op(hover.opacity)
    hover <- paste0('#toast-container.toast-', position, ' > :hover {\n  ',
                    paste0(paste(names(hov.op), hov.op, sep=":", collapse=";\n  "), ";\n}"))
  } else hover <- NULL

  if(!is.null(rgba)) rgba <- paste0('rgba(', paste0(rgba, collapse=","), ")")
  if(!is.null(hover.rgba)) hover.rgba <- paste0('rgba(', paste0(hover.rgba, collapse=","), ")")
  if(!is.null(rgba)) container <- c(container, list('background-color'=rgba))
  if(!is.null(hover.rgba)){
    hover <- paste0('#toast-container.toast-', position, ' > :hover {\n  ',
                    paste0(paste("background-color", hover.rgba, sep=":", collapse=";\n  "), ";\n}"))
  }
  if(!is.null(radius)) container <- c(container, list(
    '-moz-border-radius'=radius, '-webkit-border-radius'=radius, 'border-radius'=radius))

  if(!is.null(container))
    container <- paste0('#toast-container.toast-', position, ' > div {\n  ',
      paste0(paste(names(container), container, sep=":", collapse=";\n  "), ";\n}"))
  if(!is.null(toast))
    toast <- paste0('.toast-', position, ' {\n  ',
      paste0(paste(names(toast), toast, sep=":", collapse=";\n  "), ";\n}"))

  shiny::tags$style(shiny::HTML(paste(container, toast, hover, sep="\n")))
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

#' Add HTML content to app launch overlay
#'
#' Add HTML content on top of the semi-transparent full screen overlay shown at app launch.
#'
#' @param text character, typically a string of html.
#' @param logo character, path to image.
#' @param loading.logo character, path to image.
#' @param text.loading character, text to accompany \code{loading.logo} or alone.
#'
#' @return a \code{shiny::HTML} string.
#' @export
#'
#' @examples
#' #not run
app_overlay <- function(text, logo=NULL, loading.logo=NULL, text.loading='<h1>Loading...</h1>'){
  x <- '<div id="fade-wrapper">\n  <div id="fade-wrapper-content">'
  if(!is.null(logo)) x <- paste0(x, '<p><img src="', logo, '"></p><br/><br/>')
  if(!is.null(text)) x <- paste0(x, text)
  if(!is.null(loading.logo)) x <- paste0(x, '<p><img src="', loading.logo, '" ></p>')
  if(!is.null(text.loading)) x <- paste0(x, text.loading)
  shiny::HTML(paste0(x, '</div></div>'))
}
