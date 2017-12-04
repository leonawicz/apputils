globalVariables(c(".data"))

#' apputils: The \code{apputils} package contains common utility functions, settings and references for development use across multiple Shiny apps.
#'
#' \code{apputils} has a \code{shinydashboard} focus. \code{apputils} is a member package in the satellite sector of the SNAPverse.
#' It supports other satellites in the verse, including \code{maputils} and \code{snaputils}.
#'
#' Package functionality and areas of support covered by \code{apputils} include:
#'
#' \itemize{
#'   \item Overrides of \code{shinydashboard::valueBox}, \code{shinydashbaord::infoBox} and \code{shiny::icon} that support the use of local thumbnails images.
#'   \item Stat boxes: special type of value or info boxes for common statistics using a collection of icons provided by the package.
#'   \item Adjusted CSS styles and integration with packages like \code{rintrojs} and \code{shinytoastr} for interactive tours and toast messages.
#'   \item Functions for including app information widgets such as citations, contact info, frequently asked questions and more.
#'   \item Encapsulation of working with data frames in server.R in specific contexts and use cases to simplify code.
#'   \item Wrappers around specific use cases for Leaflet maps, data tables, and general plotting in apps.
#'   \item Helper functions for dynamic reports.
#' }
#'
#' @docType package
#' @name apputils
NULL

#' @importFrom magrittr %>%
NULL

#' Initialize apputils
#'
#' Call this function once near the top of the Shiny UI definition.
#'
#' Calling \code{use_apputils} loads package css into the head of a Shiny app.
#' Optionally calls \code{introjsUI()} and/or \code{useToastr()}, in which case the Shiny app must have these packages loaded.
#'
#' @param use_rintrojs logical, also loads \code{rintrojs} package UI components via \code{introjsUI}.
#' @param use_shinytoastr logical, also loads \code{shinytoastr} package UI components via \code{useToastr}.
#' @param force_AdminLTE logical, force load \code{AdminLTE.css} from \code{shinydashboard} even if the app does not use a dashboard layout.
#'
#' @return HTML tags placed in the head of the app UI HTML file.
#' @export
#'
#' @examples
#' \dontrun{
#' use_apputils()
#' use_apputils(force_AdminLTE = TRUE)
#' use_apputils(use_rintrojs = TRUE, use_shinytoastr = TRUE)
#' }
use_apputils <- function(use_rintrojs = FALSE, use_shinytoastr = FALSE, force_AdminLTE = FALSE) {
  shiny::addResourcePath("resources", system.file("resources", package = "apputils"))
  if(force_AdminLTE)
    shiny::addResourcePath("AdminLTE", system.file("AdminLTE", package = "shinydashboard"))

  x <- shiny::tags$head(shiny::tags$link(
    rel = "stylesheet", type = "text/css", href = "resources/apputils.css"))
  if(force_AdminLTE) x <- shiny::tagList(shiny::tags$link(
    rel = "stylesheet", type = "text/css", href = "AdminLTE/AdminLTE.css"), x)

  if(!use_rintrojs & !use_shinytoastr) return(x)
  if(use_rintrojs & use_shinytoastr)
    return(shiny::tagList(shinytoastr::useToastr(), rintrojs::introjsUI(), x))
  if(use_rintrojs)
    return(shiny::tagList(rintrojs::introjsUI(), x))
  if(use_shinytoastr)
    return(shiny::tagList(shinytoastr::useToastr(), x))
}

# nolint start
#' Update shinytoastr css
#'
#' Update toast css from shinytoastr package.
#'
#' \code{apputils} already contains some toastr css overrides (loaded via \code{use_apputils}).
#' This function allows for injecting additional or different css overrides for a specific toast container
#' that may not already be as specified by \code{apputils}. This is typically used to adjust the app intro toast,
#' hence the default for \code{position} is \code{"top-center"}.
#'
#' Note that list names and values may be quoted if necessary. See example.
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
#' @param rgba numeric, vector of four css rgba property values for background color, e.g., \code{c(0, 0, 0, 0.5)}. See details.
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
#'   list('overflow-y' = 'auto', width = '70%', height = '700px'),
#'   list(top = '100px', margin = '0 auto', left = '115px')
#' )
update_toastr_css <- function(container = NULL, toast = NULL, rgba = NULL, hover.rgba = NULL,
                              opacity = NULL, hover.opacity = NULL, radius = NULL,
                              position = "top-center"){
  if(is.null(container) & is.null(toast))
    stop("Must provide valid arguments to at least one of container or toast.")
  set_op <- function(op){
    list(opacity = op,
         '-ms-filter'=paste0("progid:DXImageTransform.Microsoft.Alpha(Opacity=", 100 * op, ")"),
         filter=paste0("alpha(", 100 * op, ")"))
  }

  if(!is.null(opacity) && is.null(rgba)){
    op <- set_op(opacity)
    container <- c(container, op)
  }
  if(!is.null(hover.opacity) & is.null(hover.rgba)){
    hov.op <- set_op(hover.opacity)
    hover <- paste0('#toast-container.toast-', position, ' > :hover {\n  ',
                    paste0(paste(names(hov.op), hov.op, sep = ":", collapse = ";\n  "), ";\n}"))
  } else hover <- NULL

  if(!is.null(rgba)) rgba <- paste0('rgba(', paste0(rgba, collapse = ","), ")")
  if(!is.null(hover.rgba)) hover.rgba <- paste0('rgba(', paste0(hover.rgba, collapse = ","), ")")
  if(!is.null(rgba)) container <- c(container, list('background-color' = rgba))
  if(!is.null(hover.rgba)){
    hover <- paste0('#toast-container.toast-', position, ' > :hover {\n  ',
                    paste0(paste("background-color", hover.rgba, sep = ":", collapse = ";\n  "), ";\n}"))
  }
  if(!is.null(radius)) container <- c(container, list(
    '-moz-border-radius' = radius, '-webkit-border-radius' = radius, 'border-radius' = radius))

  if(!is.null(container))
    container <- paste0('#toast-container.toast-', position, ' > div {\n  ',
      paste0(paste(names(container), container, sep = ":", collapse = ";\n  "), ";\n}"))
  if(!is.null(toast))
    toast <- paste0('.toast-', position, ' {\n  ',
      paste0(paste(names(toast), toast, sep = ":", collapse = ";\n  "), ";\n}"))

  shiny::tags$style(shiny::HTML(paste(container, toast, hover, sep = "\n")))
}
# nolint end

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
  paste(paste0("input.", inputs), "!== null", collapse = " & ")
}

# nolint start
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
app_overlay <- function(text, logo = NULL, loading.logo = NULL, text.loading = '<h1>Loading...</h1>'){
  x <- '<div id="fade-wrapper">\n  <div id="fade-wrapper-content">'
  if(!is.null(logo)) x <- paste0(x, '<p><img src="', logo, '"></p><br/><br/>')
  if(!is.null(text)) x <- paste0(x, text)
  if(!is.null(loading.logo)) x <- paste0(x, '<p><img src="', loading.logo, '" ></p>')
  if(!is.null(text.loading)) x <- paste0(x, text.loading)
  shiny::HTML(paste0(x, '</div></div>'))
}
# nolint end

#' Run app examples
#'
#' Launch an apputils Shiny app example in your browser.
#'
#' The \code{source} defaults to \code{"local"} for package apps. If set to \code{"remote"},
#' \code{snapp} will launch the app in your browser using the canonical url rather than the package app.
#' Options for \code{local_mode} are \code{"normal"} (default) or \code{"showcase"}
#' for tandem reactive code highlighting, and metadata if applicable. \code{local_mode} applies to \code{source = "local"} package apps.
#'
#' Available IDs include \code{"icons"}. This app displays all the available statistics and data analysis themed icons
#' available in apputils in both light and dark color styles.
#'
#' @param id character, the name of the application. See details.
#' @param source character, source of app. See details.
#' @param local_mode character, display mode.
#'
#' @export
#'
#' @examples
#' \dontrun{exApp("icons")}
exApp <- function(id, source = "local", local_mode = "normal") {
  if(!local_mode %in% c("normal", "showcase"))
    stop("`local_mode` must be 'normal' or 'showcase'.")
  x <- list.files(system.file("shiny", package = "apputils"))
  if(!id %in% x) stop("Invalid app `id`. See `exApp` documentation for available apps.")
  url <- file.path("https://uasnap.shinyapps.io", id)
  if(source == "remote"){
    utils::browseURL(url)
    cat(paste(id, "launched remotely.\n"))
    return(invisible())
  }
  shiny::runApp(system.file("shiny", id, package = "apputils"), display.mode = local_mode)
}
