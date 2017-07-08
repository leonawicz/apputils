#' Size of text in overridden shiny dashboard value box function eidget output.
#'
#' @param x text.
#' @param value size.
#' @param margin numeric, html margin size in pixels.
#' @param default.value value to repeat if necessary.
#'
#' @return a shiny taglist.
#' @export
#'
#' @examples
#' #not run
pTextSize <- function(x, value, margin=NULL, default.value=100){
  if(length(x) > 1) value <- c(value, rep(default.value, length(x) - 1))
  style <- paste0("font-size: ", value, "%;")
  if(!is.null(margin)) style <- paste0(style, " margin: ", margin, "px;")
  x <- purrr::map2(x, style, ~tags$p(.x, style=.y))
  if(length(x)==1) return(x[[1]])
  class(x) <- c("shiny.tag.list", "list")
  x
}

#' Override shinydashboard valueBox function
#'
#' This and the apputils::icon functions override functions in the shiny and shinydashboard packages
#' to provide the ability to display image file icons in value boxes.
#'
#' @param value value.
#' @param subtitle subtitle.
#' @param icon icon may be a local image file url.
#' @param color character.
#' @param width column width.
#' @param href link.
#' @param validate.color bypass \code{shinydashboard:::validateColor}.
#'
#' @return a valueBox.
#' @export
#'
#' @examples
#' #not run
valueBox <- function (value, subtitle, icon = NULL, color = "aqua", width = 4, href = NULL, validate.color=FALSE){
  if(validate.color) shinydashboard:::validateColor(color)
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = icon$name)
  if(!is.null(icon)){
    if(!icon$name %in% c("i", "img")) stop("'icon$name' must be 'i' or 'img'.")
    iconClass <- if(icon$name=="i") "icon-large" else "img"
  }
  boxContent <- shiny::div(class = paste0("small-box bg-", color),
                    shiny::div(class = "inner", shiny::h3(value), shiny::p(subtitle)), if (!is.null(icon))
                      shiny::div(class = iconClass, icon))
  if (!is.null(href))
    boxContent <- shiny::a(href = href, boxContent)
  shiny::div(class = if (!is.null(width))
    paste0("col-sm-", width), boxContent)
}

#' Override shiny icon function
#'
#' This and the apputils::valueBox functions override functions in the shiny and shinydashboard packages
#' to provide the ability to display image file icons in value boxes.
#'
#' @param name character. If \code{lib="local"}, \code{name} must be a named list with \code{src} element.
#' @param class character.
#' @param lib library, may be 'local'.
#'
#' @return an icon tag.
#' @export
#'
#' @examples
#' #not run
icon <- function (name, class = NULL, lib = "font-awesome"){
  if(lib=="local"){
    if(is.null(name$src))
      stop("If lib='local', 'name' must be a named list with a 'src' element
           and optionally 'width' (defaults to 100%).")
    if(is.null(name$width)) name$width <- "100%"
    return(shiny::tags$img(class="img img-local", src=name$src, width=name$width))
  }

  prefixes <- list(`font-awesome` = "fa", glyphicon = "glyphicon")
  prefix <- prefixes[[lib]]
  if (is.null(prefix)) {
    stop("Unknown font library '", lib, "' specified. Must be one of ",
         paste0("\"", names(prefixes), "\"", collapse = ", "))
  }
  iconClass <- ""
  if (!is.null(name))
    iconClass <- paste0(prefix, " ", prefix, "-", name)
  if (!is.null(class))
    iconClass <- paste(iconClass, class)
  iconTag <- shiny::tags$i(class = iconClass)
  if (lib == "font-awesome") {
    htmltools::htmlDependencies(iconTag) <- htmltools::htmlDependency("font-awesome",
                                                                      "4.6.3", c(href = "shared/font-awesome"), stylesheet = "css/font-awesome.min.css")
  }
  iconTag
}

#' Genrate CSS for different colored value boxes.
#'
#' Generate CSS for background color and text color of value boxes based on hex color input.
#'
#' Colors must be in hex color format with hashtag, e.g., \code{"#FFFFFF"}.
#' This function generates CSS to be added to a Shiny app.
#' New value box elements are named by appending the hex color to \code{.bg-} without the \code{#}.
#' When using the \code{valueBox} override function in \code{apputils},
#' make sure to specify a hex color "name" that is a character string without the \code{#}, e.g., "FFFFFF".
#' The only available colors that will work with \code{valueBox} in an app are those that have their CSS generated and loaded in the app as well.
#'
#' @param bg character, hex color of value box background.
#' @param col character, hex color of value box text.
#'
#' @return a character string wrapped in \code{shiny::HTML}.
#' @export
#'
#' @examples
#' #not run
valueBoxColorsCSS <- function(bg="#333333", col="#FFFFFF"){
  x <- gsub("#", "", bg)
  HTML(paste0(".bg-", x, " { background-color: #", x, " !important; color: ", col, " !important; }", collapse="\n"))
}

#' Generate CSS for a value box color palette.
#'
#' Obtain a CSS snippet defining value box colors for a full color palette from a named list of available palettes.
#'
#' The only available palette at this time is \code{"tolpal"}, which covers all colors available using the \code{tolpal} function.
#' See \code{valueBoxColorsCSS} for more information.
#'
#' @param pal character, a named color palette. See details for options.
#'
#' @return a CSS snippet defining value boxes of different colors.
#' @export
#'
#' @examples
#' #not run
valueBoxPalette <- function(pal){
  if(!pal %in% "tolpal") stop("Invalid palette name.")
  switch(pal, tolpal=valueBoxColorsCSS(unique(unlist(map(1:12, ~tolpal(.x))))))
}
