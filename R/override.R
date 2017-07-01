pTextSize <- function(x, value, margin=NULL, default.value=100){
  if(length(x) > 1) value <- c(value, rep(default.value, length(x) - 1))
  style <- paste0("font-size: ", value, "%;")
  if(!is.null(margin)) style <- paste0(style, " margin: ", margin, "px;")
  x <- purrr::map2(x, style, ~tags$p(.x, style=.y))
  if(length(x)==1) return(x[[1]])
  class(x) <- c("shiny.tag.list", "list")
  x
}

# These functions override functions in the shiny and shinydashboard packages
# to provide the ability to display image file icons in value boxes.

# override shinydashboard function
valueBox <- function (value, subtitle, icon = NULL, color = "aqua", width = 4, href = NULL){
  shinydashboard:::validateColor(color)
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

# override shiny function
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
