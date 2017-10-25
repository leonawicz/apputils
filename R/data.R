#' Filter data frame based on plot brush
#'
#' This function filters data frame rows to a subset corresponding to observations included in the brushed region of a plot.
#'
#' This implementation is for brushing restricted to the x-axis.
#' #' \code{x} and \code{brush} are commonly reactive values in \code{server.R}, storing the observed click and brush coordinates in \code{input},
#' rather than the \code{input} objects directly.
#'
#' @param data data frame.
#' @param variable, character, name of x-axis variable in \code{data}. May be numeric or categorical.
#' @param x x coordinates. See details.
#' @param brush brushed coordinates. See details.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' #not run
brushed_data <- function(data, variable, x, brush){
  if(inherits(variable, "factor") | inherits(variable, "character")){
    type <- "categorical"
  } else if(inherits(variable, "numeric")) {
    type <- "numeric"
  } else {
    stop("`variable` must inherit from numeric, factor or character.")
  }
  if(is.null(brush) & is.null(x)) return(data)
  if(type == "numeric"){
    if(is.null(brush) & !is.null(x)){
      y <- dplyr::filter(data, .data[[variable]] >= x[1] & .data[[variable]] <= x[2])
    } else y <- shiny::brushedPoints(data, brush)
    if(nrow(y) == 0) y <- data
  } else {
    dec <- data[[variable]]
    r <- if(is.null(brush) & !is.null(x)) c(x[1], x[2]) else c(brush$xmin, brush$xmax)
    intlev <- round(r[1]):round(r[2])
    intlev <- intlev[!intlev %in% c(0, nlevels(dec) + 1)]
    y <- dplyr::filter(data, as.integer(dec) %in% intlev)
  }
  y
}

#' Color re-indexing
#'
#' Re-index colors for grouped data, as displayed in plots and other widgets, when groups may be missing.
#'
#' This is typically used when group levels are in a data set but user selection (mouse brush action) in an interactive plot may not result
#' in a data subset that includes data from all levels of the grouping variable.
#'
#' @param x a data frame.
#' @param clrby column name of the grouping variable to be used for coloring.
#'
#' @return a re-indexed vector of colors from \code{tolpal}.
#' @export
#'
#' @examples
#' # not run
color_indexer <- function(x, clrby){
  x <- split(x, x[[clrby]])
  clrs <- as.list(tolpal(length(x)))
  idx <- which(purrr::map_lgl(x, ~nrow(.x) > 0))
  clrs <- clrs[idx]
  clrs
}
