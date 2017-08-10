#' Color table rows according to ggplot coloring by groups.
#'
#' @param data a special data frame.
#' @param variable a column name in the data specifying the factor used for plot colors.
#' @param colorvec a vector of colors.
#' @param alpha_append NULL or a two digit numeric or character to append to hex colors.
#'
#' @return a DT::styleEqual call.
#' @export
#'
#' @examples
#' #not run
tableRowColors <- function(data, variable, colorvec, alpha_append=NULL){
  if(!"included_" %in% names(data))
    stop("This function requires a special data table (DT package) containing an 'included_' column.")
  x <- if(variable %in% names(data) && nrow(data)!=0) sort(unique(data[[variable]])) else ""

  if(is.null(colorvec) || (length(x)==1 && x=="")){
    # no coloring
    x <- c("_TRUE", "_FALSE")
    colorvec <- c("#CCCCCC", "#FFFFFF")
  } else {
    # coloring
    colorvec <- colorvec[as.numeric(x)]
    x <- c(paste0(x, "_", TRUE), paste0(x, "_", FALSE))
    colorvec2 <- if(is.null(alpha_append)) colorvec else paste0(colorvec, alpha_append)
    colorvec <- c(colorvec, colorvec2)
  }
  DT::styleEqual(x, colorvec)
}
