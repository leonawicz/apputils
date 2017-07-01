tableRowColors <- function(data, variable, colorvec, alpha_append=NULL){
  if(!"included_" %in% names(data))
    stop("This function requires a special data table (DT package) containing an 'included_' column.")
  x <- if(variable %in% names(data) && nrow(data)!=0) sort(unique(data[[variable]])) else ""

  if(is.null(colorvec) || (length(x)==1 && x=="")){ # no coloring
    x <- c("_TRUE", "_FALSE")
    colorvec <- c("#CCCCCC", "#FFFFFF")
  } else { # coloring
    colorvec <- colorvec[as.numeric(x)]
    x <- c(paste0(x, "_", TRUE), paste0(x, "_", FALSE))
    colorvec2 <- if(is.null(alpha_append)) colorvec else paste0(colorvec, alpha_append)
    colorvec <- c(colorvec, colorvec2)
  }
  DT::styleEqual(x, colorvec)
}
