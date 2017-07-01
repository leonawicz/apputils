#' Generate a vector of colors.
#'
#' @param n integer, number of colors requested in palette.
#'
#' @return a vector of colors.
#' @export
#'
#' @examples
#' #not run
tolpal <- function(n){
  if(n==0) return()
  if(n >= 12) return(c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#ffad33", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499"))
  switch(n,
         "1"=c("#4477AA"),
         "2"=c("#4477AA", "#CC6677"),
         "3"=c("#4477AA", "#ffad33", "#CC6677"),
         "4"=c("#4477AA", "#117733", "#ffad33", "#CC6677"),
         "5"=c("#332288", "#88CCEE", "#117733", "#ffad33", "#CC6677"),
         "6"=c("#332288", "#88CCEE", "#117733", "#ffad33", "#CC6677","#AA4499"),
         "7"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#ffad33", "#CC6677","#AA4499"),
         "8"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#ffad33", "#CC6677","#AA4499"),
         "9"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#ffad33", "#CC6677", "#882255", "#AA4499"),
         "10"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#ffad33", "#661100", "#CC6677", "#882255", "#AA4499"),
         "11"=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#ffad33", "#661100", "#CC6677", "#882255", "#AA4499")
  )
}

#' Divide sample size by a factor based on the number of RCP-GCM combinations.
#'
#' This function reduces sample size of point sample overlays in plots to improve aesthetics and/or save time
#' when there are too many points to plot.
#'
#' @param x special data frame.
#' @param cru character, name of CRU data set.
#'
#' @return a number.
#' @export
#'
#' @examples
#' #not run
samplesize_factor <- function(x, cru){
  if("RCP" %in% names(x)){
    n.rcps <- nlevels(x$RCP)
    if("Historical" %in% levels(x$RCP) && n.rcps > 1) n.rcps <- n.rcps - 1
  } else n.rcps <- 1
  if("Model" %in% names(x)){
    n.models <- nlevels(x$Model)
    if(cru %in% levels(x$Model) && n.models > 1) n.models <- n.models - 1
  } else n.models <- 1
  n.rcps * n.models
}

#' Get ggplot theme.
#'
#' @return a theme.
#' @export
#'
#' @examples
#' #not run
get_plottheme <- function(){
  ggplot2::theme(
    panel.grid.major=ggplot2::element_line(size=0.5, color="grey"),
    plot.title=ggplot2::element_text(hjust=0.5),
    axis.line=ggplot2::element_line(size=.7, color="black"),
    axis.ticks.length=ggplot2::unit(0.35,"cm"),
    legend.position="bottom",
    legend.justification="right",
    legend.title=ggplot2::element_blank(),
    legend.text=ggplot2::element_text(size=14),
    text=ggplot2::element_text(size=18),
    panel.spacing.x=ggplot2::unit(0.25,"cm"),
    plot.margin=ggplot2::unit(c(0.5, 1, 0.5, 0.5),"cm"),
    strip.text=ggplot2::element_text(size=14)
  )
}

#' Create breaks for ggplot.
#'
#' @param yrs years vector for time series plot.
#' @param n.facets number of facets.
#'
#' @return a numeric vector.
#' @export
#'
#' @examples
#' #not run
get_breaks <- function(yrs, n.facets=1){
  n <- length(yrs)
  if(n.facets==1) cols <- 1 else
    if(n.facets %in% c(2,4)) cols <- 2 else cols <- 3
    if(n <= 15){
      if(cols < 2) return(yrs) else return(seq(yrs[1], yrs[n], by=2))
    }
    if(n <= 25){
      if(cols < 2) return(yrs) else return(seq(yrs[1], yrs[n], by=2))
    }
    if(n <= 50/cols) step <- 5 else
      if(n <= 100/cols) step <- 10 else step <- 20
      a <- range(yrs[yrs %% step == 0])
      a <- seq(a[1], a[2], by=step)
      unique(c(yrs[1], a, yrs[n]))
}

#' Interaction in ggplot.
#'
#' @param x a character vector.
#'
#' @return an unevaluated string for ggplot interaction term.
#' @export
#'
#' @examples
#' #not run
interact <- function(x){
  grp <- c("RCP", "GCM", "Region", "Season")
  x <- grp[grp %in% x]
  if(!length(x)) return()
  paste0("interaction(", paste0(x, collapse=","), ")")
}

#' Position for ggplot.
#'
#' @param jitter logical.
#' @param cby character, factor to color by.
#' @param w width.
#' @param h height.
#' @param wd dodge width.
#' @param dodgeable logical.
#'
#' @return a ggplot position function.
#' @export
#'
#' @examples
#' #not run
.getPosition <- function(jitter, cby, w=0.2, h=0, wd=0.75, dodgeable=FALSE){
  if(jitter) x <- ggplot2::position_jitter(width=w, height=h) else x <- "identity"
  if(!dodgeable) return(x)
  if(!is.null(cby)){
    if(jitter){
      x <- ggplot2::position_jitterdodge(jitter.width=w, jitter.height=h)
    } else {
      x <- ggplot2::position_dodge(width=wd)
    }
  }
  x
}


#' Color and facet helper.
#'
#' @param g a ggplot object.
#' @param d a data frame.
#' @param cby character.
#' @param clr character.
#' @param fby character.
#' @param scales character.
#'
#' @return a ggplot object.
#' @export
#'
#' @examples
#' #not run
#' @importFrom stats as.formula
.colorFacet <- function(g, d, cby, clr, fby, scales){
  if(!is.null(clr)){
    g <- g + ggplot2::scale_fill_manual(values=clr, limits=levels(d[[cby]])) +
      ggplot2::scale_colour_manual(values=clr, limits=levels(d[[cby]]))
  }
  if(!is.null(fby)) g <- g + ggplot2::facet_wrap(stats::as.formula(paste0("~", fby)), scales=scales)
  g
}

#' Mousover info.
#'
#' @param clk click coordinates.
#' @param dblclk double click coordinates.
#' @param hov hover coordinates.
#' @param brush brush coordinates.
#'
#' @return character string.
#' @export
#'
#' @examples
#' #not run
mouseInfo <- function(clk, dblclk, hov, brush){
  xy_str <- function(e) {
    if(is.null(e)) return("NULL\n")
    paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
  }
  xy_range_str <- function(e) {
    if(is.null(e)) return("NULL\n")
    paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1),
           " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
  }

  paste0(
    "click: ", xy_str(clk),
    "dblclick: ", xy_str(dblclk),
    "hover: ", xy_str(hov),
    "brush: ", xy_range_str(brush)
  )
}

#' Mouseover info browser feedback.
#'
#' @param x logical.
#' @param ns module namespace.
#' @param width Shiny column width.
#'
#' @return a shiny column containing mouseover plot interaction coordinate feedback.
#' @export
#'
#' @examples
#' #not run
mouseLog <- function(x, ns, width){
  if(x) shiny::column(width,
               "Mouse feedback: plot 1", shiny::verbatimTextOutput(ns("info1")),
               "Mouse feedback: plot 2", shiny::verbatimTextOutput(ns("info2"))
  ) else NULL
}
