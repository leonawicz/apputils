#' Generate a vector of colors
#'
#' The current implementation returns a vector of colors based on a specific color palette.
#' The maximum colors returned is 12.
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
  if(n >= 12) return(c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933",
                       "#ffad33", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499"))
  switch(n,
         "1"=c("#4477AA"),
         "2"=c("#4477AA", "#CC6677"),
         "3"=c("#4477AA", "#ffad33", "#CC6677"),
         "4"=c("#4477AA", "#117733", "#ffad33", "#CC6677"),
         "5"=c("#332288", "#88CCEE", "#117733", "#ffad33", "#CC6677"),
         "6"=c("#332288", "#88CCEE", "#117733", "#ffad33", "#CC6677", "#AA4499"),
         "7"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#ffad33", "#CC6677", "#AA4499"),
         "8"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#ffad33", "#CC6677",
               "#AA4499"),
         "9"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#ffad33", "#CC6677",
               "#882255", "#AA4499"),
         "10"=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#ffad33", "#661100",
                "#CC6677", "#882255", "#AA4499"),
         "11"=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#ffad33",
                "#661100", "#CC6677", "#882255", "#AA4499")
  )
}

#' Divide sample size by a factor based on the number of RCP-GCM combinations
#'
#' This function reduces sample size of point sample overlays in plots to improve aesthetics and/or save time
#' when there are too many points to plot.
#'
#' @param x special data frame.
#' @param baseline character, name of baseline data set.
#'
#' @return a number.
#' @export
#'
#' @examples
#' #not run
samplesize_factor <- function(x, baseline){
  if("RCP" %in% names(x)){
    n.rcps <- nlevels(x$RCP)
    if("Historical" %in% levels(x$RCP) && n.rcps > 1) n.rcps <- n.rcps - 1
  } else n.rcps <- 1
  if("Model" %in% names(x)){
    n.models <- nlevels(x$Model)
    if(baseline %in% levels(x$Model) && n.models > 1) n.models <- n.models - 1
  } else n.models <- 1
  n.rcps * n.models
}

#' Get ggplot theme
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
    axis.ticks.length=ggplot2::unit(0.35, "cm"),
    legend.position="bottom",
    legend.justification="right",
    legend.title=ggplot2::element_blank(),
    legend.text=ggplot2::element_text(size=14),
    text=ggplot2::element_text(size=18),
    panel.spacing.x=ggplot2::unit(0.25, "cm"),
    plot.margin=ggplot2::unit(c(0.5, 1, 0.5, 0.5), "cm"),
    strip.text=ggplot2::element_text(size=14)
  )
}

#' Create breaks for ggplot
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
    if(n.facets %in% c(2, 4)) cols <- 2 else cols <- 3
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

#' Interaction helper for ggplot
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

#' Positioning helper for ggplot
#'
#' This functions assists with position by combining information on whether or not jitter is present in a plot,
#' there is a categorical variable for coloring, and if the plot uses dodge positioning when coloring variable levels.
#'
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
getPosition <- function(jitter, cby, w=0.2, h=0, wd=0.75, dodgeable=FALSE){
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


#' Coloring and faceting helper for ggplot
#'
#' This function takes a \code{ggplot} object and source data frame, \code{d}, and adds manual color and fill scale layers
#' based on \code{cby} and \code{clr} and/or faceting based on \code{fby} and \code{scales}.
#' The data frame, \code{d}, is passed for determining the levels of the factor \code{cby} in \code{d}.
#' The \code{cby} and \code{fby} columns in \code{d} must be factors.
#' Like coloring, faceting must be done by a single factor variable, not crossed factors.
#'
#' @param g a ggplot object.
#' @param d a data frame.
#' @param cby character, factor column in \code{d}.
#' @param clr character, color vector. If \code{NULL}, no coloring by levels of \code{cby} occurs.
#' @param fby character, factor column in \code{d}.
#' @param scales character, passed to \code{scales} in \code{ggplot2::facet_wrap}.
#'
#' @return a ggplot object.
#' @export
#'
#' @examples
#' #not run
#' @importFrom stats as.formula
colorFacet <- function(g, d, cby, clr, fby, scales){
  if(!is.null(clr)){
    g <- g + ggplot2::scale_fill_manual(values=clr, limits=levels(d[[cby]])) +
      ggplot2::scale_colour_manual(values=clr, limits=levels(d[[cby]]))
  }
  if(!is.null(fby)) g <- g + ggplot2::facet_wrap(stats::as.formula(paste0("~", fby)), scales=scales)
  g
}

#' Mouseover info
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
  paste0("click: ", xy_str(clk), "dblclick: ", xy_str(dblclk),
    "hover: ", xy_str(hov), "brush: ", xy_range_str(brush))
}

#' Mouseover info browser feedback
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

#' Kilo and Mega labels for rescaled data
#'
#' Kilo (K) and Mega (M) labels for rescaled plot axes and stat boxes.
#'
#' When data are rescaled, labels in plot axes and other places can be replaced with a shorthand form.
#'
#' @param x numeric value.
#'
#' @return \code{x} as a character string, potentially in the form of a smaller value with a \code{K} or \code{M} suffix.
#' @export
#'
#' @examples
#' #not run
kilo_mega <- function(x){
  if(abs(x) < 1e4) paste0(x) else
    if(abs(x) < 1e5) paste0(round(x/1000, 1), "K") else
      if(abs(x) < 1e6) paste0(round(x/1000), "K") else
        paste0(round(x/1e6, 2), "M")
}

#' Plot observers
#'
#' Convenient wrapper around observers related to Shiny plot outputs.
#'
#' These observers require the app plots to be ggplot2 plots.
#' Note that `resetOnNew` in `plotOutput` should be set to \code{FALSE}.
#' These observers require a context where it can also be assumed that plot outputs are
#' properly isolated and responsive to updates initiated by the observers.
#' The plot functions themselves also take two data arguments, a brushed data frame that is a subset of rows from
#' a master data frame, and the master. Internally, these plots functions make their own determination
#' of how to plot brushed vs. unbrushed data.
#'
#' The reactive object passed via \code{dbrush} must be wrapped in \code{isolate}.
#' The reactive values object containing \code{rvx} and \code{rvy} must be named \code{rv_plots},
#' i.e., \code{rv_plots <- reactiveValues(x=NULL, y=NULL)}.
#' The reactive values object containing \code{rvbrush} must be named \code{rv},
#' i.e., \code{rv <- reactiveValues(brush=NULL)}.
#' \code{zoomable} is best set to \code{FALSE} for discrete x-axis plots like \code{geom_boxplot}
#' because zooming in on the discrete axis is problematic for the current brushed data observation implementation.
#'
#'
#' @param session Shiny \code{session} object.
#' @param input Shiny \code{input} object.
#' @param rv reactive values object passthrough.
#' @param rv_plots reactive values object passthrough.
#' @param dblclick character, double click input id specified in `plotOutput`.
#' @param brush character, brush input id specified in `plotOutput`.
#' @param dbrush reactive object containing data frame whose rows are based on the brushed region of the plot. See details.
#' @param rvx character, name of x coordinates object stored in a reactive values object. See details.
#' @param rvy character, name of y coordinates object stored in a reactive values object. See details.
#' @param rvbrush character, double click input id specified in `plotOutput`.
#' @param zoomable logical, whether the plot is intended to be zoomable on double click. Defaults to \code{TRUE}.
#'
#' @return this function is called for its observer side effects.
#' @export
#'
#' @examples
#' #not run
ggObserve <- function(session, input, rv, rv_plots, dblclick, brush, dbrush,
                      rvx, rvy=NULL, rvbrush=brush, zoomable=TRUE){
  # double click ggplot observation
  if(zoomable){
    shiny::observeEvent(input[[dblclick]], {
      brush <- input[[brush]]
      if(!is.null(brush)){
        rv_plots[[rvx]] <- c(brush$xmin, brush$xmax)
        if(!is.null(rvy)) rv_plots[[rvy]] <- c(brush$ymin, brush$ymax)
      } else {
        rv_plots[[rvx]] <- NULL
        if(!is.null(rvy)) rv_plots[[rvy]] <- NULL
      }
    })
    # zoom ggplot observation (reset brush on zoom)
    shiny::observe({
      x <- rv_plots[[rvx]]
      y <- if(is.null(rvy)) NULL else rv_plots[[rvy]]
      shiny::isolate(if(!is.null(x) | !is.null(y)) session$resetBrush(brush))
    })
  }
  # brushed data observation
  shiny::observeEvent(dbrush(), {
    if(is.null(dbrush())) return()
    if(is.null(rv[[rvbrush]]) || !identical(rv[[rvbrush]], dbrush()))
      rv[[rvbrush]] <- dbrush()
  })
}
