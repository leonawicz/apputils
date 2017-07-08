#' Generate a set of stat boxes.
#'
#' Generate a set of six related stat boxes that summarize the distribution of values from a data frame.
#'
#' \code{clrs} must be length-one (will be repeated six times), -two (alternating colors), or -six (all colors individually specified).
#' Colors must be provided in hex format, e.g., \code{#FFFFFF} or a valid Shiny Dashboard color.
#' This function is typically called by \code{stat_boxes_group} but may be called directly when groups of stat boxes are not needed.
#' \code{...} arguments may include \code{src}, a character vector of paths to six local image files to be used as valueBox icons.
#' Standard icons for annual and decadal stat box sets are used by default. \code{w} for icon width, if not provided, defaults to \code{"90px"}.
#'
#' @param x a data frame.
#' @param type character, type of stat box set. Must be \code{"annual"} (default) or \code{"decadal"}.
#' @param rnd decimal places to round data to for appearance in stat box (\code{valueBox} widget). Defaults to \code{0}.
#' @param clrs character, background colors of stat boxes. See details.
#' @param dec character, the sorted unique values of \code{x$Decade}. Typically provided by calling function. See \code{stat_boxes_group}.
#' @param main_title character, typically contains html. Defaults to \code{"<h4>Aggregate period statistics</h4>"}.
#' @param ... additional arguments passed to valueBox. See details.
#'
#' @return a \code{shiny::tagList} containing a heading and a fluid row of six stat boxes.
#' @export
#'
#' @examples
#' #not run
stat_boxes <- function(x, type="annual", rnd=0, clrs=c("light-blue", "blue"), dec,
                       main_title="<h4>Aggregate period statistics</h4>", ...){
  if(!type %in% c("annual", "decadal")) stop("type must be 'annual' or 'decadal'.")
  if(!length(clrs) %in% c(1,2,6)) stop("Invalid color vector.")
  if(length(clrs)==2) clrs <- clrs[c(1,1,2,1,2,2)] else if(length(clrs)==1) clrs <- rep(clrs, 6)
  if(is.null(list(...)$w)) w <- "90px"
  if(type=="annual"){
    if(is.null(list(...)$src))
      src <- paste0("stat_icon_",
                    c("normal_mean", "normal_min", "normal_max", "normal_median", "boxplot_iqr", "normal_sd"),
                    "_white.png")
    x <- dplyr::ungroup(x) %>% dplyr::summarise_(.dots=list(
      Mean_=paste0("mean(Val)"),
      Min_=paste0("min(Val)"),
      Max_=paste0("max(Val)"),
      Median_=paste0("stats::median(Val)"),
      Pct25_=paste0("stats::quantile(Val, prob=0.25)"),
      Pct75_=paste0("stats::quantile(Val, prob=0.75)"),
      SD_=paste0("stats::sd(Val)")
    )) %>% round(rnd) %>% unlist %>% purrr::map_chr(~kilo_mega(.x))

    statval <- c(x[1:4], paste(x[5], "-", x[6]), x[7])
    statlab <- list(
      c("Mean", dec), c("Min", dec), c("Max", dec), c("Median", dec), c("IQR", dec), c("Std Dev", dec)
    )

    val <- purrr::map2(statval, c(rep(75, 4), 75, 75), ~pTextSize(.x, .y))
    text <- purrr::map2(statlab, rep(150, 6), ~pTextSize(.x, .y, margin=0))
    y <- list(
      mean=apputils::valueBox(val[[1]], text[[1]], icon=apputils::icon(list(src=src[1], width=w), lib="local"), color=clrs[1], width=NULL),
      min=apputils::valueBox(val[[2]], text[[2]], icon=apputils::icon(list(src=src[2], width=w), lib="local"), color=clrs[2], width=NULL),
      max=apputils::valueBox(val[[3]], text[[3]], icon=apputils::icon(list(src=src[3], width=w), lib="local"), color=clrs[3], width=NULL),
      med=apputils::valueBox(val[[4]], text[[4]], icon=apputils::icon(list(src=src[4], width=w), lib="local"), color=clrs[4], width=NULL),
      iqr=apputils::valueBox(val[[5]], text[[5]], icon=apputils::icon(list(src=src[5], width=w), lib="local"), color=clrs[5], width=NULL),
      sd=apputils::valueBox(val[[6]], text[[6]], icon=apputils::icon(list(src=src[6], width=w), lib="local"), color=clrs[6], width=NULL)
    )
    x <- shiny::tagList(
      shiny::HTML(main_title),
      shiny::fluidRow(
        shiny::tags$head(shiny::tags$style(shiny::HTML(".small-box {height: 110px}"))),
        shiny::column(2, y$mean), shiny::column(2, y$sd), shiny::column(2, y$med),
        shiny::column(2, y$iqr), shiny::column(2, y$min), shiny::column(2, y$max)
      )
    )
    return(x)
  } else {
    # add later
    #src <- paste0("stat_icon_",
    #              c("normal_mean", "normal_min", "normal_max", "normal_median", "boxplot_iqr", "normal_sd"),
    #              "_white.png")
  }
}

#' Generate a group of stat boxes sets.
#'
#' Generate a group of multiple sets of six related stat boxes that summarize the distribution of values from a multiple data frames.
#'
#' \code{...} arguments may include \code{src}, a character vector of paths to six local image files to be used as valueBox icons.
#' Standard icons for annual and decadal stat box sets are used by default. \code{w} for icon width, if not provided, defaults to \code{"90px"}.
#'
#' @param x a  master data frame, which may or may not be split into multiple groups, depending on \code{clrby}.
#' @param clrby character, column name of \code{x} to color groups of stat box sets by. May be \code{NULL}.
#' @param clrs.default list of color vectors, one vector for each group.
#' @param type character, type of stat box set. Must be \code{"annual"} (default) or \code{"decadal"}.
#' @param rnd decimal places to round data to for appearance in stat box (\code{valueBox} widget). Defaults to \code{0}.
#' @param main_title character, typically contains html. Defaults to \code{"<h4>Aggregate period statistics</h4>"}.
#' @param prevent logical, whether stat boxes should be prevented (contextual, e.g., no data available in app).
#' @param ... additional arguments passed to \code{stat_boxes}. See details.
#'
#' @return a group list of tag lists containing stat box sets.
#' @export
#'
#' @examples
#' #not run
stat_boxes_group <- function(x, clrby, clrs.default=list(c("light-blue", "blue")), type="annual", rnd=0,
  main_title="<h4>Aggregate period statistics</h4>", prevent, ...){

  dec <- as.character(sort(unique(x$Decade)))
  if(length(dec) > 1) dec <- paste(dec[1], dec[length(dec)], sep=" - ")
  if(prevent || nrow(x)==0) return()
  if(is.null(clrby)){
    x <- list(x)
    clrs <- clrs.default
    subtitles <- list(main_title)
  } else {
    lev <- levels(x[[clrby]])
    x <- split(x, x[[clrby]])
    clrs <- as.list(tolpal(length(x)))
    idx <- which(purrr::map_lgl(x, ~nrow(.x) > 0))
    x <- x[idx]
    clrs <- clrs[idx]
    lev <- lev[lev %in% names(x)]
    x <- x[match(lev, names(x))]
    subtitles <- paste0("<h5>", names(x), "</h5>")
    subtitles[1] <- paste0(main_title, subtitles[1])
  }
  purrr::map(seq_along(x), ~stat_boxes(x[[.x]], type=type, rnd=rnd,
                                       clrs=gsub("#", "", clrs[[.x]]),
                                       dec=dec, main_title=subtitles[[.x]], ...))
}
