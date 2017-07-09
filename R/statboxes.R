#' Generate a set of stat boxes.
#'
#' Generate a set of six related stat boxes that summarize the distribution of values from a data frame.
#'
#' \code{clrs} must be length-one (will be repeated six times), -two (alternating colors), or -six (all colors individually specified).
#' Colors must be provided in hex format, e.g., \code{#FFFFFF} or a valid Shiny Dashboard color.
#' This function is typically called by \code{stat_boxes_group} but may be called directly when groups of stat boxes are not needed.
#' \code{text.size} and \code{value.size} are length-six or length-one (repeated) vectors. Defaults are better suited to
#' \code{type="valueBox"} than \code{type="infoBox"}. Stat box icons are fixed images and come with the \code{apputils} package.
#'
#' @param x a data frame.
#' @param type character, type of stat box set. Must be \code{"annual"} (default) or \code{"decadal"}.
#' @param style character, \code{"valueBox"} (default) or \code{"infoBox"}.
#' @param rnd decimal places to round data to for appearance in stat box (\code{valueBox} widget). Defaults to \code{0}.
#' @param clrs character, background colors of stat boxes. See details.
#' @param dec character, the sorted unique values of \code{x$Decade}. Typically provided by calling function. See \code{stat_boxes_group}.
#' @param main_title character, typically contains html. Defaults to \code{"<h4>Aggregate period statistics</h4>"}.
#' @param height character, stat box height. defaults to \code{"110px"}.
#' @param width.icon character, icon image width, defaults to \code{"90px"}.
#' @param text.size numeric, scaling for text size, defaults to \code{75}. See details.
#' @param value.size numeric, scaling for value size, defaults to \code{150}. See details.
#'
#' @return a \code{shiny::tagList} containing a heading and a fluid row of six stat boxes.
#' @export
#'
#' @examples
#' #not run
stat_boxes <- function(x, type="annual", style="valueBox", rnd=0, clrs=c("light-blue", "blue"), dec,
                       main_title="<h4>Aggregate period statistics</h4>",
                       height="110px", width.icon="90px", text.size=75, value.size=150){
  if(!type %in% c("annual", "decadal")) stop("type must be 'annual' or 'decadal'.")
  if(!length(clrs) %in% c(1,2,6)) stop("Invalid color vector.")
  if(length(clrs)==2) clrs <- clrs[c(1,1,2,1,2,2)] else if(length(clrs)==1) clrs <- rep(clrs, 6)
  if(type=="annual"){
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
    val <- purrr::map2(statval, value.size, ~pTextSize(.x, .y))
    text <- purrr::map2(statlab, text.size, ~pTextSize(.x, .y, margin=0))
    if(style=="valueBox"){
      y <- purrr::map(seq_along(text), ~valueBox(val[[.x]], text[[.x]],
        apputils::icon(list(src=src[.x], width=width.icon), lib="local"), clrs[.x], width=NULL))
    } else {
      y <- purrr::map(seq_along(text), ~infoBox(text[[.x]], val[[.x]], NULL,
        apputils::icon(list(src=src[.x], width=width.icon), lib="local"), clrs[.x], width=NULL))
    }
    names(y) <- c("mean", "min", "max", "med", "iqr", "sd")
    x <- shiny::tagList(
      shiny::HTML(main_title),
      shiny::fluidRow(
        shiny::tags$style(shiny::HTML(paste0(".small-box {height: ", height, " !important;}"))),
        shiny::column(2, y$mean), shiny::column(2, y$sd), shiny::column(2, y$med),
        shiny::column(2, y$iqr), shiny::column(2, y$min), shiny::column(2, y$max)
      )
    )
    return(x)
  } else {
    dots <- paste0("mean(Val)")
    pr <- !x$Var[1] %in% c("tas", "tasmin", "tasmax")
    x <- dplyr::group_by(x, Decade) %>% dplyr::summarise_(.dots=list(Decadal_mean=dots)) %>%
      dplyr::rename(Val=Decadal_mean)
    v <- "Val"
    idx.mn <- which.min(x[[v]])
    idx.mx <- which.max(x[[v]])
    idx.dn <- if(nrow(x)==1) NA else seq(which.min(diff(x[[v]])), length.out=2)
    idx.up <- if(nrow(x)==1) NA else seq(which.max(diff(x[[v]])), length.out=2)
    tot <- tail(x[[v]], 1) - x[[v]][1]
    tot2 <- if(pr){
      ifelse(tot < 1 & tot > 0, 1, ifelse(tot < 0 & tot > -1, -1, round(tot)))
    } else round(tot, rnd)
    x <<- x
    pct <- if(!pr) NA else paste0(round(100*(tail(x[[v]], 1) / x[[v]][1] - 1)), "%")

    statval <- list(
      mn=kilo_mega(round(x[[v]][idx.mn], rnd)),
      mx=kilo_mega(round(x[[v]][idx.mx], rnd)),
      dn=if(is.na(idx.dn[1])) NA else kilo_mega(round(diff(x[[v]])[idx.dn[1]], rnd)),
      up=if(is.na(idx.up[1])) NA else kilo_mega(round(diff(x[[v]])[idx.up[1]], rnd)),
      totdif=kilo_mega(tot2),
      totpct=pct
    )

    src.dnup <- c("stat_icon_bar_deltaNeg_white.png", "stat_icon_bar_deltaPos_white.png")
    low.change <- "Max loss"
    high.change <- "Max gain"
    if(!is.na(statval$dn[1]) && statval$dn > 0) { src.dnup[1] <- src.dnup[2]; low.change <- "Min Gain" }
    if(!is.na(statval$up[1]) && statval$up < 0) { src.dnup[2] <- src.dnup[1]; high.change <- "Min loss" }
    if(tot < 0){
      src.totals <- c("stat_icon_ts_deltaDec_white.png", "stat_icon_ts_deltaPctDec_white.png")
    } else {
      src.totals <- c("stat_icon_ts_deltaInc_white.png", "stat_icon_ts_deltaPctInc_white.png")
    }
    dec <- if(nrow(x)==1) paste(x$Decade[1]) else paste(x$Decade[c(1, nrow(x))], collapse=" - ")

    statlab <- list(
      c("Min", paste(x$Decade[idx.mn])),
      c("Max", paste(x$Decade[idx.mx])),
      c(low.change, paste(x$Decade[idx.dn], collapse=" - ")),
      c(high.change, paste(x$Decade[idx.up], collapse=" - ")),
      c("Total change", dec),
      c("% change", dec)
    )
    val <- purrr::map2(statval, value.size, ~pTextSize(.x, .y))
    text <- purrr::map2(statlab, text.size, ~pTextSize(.x, .y, margin=0))
    src <- c("stat_icon_normal_min_white.png", "stat_icon_normal_max_white.png", src.dnup[1], src.dnup[2], src.totals[1], src.totals[2])
    if(style=="valueBox"){
      y <- purrr::map(seq_along(text), ~valueBox(val[[.x]], text[[.x]],
        apputils::icon(list(src=src[.x], width=width.icon), lib="local"), clrs[.x], width=NULL))
    } else {
      y <- purrr::map(seq_along(text), ~infoBox(text[[.x]], val[[.x]], NULL,
        apputils::icon(list(src=src[.x], width=width.icon), lib="local"), clrs[.x], width=NULL))
    }
    y <- list(
      mn=valueBox(val[[1]], text[[1]], icon=icon(list(src="stat_icon_normal_min_white.png", width="90px"), lib="local"), color=clrs[1], width=NULL),
      mx=valueBox(val[[2]], text[[2]], icon=icon(list(src="stat_icon_normal_max_white.png", width="90px"), lib="local"), color=clrs[2], width=NULL),
      dn=valueBox(val[[3]], text[[3]], icon=icon(list(src=src.dnup[1], width="90px"), lib="local"), color=clrs[3], width=NULL),
      up=valueBox(val[[4]], text[[4]], icon=icon(list(src=src.dnup[2], width="90px"), lib="local"), color=clrs[4], width=NULL),
      totdif=valueBox(val[[5]], text[[5]], icon=icon(list(src=src.totals[1], width="90px"), lib="local"), color=clrs[5], width=NULL),
      totpct=valueBox(val[[6]], text[[6]], icon=icon(list(src=src.totals[2], width="90px"), lib="local"), color=clrs[6], width=NULL)
    )
    names(y) <- c("mn", "mx", "dn", "up", "totdif", "totpct")
    x <- shiny::tagList(
      shiny::HTML(main_title),
      shiny::fluidRow(
        shiny::tags$style(shiny::HTML(paste0(".small-box {height: ", height, " !important;}"))),
        shiny::column(2, y$totdif), shiny::column(2, y$totpct), shiny::column(2, y$dn),
        shiny::column(2, y$up), shiny::column(2, y$mn), shiny::column(2, y$mx)
      )
    )
  }
}

#' Generate a group of stat boxes sets.
#'
#' Generate a group of multiple sets of six related stat boxes that summarize the distribution of values from a multiple data frames.
#'
#' \code{text.size} and \code{value.size} are length-six or length-one (repeated) vectors. Defaults are better suited to
#' \code{type="valueBox"} than \code{type="infoBox"}. See \code{stat_boxes} directly for more information on internal function.
#' If there is a need to have more complete control over customizing content of value or info boxes, use \code{valueBox}
#' or \code{infoBox} directly. \code{stat_boxes} and \code{stat_boxes_group} are two levels of wrapper functions that are intended to
#' produce specific content for summarizing values in a data frame, pooled or by group, repsectively.
#'
#' @param x a  master data frame, which may or may not be split into multiple groups, depending on \code{clrby}.
#' @param clrby character, column name of \code{x} to color groups of stat box sets by. May be \code{NULL}.
#' @param clrs.default list of color vectors, one vector for each group.
#' @param type character, type of stat box set. Must be \code{"annual"} (default) or \code{"decadal"}.
#' @param style character, \code{"valueBox"} (default) or \code{"infoBox"}.
#' @param rnd decimal places to round data to for appearance in stat box (\code{valueBox} widget). Defaults to \code{0}.
#' @param main_title character, typically contains html. Defaults to \code{"<h4>Aggregate period statistics</h4>"}.
#' @param height character, stat box height. defaults to \code{"110px"}.
#' @param width.icon character, icon image width, defaults to \code{"90px"}.
#' @param text.size numeric, scaling for text size, defaults to \code{75}. See details.
#' @param value.size numeric, scaling for value size, defaults to \code{150}. See details.
#' @param prevent logical, whether stat boxes should be prevented (contextual, e.g., no data available in app).
#'
#' @return a group list of tag lists containing stat box sets.
#' @export
#'
#' @examples
#' #not run
stat_boxes_group <- function(x, clrby, clrs.default=list(c("light-blue", "blue")), type="annual", style="valueBox", rnd=0,
  main_title="<h4>Aggregate period statistics</h4>", height="110px", width.icon="90px", text.size=75, value.size=150, prevent){
  if(!style %in% c("valueBox", "infoBox")) stop("Invalid style argument.")
  if(length(style)==1) style <- rep(style, 6)
  if(length(style) != 6) stop("style argument must be vector of length one or six.")
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
  purrr::map(seq_along(x), ~stat_boxes(x[[.x]], type, style[.x], rnd, gsub("#", "", clrs[[.x]]),
                                       dec, subtitles[[.x]], height, width.icon, text.size, value.size))
}
