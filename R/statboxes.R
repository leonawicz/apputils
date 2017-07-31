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
#' @param dec character, the sorted unique values of \code{x$Decade}. Typically provided by calling function. See \code{stat_boxes_group}.
#' @param height character, stat box height. defaults to \code{"110px"}.
#' @param width.icon character, icon image width, defaults to \code{"90px"}.
#' @param text.size numeric, scaling for text size, defaults to \code{75}. See details.
#' @param value.size numeric, scaling for value size, defaults to \code{150}. See details.
#' @param output character, \code{"boxes"} (default) or \code{"list"}.
#' A list is useful for returning raw output, e.g. for sending to a rmarkdown report.
#' @param main_title character, typically contains html. Defaults to \code{"<h4>Aggregate period statistics</h4>"}.
#' @param clrs character, background colors of stat boxes. See details.
#' @param theme color theme, either \code{"white"} (default) or \code{"black"}.
#'
#' @return a \code{shiny::tagList} containing a heading and a fluid row of six stat boxes.
#' @export
#'
#' @examples
#' #not run
stat_boxes <- function(x, type="annual", style="valueBox", rnd=0, dec, height="110px",
  width.icon="90px", text.size=75, value.size=150, output="boxes",
  main_title="<h4>Aggregate period statistics</h4>", clrs=c("light-blue", "blue"), theme="white"){

  if(!type %in% c("annual", "decadal")) stop("type must be 'annual' or 'decadal'.")
  if(!length(clrs) %in% c(1,2,6)) stop("Invalid color vector.")
  if(length(clrs)==2) clrs <- clrs[c(1,1,2,1,2,2)] else if(length(clrs)==1) clrs <- rep(clrs, 6)
  if(type=="annual"){
    src <- statIcon(c("mean", "min", "max", "median", "iqr", "sd"), theme)
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
    if(output=="list"){
      names(statval) <- purrr::map_chr(statlab, ~.x[1])
      return(statval)
    }
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
    y <- shiny::tagList(
      shiny::HTML(main_title),
      shiny::fluidRow(
        shiny::tags$style(shiny::HTML(paste0(".small-box {height: ", height, " !important;}"))),
        shiny::column(2, y$mean), shiny::column(2, y$sd), shiny::column(2, y$med),
        shiny::column(2, y$iqr), shiny::column(2, y$min), shiny::column(2, y$max)
      )
    )
    return(y)
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
    tot <- utils::tail(x[[v]], 1) - x[[v]][1]
    tot2 <- if(pr){
      ifelse(tot < 1 & tot > 0, 1, ifelse(tot < 0 & tot > -1, -1, round(tot)))
    } else round(tot, rnd)
    pct <- if(!pr) NA else paste0(round(100*(utils::tail(x[[v]], 1) / x[[v]][1] - 1)), "%")

    statval <- list(
      mn=kilo_mega(round(x[[v]][idx.mn], rnd)),
      mx=kilo_mega(round(x[[v]][idx.mx], rnd)),
      dn=if(is.na(idx.dn[1])) NA else kilo_mega(round(diff(x[[v]])[idx.dn[1]], rnd)),
      up=if(is.na(idx.up[1])) NA else kilo_mega(round(diff(x[[v]])[idx.up[1]], rnd)),
      totdif=kilo_mega(tot2),
      totpct=pct
    )

    src.dnup <- statIcon(c("bardec", "barinc"), theme)
    low.change <- "Max loss"
    high.change <- "Max gain"
    if(!is.na(statval$dn[1]) && statval$dn > 0) { src.dnup[1] <- src.dnup[2]; low.change <- "Min Gain" }
    if(!is.na(statval$up[1]) && statval$up < 0) { src.dnup[2] <- src.dnup[1]; high.change <- "Min loss" }
    if(tot < 0){
      src.totals <- statIcon(c("dec", "pctdec"), theme)
    } else {
      src.totals <- statIcon(c("inc", "pctinc"), theme)
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
    if(output=="list"){
      statval <- purrr::map_chr(statval, ~as.character(.x))
      names(statval) <- purrr::map_chr(statlab, ~.x[1])
      return(statval)
    }
    val <- purrr::map2(statval, value.size, ~pTextSize(.x, .y))
    text <- purrr::map2(statlab, text.size, ~pTextSize(.x, .y, margin=0))
    src <- c(statIcon(c("min", "max"), theme), src.dnup[1], src.dnup[2], src.totals[1], src.totals[2])
    if(style=="valueBox"){
      y <- purrr::map(seq_along(text), ~valueBox(val[[.x]], text[[.x]],
        apputils::icon(list(src=src[.x], width=width.icon), lib="local"), clrs[.x], width=NULL))
    } else {
      y <- purrr::map(seq_along(text), ~infoBox(text[[.x]], val[[.x]], NULL,
        apputils::icon(list(src=src[.x], width=width.icon), lib="local"), clrs[.x], width=NULL))
    }
    names(y) <- c("mn", "mx", "dn", "up", "totdif", "totpct")
    shiny::tagList(
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
#' @param type character, type of stat box set. Must be \code{"annual"} (default) or \code{"decadal"}.
#' @param style character, \code{"valueBox"} (default) or \code{"infoBox"}.
#' @param rnd decimal places to round data to for appearance in stat box (\code{valueBox} widget). Defaults to \code{0}.
#' @param height character, stat box height. defaults to \code{"110px"}.
#' @param width.icon character, icon image width, defaults to \code{"90px"}.
#' @param text.size numeric, scaling for text size, defaults to \code{75}. See details.
#' @param value.size numeric, scaling for value size, defaults to \code{150}. See details.
#' @param output character, \code{"boxes"} (default) or \code{"list"}.
#' A list is useful for returning raw output, e.g. for sending to a rmarkdown report.
#' @param main_title character, typically contains html. Defaults to \code{"<h4>Aggregate period statistics</h4>"}.
#' @param clrs.default list of color vectors, one vector for each group.
#' @param prevent logical, whether stat boxes should be prevented (contextual, e.g., no data available in app).
#' @param theme color theme, either \code{"white"} (default) or \code{"black"}.
#'
#' @return a group list of tag lists containing stat box sets.
#' @export
#'
#' @examples
#' #not run
stat_boxes_group <- function(x, clrby, type="annual", style="valueBox", rnd=0, height="110px",
  width.icon="90px", text.size=75, value.size=150, output="boxes", main_title="<h4>Aggregate period statistics</h4>",
  clrs.default=list(c("light-blue", "blue")), prevent, theme="white"){

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
  purrr::map(seq_along(x), ~stat_boxes(x[[.x]], type, style[.x], rnd, dec, height, width.icon, text.size,
                                       value.size, output, subtitles[[.x]], gsub("#", "", clrs[[.x]]), theme))
}

#' Accessor for apputils icons
#'
#' Access path to local \code{apputils} stat box icon image files.
#'
#' This accessor function provides convenient access to local package resources.
#' Specifically, given an ID, it returns the resource path to a stat box icon png file.
#' This is typically used in a Shiny app that has \code{apputils} loaded.
#' Available options include:
#' \describe{
#'   \item{normal}{Normal distribution density curve}
#'   \item{min}{Normal curve with minimum line and statistic notation}
#'   \item{max}{As above, for the maximum}
#'   \item{mean}{As above, for the mean}
#'   \item{sd}{As above, with two lines marking +/- one standard deviation}
#'   \item{median}{As above, for the median}
#'   \item{iqr}{Box plot with interquartile range marked and labeled}
#'   \item{b0}{Beta zero hat for linear models, scatter plot with linear model intercept highlight}
#'   \item{b1}{Beta one hat for linear models, scatter plot with linear model slope highlight}
#'   \item{r2}{R-squared, scatter plot with correlation highlight}
#'   \item{pvalue}{Normal distribution curve with lower tail highlight}
#'   \item{inc}{Increasing trend line with delta change symbol}
#'   \item{dec}{As above but decreasing}
#'   \item{pctinc}{As above but with percentage symbol added}
#'   \item{pctdec}{As above but decreasing and with percentage symbol added}
#'   \item{barinc}{Increasing consecutive bars with arrow change indicators}
#'   \item{bardec}{As above but decreasing}
#' }
#'
#' @param id character, id to be matched to a stat box icon.
#' @param theme character, \code{"white"} (default) or \code{"black"}.
#'
#' @return character string specifying the local image file location in the \code{apputils} package.
#' @export
#'
#' @examples
#' statIcon("normal")
statIcon <- function(id, theme="white"){
  if(!theme %in% c("white", "black")) stop("theme must be 'white' or 'black'.")
  iconopts <- c('normal', 'min', 'max', 'mean', 'sd', 'median', 'iqr', 'b0', 'b1',
                'r2', 'pvalue', 'inc', 'dec', 'pctinc', 'pctdec', 'barinc', 'bardec')
  if(!all(id %in% iconopts)) stop("Invalid id. See help for options.")
  x <- sapply(id, function(x) switch(x,
              normal="stat_icon_normal_dist",
              min="stat_icon_normal_min",
              max="stat_icon_normal_max",
              mean="stat_icon_normal_mean",
              sd="stat_icon_normal_sd",
              median="stat_icon_normal_median",
              iqr="stat_icon_boxplot_iqr",
              b0="stat_icon_pars_b0hat",
              b1="stat_icon_pars_b1hat",
              r2="stat_icon_pars_r2",
              pvalue="stat_icon_pars_pvalue",
              inc="stat_icon_ts_deltaInc",
              dec="stat_icon_ts_deltaDec",
              pctinc="stat_icon_ts_deltaPctInc",
              pctdec="stat_icon_ts_deltaPctDec",
              barinc="stat_icon_bar_deltaPos",
              bardec="stat_icon_bar_deltaNeg"))
  paste0("resources/images/", x, "_", theme, ".png")
}

#' Generate png thumbnail icons for various quantities
#'
#' Generate png thumbnail icons for various quantities including common statistics, model parameter estimates,
#' trends and rates of change.
#'
#' The standard white and dark theme icon versions are procuded using the default call, \code{makeIcons()},
#' and \code{makeIcons("black", "gray30", "black")}, respectively.
#' The function code is repetetive and includes hardcoded values because each icon is uniquely tailored,
#' for example, for proper text placement. This function serves as a package reference to how the standard icon set
#' is created from scratch.
#'
#' @param primary_color primary color.
#' @param secondary_color secondary color.
#' @param color_theme color theme.
#' @param dir output directory.
#'
#' @return This function is used for its png file-generating side effects.
#' @export
#'
#' @examples
#' #not run
makeIcons <- function(primary_color="#FFFFFF", secondary_color="#FFFFFF75", color_theme="white", dir="."){
  # setup
  set.seed(1)
  xlm <- c(-4.5, 4.5)
  x <- seq(xlm[1], xlm[2], length=1000)
  y <- dnorm(x)
  x2 <- rnorm(500000)
  x2 <- x2[x2 > xlm[1] & x2 < xlm[2]]
  mar <- c(0.1, 0.1, 0.1 ,0.1)
  sysfonts::font.add("cam", "cambriaz.TTF")
  showtext::showtext.auto()

  # distribution icons
  Cairo::CairoPNG(paste0(dir, "/stat_icon_normal_dist_", color_theme, ".png"), width=96, height=96, bg="transparent")
  par(lwd=2, mar=mar, family="cam")
  plot(x, y, type="n", axes=FALSE, xlab="", ylab="", xlim=xlm)
  hist(x2, breaks=seq(xlm[1], xlm[2], by=1), freq=FALSE, add=TRUE, border=primary_color)
  lines(x, y, col=primary_color)
  dev.off()

  Cairo::CairoPNG(paste0(dir, "/stat_icon_normal_mean_", color_theme, ".png"), width=96, height=96, bg="transparent")
  par(lwd=2, mar=mar, family="cam")
  plot(x, y, type="n", axes=FALSE, xlab="", ylab="", xlim=xlm)
  hist(x2, breaks=seq(xlm[1], xlm[2], by=1), freq=FALSE, add=TRUE, border=secondary_color)
  lines(x, y, col=secondary_color)
  abline(v=0, lwd=3, lty=2, col=primary_color)
  legend("topright", legend=expression(bolditalic(bar(x))), bty ="n", pch=NA, cex=3,  yjust=1, adj=c(-0.5, 0), text.col=primary_color)
  dev.off()

  Cairo::CairoPNG(paste0(dir, "/stat_icon_normal_min_", color_theme, ".png"), width=96, height=96, bg="transparent")
  par(lwd=2, mar=mar, family="cam")
  plot(x, y, type="n", axes=FALSE, xlab="", ylab="", xlim=xlm)
  hist(x2, breaks=seq(xlm[1], xlm[2], by=1), freq=FALSE, add=TRUE, border=secondary_color)
  lines(x, y, col=secondary_color)
  abline(v=xlm[1], lwd=3, lty=2, col=primary_color)
  legend("topright", legend=expression(bolditalic(x[(1)])), bty ="n", pch=NA, cex=1.8, adj=c(-0.275, 0), text.col=primary_color)
  dev.off()

  Cairo::CairoPNG(paste0(dir, "/stat_icon_normal_max_", color_theme, ".png"), width=96, height=96, bg="transparent")
  par(lwd=2, mar=mar, family="cam")
  plot(x, y, type="n", axes=FALSE, xlab="", ylab="", xlim=xlm)
  hist(x2, breaks=seq(xlm[1], xlm[2], by=1), freq=FALSE, add=TRUE, border=secondary_color)
  lines(x, y, col=secondary_color)
  abline(v=xlm[2], lwd=3, lty=2, col=primary_color)
  legend("topleft", legend=expression(bolditalic(x[(n)])), bty ="n", pch=NA, cex=1.8,  adj=c(1, 0), text.col=primary_color)
  dev.off()

  Cairo::CairoPNG(paste0(dir, "/stat_icon_normal_median_", color_theme, ".png"), width=96, height=96, bg="transparent")
  par(lwd=2, mar=mar, family="cam")
  plot(x, y, type="n", axes=FALSE, xlab="", ylab="", xlim=xlm)
  hist(x2, breaks=seq(xlm[1], xlm[2], by=1), freq=FALSE, add=TRUE, border=secondary_color)
  lines(x, y, col=secondary_color)
  abline(v=0, lwd=3, lty=2, col=primary_color)
  legend("topright", legend=expression(bolditalic(tilde(x))), bty ="n", pch=NA, cex=3,  adj=c(-0.5, 0), text.col=primary_color)
  dev.off()

  Cairo::CairoPNG(paste0(dir, "/stat_icon_normal_sd_", color_theme, ".png"), width=96, height=96, bg="transparent")
  par(lwd=2, mar=mar, family="cam")
  plot(x, y, type="n", axes=FALSE, xlab="", ylab="", xlim=xlm)
  hist(x2, breaks=seq(xlm[1], xlm[2], by=1), freq=FALSE, add=TRUE, border=secondary_color)
  lines(x, y, col=secondary_color)
  abline(v=c(-1,1), lwd=3, lty=2, col=primary_color)
  legend("topright", legend=expression(bolditalic(s)), bty="n", pch=NA, cex=3, adj=c(-0.5, 0), text.col=primary_color)
  dev.off()

  Cairo::CairoPNG(paste0(dir, "/stat_icon_pars_pvalue_", color_theme, ".png"), width=96, height=96, bg="transparent")
  par(lwd=2, mar=mar, family="cam")
  plot(x, y, type="n", axes=FALSE, xlab="", ylab="", xlim=xlm)
  hist(x2, breaks=seq(xlm[1], xlm[2], by=1), freq=FALSE, add=TRUE, border=secondary_color)
  qn <- qnorm(0.05)
  idx <- which(x <= qn)
  polygon(c(x[idx], rev(x[idx])), c(y[idx], rep(0, length(idx))), col=primary_color, border=NA)
  lines(x, y, col=secondary_color)
  abline(v=qn, lwd=3, lty=2, col=primary_color)
  arrows(qn, 0.1, min(x), 0.1, lwd=3, col=primary_color, length=0.15)
  legend("topright", legend=expression(bolditalic(p)), bty ="n", pch=NA, cex=3,  yjust=1, adj=c(-0.5, 0), text.col=primary_color)
  dev.off()

  showtext::showtext.auto(enable=FALSE)

  Cairo::CairoPNG(paste0(dir, "/stat_icon_boxplot_iqr_", color_theme, ".png"), width=96, height=96, bg="transparent")
  par(lwd=2, mar=mar, family="cam")
  boxplot(x2, outline=FALSE, axes=FALSE, frame=FALSE,  lty=1, border=secondary_color, boxcol=primary_color)
  text(1.35, -0.05, expression("}"), cex=2, col=primary_color)
  showtext::showtext.begin()
  text(1.35, 1.5, expression("IQR"), cex=1.5, col=primary_color)
  showtext::showtext.end()
  dev.off()

  showtext::showtext.auto()

  # time series icons
  y <- scale(c(0.3,0.4,2,0.7,2,1.5,3.5,2.75,4))
  x <- scale(seq_along(y))

  Cairo::CairoPNG(paste0(dir, "/stat_icon_ts_deltaDec_", color_theme, ".png"), width=96, height=96, bg="transparent")
  par(lwd=2, mar=mar, family="cam")
  plot(0,0, type="n", axes=FALSE, xlab="", ylab="", xlim=range(x), ylim=range(y))
  lines(x, rev(y), lty=2, col=secondary_color)
  arrows(x[1], y[length(y)], x[length(x)], y[1], lwd=3, col=primary_color)
  legend("topright", legend=expression(bolditalic(Delta)), bty ="n", pch=NA, cex=1.8, adj=c(-0.75, 0), text.col=primary_color)
  dev.off()

  Cairo::CairoPNG(paste0(dir, "/stat_icon_ts_deltaInc_", color_theme, ".png"), width=96, height=96, bg="transparent")
  par(lwd=2, mar=mar, family="cam")
  plot(0,0, type="n", axes=FALSE, xlab="", ylab="", xlim=range(x), ylim=range(y))
  lines(x, y, lty=2, col=secondary_color)
  arrows(x[1], y[1], x[length(x)], y[length(y)], lwd=3, col=primary_color)
  legend("topleft", legend=expression(bolditalic(Delta)), bty ="n", pch=NA, cex=1.8,  adj=c(2.5, 0), text.col=primary_color)
  dev.off()

  Cairo::CairoPNG(paste0(dir, "/stat_icon_ts_deltaPctDec_", color_theme, ".png"), width=96, height=96, bg="transparent")
  par(lwd=2, mar=mar, family="cam")
  plot(0,0, type="n", axes=FALSE, xlab="", ylab="", xlim=range(x), ylim=range(y))
  lines(x, rev(y), lty=2, col=secondary_color)
  arrows(x[1], y[length(y)], x[length(x)], y[1], lwd=3, col=primary_color)
  legend("topright", legend=expression(bolditalic(symbol("\045")~Delta)), bty ="n", pch=NA, cex=1.8, adj=c(-0.25, 0), text.col=primary_color)
  dev.off()

  Cairo::CairoPNG(paste0(dir, "/stat_icon_ts_deltaPctInc_", color_theme, ".png"), width=96, height=96, bg="transparent")
  par(lwd=2, mar=mar, family="cam")
  plot(0,0, type="n", axes=FALSE, xlab="", ylab="", xlim=range(x), ylim=range(y))
  lines(x, y, lty=2, col=secondary_color)
  arrows(x[1], y[1], x[length(x)], y[length(y)], lwd=3, col=primary_color)
  legend("topleft", legend=expression(bolditalic(symbol("\045")~Delta)), bty ="n", pch=NA, cex=1.8,  adj=c(0.9, 0), text.col=primary_color)
  dev.off()

  # bar icons
  Cairo::CairoPNG(paste0(dir, "/stat_icon_bar_deltaNeg_", color_theme, ".png"), width=96, height=96, bg="transparent")
  par(lwd=2, mar=mar, family="cam")
  barplot(c(4,1), axes=FALSE, lty=1, border=primary_color, col=secondary_color)
  arrows(1.6, 4, 1.6, 1.2, lwd=3, col=primary_color)
  legend("topright", legend=expression(bolditalic(Delta)), bty ="n", pch=NA, cex=1.8, adj=c(-0.5, 0), text.col=primary_color)
  dev.off()

  Cairo::CairoPNG(paste0(dir, "/stat_icon_bar_deltaPos_", color_theme, ".png"), width=96, height=96, bg="transparent")
  par(lwd=2, mar=mar, family="cam")
  barplot(c(1,4), axes=FALSE, lty=1, border=primary_color, col=secondary_color)
  arrows(1, 1.2, 1, 4, lwd=3, col=primary_color)
  legend("topleft", legend=expression(bolditalic(Delta)), bty ="n", pch=NA, cex=1.8, adj=c(2.5, 0), text.col=primary_color)
  dev.off()

  # linear models
  set.seed(7)
  x <- rnorm(25, sd=1.5)
  y <- 0.5*x + rnorm(25, sd=0.5)
  lm1 <- lm(y ~ x)
  Cairo::CairoPNG(paste0(dir, "/stat_icon_pars_r2_", color_theme, ".png"), width=96, height=96, bg="transparent")
  par(lwd=2, mar=mar, family="cam")
  plot(0,0, type="n", axes=FALSE, xlab="", ylab="", xlim=range(x), ylim=range(y) + c(0, 0.1))
  axis(side=1, at=range(x) + c(-1, 1)*c(diff(range(x))*0.04), labels=FALSE, lwd.ticks=0, lwd=3, col=primary_color)
  axis(side=2, at=range(y) + c(-1, 1)*c(diff(range(y))*0.04), labels=FALSE, lwd.ticks=0, lwd=3, col=primary_color)
  points(x, y, pch=1, cex=1, col=secondary_color)
  abline(lm1, lwd=3, lty=2, col=primary_color)
  legend("topleft", legend=expression(bolditalic(R^2)), bty ="n", pch=NA, cex=2.4, adj=c(1.4, 0), text.col=primary_color)
  dev.off()

  Cairo::CairoPNG(paste0(dir, "/stat_icon_pars_b0hat_", color_theme, ".png"), width=96, height=96, bg="transparent")
  par(lwd=2, mar=mar, family="cam")
  plot(0,0, type="n", axes=FALSE, xlab="", ylab="", xlim=range(x), ylim=range(x) + c(0, 0.1))
  axis(side=1, at=range(x) + c(-1, 1)*c(diff(range(x))*0.04), labels=FALSE, lwd.ticks=0, lwd=3, col=secondary_color)
  axis(side=2, at=range(x) + c(-1, 1)*c(diff(range(x))*0.04), labels=FALSE, lwd.ticks=0, lwd=3, col=secondary_color)
  points(x, y, cex=1, pch=1, col=secondary_color)
  abline(lm1, lwd=3, lty=2, col=secondary_color)
  a <- lm1$coefficients[2]*min(y) + lm1$coefficients[1] - 0.3
  arrows(max(x), a, min(x), a, lwd=3, col=primary_color)
  legend("topleft", legend=expression(bolditalic(hat(beta[0]))), bty ="n", pch=NA, cex=2.2, adj=c(1.4, 0.1), text.col=primary_color)
  dev.off()

  Cairo::CairoPNG(paste0(dir, "/stat_icon_pars_b1hat_", color_theme, ".png"), width=96, height=96, bg="transparent")
  par(lwd=2, mar=mar, family="cam")
  plot(0,0, type="n", axes=FALSE, xlab="", ylab="", xlim=range(x), ylim=range(y) + c(0, 0.1))
  axis(side=1, at=range(x) + c(-1, 1)*c(diff(range(x))*0.04), labels=FALSE, lwd.ticks=0, lwd=3, col=secondary_color)
  axis(side=2, at=range(y) + c(-1, 1)*c(diff(range(y))*0.04), labels=FALSE, lwd.ticks=0, lwd=3, col=secondary_color)
  points(x, y, pch=1, cex=1, col=secondary_color)
  abline(lm1, lwd=2, lty=2, col=primary_color)
  segments(-1.2, -0.9, 3.3, -0.9, lwd=3, col=primary_color)
  arrows(3.5, -0.9, 3.5, 1.4, lwd=3, col=primary_color)
  legend("topleft", legend=expression(bolditalic(hat(beta[1]))), bty ="n", pch=NA, cex=2.2, adj=c(1.4, 0.1), text.col=primary_color)
  dev.off()
}
