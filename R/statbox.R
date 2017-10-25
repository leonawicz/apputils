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
statIcon <- function(id, theme = "white"){
  if(!theme %in% c("white", "black")) stop("theme must be 'white' or 'black'.")
  iconopts <- c("normal", "min", "max", "mean", "sd", "median", "iqr", "b0", "b1",
                "r2", "pvalue", "inc", "dec", "pctinc", "pctdec", "barinc", "bardec")
  if(!all(id %in% iconopts)) stop("Invalid id. See help for options.")
  x <- sapply(id, function(x) switch(x,
              normal = "stat_icon_normal_dist",
              min = "stat_icon_normal_min",
              max = "stat_icon_normal_max",
              mean = "stat_icon_normal_mean",
              sd = "stat_icon_normal_sd",
              median = "stat_icon_normal_median",
              iqr = "stat_icon_boxplot_iqr",
              b0 = "stat_icon_pars_b0hat",
              b1 = "stat_icon_pars_b1hat",
              r2 = "stat_icon_pars_r2",
              pvalue = "stat_icon_pars_pvalue",
              inc = "stat_icon_ts_deltaInc",
              dec = "stat_icon_ts_deltaDec",
              pctinc = "stat_icon_ts_deltaPctInc",
              pctdec = "stat_icon_ts_deltaPctDec",
              barinc = "stat_icon_bar_deltaPos",
              bardec = "stat_icon_bar_deltaNeg"))
  paste0("resources/images/", x, "_", theme, ".png")
}

#' Generate png thumbnail icons for various quantities
#'
#' Generate png thumbnail icons for various quantities including common statistics, model parameter estimates,
#' trends and rates of change.
#'
#' The standard white and dark theme icon versions are produced using the default call, \code{makeIcons()},
#' and \code{makeIcons("black", "gray30", "black")}, respectively.
#' The function code is repetitive and includes hardcoded values because each icon is uniquely tailored,
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
makeIcons <- function(primary_color = "#FFFFFF", secondary_color = "#FFFFFF75", color_theme = "white", dir = "."){
  # setup
  set.seed(1)
  xlm <- c(-4.5, 4.5)
  x <- seq(xlm[1], xlm[2], length = 1000)
  y <- stats::dnorm(x)
  x2 <- stats::rnorm(500000)
  x2 <- x2[x2 > xlm[1] & x2 < xlm[2]]
  mar <- c(0.1, 0.1, 0.1, 0.1)
  sysfonts::font.add("cam", "cambriaz.TTF")
  showtext::showtext.auto()

  files <- paste0(c(
    "stat_icon_normal_dist", "stat_icon_normal_mean", "stat_icon_normal_min", "stat_icon_normal_max",
    "stat_icon_normal_median", "stat_icon_normal_sd", "stat_icon_pars_pvalue", "stat_icon_boxplot_iqr",
    "stat_icon_ts_deltaDec", "stat_icon_ts_deltaInc", "stat_icon_ts_deltaPctDec",
    "stat_icon_ts_deltaPctInc", "stat_icon_bar_deltaNeg", "stat_icon_bar_deltaPos",
    "stat_icon_pars_r2", "stat_icon_pars_b0hat", "stat_icon_pars_b1hat"
    ), "_", color_theme, ".png")
  # distribution icons
  Cairo::CairoPNG(file.path(dir, files[1]), width = 96, height = 96, bg = "transparent")
  graphics::par(lwd = 2, mar = mar, family = "cam")
  graphics::plot(x, y, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = xlm)
  graphics::hist(x2, breaks = seq(xlm[1], xlm[2], by = 1), freq = FALSE, add = TRUE, border = primary_color)
  graphics::lines(x, y, col = primary_color)
  grDevices::dev.off()

  Cairo::CairoPNG(file.path(dir, files[2]), width = 96, height = 96, bg = "transparent")
  graphics::par(lwd = 2, mar = mar, family = "cam")
  graphics::plot(x, y, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = xlm)
  graphics::hist(x2, breaks = seq(xlm[1], xlm[2], by = 1), freq = FALSE, add = TRUE, border = secondary_color)
  graphics::lines(x, y, col = secondary_color)
  graphics::abline(v = 0, lwd = 3, lty = 2, col = primary_color)
  graphics::legend("topright", legend = expression(bolditalic(bar(x))), bty = "n",
         pch = NA, cex = 3,  yjust = 1, adj = c(-0.5, 0), text.col = primary_color)
  grDevices::dev.off()

  Cairo::CairoPNG(file.path(dir, files[3]), width = 96, height = 96, bg = "transparent")
  graphics::par(lwd = 2, mar = mar, family = "cam")
  graphics::plot(x, y, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = xlm)
  graphics::hist(x2, breaks = seq(xlm[1], xlm[2], by = 1), freq = FALSE, add = TRUE, border = secondary_color)
  graphics::lines(x, y, col = secondary_color)
  graphics::abline(v = xlm[1], lwd = 3, lty = 2, col = primary_color)
  graphics::legend("topright", legend = expression(bolditalic(x[(1)])), bty = "n",
         pch = NA, cex = 1.8, adj = c(-0.275, 0), text.col = primary_color)
  grDevices::dev.off()

  Cairo::CairoPNG(file.path(dir, files[4]), width = 96, height = 96, bg = "transparent")
  graphics::par(lwd = 2, mar = mar, family = "cam")
  graphics::plot(x, y, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = xlm)
  graphics::hist(x2, breaks = seq(xlm[1], xlm[2], by = 1), freq = FALSE, add = TRUE, border = secondary_color)
  graphics::lines(x, y, col = secondary_color)
  graphics::abline(v = xlm[2], lwd = 3, lty = 2, col = primary_color)
  graphics::legend("topleft", legend = expression(bolditalic(x[(n)])), bty = "n",
         pch = NA, cex = 1.8,  adj = c(1, 0), text.col = primary_color)
  grDevices::dev.off()

  Cairo::CairoPNG(file.path(dir, files[5]), width = 96, height = 96, bg = "transparent")
  graphics::par(lwd = 2, mar = mar, family = "cam")
  graphics::plot(x, y, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = xlm)
  graphics::hist(x2, breaks = seq(xlm[1], xlm[2], by = 1), freq = FALSE, add = TRUE, border = secondary_color)
  graphics::lines(x, y, col = secondary_color)
  graphics::abline(v = 0, lwd = 3, lty = 2, col = primary_color)
  graphics::legend("topright", legend = expression(bolditalic(tilde(x))), bty = "n",
         pch = NA, cex = 3,  adj = c(-0.5, 0), text.col = primary_color)
  grDevices::dev.off()

  Cairo::CairoPNG(file.path(dir, files[6]), width = 96, height = 96, bg = "transparent")
  graphics::par(lwd = 2, mar = mar, family = "cam")
  graphics::plot(x, y, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = xlm)
  graphics::hist(x2, breaks = seq(xlm[1], xlm[2], by = 1), freq = FALSE, add = TRUE, border = secondary_color)
  graphics::lines(x, y, col = secondary_color)
  graphics::abline(v = c(-1, 1), lwd = 3, lty = 2, col = primary_color)
  graphics::legend("topright", legend = expression(bolditalic(s)), bty = "n",
         pch = NA, cex = 3, adj = c(-0.5, 0), text.col = primary_color)
  grDevices::dev.off()

  Cairo::CairoPNG(file.path(dir, files[7]), width = 96, height = 96, bg = "transparent")
  graphics::par(lwd = 2, mar = mar, family = "cam")
  graphics::plot(x, y, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = xlm)
  graphics::hist(x2, breaks = seq(xlm[1], xlm[2], by = 1), freq = FALSE, add = TRUE, border = secondary_color)
  qn <- stats::qnorm(0.05)
  idx <- which(x <= qn)
  graphics::polygon(c(x[idx], rev(x[idx])), c(y[idx], rep(0, length(idx))), col = primary_color, border = NA)
  graphics::lines(x, y, col = secondary_color)
  graphics::abline(v = qn, lwd = 3, lty = 2, col = primary_color)
  graphics::arrows(qn, 0.1, min(x), 0.1, lwd = 3, col = primary_color, length = 0.15)
  graphics::legend("topright", legend = expression(bolditalic(p)), bty = "n",
         pch = NA, cex = 3,  yjust = 1, adj = c(-0.5, 0), text.col = primary_color)
  grDevices::dev.off()

  showtext::showtext.auto(enable = FALSE)

  Cairo::CairoPNG(file.path(dir, files[8]), width = 96, height = 96, bg = "transparent")
  graphics::par(lwd = 2, mar = mar, family = "cam")
  graphics::boxplot(x2, outline = FALSE, axes = FALSE, frame = FALSE,  lty = 1,
                    border = secondary_color, boxcol = primary_color)
  graphics::text(1.35, -0.05, expression("}"), cex = 2, col = primary_color)
  showtext::showtext.begin()
  graphics::text(1.35, 1.5, expression("IQR"), cex = 1.5, col = primary_color)
  showtext::showtext.end()
  grDevices::dev.off()

  showtext::showtext.auto()

  # time series icons
  y <- scale(c(0.3, 0.4, 2, 0.7, 2, 1.5, 3.5, 2.75, 4))
  x <- scale(seq_along(y))

  Cairo::CairoPNG(file.path(dir, files[9]), width = 96, height = 96, bg = "transparent")
  graphics::par(lwd = 2, mar = mar, family = "cam")
  graphics::plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = range(x), ylim = range(y))
  graphics::lines(x, rev(y), lty = 2, col = secondary_color)
  graphics::arrows(x[1], y[length(y)], x[length(x)], y[1], lwd = 3, col = primary_color)
  graphics::legend("topright", legend = expression(bolditalic(Delta)), bty = "n",
         pch = NA, cex = 1.8, adj = c(-0.75, 0), text.col = primary_color)
  grDevices::dev.off()

  Cairo::CairoPNG(file.path(dir, files[10]), width = 96, height = 96, bg = "transparent")
  graphics::par(lwd = 2, mar = mar, family = "cam")
  graphics::plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = range(x), ylim = range(y))
  graphics::lines(x, y, lty = 2, col = secondary_color)
  graphics::arrows(x[1], y[1], x[length(x)], y[length(y)], lwd = 3, col = primary_color)
  graphics::legend("topleft", legend = expression(bolditalic(Delta)), bty = "n",
         pch = NA, cex = 1.8,  adj = c(2.5, 0), text.col = primary_color)
  grDevices::dev.off()

  Cairo::CairoPNG(file.path(dir, files[11]), width = 96, height = 96, bg = "transparent")
  graphics::par(lwd = 2, mar = mar, family = "cam")
  graphics::plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = range(x), ylim = range(y))
  graphics::lines(x, rev(y), lty = 2, col = secondary_color)
  graphics::arrows(x[1], y[length(y)], x[length(x)], y[1], lwd = 3, col = primary_color)
  graphics::legend("topright", legend = expression(bolditalic(symbol("\045")~Delta)), bty = "n",
         pch = NA, cex = 1.8, adj = c(-0.25, 0), text.col = primary_color)
  grDevices::dev.off()

  Cairo::CairoPNG(file.path(dir, files[12]), width = 96, height = 96, bg = "transparent")
  graphics::par(lwd = 2, mar = mar, family = "cam")
  graphics::plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = range(x), ylim = range(y))
  graphics::lines(x, y, lty = 2, col = secondary_color)
  graphics::arrows(x[1], y[1], x[length(x)], y[length(y)], lwd = 3, col = primary_color)
  graphics::legend("topleft", legend = expression(bolditalic(symbol("\045")~Delta)), bty = "n",
         pch = NA, cex = 1.8,  adj = c(0.9, 0), text.col = primary_color)
  grDevices::dev.off()

  # bar icons
  Cairo::CairoPNG(file.path(dir, files[13]), width = 96, height = 96, bg = "transparent")
  graphics::par(lwd = 2, mar = mar, family = "cam")
  graphics::barplot(c(4, 1), axes = FALSE, lty = 1, border = primary_color, col = secondary_color)
  graphics::arrows(1.6, 4, 1.6, 1.2, lwd = 3, col = primary_color)
  graphics::legend("topright", legend = expression(bolditalic(Delta)), bty = "n",
         pch = NA, cex = 1.8, adj = c(-0.5, 0), text.col = primary_color)
  grDevices::dev.off()

  Cairo::CairoPNG(file.path(dir, files[14]), width = 96, height = 96, bg = "transparent")
  graphics::par(lwd = 2, mar = mar, family = "cam")
  graphics::barplot(c(1, 4), axes = FALSE, lty = 1, border = primary_color, col = secondary_color)
  graphics::arrows(1, 1.2, 1, 4, lwd = 3, col = primary_color)
  graphics::legend("topleft", legend = expression(bolditalic(Delta)), bty = "n",
         pch = NA, cex = 1.8, adj = c(2.5, 0), text.col = primary_color)
  grDevices::dev.off()

  # linear models
  set.seed(7)
  x <- stats::rnorm(25, sd = 1.5)
  y <- 0.5*x + stats::rnorm(25, sd = 0.5)
  lm1 <- stats::lm(y ~ x)
  Cairo::CairoPNG(file.path(dir, files[15]), width = 96, height = 96, bg = "transparent")
  graphics::par(lwd = 2, mar = mar, family = "cam")
  graphics::plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = range(x), ylim = range(y) + c(0, 0.1))
  graphics::axis(side = 1, at = range(x) + c(-1, 1)*c(diff(range(x))*0.04),
                 labels = FALSE, lwd.ticks = 0, lwd = 3, col = primary_color)
  graphics::axis(side = 2, at = range(y) + c(-1, 1)*c(diff(range(y))*0.04),
                 labels = FALSE, lwd.ticks = 0, lwd = 3, col = primary_color)
  graphics::points(x, y, pch = 1, cex = 1, col = secondary_color)
  graphics::abline(lm1, lwd = 3, lty = 2, col = primary_color)
  graphics::legend("topleft", legend = expression(bolditalic(R^2)), bty = "n",
         pch = NA, cex = 2.4, adj = c(1.4, 0), text.col = primary_color)
  grDevices::dev.off()

  Cairo::CairoPNG(file.path(dir, files[16]), width = 96, height = 96, bg = "transparent")
  graphics::par(lwd = 2, mar = mar, family = "cam")
  graphics::plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = range(x), ylim = range(x) + c(0, 0.1))
  graphics::axis(side = 1, at = range(x) + c(-1, 1)*c(diff(range(x))*0.04),
                 labels = FALSE, lwd.ticks = 0, lwd = 3, col = secondary_color)
  graphics::axis(side = 2, at = range(x) + c(-1, 1)*c(diff(range(x))*0.04),
                 labels = FALSE, lwd.ticks = 0, lwd = 3, col = secondary_color)
  graphics::points(x, y, cex = 1, pch = 1, col = secondary_color)
  graphics::abline(lm1, lwd = 3, lty = 2, col = secondary_color)
  a <- lm1$coefficients[2]*min(y) + lm1$coefficients[1] - 0.3
  graphics::arrows(max(x), a, min(x), a, lwd = 3, col = primary_color)
  graphics::legend("topleft", legend = expression(bolditalic(hat(beta[0]))), bty = "n",
         pch = NA, cex = 2.2, adj = c(1.4, 0.1), text.col = primary_color)
  grDevices::dev.off()

  Cairo::CairoPNG(file.path(dir, files[17]), width = 96, height = 96, bg = "transparent")
  graphics::par(lwd = 2, mar = mar, family = "cam")
  graphics::plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = range(x), ylim = range(y) + c(0, 0.1))
  graphics::axis(side = 1, at = range(x) + c(-1, 1)*c(diff(range(x))*0.04),
                 labels = FALSE, lwd.ticks = 0, lwd = 3, col = secondary_color)
  graphics::axis(side = 2, at = range(y) + c(-1, 1)*c(diff(range(y))*0.04),
                 labels = FALSE, lwd.ticks = 0, lwd = 3, col = secondary_color)
  graphics::points(x, y, pch = 1, cex = 1, col = secondary_color)
  graphics::abline(lm1, lwd = 2, lty = 2, col = primary_color)
  graphics::segments(-1.2, -0.9, 3.3, -0.9, lwd = 3, col = primary_color)
  graphics::arrows(3.5, -0.9, 3.5, 1.4, lwd = 3, col = primary_color)
  graphics::legend("topleft", legend = expression(bolditalic(hat(beta[1]))), bty = "n",
         pch = NA, cex = 2.2, adj = c(1.4, 0.1), text.col = primary_color)
  grDevices::dev.off()
}
