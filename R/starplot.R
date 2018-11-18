#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Create starplot
#'
#' @param x A numerical vector. Value to display as sector 
#' @param r A numerical value. Overall scale factor. Default value is 1., 100%
#' will a factor of 1.
#' @param at A numerical vector of length 2. Coordinates in the figure for
#' polygons and segments.
#' @param xrmax A numerical value. Maximum x coordinate value for radius.
#' @param yrmax A numerical value. Maximum y coordinate value for radius.
#' @param add A logical. Default is \code{FALSE}, no line is drawn in sector.
#' @param cst A numerical value. If provided, used for adjusting positioning of
#' starplot/text elements depending if network text is displayed
#' @param cex.text A numerical value. Text size scaling value.
#' @param ylmax A numerical value. Max line length
#' @param cst.text A numerical value. Sector text size positioning adjustment.
#' @param text A logical. Default is \code{TRUE}, sector legend is shown.
#' @param text.type A character vector. If value is "radial", sector legend is
#' drawn radially around the sector. Otherwise, straight.
#' @param main A character vector. Title for the figure.
#' @param cex.main A numerical value. Text scaling factor for title.
#' @param col A string vector. Colors of sectors.
#' @param col.border A character vector. Color for sector border.
#' @param col.circle A character vector. Color for startplot contour.
#' @param lwd.circle A numerical value. Line width scaling value.
#' @param col.bg A character vector. Color of the figure background.
#' @param by A string vector. Sectors to put together 
#' @param colby A string vector. Not used yet.
#' @param textby A string vector. Not used yet.
#' @param cstby A numerical value. Not used yet.
#' @param outmeanby A numerical vector. Not used yet.
#' @param cex.textby A numerical value. Not used yet.
#' @param lwd A numerical value. Sectors segments size scaling value.
#' @param line.radius A logical. Default is \code{FALSE}, no radius line is
#' shown
#' @param lwd.line A numerical value. Radius line width scaling factor.
#' @param lty.line A integer value. Radius line type.
#' @param col.line A character vector. Radius line color.
#' @param border A character vector. Color for polygons border
#' @param ... Optional additional parameters to be passed to plot function
#' @importFrom graphics points
#' @importFrom graphics polygon
#'
starplot <- function(x, r = 1, at = c(0, 0), xrmax = NULL, yrmax = NULL, add = FALSE, cst = 1.2,
                     cex.text = 1, ylmax = r * cst, cst.text = cst, text = TRUE, text.type = c("radial", "straight")[1],
                     main = "", cex.main = 1.3, col = NULL, col.border = "grey40", col.circle = "grey80", lwd.circle = 1,
                     col.bg = "white", by = NULL, colby = NULL, textby = TRUE, cstby = 1.2 * cst, outmeanby = NULL,
                     cex.textby = cex.text, lwd = 2, line.radius = FALSE, lwd.line = 1, lty.line = 3, col.line = "grey40",
                     border = NA, ...) {
  
  if (!is.null(by)) {
    by = factor(by)
    x = x[order(unclass(by))]
    if (!is.null(col)) {
      col0 = col[order(unclass(by))]
      col02 = col.border
    }
    by = by[order(unclass(by))]
  }
  pinum = 3.14159259
  if (is.null(col)) {
    col0 = rainbow(length(x), s = 0.6, v = 0.75)
    col02 = rainbow(length(x), s = 0.4, v = 0.5)
  } else {
    col0 = col
    col02 = rep(col.border, length(col0))
  }
  theta = 2 * pinum/length(x)
  max0 = r
  xylim <- par("usr")
  plotdim <- par("pin")
  c1 = (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) * plotdim[1]/plotdim[2]
  max1 = (yradius <- r * c1)
  if (!is.null(xrmax) & !is.null(yrmax)) {
    yradius = yradius/r * xrmax
    r = xrmax
    max1 = yradius
    max0 = r
  }
  if (!is.null(yrmax)) {
    if (max1 > yrmax) {
      yradius = yrmax
      r = yradius/c1
      max1 = yradius
      max0 = r
    }
  }
  poly0 <- cbind(r * cos(theta * seq(0, length(x), length.out = 200)), yradius * sin(theta *
                                                                                       seq(0, length(x), length.out = 200)))
  if (add == FALSE) {
    at = c(0, 0)
    plot(poly0[, 1], poly0[, 2], xlim = c(-ylmax, ylmax), ylim = c(-ylmax, ylmax), type = "l",
         xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "grey85", bty = "n", ...)
  } else {
    points(at[1] + poly0[, 1], at[2] + poly0[, 2], type = "l")
  }
  polygon(at[1] + poly0[, 1], at[2] + poly0[, 2], border = col.circle, lwd = lwd.circle,
          col = col.bg)
  for (j in 1:length(x)) {
    r1 = x[j]/max(x) * max0
    r2 = x[j]/max(x) * max1
    tmp1 = rbind(c(0.01 * max0, -0.01 * max1), cbind(0.01 * max0 + r1 * cos(theta * (0.5 +
                                                                                       seq(j, j + 1, length.out = 20))), -0.01 * max1 + r2 * sin(theta * (0.5 + seq(j,
                                                                                                                                                                    j + 1, length.out = 20)))))
    for (m in 1:2) {
      tmp1[, m] = at[m] + tmp1[, m]
    }
    polygon(tmp1, col = col02[j], border = border)
    tmp2 = rbind(c(0, 0), cbind(r1 * cos(theta * (0.5 + seq(j, j + 1, length.out = 20))),
                                r2 * sin(theta * (0.5 + seq(j, j + 1, length.out = 20)))))
    for (m in 1:2) {
      tmp2[, m] = at[m] + tmp2[, m]
    }
    if (line.radius == TRUE) {
      segments(at[1], at[2], at[1] + r * cos(theta * (0.5 + j + 0.5)), at[2] + r * sin(theta *
                                                                                         (0.5 + j + 0.5)), lwd = lwd.line, col = col.line, lty = lty.line)
    }
    polygon(tmp2, col = col0[j], border = border)
    if (!is.null(names(x)) & text == TRUE) {
      if (text.type == "radial") {
        arctext(names(x)[j], center = c(0, 0), radius = cst.text * max0, middle = theta *
                  (0.5 + seq(j, j + 1, length.out = 20))[10], stretch = 1.2, cex = cex.text)
      } else {
        anglej = theta * (0.5 + seq(j, j + 1, length.out = 20))[11]/(2 * pinum) * 360
        if (anglej > 90 & anglej < 270) {
          anglej = anglej - 180
        }
        text(cst.text * max0 * cos(theta * (0.5 + seq(j, j + 1, length.out = 20))[11]),
             cst.text * max1 * sin(theta * (0.5 + seq(j, j + 1, length.out = 20))[11]),
             names(x)[j], cex = cex.text, srt = anglej)
      }
    }
  }
  title(main = main, cex.main = cex.main)
}
