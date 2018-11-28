#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Create BIF barplot figure
#'
#' @param bif A list object containing BIF results
#' @param m1 An integer. Horizontal layout setting.
#' @param m2 An integer. Vertical layout setting.
#' @param col A string vector containing colors of the BIF bars.
#' @param mar An integer. Margin value
#' @param cex.txt A numerical. text plotting scale value
#' @param border.col A character vector, color name for border
#' @param colbartxt A character vector, color name for bar text
#' @param title.prefix A character vector, prefix string for title
#' @param starplot A logical. TRUE means starplot will be displayed
#' @param pie A logical. TRUE means pie will be displayed
#' @param signed A logical. TRUE means that signed BIF will be represented
#' @param cex.axis A numerical. Magnification of axis annotation relative to
#' cex
#' @param starplot.BW A logical for plotting a black and white starplot
#' @param ... Additional parameters to be passed to low level function call.
#'
#' @importFrom plotrix floating.pie
#' @importFrom grDevices terrain.colors
#' @importFrom grDevices gray.colors
#' @importFrom graphics par
#' @importFrom graphics axis
#' @importFrom graphics rect
#' @importFrom graphics abline
#' @importFrom graphics text
#' @include starplot.R
#' @return An invisible list with BIF plotted values, colors, pie and starplot values.
#'
#'
barplotBIF <-
  function(bif,
           m1 = NULL,
           m2 = NULL,
           col = terrain.colors(nrow(bif$rbif)),
           mar = 12,
           cex.txt = 1,
           border.col = "grey30",
           colbartxt = "black",
           title.prefix = "",
           starplot = FALSE,
           pie = TRUE,
           signed = FALSE,
           cex.axis = 1,
           starplot.BW = FALSE,
           ...) {
    if (starplot == TRUE & pie == TRUE) {
      stop("One of starplot or pie can be TRUE")
    }
    starplotpie <- starplot || pie
    bif <- bif[[1]]
    relto <- bif$relativeto
    C <- bif$starplot  #For plottign pie charts
    bf <- as.vector(bif$coefficients)  #For main barplot
    names(bf) <- rownames(bif$coefficients)
    if (is.null(m1) | is.null(m2)) {
      m1 <- ceiling(sqrt(nrow(C)))
      m2 <- 0
      while (m1 * m2 < nrow(C)) {
        m2 <- m2 + 1
      }
    }
    par(mar = c(mar, 8, 2, 2))
    # rownames(bif$rbif)=gsub('\n','',rownames(bif$rbif),fixed=TRUE)
    col2 <- col
    mx <- max(100 * bif$rbif[, colnames(bif$rbif) == "BIF"] ^ 2)
    ylimin <- -45
    if (signed == TRUE) {
      bf <-
        bf * as.vector(sign(bif$r2[, colnames(bif$r2) == "BIF", drop = FALSE]))
    }
    if (starplotpie == FALSE & signed == FALSE) {
      ylimin <- 0
    }
    if (starplotpie == FALSE & signed == TRUE) {
      ylimin <- min(0, min(bf))
    }
    if (starplotpie == TRUE & signed == TRUE) {
      ylimin <- min(0, min(bf)) - 45
    }
    bp <-
      barplot(
        bf,
        col = col2,
        ylab = "%",
        main = paste(title.prefix, "Relative BIF"),
        las = 3,
        ylim = c(ylimin, 1.2 * mx + 28 * cex.txt),
        yaxt = "n",
        border = border.col,
        ...
      )  #*sign(bif$sc$BIF$r2)
    axis(
      2,
      at = seq(0, round(mx), by = 20),
      labels = seq(0, round(mx), by = 20),
      cex.axis = cex.axis
    )
    rect(-10,
         min(0, min(bf)),
         max(bp) + 10,
         mx,
         col = "grey90",
         border = NA)
    # grid(,col='white',lwd=3)
    abline(
      v = bp,
      col = "white",
      lwd = 2,
      lty = 33
    )
    bp <-
      barplot(
        bf,
        col = col2,
        add = TRUE,
        names.arg = rep("", nrow(bif$rbif)),
        yaxt = "n",
        border = border.col
      )
    abline(h = 0, lwd = 1.3)
    bp0 = bp
    if (length(bf) == 1) {
      bp0 = 0.7
      bp = c(0.2, 1.2)
    }
    if (max(bf) == 0) {
      starplot = FALSE
      pie = FALSE
    }
    if (starplot == TRUE) {
      for (k in 1:nrow(C)) {
        starplot(
          C[k,],
          r = 1,
          at = c(bp0[k], min(0, min(bf)) - 22),
          xrmax = (bp[2] -
                     bp[1]) /
            2,
          yrmax = 10,
          add = TRUE,
          cst = 1.2,
          cex.text = 1,
          ylmax = 0.6,
          text.type = c("radial", "straight")[1],
          main = "",
          col = getNets(colnames(C),
                        BW = starplot.BW)$col0,
          text = FALSE
        )
      }
    }
    if (pie == TRUE) {
      CTRB <- bif$pie
      xylim <- par("usr")
      plotdim <- par("pin")
      radius <- (bp[2] - bp[1]) / 2.5
      if (length(bf) == 1) {
        radius = 0.3
      }
      yradius <-
        radius * (xylim[4] - xylim[3]) / (xylim[2] - xylim[1]) * plotdim[1] / plotdim[2]
      while (yradius > abs(ylimin) / 2.5) {
        radius <- 0.999 * radius
        yradius <-
          radius * (xylim[4] - xylim[3]) / (xylim[2] - xylim[1]) * plotdim[1] / plotdim[2]
      }
      for (k in 1:nrow(C)) {
        if (bf[k] > 1e-10) {
          plotrix::floating.pie(
            bp0[k],
            min(0, min(bf)) - 22,
            CTRB[k,] + 1e-10,
            radius = radius,
            col = getNets(colnames(C), BW = starplot.BW)$col0,
            shadow = FALSE,
            border = "grey90"
          )
        }
      }
    }
    txt0 <-
      round(bif$r2[, colnames(bif$r2) == "BIF", drop = FALSE], 2)
    bp <- as.vector(bp)
    if (max(bf) > 0) {
      for (k in 1:length(txt0)) {
        x0 <- txt0[k]
        if (k %in% unique(relto)) {
          text(
            bp[k],
            mx,
            "  (REF)",
            col = colbartxt,
            srt = 90,
            cex = cex.txt,
            adj = c(0, 0)
          )
        } else {
          text(
            bp[k],
            mx,
            expression(delta),
            col = colbartxt,
            srt = 90,
            cex = 1.1 *
              cex.txt,
            adj = c(0, 0)
          )
          text(
            bp[k],
            mx,
            paste("  = ", x0, sep = ""),
            col = colbartxt,
            srt = 90,
            cex = cex.txt,
            adj = c(0, 0)
          )
        }
      }
    }
    par(bg = "white")
    star <- NULL
    if (starplot == TRUE) {
      star <- bif$starplot
    }
    if (pie == TRUE) {
      star <- bif$pie  #is bif$contrib
    }
    res <-
      list(
        values = bf,
        text = txt0,
        col = col2,
        starplot = bif$starplot,
        pie = bif$pie
      )
    return(invisible(res))
  }
