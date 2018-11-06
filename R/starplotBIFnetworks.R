#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Starplot per contrasts
#'
#' @param bif
#' @param col
#' @param col.bg
#' @param which.to.plot
#' @param cex.txt
#' @param title.prefix
#' @param names.arg
#' @param cex.main
#' @param cex.network
#' @param BW
#' @param cst
#' @param border.color
#' @param cst.border
#' @param cex
#' @include utils.R


starplotBIFnetworks <- function(bif, colours = Colors()(ncol(bif[[1]]$contrib)), col.bg = "grey90",
    which.to.plot = NULL, cex.txt = 1.5, title.prefix = "", names.arg = NULL, cex.main = 4,
    cex.network = cex.main, BW = FALSE, cst = 1.1, border.color = "grey40", cst.border = 1.1,
    cex = 1.5) {

    if (is.null(which.to.plot)) {
        which.to.plot <- ncol(bif[[1]]$contrib)
    }
    C0 <- sqrt(bif$BIF$starplot.area) * 100
    if (!is.null(names.arg)) {
        rownames(C0) <- names.arg
    }
    colnames(C0) <- paste0(title.prefix, colnames(C0))
    lay <- getPlotLayout(ncol(bif$BIF$starplot.area))
    lay <- cbind(rep(1, nrow(lay)), 1 + lay)
    layout(lay)
    ltrs <- make.unique(rep(LETTERS[1:26], ceiling(nrow(C0)/26))[1:nrow(C0)], sep = "-")
    par(pty = "s")
    plot(c(0, 1), c(0, 1), xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n",
        cex = 0.1, col = "white")
    alpha <- 3
    size <- rep(alpha, nrow(C0))
    nms <- paste(ltrs, rownames(C0), sep = ": ")
    mx <- mx2 <- Inf
    while (mx > 0.95 | mx2 > 1/(length(nms) + 1)) {
        alpha <- 0.95 * alpha
        size <- rep(alpha, nrow(C0))
        wh <- sapply(1:length(nms), function(i) {
            wid <- strwidth(nms[i], cex = size[i])
            ht <- strheight(nms[i], cex = size[i])
            return(c(wid, ht))
        })
        mx <- max(wh[1, ])
        mx2 <- max(wh[2, ])
        if (alpha < 0.1) {
            mx <- mx2 <- 0
        }
    }
    text(rep(0.15, nrow(C0)), seq(0, 1, length.out = nrow(C0)), rev(nms), cex = size,
        col = "grey20", pos = 4)
    points(rep(0.1, nrow(C0)), seq(0, 1, length.out = nrow(C0)), col = "black", cex = 1.25 *
        cex, pch = 15)
    points(rep(0.1, nrow(C0)), seq(0, 1, length.out = nrow(C0)), col = rev(colours),
        cex = cex, pch = 15)
    for (k in 1:ncol(bif$BIF$starplot.area)) {
        tmp <- C0[, k]
        names(tmp) <- ltrs
        starplot(rev(tmp), col = rev(colours), main = colnames(bif$BIF$starplot.area)[k],
            cex.text = cex.txt, text.type = c("radial", "straight")[2], line.radius = TRUE,
            col.bg = col.bg, cex.main = cex.main, cst = cst, border = "black", cst.text = cst.border,
            col.border = border.color)
    }
    x <- NULL
    return(x)
}
