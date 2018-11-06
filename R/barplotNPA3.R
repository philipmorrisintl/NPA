#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################


#' Create NPA summary: barplot and leading nodes (top 10 by default)
#'
#' @param np A R list with the NPA scoring results
#' @param type An integer value. Type of barplot. If \code{type} is 1, default version
#' showing a simple barplot with "O.K." statistics. \code{type} = 2,
#' wordclouds showing leading nodes are represented on the right side of the
#' barplot, this mode required a large figure. \code{type} = 3, a simplified
#' version of the barplot is generated using ggplots R package.
#' @param bg A character vector. Color of the background for the plot.
#' @param mar A numerical vector. Margin setting to be adjusted for x axis
#' contrast names legend.
#' @param main A character vector. Title for the figure
#' @param maxLN A integer value. Maximum number of leading nodes shown in the
#' text cloud figure.
#' @param layout.mat A matrix describing the layout
#' @param mar.upcontrib A numerical value for top margin of the leading nodes
#' contributions
#' @param BW A logical. Default is \code{FALSE}, colored version will be drawn
#' rather than grayscale (for printing).
#' @param ... Additional parameters to be passed to barplotNPA function
#' @include barplotNPA.R
#'
barplotNPA3 <- function(np, type=1, bg = "grey90", mar = 8, main = "NPA", maxLN = 10, layout.mat = NULL,
    mar.upcontrib = 2, BW = FALSE, cex.main=0.5, ...) {

    oks <- which(apply(np$pvperm, 2, max) < 0.05 & np$ci.down > 0)
    nok <- length(oks)
    if (is.null(layout.mat)) {
        m2 <- floor(sqrt(nok))
        m1 <- m2
        while (m1 * m2 < nok) {
            m1 <- m1 + 1
        }
        if (m1 * m2 != 0) {
            m <- cbind(rep(1, m1), 1 + matrix(1:(m1 * m2), ncol = m2, byrow = TRUE))
        }
    } else {
        m <- cbind(rep(1, nrow(layout.mat)), 1 + layout.mat)
        m2 <- ncol(layout.mat)
    }
    if (type==1) {
        m2 <- 0
    }
    if (names(dev.cur()) == "X11") {
        warning("Set X11.options(type=\"cairo\") for semi-transparency ...")
    }
    if (type==2) {

        if (ncol(np$edges) == 3) {
            d0 <- getBIFdist(list(`CST / tmp` = np))
        } else {
            d0 <- getBIFdist(list(`Cell Stress / tmp` = np))
        }
        d <- d0$dist
    }
    out <- which(!(apply(np$pvperm, 2, max) < 0.05 & np$ci.down > 0))
    ylim0 <- c(0, max(np$ci.up))
    wid <- c(1.3 * m2, 1)
    if (m1 * m2 == 0) {
        m <- matrix(1, ncol = 1)
        wid <- 1
    } else {
        layout(m, widths = wid)
        par(mar = c(mar, 2, 12, 3))
    }
    bp <- barplotNPA(np, ylim = ylim0, yaxt = "n", bg = bg,
        ylab = "", title.prefix = main, mar = c(mar, 4, 4, 2), BW = BW, ...)
    axis(2, at = seq(0, max(np$coefficients), length.out = 5), labels = paste(seq(0,
        1, length.out = 5)))
    x <- bp[[1]][, 1]
    rect(-2 * max(x), -2 * max(np$ci.up), 2 * max(x), 0, col = gray(0.9, alpha = 0.5))
   
    if (type==2) {
        len <- getNPALE(np, type = "nodes", plotit = FALSE)
        contrib <- sapply(len, function(l) l$x[order(names(l$x))])
        sg <- sign(np$nodes.coefficients)
        sg <- sg[match(rownames(contrib), rownames(sg)), , drop = FALSE]
        if (!all(rownames(sg) == rownames(contrib))) {
            stop("Issue rownames")
        }
        contrib <- contrib * sg
        par(mar = c(0, 0, mar.upcontrib, 0), pty = "m")
        for (k in oks) {
            tmp <- contrib[, k]
            sgn <- sign(np$nodes.coefficients.ci.up[, k] * np$nodes.coefficients.ci.down[,
                        k])
        sgn <- sgn[match(names(tmp), names(sgn))]
        add1 <- rep("", length(tmp))
        add1[names(tmp) %in% rownames(len[[k]]$leadingNodes)] <- "*"
        add2 <- rep("(+)", length(tmp))
        add2[tmp < 0] <- "(-)"
        add2[tmp == 0] <- ""
        tmp[sgn == -1] <- 0
        names(tmp) <- sapply(strsplit(names(tmp), ""), function(x) {
                             if (length(x) > 23) {
                                 x <- paste(c(x[1:min(length(x), 23)], "..."), collapse = "")
                             } else {
                                 x <- paste(x, collapse = "")
                             }
                             return(x)
                        })
        names(tmp) <- paste(add1, names(tmp), add2, sep = "")
        tmp <- tmp[order(abs(tmp), decreasing = TRUE)]
        tmp <- tmp[1:min(length(tmp), maxLN)]
        col00 <- c(rev(brewer.pal(5, "Blues")[c(4, 5)]), "black", brewer.pal(5, "Reds")[c(4,
                                                                                          5)])
        if(BW == TRUE){
            col00 <- c("gray30", "gray60", "white","gray60", "gray30")
        }
        coltmp <- colorscale(sign(tmp) * ecdf(abs(tmp))(abs(tmp)), col1 = col00,
                             signed = TRUE)
        plot(0, 0, xlim = c(0, 1), ylim = c(0, 1.1), xaxt = "n", yaxt = "n", xlab = "",
             ylab = "", bty = "n", cex = 0)
        rect(0.01, -2, 0.99, 2, col = "white", border = "white")
        if (k == oks[1]) {
            mxnm <- colnames(contrib)[which.max(nchar(colnames(contrib)[oks]))]
            cex.value <- 1
            wid <- tryCatch(
                            strwidth(mxnm, cex = cex.value),
                            error = function(e) 0.99)
            while (wid > 0.8) {
                cex.value <- cex.value - 0.05
                wid <- tryCatch(
                                strwidth(mxnm, cex = cex.value),
                                error = function(e) 0.99)
                if (cex.value < 0.4) {
                    wid <- 0.99
                    break
                }
            }
        }
        title(main = colnames(contrib)[k], cex.main = cex.main)
        alpha <- 3
        size <- alpha * rev(1.5 * (ecdf(abs(tmp))(abs(tmp)) + 0.5))
        words <- rev(names(tmp))
        mx <- mx2 <- Inf
        while (mx > 0.95 | mx2 > 1/(length(tmp) + 1)) {
            alpha <- 0.98 * alpha
            size <- alpha * rev(1.5 * (ecdf(abs(tmp))(abs(tmp)) + 0.5))
            wh <- sapply(1:length(words), function(i) {
                         wid <- strwidth(words[i], cex = size[i])
                         ht <- strheight(words[i], cex = size[i])
                         return(c(wid, ht))
                                })
            mx <- max(wh[1, ])
            mx2 <- max(wh[2, ])
        }
        text(rep(0.5, length(tmp)), seq(0, 1, length.out = length(tmp)), words, col = rev(coltmp),
             cex = size)
        }
    }
}
