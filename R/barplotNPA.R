#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Create NPA barplot with statistics (*O*K*)
#'
#' @param x0 List object containing NPA scoring results
#' @param mar numerical vector indicating margin size c(bottom, left, top,
#' right)
#' @param modulo An integer value for the number of barplot diplayed per
#' columns
#' @param m1 An integer. Horizontal layout setting.
#' @param m2 An integer. Vertical layout setting.
#' @param col A string vector containing colors of the BIF bars.
#' @param border.col A character vector, color name for border
#' @param title.prefix A character vector, prefix string for title
#' @param main A character vector, title for the NPA figure.
#' @param single A logical. Default is TRUE which means only one barplot will
#' be drawn (for case where \code{type} is 'npa') 
#' @param which.hyp A integer vector with indexes of which nodes the barplot is
#' going to be plotted.
#' @param which An integer vector containing indexes of the contrasts to be
#' shown in the barplot.
#' @param noCI A logical. Default is \code{FALSE} which means no confidence
#' interval arrow will be drawn. 
#' @param reorder An integer vector for indexes of contrasts for optionally
#' reodering them on the barplot.
#' @param ylim A numerical vector (min, max) for y axis bounds
#' @param bg A character vector, the plot background color
#' @param alphaperm A numerical. Parameter for the p-value related "O.K."
#' statistic details display
#' @param cex.legend A numerical. Text scaling value relative to 1 for the text
#' legend.
#' @param cex.annot A numerical. Text scaling value relative to 1 for bars
#' annotations of the barplot
#' @param type A character vector. Can be 'npa' or 'nodes'.
#' @param overwritecolors A logical. If \code{FALSE}, the "non-significant" NPA
#'value for a given contrast will be drawn in grey. If \code{TRUE}, the assigned
#' color will be used.
#' @param legend.text A logical. By default, \code{TRUE} will display the
#' "O.K." statistic legend.
#' @param par.call A logical. If \code{TRUE}, several columns and lines will be
#' used to draw several barplot.
#' @param lwd A numerical value for relative line with of arrows, segments
#' drawn
#' @param colCI A character vector. Color of the confidence interval arrows.
#' @param ylab A character vector. Label for the y axis
#' @param simplified A logical. Default is \code{FALSE}. The barplot shows
#'complete information. \code{TRUE} will show a minimal version of the NPA
#'barplot
#' @param relative A logical. If \code{TRUE}, the barplot will be displayed
#' relativly to the max NPA accross the contrasts.
#' @param details A logical. If \code{TRUE}, additional information is
#'displayed regarding the permutation tests.
#' @param BW A logical. Default is `TRUE`, colored figure is generated
#' If `FALSE` is used, grayscale is used.
#' @param ... Optional additional parameter to be passed
#' @importFrom graphics barplot.default
#' @importFrom graphics grid
#' @importFrom graphics arrows
#' @importFrom graphics legend
#' @importFrom stats median
#' @importFrom stats quantile
#' @importFrom graphics segments
#' @include utils.R
#' @return Barplot of vector of barplots object
#'
barplotNPA <- function(x0, mar = c(12, 4, 4, 2), modulo = 0, m1 = NULL, m2 = NULL,
    col = NULL, border.col = NA, title.prefix = "", main = NULL, single = TRUE, which.hyp = NULL,
    which = NULL, noCI = FALSE, reorder = NULL, ylim = NULL, bg = "grey90", alphaperm = 0.05,
    cex.legend = 0.7, cex.annot = 1, type = c("npa", "nodes")[1], overwritecolors = FALSE,
    legend.text = FALSE, par.call = TRUE, lwd = 1, colCI = "grey50", ylab = "NPA",
    simplified = FALSE, relative = FALSE, details = FALSE, BW = FALSE, ...) {
    if (type == "npa" & !is.null(which)) {
        x0$coefficients <- x0$coefficients[which]
        x0$ci.down <- x0$ci.down[which]
        x0$ci.up <- x0$ci.up[which]
        x0$pvperm <- x0$pvperm[, which]
        x0$pvalue <- x0$pvalue[which]
        x0$test[2:3] <- lapply(x0$test[2:3], function(X) X[which, ])
    }
    if (type == "npa" & relative == TRUE) {
        ylab <- "%"
        x0$ci.up <- 100 * ((x0$ci.up - x0$coefficients)/max(x0$coefficients) + x0$coefficients/max(x0$coefficients))
        x0$ci.down <- 100 * (x0$coefficients/max(x0$coefficients) - (x0$coefficients -
            x0$ci.down)/max(x0$coefficients))
        x0$coefficients <- 100 * (x0$coefficients/max(x0$coefficients))
    }
    if (type == "nodes") {
        x0 <- list(coefficients = x0$nodes.coefficients, ci.up = x0$nodes.coefficients.ci.up,
            ci.down <- x0$nodes.coefficients.ci.down)
    }
    if (type == "nodes" & relative == TRUE) {
        x0 <- list(coefficients = t(apply(x0$nodes.coefficients, 1, function(x) x/max(abs(x)))),
            ci.up <- t(apply(x0$nodes.coefficients.ci.up, 1, function(x) x/max(abs(x)))),
            ci.down <- t(apply(x0$nodes.coefficients.ci.down, 1, function(x) x/max(abs(x)))))
    }
    if (all(!c("coefficients", "pvalue", "ci.up", "ci.down") %in% names(x0)) == TRUE) {
        stop("x must be a list with names coefficients, p.value,ci.up,ci.down")
    }
    if (simplified == TRUE) {
        legend.text <- FALSE
    }
    if (!is.matrix(x0$coefficients)) {
        nm0 <- names(x0$coefficients)
        x0$coefficients <- matrix(x0$coefficients, nrow = 1)
        colnames(x0$coefficients) <- nm0
        if (!is.null(x0$ci.down) & !is.null(x0$ci.up)) {
            x0$ci.up <- matrix(x0$ci.up, nrow = 1)
            x0$ci.down <- matrix(x0$ci.down, nrow = 1)
            colnames(x0$ci.up) <- colnames(x0$ci.down) <- nm0
        }
    }
    if (!is.null(x0$pvperm)) {
        if (!is.matrix(x0$pvperm)) {
            x0$pvperm <- matrix(x0$pvperm, ncol = 1)
        }
    }
    if (!is.null(x0$pvalue)) {
        if (!is.matrix(x0$pvalue)) {
            x0$pvalue <- matrix(x0$pvalue, ncol = 1)
        }
    }
    if (is.null(col)) {
        col <- Colors()(ncol(x0$coefficients))
        if(BW == TRUE){
            col <- gray.colors(ncol(x0$coefficients), start=0.2, end=0.7)
        }
    }else{
        if(BW == TRUE){
            col =  gray(colSums(col2rgb(col))/(3*255))
        }
    }
    if (is.null(reorder)) {
        reorder <- 1:ncol(x0$coefficients)
    }
    flag <- 0
    w0 <- which.hyp
    if (is.character(which.hyp)) {
        which.hyp <- c(1:nrow(x0$coefficients))[rownames(x0$coefficients) %in% which.hyp]
        if (length(which.hyp) > 0) {
            flag <- 1
        }
    }
    if (!is.null(w0) & flag == 0 & !is.numeric(which.hyp)) {
        which.hyp <- 1
    }
    if (flag == 0 & !is.numeric(which.hyp)) {
        which.hyp <- 1
    }
    # reorder
    ciup <- x0$ci.up[, reorder]
    cidown <- x0$ci.down[, reorder]
    x0$pvperm <- x0$pvperm[, reorder]
    if (!is.null(x0$pvperm)) {
        if (!is.matrix(x0$pvperm)) {
            x0$pvperm <- matrix(x0$pvperm, ncol = 1)
        }
    }
    pv <- x0$pvalue[reorder]
    x <- x0$coefficients[, reorder]
    if (!is.matrix(cidown) & !is.null(cidown) & !is.null(ciup)) {
        nm0 <- names(cidown)
        cidown <- matrix(cidown, nrow = 1)
        colnames(cidown) <- nm0
        nm0 <- names(ciup)
        ciup <- matrix(ciup, nrow = 1)
        colnames(ciup) <- nm0
    }
    # if(noCI==TRUE){ciup=cidown=NULL}
    if (!is.matrix(pv) & !is.null(pv)) {
        nm0 <- names(pv)
        pv <- matrix(pv, nrow = 1)
        colnames(pv) <- nm0
    }
    if (!is.matrix(x)) {
        nm0 <- names(x)
        x <- matrix(x, nrow = 1)
        colnames(x) <- nm0
    }
    if (is.matrix(x)) {
        nm <- colnames(x)
    }
    if (is.vector(x)) {
        nm <- names(x)
        x <- matrix(x, nrow = 1)
        colnames(x) <- nm
    }
    if (modulo > 0) {
        if (length(x)%%modulo == 0) {
            col <- col[1:modulo]
            col <- rep(col, ceiling(ncol(x)/modulo))
        }
    }
    if (is.null(m1) | is.null(m2)) {
        m1 <- m2 <- max(1, floor(sqrt(length(which.hyp))))
        while (m1 * m2 <= nrow(x)) {
            m2 <- m2 + 1
        }
    }
    if (par.call == TRUE) {
        if (single == FALSE) {
            par(mar = mar, bg = bg, mfrow = c(m1, m2))
        }
        if (single == TRUE) {
            par(mar = mar, bg = bg)
        }
    }
    bps <- vector("list", length(which.hyp))
    for (k in which.hyp) {
        if (is.null(ylim)) {
            ymin <- min(0, min(x[k, ], na.rm = TRUE)) - 0.1 * (max(x[k, ], na.rm = TRUE) -
                min(x[k, ], na.rm = TRUE))
            ymax <- max(0, max(x[k, ], na.rm = TRUE)) + 0.1 * (max(x[k, ], na.rm = TRUE) -
                min(x[k, ], na.rm = TRUE))
            if (all(!is.na(cidown[k, ])) & all(!is.na(ciup[k, ]))) {
                if (!is.null(ciup) & !is.null(cidown)) {
                  ymin <- min(0, min(cidown[k, ], na.rm = TRUE)) - 0.1 * (max(ciup[k,
                    ], na.rm = TRUE) - min(ciup[k, ], na.rm = TRUE))
                  ymax <- max(ciup[k, ], na.rm = TRUE) + 0.1 * (max(ciup[k, ], na.rm = TRUE) -
                    min(ciup[k, ], na.rm = TRUE))
                }
            }
        }
        if (!is.null(ylim)) {
            ymin <- min(ylim)
            ymax <- max(ylim)
        }
        if ("pvspecificity" %in% names(x0)) {
            col[x0$pvspecificity >= alphaperm] <- "grey90"
        }
        if ("pvperm" %in% names(x0)) {
            for (j in 1:ncol(x0$pvperm)) {
                if (x0$pvperm[1, j] >= alphaperm | x0$pvperm[2, j] >= alphaperm |
                  (sign(cidown[k, j]) != sign(ciup[k, j]))) {
                  if (overwritecolors == FALSE) {
                    col[j] <- "grey90"
                  }
                }
            }
        }
        if (is.null(main)) {
            tit <- paste(title.prefix, rownames(x)[k])
            bps[[k]] <- bp <- barplot.default(x[k, ], las = 3, col = col, ylim = c(ymin,
                ymax), main = tit, border = NA, ylab = ylab, ...)
        } else {
            bps[[k]] <- bp <- barplot.default(x[k, ], las = 3, col = col, ylim = c(ymin,
                ymax), border = NA, ylab = ylab, main = main, ...)
        }
        rect(-2 * min(bp), ymin, 2 * max(bp), ymax, col = "white", border = NA)
        abline(h = 0)
        grid(length(bp))
        if (relative == TRUE) {
            abline(h = 100, col = "grey50", lty = 2)
        }
        barplot.default(x[k, ], las = 3, col = col, ylim = c(ymin, ymax), main = paste(title.prefix,
            rownames(x)[k]), border = border.col, add = TRUE, ...)
        if (!is.null(ciup) & !is.null(cidown) & noCI == FALSE) {
            if (all(!is.na(cidown[k, ])) & all(!is.na(ciup[k, ]))) {
                arrows(bp, cidown[k, ], bp, ciup[k, ], code = 3, angle = 90, length = 0.05,
                  col = colCI, lwd = lwd)
            }
        }
        if (simplified == FALSE) {
            if(BW == FALSE){
                    colO = "green3"
                    colK = "royalblue"
                    colpv = "red2"
            }
            if(BW == TRUE){colO = colK = colpv = "grey20"}

            if ("pvperm" %in% names(x0)) {
                for (j in 1:ncol(x0$pvperm)) {
                  df <- diff(bp)
                  if (length(df) == 0) {
                    df <- 0.2
                  } else {
                    df <- df[1]
                  }
                  if (x0$pvperm[1, j] < alphaperm) {
                    text(bp[j] - df/4, x[k, j], "*o", pos = 2 + sign(x[k, j]), col = colO,
                      cex = cex.annot)
                  } else {
                    if (x0$pvperm[1, j] < 2 * alphaperm) {
                      text(bp[j] - df/4, x[k, j], ". o", pos = 2 + sign(x[k, j]),
                        col = "grey", cex = cex.annot)
                    }
                  }
                  if (x0$pvperm[2, j] < alphaperm) {
                    text(bp[j] + df[1]/4, x[k, j], "k*", pos = 2 + sign(x[k, j]),
                      col = colK, cex = cex.annot)
                  } else {
                    if (x0$pvperm[2, j] < 2 * alphaperm) {
                      text(bp[j] + df/4, x[k, j], "k.", pos = 2 + sign(x[k, j]),
                        col = "grey", cex = cex.annot)
                    }
                  }
                }
            }
            # pv=NULL
            if (!is.null(pv)) {
                txt <- rep("", ncol(x))
                txt[pv[k, ] < 0.05] <- "*"
                # txt[pv[k,]<0.01]='***'
                text(bp, x[k, ], txt, pos = 2 + sign(x[k, ]), col = colpv, cex = cex.annot)
            }
            if (is.null(pv)) {
                if (!is.null(ciup) & !is.null(cidown)) {
                  txt <- rep("", ncol(x))
                  txt[sign(cidown[k, ]) == sign(ciup[k, ]) & (abs(sign(cidown[k,
                    ])) + abs(sign(ciup[k, ])) > 0)] <- "*"
                  text(bp, x[k, ], txt, pos = 2 + sign(x[k, ]), col = colpv, cex = cex.annot)
                }
            }
        }
        if (simplified == TRUE) {
            if ("pvperm" %in% names(x0)) {
                txt <- rep("", ncol(x))
                txt[sign(cidown[k, ]) == sign(ciup[k, ]) & (abs(sign(cidown[k, ])) +
                  abs(sign(ciup[k, ])) > 0) & x0$pvperm[1, ] < alphaperm & x0$pvperm[2,
                  ] < alphaperm] <- "***"
                text(bp, x[k, ], txt, pos = 2 + sign(x[k, ]), col = colpv, cex = cex.annot)
            }
        }
        if (legend.text == TRUE) {
            if ("pvperm" %in% names(x0)) {
                legend("topleft", c("* : P-value (w.r.t. exp replicate)< 0.05", paste("*o: dOwnstream perm. P-value <",
                  round(alphaperm, 2)), paste("k*: bacKbone perm. P-value <", round(alphaperm,
                  2)), paste(".o:", round(alphaperm, 2), "< dOwnstream perm. P-value <",
                  round(2 * alphaperm, 2)), paste("k.: ", round(alphaperm, 2), "< bacKbone perm. P-value <",
                  round(2 * alphaperm, 2))), text.col = c(colpv, colO, colK,
                  "grey50", "grey50"), bty = "n", bg = NA, cex = cex.legend)
            } else {
                if (!is.null(ciup) & !is.null(cidown)) {
                  legend("topleft", c("*: P-value (w.r.t. exp replicate)< 0.05"),
                    text.col = c(colpv), bty = "n", bg = NA)
                }
            }
        }
        if (details == TRUE) {
            if ("test" %in% names(x0)) {
                ll <- vector("list", 2)
                for (k in 1:2) {
                  ll[[k]] <- x0$test[[k + 1]]
                }
                names(ll) <- names(x0$test)[2:3]
                ll <- lapply(ll, function(X) X[reorder, ])
                pp <- sapply(ll, function(Y) {
                  if (!is.matrix(Y)) {
                    Y <- matrix(Y, nrow = 1)
                  }
                  y <- apply(Y, 1, function(x) median(x, na.rm = TRUE))
                  return(y)
                })
                if (!is.matrix(pp)) {
                  nm <- names(pp)
                  pp <- matrix(pp, nrow = 1)
                  colnames(pp) <- nm
                }
                color <- "grey"
                if (length(bp) == 1) {
                  w0 <- 1
                } else {
                  w0 <- diff(bp)[1]
                }
                pp <- sapply(ll, function(Y) {
                  if (!is.matrix(Y)) {
                    Y <- matrix(Y, nrow = 1)
                  }
                  y <- apply(Y, 1, function(x) quantile(x, na.rm = TRUE, probs = 0.95))
                  return(y)
                })
                if (!is.matrix(pp)) {
                  nm <- names(pp)
                  pp <- matrix(pp, nrow = 1)
                  colnames(pp) <- nm
                }
                color2 <- "black"
                if (length(bp) == 1) {
                  w0 <- 1
                } else {
                  w0 <- diff(bp)[1]
                }
                segments(bp - w0/4, pp[, 1], bp + w0/4, pp[, 1], col = color2, lwd = 2,
                  lty = 1)
                segments(bp - w0/4, pp[, 2], bp + w0/4, pp[, 2], col = color2, lwd = 2,
                  lty = 33)
                if (legend.text == TRUE) {
                  legend("topright", c(paste("95%", c("'o'", "'k'"), colnames(pp))),
                    lty = c(1, 2), lwd = 2, col = c(rep(color2, ncol(pp))), bty = "n")
                }
            }
        }
    }
    par(bg = "white")
    return(bps)
}
