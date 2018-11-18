#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Starplot per networks
#' @param bif A list containing the BIF metrics.
#' @param m1 An integer for number of rows layout. By default `NULL` for automatic layout.
#' @param m2 An integer for number of columns layout. By default `NULL` for automatic layout.
#' @param col A string vector. Colors for the networks families. If not specified, default palette is used.
#' @param reorder A integer vector. Indexes of networks families to be reordered.
#' @param which.to.plot A integer vector. Indexes of network families to be ploted.
#' @param contrib.txt A logical. It `TRUE`, contibution legend text is displayed.
#' @param bw A logical. Default is `FALSE`, colored figure is generated.
#' @param cex.txt A number. The character expansion factor for network names text.
#' @param marleft A number. Left margin value for the figure.
#' @param col.contribtext A character vector. Color of the sector separation for network famililes.
#' @param text.network A logical. If `TRUE`, network names abreviations are drawn on the figure.
#' Default is `FALSE`.
#' @param cex.textnetwork A numerical value. Scaling of text of the network if `text.network` is `TRUE`.
#' @param text.subnetwork A logical value. If `TRUE` and `text.network` argument is `TRUE`, subnetworks
#' abreviation are drawn. Default is `TRUE`.
#' @param cex.network. A numerical value. Text scaling factor for network names.
#' @param add.networks A logical. If `TRUE` (default), the network family color legend (and name is `text.network` is `TRUE`) is
#' shown.
#' @param title.prefix A character vector for prefifing the title of the plot if `add.legend` is `TRUE`.
#' @param names.arg  A string vector. Custom names for comparisons
#' @param lwd.axis A numerical value. Scaling factor for width of sector separators.
#' @param lty.axis An integer value. Type of lines for the sector separators.
#' @param cex.legend A numerical value. If legend is used (`add.legend` is `TRUE`), scaling factor for networks family names.
#' @param cex.main A numerical value. Text scaling factor for titles (in legend and comparisons names).
#' @param add.legend A logical. If `TRUE`, a legend is added the figure for network families colors and the overall contributions.
#' @param radius.text A logical. If `TRUE`, the RBIF value and sigma is shown aside the comparison name. If `FALSE`, only
#' the comparison name is shown. Default is `TRUE`.
#' @param layout.auto A logical. If `TRUE`, an automatic layout is performed using `m1` and `m2` values.
#' @param BW A logical. If `TRUE`, grayscale colors are used. Defaul is `FALSE` .
#' @param col.border A character vector. Color of segments for sectors in starplots.
#' @param text.slice A string vector. Default is `NULL`, names from the bif object is used for networks text.
#' @param cst A numerical value. If provided, used for adjusting positioning of
#' starplot/text
#' @param border.color. A character vector. Color for starplot sector border.
#' @param cst.border A numerical value. Starplot sector text size positioning adjustment.
#' @param ... Additional aguments to be passed to plot function
#'
#' @importFrom plotrix arctext
#' @importFrom graphics pie
#' @export
starplotBIFcontrasts <- function(bif, m1 = NULL, m2 = NULL, col = Colors(nrow(bif[[1]]$rbif)),
    reorder = 1:ncol(bif[[1]]$contrib), which.to.plot = 1:ncol(bif[[1]]$contrib),
    contrib.txt = TRUE, bw = FALSE, cex.txt = 0.7, marleft = 13,
    col.contribtext = "grey40", text.network = FALSE, cex.textnetwork = 1, text.subnetwork = text.network,
    cex.network = 0.8, add.networks = TRUE, title.prefix = "", names.arg = NULL,
    lwd.axis = 2, lty.axis = 3, cex.legend = 1.5, cex.main = 1.2, add.legend = FALSE,
    radius.text = TRUE, layout.auto = TRUE, BW = FALSE, col.border = NULL, text.slice = NULL,
    cst = NULL, border.color = NULL, cst.border = 1.1, ...) {
    box <- FALSE
    getC <- function(bif0) {
        bif0$contrib <- bif0$contrib[, reorder, drop = FALSE]
        bif0$r2 <- bif0$r2[reorder, , drop = FALSE]
        bif0$rbif <- bif0$rbif[reorder, , drop = FALSE]
        bif0$relativeto <- bif0$relativeto[reorder, drop = FALSE]
        allbifs <- bif0$rbif[, which(colnames(bif0$rbif) != "BIF"), drop = FALSE] *
            100
        r2 <- bif0$r2[, which(colnames(bif0$r2) != "BIF"), drop = FALSE] * 100
        relto <- bif0$relativeto
        C <- C0 <- apply(sqrt(t(bif0$contrib)) * 100, 2, function(x) x * bif0$rbif[,
            colnames(bif0$rbif) == "BIF"])
        if (min(dim(bif0$contrib)) == 1) {
            C <- C0 <- matrix(C, ncol = ncol(t(bif0$contrib)), nrow = nrow(t(bif0$contrib)))
            rownames(C) <- rownames(C0) <- rownames(t(bif0$contrib))
            colnames(C) <- colnames(C0) <- colnames(t(bif0$contrib))
        }
        return(C)
    }
    Cs <- lapply(bif, getC)
    C <- Cs$BIF
    r2s.txt <- round(bif$BIF$r2[, colnames(bif$BIF$r2) == "BIF", drop = FALSE], 2)
    rbifs <- 100 * bif$BIF$rbif[, colnames(bif$BIF$rbif) == "BIF"]^2
    if (is.null(m1) | is.null(m2)) {
        m2 <- ceiling(sqrt(length(which.to.plot)))
        m1 <- 0
        while (m1 * m2 < length(which.to.plot)) {
            m1 <- m1 + 1
        }
    }
    if (!is.null(names.arg)) {
        if (length(names.arg) != nrow(C)) {
            stop("names.arg must be of righ length")
        }
        rownames(C) <- names.arg
    }
    # C=apply(C0,2,function(x) x/max(x)) C[is.na(C)]=0
    if (is.null(m1) | is.null(m2)) {
        m2 <- ceiling(sqrt(length(which.to.plot)))
        m1 <- 1
    }
    madd <- 2
    if (add.legend == FALSE | text.network == TRUE) {
        madd <- 0
    }
    while (m1 * m2 < length(which.to.plot) + madd) {
        m1 <- m1 + 1
    }
    col0 <- col02 <- getNets(colnames(C), BW = BW)$col
    if (!is.null(col.border)) {
        col02 <- rep(col.border, length(col0))
    }
    if (BW == TRUE) {
        col02 <- rep("black", length(col0))
    }
    # layout(cbind(1:m1,matrix(m1+c(1:(m1*m2)),ncol=m2,byrow=TRUE)))
    if (layout.auto == TRUE) {
        par(mar = c(2, marleft, 7, 3), mfrow = c(m1, m2))
    }
    # barplot(sort(colSums(t(bif$contrib)*100)/ncol(C)),col=col0[order(colSums(t(bif$contrib)))],horiz=TRUE,main=paste(title.prefix,'\nOverall
    # Contributions'),las=1,cex.names=cex.txt)
    par(mar = c(1, 1, 4, 1), bty = "n", pty = "s")
    abbreviations <- NULL
    pinum <- 3.14159259
    for (k in which.to.plot) {
        par(pty = "s")
        theta <- 2 * pinum/ncol(C)
        max0 <- ylmax <- max(C)
        poly0 <- cbind(max0 * cos(theta * seq(0, ncol(C), length.out = 200)), max0 *
            sin(theta * seq(0, ncol(C), length.out = 200)))
        tit00 <- paste(gsub("\n", "", rownames(C)[k], fixed = TRUE), " RBIF=", round(rbifs[k]),
            "% \n(delta=", r2s.txt[k], ",ref=", gsub("\n", "", rownames(C))[bif$BIF$relativeto[k]],
            ")", sep = "")
        part1 <- paste0(gsub("\n", "", rownames(C)[k], fixed = TRUE), " (RBIF=",
            round(rbifs[k]), "%,")
        part2 <- paste0("=", r2s.txt[k], ")")
        tit00 <- bquote(.(part1) ~ delta ~ .(part2))
        if (radius.text == FALSE) {
            tit00 <- paste(gsub("\n", "", rownames(C)[k], fixed = TRUE))
        }
        if (add.networks == TRUE) {
            cst <- 2
        } else {
            if (text.network == TRUE) {
                if (is.null(cst)) {
                  cst <- 1.8
                }
            } else {
                cst <- 1
            }
        }
        plot(poly0[, 1], poly0[, 2], xlim = c(-cst * ylmax, cst * ylmax), ylim = c(-cst *
            ylmax, cst * ylmax), type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
            main = tit00, col = "grey85", cex.main = cex.main, ...)
        if (!is.null(border.color)) {
            for (g in 1:length(border.color)) {
                theta2 <- 2 * pinum/length(border.color)
                poly02 <- rbind(c(0, 0), cbind(cst.border * max0 * cos(theta2 * seq(g +
                  0.5, g + 1.5, length.out = 200)), cst.border * max0 * sin(theta2 *
                  seq(g + 0.5, g + 1.5, length.out = 200))), c(0, 0))
                mixcol <- (col2rgb(border.color) + 0.2 * col2rgb(rep("gray70", length(border.color))))
                mixcol <- apply(mixcol, 2, function(x) x/sum(x))
                bdcol2 <- apply(mixcol, 2, function(x) rgb(x[1], x[2], x[3]))
                # polygon(0.03 * max0+ poly02[, 1], -0.03 * max0+poly02[, 2], col = bdcol2[g],
                # border = 'white')
                polygon(poly02[, 1], poly02[, 2], col = border.color[g], border = "white")
            }
            cst.border2 <- max0 + (cst.border * max0 - max0)/2
            cst.border2 <- cst.border2/max0
            polygon(cst.border2 * poly0[, 1], cst.border2 * poly0[, 2], col = "white",
                border = NA)
        }
        polygon(0.03 * max0 + poly0[, 1], -0.03 * max0 + poly0[, 2], col = "grey60",
            border = NA)
        polygon(poly0[, 1], poly0[, 2], col = "grey80", border = NA)
        if (bif$BIF$relativeto[k] == k) {
            polygon(poly0[, 1], poly0[, 2], border = "black", density = 0, lwd = 2)
        }
        # rect(-2*ylmax,-2*ylmax,2*ylmax,2*ylmax,col='white',border='black')
        for (j in 1:ncol(C)) {
            r <- C[k, j]
            C2 <- Cs[[j + 1]]
            rs <- C2[k, ]
            max2 <- 0.8 * cst/3 * max0/2
            rs <- max2 * rs/max(rs)
            ns <- ncol(Cs[[j + 1]])
            relt <- bif[[j + 1]]$relativeto[k]
            if (!is.na(lty.axis)) {
                segments(0, 0, 1 * max0 * cos(theta * (0.5 + seq(j, j + 1, length.out = 20)))[10],
                  1 * max0 * sin(theta * (0.5 + seq(j, j + 1, length.out = 20)))[10],
                  lty = lty.axis, col = col.contribtext, lwd = lwd.axis)
            }
            polygon(rbind(c(0.01 * max0, -0.01 * max0), cbind(0.01 * max0 + r * cos(theta *
                (0.5 + seq(j, j + 1, length.out = 20))), -0.01 * max0 + r * sin(theta *
                (0.5 + seq(j, j + 1, length.out = 20))))), col = col02[j], border = col02[j])
            polygon(rbind(c(0, 0), cbind(r * cos(theta * (0.5 + seq(j, j + 1, length.out = 20))),
                r * sin(theta * (0.5 + seq(j, j + 1, length.out = 20))))), col = col0[j],
                border = col02[j])
            if (text.network == TRUE) {
                add.legend <- FALSE
                if (is.null(text.slice)) {
                  plotrix::arctext(gsub("DACS/", "", colnames(C)[j], fixed = TRUE), center = c(0,
                    0), radius = cst * max0, middle = theta * (0.5 + seq(j, j + 1,
                    length.out = 20))[10], stretch = 1.2, cex = cex.textnetwork)
                }
                if (!is.null(text.slice)) {
                  r00 <- cst * max0
                  angle <- theta * (0.5 + seq(j, j + 1, length.out = 20))[10]
                  text(r00 * cos(angle), r00 * sin(angle), text.slice[j], srt = theta,
                    cex = cex.textnetwork)
                }
            }
            # SIDE STAR
            if (add.networks == TRUE) {
                at <- c(1.5 * max0 * cos(theta * (0.5 + seq(j, j + 1, length.out = 20)))[10],
                  1.5 * max0 * sin(theta * (0.5 + seq(j, j + 1, length.out = 20)))[10])
                from <- c(1.05 * max0 * cos(theta * (0.5 + seq(j, j + 1, length.out = 20)))[10],
                  1.05 * max0 * sin(theta * (0.5 + seq(j, j + 1, length.out = 20)))[10])
                for (b in 10:0) {
                  eta <- theta * (0.5 + seq(j, j + 1, length.out = 20))[10]
                  to1 <- at + c(b/10 * max2 * cos(eta + pinum/2), b/10 * max2 * sin(eta +
                    pinum/2))
                  to2 <- at + c(b/10 * max2 * cos(eta + 3 * pinum/2), b/10 * max2 *
                    sin(eta + 3 * pinum/2))
                  polygon(rbind(from, to1, to2, from), col = paste("grey", round(70 +
                    20 * b/10), sep = ""), border = NA)
                }
                theta2 <- 2 * pinum/ns
                poly0 <- cbind(at[1] + max2 * cos(theta2 * seq(0, ns, length.out = 200)),
                  at[2] + max2 * sin(theta2 * seq(0, ns, length.out = 200)))
                polygon(0.05 * max2 + poly0[, 1], -0.05 * max2 + poly0[, 2], col = "grey70",
                  border = NA)
                polygon(poly0[, 1], poly0[, 2], col = "grey90", border = NA)
                outname <- c("to", "by", "of", "and", "the", "in")
                keepname <- c("TP53", "Th17", "TP63", "TP73", "Calcium", "Hedgehog",
                  "Jak", "Stat", "TNFR1Fas", "TNFR1", "Treg", "HIF1A", "NFKB", "MAPK",
                  "NFE2L2", "mTOR")
                # nm2=sapply(strsplit(gsub('-',' ',names(rs)),' '),function(x)
                # paste(sapply(x,function(y){n=nchar(y);if(n>4){n=1};if(y%in%outname){y=''}else{y=substr(y,1,n)};if(n==1){y=toupper(y)};return(y)}),collapse=''))
                nm2 <- NULL
                for (kk in 1:ncol(C2)) {
                  x <- strsplit(gsub("-", " ", gsub("^-", "", colnames(C2)[kk])),
                    " ")[[1]]
                  nm22 <- NULL
                  for (mm in 1:length(x)) {
                    y <- x[[mm]]
                    n <- nchar(y)
                    if (n > 1 & length(x[!x %in% outname]) >= 4) {
                      n <- 2
                    }
                    if (n > 3 & length(x[!x %in% outname]) < 4) {
                      n <- 3
                    }
                    if (n > 4 & length(x[!x %in% outname]) < 3) {
                      n <- 4
                    }
                    if (length(x) == 1) {
                      n <- 5
                    }
                    if (!y %in% keepname) {
                      if (y %in% outname) {
                        y <- ""
                      } else {
                        y <- substr(y, 1, n)
                      }
                      if (n == 1) {
                        y <- toupper(y)
                      }
                    }
                    if (y != "") {
                      tmp <- strsplit(y, "")[[1]]
                      tmp[1] <- toupper(tmp)[1]
                      y <- paste(tmp, collapse = "")
                    }
                    nm22 <- c(nm22, y)
                  }
                  nm22 <- paste(nm22, collapse = "")
                  nm2 <- c(nm2, substr(nm22, 1, min(nchar(nm22), 10)))
                }
                abbreviations <- rbind(abbreviations, cbind(gsub("//", "/", paste(colnames(C)[j],
                  names(rs), sep = "/"), fixed = TRUE), nm2))
                for (h in 1:length(rs)) {
                  polygon(rbind(at + c(0.03 * max2, -0.03 * max2), cbind(0.03 * max2 +
                    at[1] + rs[h] * cos(theta2 * (0.5 + seq(h, h + 1, length.out = 20))),
                    -0.03 * max2 + at[2] + rs[h] * sin(theta2 * (0.5 + seq(h, h +
                      1, length.out = 20))))), col = col02[j], border = NA)
                  polygon(rbind(at, cbind(at[1] + rs[h] * cos(theta2 * (0.5 + seq(h,
                    h + 1, length.out = 20))), at[2] + rs[h] * sin(theta2 * (0.5 +
                    seq(h, h + 1, length.out = 20))))), col = col0[j], border = col02[j])
                  if (text.subnetwork == TRUE) {
                    h2 <- seq(h, h + 1, length.out = 20)[10] + 0.5
                    text(at[1] + 1.1 * max2 * cos(theta2 * h2), at[2] + 1.1 * max2 *
                      sin(theta2 * h2), nm2[h], srt = ifelse(theta2 * h2 <= pinum/2 |
                      theta2 * h2 >= 3 * pinum/2, 360 * theta2 * h2/(2 * pinum),
                      360 * theta2 * h2/(2 * pinum) - 180), cex = cex.network)
                  }
                }
            }
            if (box == TRUE) {
                rect(0.9 * max0 * cos(theta * (0.5 + seq(j, j + 1, length.out = 20)))[10] -
                  0.12 * cex.txt * max0, 0.9 * max0 * sin(theta * (0.5 + seq(j, j +
                  1, length.out = 20)))[10] - 0.09 * cex.txt * max0, 0.9 * max0 *
                  cos(theta * (0.5 + seq(j, j + 1, length.out = 20)))[10] + 0.12 *
                  cex.txt * max0, 0.9 * max0 * sin(theta * (0.5 + seq(j, j + 1, length.out = 20)))[10] +
                  0.09 * cex.txt * max0, col = "white", border = "grey")
            }
            if (contrib.txt == TRUE) {
                ctrb.text <- paste("(", round(100 * t(bif$BIF$contrib)[k, j], 1),
                  "%)", sep = "")
                r00 <- max0 + (cst * max0 - max0)/2
                if (add.networks == TRUE) {
                  r00 <- max0 + (cst * max0 - max0)/7
                }
                plotrix::arctext(ctrb.text, center = c(0, 0), radius = r00, middle = theta *
                  (0.5 + seq(j, j + 1, length.out = 20))[10], stretch = 1.2, cex = cex.txt)
            }
            # text(0.9 * max0 * cos(theta * (0.5 + seq(j, j + 1, length.out = 20)))[10], 0.9
            # * max0 * sin(theta * (0.5 + seq(j, j + 1, length.out = 20)))[10],
            # paste(round(100 * t(bif$BIF$contrib)[k, j], 1), '%', sep = ''), col =
            # col.contribtext, cex = cex.txt)
        }
    }
    if (add.legend == TRUE) {
        pie(colSums(t(bif$BIF$contrib) * 100)/ncol(C), col = col0, labels = c(1:ncol(C)),
            border = NA, main = paste(title.prefix, "\nOverall Contributions"), cex.main = cex.main,
            cex = 0.9 * cex.main)
        plot(0, 0, xaxt = "n", yaxt = "n", col = "white", cex = 1e-04)
        legend("topleft", paste(1:ncol(C), gsub("DACS/", "", colnames(C), fixed = TRUE),
            sep = ":"), cex = cex.legend, col = col0, pch = 15, bty = "n")
    }
    if (!is.null(abbreviations)) {
        abbreviations <- unique(abbreviations)
        colnames(abbreviations) <- c("NetID", "Abbreviation")
    }
    par(pty = "m")
    colnames(r2s.txt) <- "delta"
    res <- list(radius = C, col = col0, RBIF = rbifs, abbreviations = abbreviations,
        contributions = t(bif$BIF$contrib), delta = r2s.txt)
    return(invisible(res))
}
