#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Get leading nodes (base function)
#'
#' @param Q A numerical matrix. Q metric of the NPA computation
#' @param Y A numerical matrix. Y metric of the NPA computation
#' @param prop A numerical value. A threshold value, for leading nodes
#' detection
#' @param plotit A logical. Default is \code{TRUE}, a plot is generated.
#' @param cex.txt A numerical value. Text size scaling in the plot
#' @return A R list object with leadingNodes data.frame, names and values of
#' contribution 
getLE <- function(Q, Y, prop = 0.8, plotit = TRUE, cex.txt = 0.7) {
    D <- 2 * Q %*% Y
    LEnodes <- lapply(1:ncol(D), function(j) {
        ord <- order((0.5 * D * Y)[, j], decreasing = TRUE)
        x <- (0.5 * D * Y)[ord, j]
        f <- Y[ord, j]
        flagx <- 0
        if (all(x == 0)) {
            x <- x + 10
            flagx <- 1
        } else {
            x <- x/sum(x)
        }
        in0 <- which(cumsum(x) < prop * sum(x))
        if (length(in0) == 0) {
            if (x[1] >= prop * sum(x)) {
                in0 <- 1
            }
        }
        y <- names(x)[in0]
        if (flagx == 1) {
            x <- 0 * x
        }
        return(list(leadingNodes = cbind(GeneId = y, Sign = sign(f)[in0], Contrast = rep(gsub("\n",
            "", colnames(Y)[j], fixed = TRUE), length(y))), names = names(x), x = x,
            cumsum = cumsum(x)))
    })
    names(LEnodes) <- colnames(D)
    LEnodes <- lapply(LEnodes, function(x) {
        y <- x
        if (class(y$leadingNodes) != "matrix") {
            y$leadingNodes <- matrix(y$leadingNodes, nrow = 1)
        }
        return(y)
    })
    if (plotit == TRUE) {
        for (k in 1:length(LEnodes)) {
            x <- LEnodes[[k]]
            in0 <- which(x$cumsum >= prop)[1]:length(x$cumsum)
            if (length(in0) == length(x$cumsum)) {
                in0 <- 2:length(x$cumsum)
            }
            par(mar = c(5, 4, 8, 2))
            plot(x$cumsum, ylab = "Contribution to NPA", xlim = c(-0.15 * length(x$cumsum),
                length(x$cumsum) + 1), pch = 20, cex = c(rep(0.5, length(x$cumsum) -
                length(in0)), rep(1, length(in0))), col = c(rep("orange2", length(x$cumsum) -
                length(in0)), rep("royalblue", length(in0))), main = paste("Leading edges\n",
                names(LEnodes)[k]))
            if (length(in0) < length(x$cumsum)) {
                text((1:length(x$cumsum))[-in0], x$cumsum[-in0], names(x$cumsum)[-in0],
                  cex = cex.txt)
            }
            grid()
            abline(h = 0.8, v = in0[1] - 0.5)
        }
    }
    return(LEnodes)
}
