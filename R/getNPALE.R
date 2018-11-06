#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Get NPA leading nodes or leading genes
#'
#' @param npares A R list object. NPA scores results.
#' @param plotit A logical. Default is \code{FALSE}, not plot is generated.
#' @param which A integer vector. Indexes of contrast to show in the figure
#' @param cex.txt A numerical value. Text size scaling.
#' @param prop A numerical vector of length 2. Threshold values, for leading nodes
#' and leading genes detection.
#' @param type A character vector. Value in "nodes" or "genes" to get leading
#'nodes or leading genes.
#' @param node A character vector. Optional node name.
#' @return A R list object with leadingNodes data.frame, names and values of
#' contribution.
#'
getNPALE <- function(npares, plotit = FALSE, which = 1:length(npares$coefficients),
    cex.txt = rep(0.6, 2), prop = c(0.8, 1), type = c("nodes", "genes")[1], node = NULL) {

    which0 <- which
    if (length(which0) == 1) {
        which <- c(which, which)
    }
    if (is.null(node)) {
        if (type == "nodes") {
            le <- getLE(npares$Qbackbone, npares$nodes.coefficients[, which, drop = FALSE],
                prop = prop[1], plotit = plotit, cex.txt = cex.txt[1])
        }
        if (type == "genes") {
            K <- t(npares$L3invL2) %*% npares$Qbackbone %*% npares$L3invL2
            le <- getLE(K, npares$Y[, which, drop = FALSE], prop = prop[2], plotit = plotit,
                cex.txt = cex.txt[2])
            le <- lapply(le, function(x) {
                x$leadingNodes <- data.frame(Rank = 1:nrow(x$leadingNodes), x$leadingNodes)
                return(x)
            })
        }
    }
    if (!is.null(node)) {
        if (!node %in% rownames(npares$L3invL2)) {
            stop("node not in backbone!")
        }
        tmp <- apply(npares$Y[, which, drop = FALSE], 2, function(x) x * -npares$L3invL2[rownames(npares$L3invL2) ==
            node, ])
        le <- vector("list", ncol(tmp))
        names(le) <- colnames(tmp)
        for (k in 1:ncol(tmp)) {
            x <- tmp[, k]
            # sum(x) is backbone node coeff
            if (sum(x) >= 0) {
                nm <- names(cumsum(sort(x[x > 0], decreasing = TRUE)))[cumsum(sort(x[x >
                  0], decreasing = TRUE)) - max(x[x > 0]) <= 0.8 * (sum(x[x > 0]) -
                  max(x[x > 0]))]
            }
            if (sum(x) < 0) {
                nm <- names(cumsum(sort(x[x < 0], decreasing = FALSE)))[cumsum(sort(x[x <
                  0], decreasing = FALSE)) - min(x[x < 0]) >= 0.8 * (sum(x[x < 0]) -
                  min(x[x < 0]))]
            }
            in0 <- which(names(x) %in% nm)
            x <- x[in0]
            if (sum(x) >= 0) {
                x <- sort(x, decreasing = TRUE)
            }
            if (sum(x) < 0) {
                x <- sort(x, decreasing = FALSE)
            }
            le[[k]] <- list(leadingNodes = data.frame(Rank = c(1:length(x)), GeneId = names(x),
                Sign = sign(x), Contrast = rep(colnames(npares$nodes.coefficients[,
                  which, drop = F])[k], length(x))))
        }
    }
    if (length(which0) == 1) {
        le <- le[1]
    }
    return(le)
}
