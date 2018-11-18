#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Draw NPA backbone with scores in JavaScript for display in web browser
#'
#' @importFrom RGraph2js graph2js
#' @param np A R list object. NPA scores results
#' @param pathout A character vector. Folder path where to generated files
#' @param filename A character vector. HTML filename containing the graph
#' @param which A integer vector. Indexes of contrasts to use.
#' @param open.in.browser A logical. Default is \code{FALSE}, default web
#' browser will not be opened.
#' @param text.nodes A matrix of character vectors. Text content per nodes and
#' contrasts
#' @param opts A R list object. Options to be passed to graph2js function
#' @param ... Optional additional parameters to be passed to graph2js function
#' @return A R list object. Object returned by graph2js function call.
#' @importFrom stats na.omit
#' @importFrom utils browseURL
#' @include getNPALE.R
#' @include colorscale.R
#'
drawNPAjs <- function(np, pathout = getwd(), filename = NULL, which = NULL, open.in.browser = FALSE,
    text.nodes = NULL, opts = list(), ...) {
    if (!is.null(which)) {
        np <- NPAsubset(np, which)
    }
    ln <- getNPALE(np, plotit = FALSE)
    ln2 <- lapply(ln, function(x) {
        ranks <- 1:nrow(x$leadingNodes)
        if (nrow(x$leadingNodes) == 0) {
            ranks <- 1:length(x$leadingNodes)
        }
        data.frame(Id = x$leadingNodes[, 1], Rank = ranks)
    })
    ln3 <- lapply(ln2, function(x) return(x[order(x$Id), ]))
    if (is.null(text.nodes)) {
        textnode <- matrix("", nrow(np$nodes.coefficients), ncol(np$nodes.coefficients))
        rownames(textnode) <- rownames(np$nodes.coefficients)
        colnames(textnode) <- paste("TEXT", colnames(np$nodes.coefficients))
        for (k in 1:ncol(textnode)) {
            textnode[match(ln3[[k]]$Id, rownames(textnode)), k] <- paste("LN", ln3[[k]]$Rank,
                sep = "")
        }
        textnode[sign(np$nodes.coefficients.ci.down) * sign(np$nodes.coefficients.ci.up) ==
            1] <- paste(textnode[sign(np$nodes.coefficients.ci.down) * sign(np$nodes.coefficients.ci.up) ==
            1], "*", sep = " ")
    } else {
        textnode <- text.nodes
    }
    colornode <- matrix(colorscale(np$nodes.coefficients, signed = TRUE), nrow = nrow(np$nodes.coefficients),
        ncol = ncol(np$nodes.coefficients))
    colnames(colornode) <- paste("Color", colnames(np$nodes.coefficients))
    rownames(colornode) <- rownames(np$nodes.coefficients)
    shapeNode <- rep("rect", nrow(np$nodes.coefficients))
    n.prop <- data.frame(shape = shapeNode)
    rownames(n.prop) <- rownames(np$nodes.coefficients)
    getSignedAdj <- function(E1) {
        nds <- unique(as.vector(E1[, c(1, 3)]))
        A <- tapply(as.numeric(E1[, 2]), list(factor(E1[, 1], levels = nds), factor(E1[,
            3], levels = nds)), sum)
        A[is.na(A)] <- 0
        A[abs(A) > 1] <- sign(A[abs(A) > 1])
        return(A)
    }
    E <- as.matrix(np$model$model$edges[, c(2:4)])
    E0 <- E
    E0[, 2] <- "1"
    E0[E[, 2] %in% c("=|", "-|"), 2] <- "-1"
    a <- getSignedAdj(E0)
    if (nrow(np$nodes.coefficients) > 30) {
        opts$displayNetworkEveryNLayoutIterations <- 0
    }
    m <- match(rownames(n.prop), rownames(a))
    nodes.prop <- as.data.frame(n.prop[m, ])
    nodes.prop <- na.omit(nodes.prop)
    rownames(nodes.prop) <- rownames(n.prop)[!is.na(m)]
    colnames(nodes.prop) <- colnames(n.prop)
    values <- t(apply(np$nodes.coefficients, 1, FUN = function(x) {
        x/max(abs(x))
    }))
    m <- match(rownames(a), rownames(values))
    inner.values <- as.data.frame(values[m, ])
    inner.values <- na.omit(inner.values)
    stopifnot(all(rownames(inner.values) == rownames(a)))
    # rownames(inner.values) <- rownames(values)[!is.na(m)]
    colnames(inner.values) <- colnames(values)
    m <- match(rownames(a), rownames(colornode))
    inner.colors <- as.data.frame(colornode[m, ])
    inner.colors <- na.omit(inner.colors)
    stopifnot(all(rownames(inner.colors) == rownames(a)))
    # rownames(inner.colors) <- rownames(colornode)[!is.na(m)]
    colnames(inner.colors) <- colnames(colornode)
    m <- match(rownames(a), rownames(textnode))
    inner.texts <- as.data.frame(textnode[m, ])
    inner.texts <- na.omit(inner.texts)
    # rownames(inner.texts) <- rownames(textnode)[!is.na(m)]
    colnames(inner.texts) <- colnames(textnode)
    stopifnot(all(rownames(inner.texts) == rownames(a)))
    l <- graph2js(a, innerValues = inner.values, innerColors = inner.colors, innerTexts = inner.texts,
        nodesProp = nodes.prop, opts = opts, outputDir = pathout, filename = filename,
        ...)
    if (!is.null(pathout)) {
        if (open.in.browser) {
            browseURL(l$filepath)
        }
    }
    return(l)
}
