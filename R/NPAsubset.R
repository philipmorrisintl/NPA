#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Subset contrast in an NPA object
#'
#' @param np A R list object. A NPA score results.
#' @param in0 A integer vector. Indices of contrasts to keep in the subset.
#' @return np A R list object. A NPA score results with a subset of contrasts.
#'
NPAsubset <- function(np, in0) {
    np1 <- np
    np1$coefficients <- np$coefficients[in0]
    np1$coefficients.var <- np$coefficients.var[in0]
    np1$ci.up <- np$ci.up[in0]
    np1$ci.down <- np$ci.down[in0]
    np1$nodes.coefficients <- np$nodes.coefficients[, in0, drop = FALSE]
    np1$nodes.coefficients.ci.up <- np$nodes.coefficients.ci.up[, in0, drop = FALSE]
    np1$nodes.coefficients.ci.down <- np$nodes.coefficients.ci.down[, in0, drop = FALSE]
    np1$nodes.coefficients.pvalue <- np$nodes.coefficients.pvalue[, in0, drop = FALSE]
    if (!is.null(np$pvperm)) {
        np1$pvperm <- np$pvperm[, in0, drop = FALSE]
    }
    if (!is.null(np$pvperm.coefficients)) {
        np1$pvperm.coefficients <- np$pvperm.coefficients[, in0, , drop = FALSE]
    }
    np1$test[[1]] <- np$test[[1]][, in0, drop = FALSE]
    np1$test[[2]] <- np$test[[2]][in0, , drop = FALSE]
    np1$test[[3]] <- np$test[[3]][in0, , drop = FALSE]
    np1$Y <- np1$Y[, in0, drop = FALSE]
    if (!is.null(np1$lnContribs)) {
        np1$lnContribs <- np1$lnContribs[, in0]
    }
    return(np1)
}
