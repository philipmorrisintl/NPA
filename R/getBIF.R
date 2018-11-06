#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Compute BIF based on NPA scoring
#'
#' @param npall0 A R list object. List of NPA objects for different networks.
#' @param which An integer vector. Indexes of contrasts to use.
#' @param relativeto An integer value. Index of the NPA used as reference.
#' @param group.relativeMAX factor, for selecting comparisons used as reference.
#' @param lev A numerical value. Levels at which the "OK: stats are significant, default value is 0.05.
#' @param nets A string vector. Network family names to be used.
#' @param weighted A logical. Default is \code{TRUE}, weighted BIF is computed.
#' @param force.weighted A logical. Default is \code{FALSE}. Used to force
#' weighted mode even if knitted networks are used.
#' @param ... Additional optional parameter to be passed to getWeightedBIF.
#' @return A R list object containing overall BIF results in a slot and BIF
#' results per network families in other slots.
#'
getBIF <- function(npall0, which = 1:length(npall0[[1]]$coefficients), relativeto = NULL,
    group.relativeMAX = NULL, lev = 0.05, nets = NULL, weighted = TRUE, force.weighted = FALSE,
    ...) {
    if (all(getsplit(names(npall0), "/", 1) %in% c("Cell Proliferation", "Cell Stress",
        "DACS", "IPN", "TRAG", "CV-IPN"))) {
        if (weighted == TRUE) {
            warning("Weighted is only valid fo Knitted networks! Setting weighted back to FALSE...")
            if (force.weighted == FALSE) {
                weighted <- FALSE
            }
        }
    }
    if (weighted == TRUE) {
        bif <- getWeightedBIF(npall0, which = which, relativeto = relativeto, group.relativeMAX = group.relativeMAX,
            lev = lev, nets = nets, ...)
        attr(bif, "method") <- "weighted"
    } else {
        stop("Non weighted BIF not suported anymore")
    }
    bif <- lapply(bif, function(bif0) {
        bif0$starplot <- apply(sqrt(t(bif0$contrib)), 2, function(x) x * bif0$rbif[,
            colnames(bif0$rbif) == "BIF"])
        bif0$starplot.area <- bif0$starplot^2
        if (min(dim(bif0$contrib)) == 1) {
            C <- matrix(bif0$starplot, ncol = ncol(t(bif0$contrib)), nrow = nrow(t(bif0$contrib)))
            rownames(C) <- rownames(t(bif0$contrib))
            colnames(C) <- colnames(t(bif0$contrib))
            bif0$starplot <- C
        }
        bif0$pie <- t(bif0$contrib)
        bif0$coefficients <- 100 * bif0$rbif[, colnames(bif0$rbif) == "BIF"]^2
        bif0$coefficients <- as.matrix(bif0$coefficients)
        colnames(bif0$coefficients) <- "RBIF"
        return(bif0)
    })
    return(bif)
}
