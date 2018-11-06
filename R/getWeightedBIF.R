#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Get BIF result susing network overlaps and fairness principle
#'
#' @param npall0 A R list object. List of NPA objects for different networks.
#' @param which An integer vector. Indexes of contrasts to use.
#' @param relativeto An integer value. Index of the NPA used as reference.
#' @param group.relativeMAX factor, for selecting comparisons used as reference.
#' @param lev A numerical value. Levels at which the "OK: stats are significant, default value is 0.05.
#' @param nets A string vector. Network family names.
#' @param ... Optional additional parameters to be passed to getBIF0weighted.
#' function
#' @return A R list object. BIF results.
#'
getWeightedBIF <- function(npall0, which = 1:length(npall0[[1]]$coefficients), relativeto = NULL,
    group.relativeMAX = NULL, lev = 0.05, nets = NULL, ...) {

    if (is.null(nets)) {
        nets <- getNets(names(npall0))$nets
    }
    bif <- vector("list", nlevels(nets) + 1)
    names(bif) <- c("BIF", levels(nets))
    bif[[1]] <- getBIF0weighted(npall0, which = which, relativeto = relativeto, group.relativeMAX = group.relativeMAX,
        lev = lev, nets = nets, ...)
    for (k in 1:nlevels(nets)) {
        nptmp <- npall0[which(nets == levels(nets)[k])]
        names(nptmp) <- gsub(levels(nets)[k], "", names(nptmp), fixed = TRUE)
        names(nptmp) <- gsub("/", "", names(nptmp), fixed = TRUE)
        bif[[k + 1]] <- getBIF0weighted(nptmp, which = which, relativeto = bif[[1]]$relativeto,
            group.relativeMAX = group.relativeMAX, lev = lev, refs = bif[[1]]$sc$BIF,
            nets = factor(names(nptmp)), ...)
    }
    return(bif)
}
