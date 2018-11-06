#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Get Networks Familie based on network names
#'
#' @param input A R list object or a string vector. If list if provided,
#' slots names will be used, otherwise, string vector
#' @param BW A logical. Default is \code{FALSE}, colored values will be
#' provided for networks and families names, otherwise grayscale colors.
#' @return A R list object with networks family names, associated colors.

getNets <- function(input, BW=FALSE) {
    lev1 <- c("Cell Proliferation", "Cell Stress", "DACS/Apoptosis", "DACS/Autophagy",
        "DACS/DNA Damage", "DACS/Necroptosis", "DACS/Senescence", "IPN", "TRAG",
        "CV-IPN", "DACS")
    lev2 <- c("CFA", "CPR", "CST", "IPN", "TRA", "VIP")
    if (!is.list(input) & !is.character(input)) {
        stop("input must be a character vector or a list")
    }
    if (is.list(input)) {
        nm <- names(input)
    }
    if (is.character(input)) {
        nm <- input
    }
    nets <- sapply(strsplit(nm, " / ", fixed = TRUE), function(x) x[1])
    if (all(nets %in% lev1)) {
        res <- getNets1(input, BW = BW)
    }
    if (all(nets %in% lev2)) {
        res <- getNets2(input, BW = BW)
    }
    if (!all(nets %in% lev1) & !all(nets %in% lev2)) {
        res <- list(nets = factor(rep("", length(nm))), col = rep("grey80", length(nm)),
            col0 = "grey80")
    }
    return(res)
}
