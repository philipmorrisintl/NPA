#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Ranem contrast within an NPAall object
#'
#' @param npall A R list object. List of NPA objects for different networks
#' @param nm A string vector. Alternative names for contrasts.
#' @return A R list object. NPAall object with new contrast names.
#'
NPAallrename <- function(npall, nm) {
    if (length(nm) != length(npall[[1]]$coefficients)) {
        stop("nm is not of the right length")
    }
    npall_renamed <- lapply(npall, function(np) {
        names(np$coefficients) <- nm
        names(np$ci.up) <- nm
        names(np$ci.down) <- nm
        colnames(np$nodes.coefficients) <- nm
        colnames(np$nodes.coefficients.ci.down) <- nm
        colnames(np$nodes.coefficients.ci.up) <- nm
        if (!is.null(np$nodes.coefficients.pvalue)) {
            colnames(np$nodes.coefficients.pvalue) <- nm
        }
        if (!is.null(np$test)) {
            rownames(np$test$downstream) <- nm
            rownames(np$test$backbone) <- nm
            colnames(np$test$pv) <- nm
        }
        colnames(np$pvperm) <- nm
        if (!is.null(np$pvperm.coefficients)) {
            colnames(np$pvperm.coefficients) <- nm
        }
        if ("data" %in% names(np)) {
            names(np$data) <- nm
        }
        return(np)
    })
    if (!is.null(attr(npall, "data"))) {
        id <- attr(npall, "data")
        names(id) <- nm
        attr(npall_renamed, "data") <- id
    }
    attr(npall_renamed, "species") <- attr(npall, "species")
    attr(npall_renamed, "exp.subtype") <- attr(npall, "exp.subtype")
    attr(npall_renamed, "tissue.network") <- attr(npall, "tissue.network")
    return(npall_renamed)
}
