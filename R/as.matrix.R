#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

NPA$unlock()
# ====================================================================
# Method: as.matrix
# ====================================================================
#' Generate a NPA results matrix showing ranked leading nodes,
#' sign and statistics
#' @exportMethod as.matrix
#' @param x NPA R6 class instance
#' @param type a character vector. If `type` is set to "coefficients",
#' a numeric matrix with NPA nodes coefficents for each comparison is returned.
#' If `type` is set to "leadingnodes", a character matrix with leading nodes
#' for each comparison is returned, showing rank for a given and additional
#' information
#'
#' @docType methods
#' @include getNPALEtable2.R
setMethod(
    "as.matrix",
    signature(x = "NPA"),
    function(x, type = c("coefficients", "leadingnodes"))
        as.matrix.NPA(x, type)
)

as.matrix.NPA <- function(x, type = c("coefficients", "leadingnodes")) {
    type <- match.arg(type)
    x$as.matrix(type)
}

NPA$set("public", "as.matrix", function(type) {
    if (type == "coefficients") {
        return(private$data$nodes.coefficients)
    } else {
        m <- getNPALEtable2(private$data)
        attr(m, "rank") <- "The first number indicates the node ranking."
        attr(m, "leading") <- paste0(
            "A * after the ranking indicates that the ",
            "node is a leading node and a ! indicates a non-significant ",
            "differential backbone value with respect to experimental ",
            "variation. The (+) or (-) signs represent the inferred ",
            "activation or inhibition of the node (based on its differential ",
            "backbone value)."
        )
        attr(m, "contribution") <- paste0(
            "The percentages indicate the contribution ",
            "of the node to the NPA score."
        )
    }
    return(m)
})

NPA$lock()
