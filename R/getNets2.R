#' Get Network familisd CBN networks
#'
#' @param input A R list object or a string vector. If list if provided,
#' slots names will be used, otherwise, string vector
#' @param levels A string vector. Network family names for second generation of
#' networks (CBN 1.3)
#' @param BW A logical. Default is \code{FALSE}, colored values will be
#' provided for networks and families names, otherwise grayscale colors.
#' @return A R list object with networks family names, associated colors.

getNets2 <- function(input, levels = c("CFA", "CPR", "CST", "IPN", "TRA", "VIP"),
                     BW = FALSE) {
    if (!is.list(input) & !is.character(input)) {
        stop("input must be a character vector or a list")
    }
    if (is.list(input)) {
        nm = names(input)
    }
    if (is.character(input)) {
        nm = input
    }

    nets = sapply(strsplit(nm, " / ", fixed = TRUE), function(x) x[1])
    if (length(grep("DACS", nets)) > 0) {
        nets[nets == "DACS"] = sapply(strsplit(nm[nets == "DACS"], "-", fixed = TRUE),
                                      function(x) x[1])
    }
    nets = factor(nets, levels = levels)
    col00 = c(rainbow(9, s = 0.6, v = 0.75), "#E36115")[c(3, 1, 2, 8, 6, 10)]
    if (BW == TRUE) {
        col00 = gray.colors(nlevels(nets), start = 0.1, end = 0.9, gamma = 2.2, alpha = NULL)
    }

    colnet = col00[unclass(nets)]
    col0 = col00[which(table(nets) > 0)]
    res = list(nets = factor(nets), col = colnet, col0 = col0)
    return(res)
}
