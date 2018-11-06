#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Get substrings of a \code{character} vector.
#'
#' Get some sub-string of a \code{character} vector.
#' @param nm0 A \code{character} \code{vector}.
#' @param splitarg A \code{character} string containing regular expression(s) (unless fixed = TRUE) to use
#' for splitting.
#' @param k An \code{integer}.
#' @param remove A \code{logical} argument for keeping or removing the matched substrings.
#' @param fixed \code{logical}. If TRUE match split exactly, otherwise use regular expressions.
#' @param last \code{logical}. If TRUE, the last matched substring will be extracted.
#' @param last.n \code{integer}.
#' @return A \code{character} \code{vector} of substrings.
getsplit <- function(nm0, splitarg, k=1, remove=FALSE, fixed=TRUE, last=FALSE,
                     last.n=1) {
    nm0 <- as.character(nm0)
    if (last == FALSE) {
        if (remove == FALSE) {
            y <- sapply(
                strsplit(nm0, splitarg, fixed=fixed),
                function(x) {
                    paste(x[k], collapse=splitarg, sep="")
                }
            )
        }
        if (remove == TRUE) {
            y <- sapply(
                strsplit(nm0, splitarg, fixed=fixed),
                function(x) {
                    paste(x[-k], collapse=splitarg, sep="")
                }
            )
        }
    } else {
        if (remove == FALSE) {
            y <- sapply(
                strsplit(nm0, splitarg, fixed=fixed),
                function(x) {
                    paste(
                        x[(length(x) - last.n + 1):length(x)],
                        collapse=splitarg,
                        sep=""
                    )
                }
            )
        }
        if (remove == TRUE) {
            y <- sapply(
                strsplit(nm0, splitarg, fixed=fixed),
                function(x) {
                    paste(
                        x[-c((length(x) - last.n + 1):length(x))],
                        collapse=splitarg,
                        sep=""
                    )
                }
            )
        }
    }
    names(y) <- names(nm0)
    return(y)
}
