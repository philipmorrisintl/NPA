#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Creates a color scale.
#'
#' @param x A \code{vector of numeric} of the data values.
#' @param col1 Color 1
#' @param col2 Color 2
#' @param col3 Color 3
#' @param col4 Color 4
#' @param col5 Color 5
#' @param signed A \code{logical} that specifies whether a symmetric color scale should be created.
#' @param minx Minimum value.
#' @param maxx Maximum value.
#'
#' @return A vector of colors.
colorscale <- function(x, col1 = "navy", col2 = "blue", col3 = "white", col4 = "orange",
    col5 = "red3", signed = FALSE, minx = min(x, na.rm = TRUE), maxx = max(x, na.rm = TRUE)) {
    if (length(col1) == 5) {
        col2 <- col1[2]
        col3 <- col1[3]
        col4 <- col1[4]
        col5 <- col1[5]
        col1 <- col1[1]
    }
    # if (signed == TRUE & (min(x, na.rm = TRUE) > 0 | max(x, na.rm = TRUE) < 0)) {
        # warning("No real signed version possible")
    # }
    if (signed == TRUE) {
        cole <- rep(NA, length(x))
        if (length(cole[x < 0 & !is.na(x)]) > 0) {
            cole[x < 0 & !is.na(x)] <- colorscale0(c(x[x < 0 & !is.na(x)], 0), col1,
                col2, col3, col4 = NULL, col5 = NULL, minx = minx, maxx = 0)[-length(c(x[x <
                0 & !is.na(x)], 0))]
        }
        if (length(cole[x >= 0 & !is.na(x)]) > 0) {
            cole[x >= 0 & !is.na(x)] <- colorscale0(c(0, x[x >= 0 & !is.na(x)]),
                col3, col4, col5, col4 = NULL, col5 = NULL, minx = 0, maxx = maxx)[-1]
        }
    }
    if (signed == FALSE) {
        cole <- colorscale0(x, col1, col2, col3, col4, col5, minx = minx, maxx = maxx)
    }
    return(cole)
}


#' Creates a color scale (base function).
#'
#' @importFrom gplots colorpanel
#' @param x A \code{vector of numeric} of the data values.
#' @param col1 Color 1
#' @param col2 Color 2
#' @param col3 Color 3
#' @param col4 Color 4
#' @param col5 Color 5
#' @param minx Minimum value.
#' @param maxx Maximum value.
#'
#' @return Vector of colors
colorscale0 <- function(x, col1 = "navy", col2 = "blue", col3 = "white", col4 = "orange",
    col5 = "red3", minx = min(x, na.rm = TRUE), maxx = max(x, na.rm = TRUE)) {
    x <- c(minx, maxx, x)
    if (!is.null(col4) & !is.null(col5))
        color0 <- mypanel(10001, col1, col2, col3, col4, col5)
    if (is.null(col4) | is.null(col5)) {
        color0 <- colorpanel(10001, col1, col2, col3)
    }
    color0 <- color0[findInterval(x, seq(minx - 1e-06, maxx + 1e-06, length.out = 10001),
        all.inside = TRUE)]
    color0 <- color0[-c(1:2)]
    return(color0)
}
