#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Get contribution matrix a each network family (in columns) for each
#' contrast (in row)
#'
#' @param bif0  A R list object. BIF slot of the results given by getBIF
#' function
#'
#' @return A numerical matrix with contribution to the BIF of each network
#' family for each contrast
getC <- function(bif0) {
  bif0$contrib <- bif0$contrib[, , drop = FALSE]
  bif0$r2 <- bif0$r2[, , drop = FALSE]
  bif0$rbif <- bif0$rbif[, , drop = FALSE]
  bif0$relativeto <- bif0$relativeto[, drop = FALSE]
  allbifs <- bif0$rbif[, which(colnames(bif0$rbif) != "BIF"),
    drop = FALSE] * 100
  r2 <- bif0$r2[, which(colnames(bif0$r2) != "BIF"), drop = FALSE] *
    100
  relto <- bif0$relativeto
  C0 <- apply(sqrt(t(bif0$contrib)) * 100, 2, function(x) x *
    bif0$rbif[, colnames(bif0$rbif) == "BIF"])
  if (min(dim(bif0$contrib)) == 1) {
    C0 <- matrix(C0, ncol = ncol(t(bif0$contrib)), nrow = nrow(t(bif0$contrib)))
    rownames(C0) <- rownames(t(bif0$contrib))
    colnames(C0) <- colnames(t(bif0$contrib))
  }
  return(C0)
}
