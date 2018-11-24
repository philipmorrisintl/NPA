#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Create colorpalette.
#'
#' @param n A \code{integer}.
#' @param name A \code{character} string used in rownames(brewer.pal.info).
#' @return A \code{funciton} to generate color palette.
#' @importFrom RColorBrewer brewer.pal
Colors <- function(n, name="Spectral") {
    return(colorRampPalette(brewer.pal(n=6, name=name)))
}

#' Inverse a matrix and generalize inverse it if singular
#'
#' Inverse a matrix and generalize inverse it if singular
#' @param M A \code{numeric} \code{matrix}.
#' @importFrom methods is
#' @return Inverse or pseudo inverse of the \code{matrix} M.
#'
solve2 <- function(M) {
    Minv <- try(solve(M), silent = TRUE)
    if (is(class(Minv), "try-error")) {
        svM <- svd(M)
        lambdainv <- rep(0, length(svM$d))
        lambdainv[abs(svM$d) > 1e-13] <- 1/svM$d[abs(svM$d) > 1e-13]
        Minv <- svM$v %*% diag(lambdainv) %*% t(svM$u)
        rownames(Minv) <- rownames(M)
        colnames(Minv) <- colnames(M)
    }
    return(Minv)
}

#' Get layout for a plot.
#' @param N An \code{integer} for the number of elements to layout.
#' @return A \code{matrix} of layout.

getPlotLayout <- function(N) {
  if (is.list(N)) {
    N <- length(N)
  }
  m2 <- ceiling(sqrt(N))
  m1 <- 0
  while (m1 * m2 < N) {
    m1 <- m1 + 1
  }
  layoutmat <- matrix(1:(m1 * m2), nrow=m1, byrow=TRUE)
  return(layoutmat)
}

#' Get adjacency matrix from edge matrix
#'
#' @param E1 a 2 or 3 column matrix of edges. Columns corresponds to edge source, sign (optional), and edges sink.
#' @param symmetric logical, shoudl the adjacency matrix be symmetrized?
#' @return An adjacency matrix
#' @export
#'
getAdj <- function(E1, symmetric = TRUE) {
  if (ncol(E1) == 2) {
    E1 <- cbind(E1[, 1], rep(NA, nrow(E1)), E1[, 2])
  }
  nds <- sort(unique(as.vector(E1[, c(1, 3)])))
  A <- tapply(rep(1, nrow(E1)), list(factor(E1[, 1], levels = nds), factor(E1[,
    3], levels = nds)), sum)
  A[is.na(A)] <- 0
  A[abs(A) > 1] <- A[abs(A) > 1]
  if (symmetric == TRUE & !all(A == t(A))) {
    A <- getSym(A)
  }
  return(A)
}

#' Symmetrize a matrix (typically adjacency matrix)
#'
#' @param a a square adjacency matrix
#' @return An adjacency symetric matrix
#' @export
#'
getSym <- function(a) {
  d0 <- diag(a)
  a <- a + t(a)
  a[abs(a) > 1] <- 1
  diag(a) <- d0
  return(a)
}



