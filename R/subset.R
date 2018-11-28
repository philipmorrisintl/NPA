#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################


setGeneric("subset")

#' Subsetting NPA object with indices to retrive a NPA object with results for
#' the given contrasts indices
#' @param x A R6 class object, NPA or NPAList object
#' @param indices A integer vector of indexes of comparisons (for NPA)
#' or networks models (for a NPAList)
#' @param other.indicies A integer vector. For a NPAList object,
#' indexes for comparisons.
#' @return A R6 class object, either a NPA or NPAList object subset.
#' @exportMethod subset

setMethod("subset", c(x = "R6"), function(x, indices, other.indicies=NULL) {
  x$subset(indices, other.indicies)
})

#getGeneric('[')
#`[.NPA` <- function(x, i=NULL) {
#  return(x$subset(i))
#}

#getGeneric('[<-')
#`[.NPA<-` <- function(x, i) {
#  stop("Not implemented assignment")
#}

NPA$unlock()
NPA$set("public", "subset", function(ind, ind2=NULL) {
  "Return subset of contrasts for the NPA R6 classe object"
  if (is.null(ind)) {
    return(invisible(NPA$new(private$data, private$network)))
  }
  if (max(ind) > length(private$data$coefficients)) {
    stop(paste0(
      "Given in indices are exceeding number of comparisons"))
  }
  return(invisible(NPA$new(NPAsubset(private$data, ind), private$network)))
})
NPA$lock()


#getGeneric('[')
#`[.NPAList` <- function(x, i=NULL, j=NULL) {
#  return(x$subset(i, j))
#}

#getGeneric('[<-')
#`[.NPAList<-` <- function(x, i, j) {
#  stop("Not implemented assignment")
#}

NPAList$unlock()
NPAList$set("public", "subset", function(indi, indj) {
  "Return subset of contrasts for the NPA R6 classe object"
  if (!is.null(indi)) {
    if (max(indi) > length(private$data)) {
      stop("Given models indices are exceeding number of models")
    }
    data <- private$data[indi]
  } else {
    data <- private$data
  }
  if (!is.null(indj)) {
    if (max(indj) > length(private$data[[1]]$coefficients)) {
      stop(paste0(
        "Given comparison indices are exceeding number of comparisons"))
    }
    data <- lapply(data, function(np) NPAsubset(np, indj))
  }
  return(invisible(NPAList$new(data, private$species)))
})
NPAList$lock()
