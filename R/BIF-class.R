#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Class providing object with methods for storing BIF computation results
#'
#' @docType class
#' @importFrom R6 R6Class
#' @return Object of \code{\link{R6Class}} named BIF containing BIF result data
#' @format \code{\link{R6Class}} object.
#' @field data A R list object containing BIF scoring results
#' 

BIF <- R6Class("BIF",
               private = list(
                 data = NULL
               ),
               public = list(
                 initialize = function(data) {
                   private$data <- data
                 },
                 get_data = function() private$data
               ),
               lock_class = TRUE
)
setOldClass(c("BIF", "R6"))


# Enabling modification of class definition
BIF$unlock()


#==============================================================================
# R6 Method: Pretty print of the BIF object
#==============================================================================
BIF$set("public", "print", function(...) {
  "Pretty printing for a BIF R6 class object"
  s <- paste0("BIF Object")
  print(s)
  invisible(self)
})

#==============================================================================
# Method: as.matrix
#==============================================================================
#' Generate a BIF results matrix showing different computed metrics
#' @exportMethod as.matrix
#' @param x BIF R6 class instance
#' @param family String, network family name. If not provided, the BIF metrics are given for network famililies,
#' if a familiy is specified, metrics for the networks are given.
#' @param type String, metric type in `rbif`, `r2`, `contrib` and `coefficients`
#' @docType methods
setMethod("as.matrix",
          signature(x = "BIF"),
          function(x, ...) {
            arguments <- list(...)
            if (! "type" %in% names(arguments)) {
              type <- "rbif" 
            }
            if (! "family" %in% names(arguments)) {
              family <- NULL
            }
            as.matrix.BIF(x, family = arguments$family, type = arguments$type)
          })

as.matrix.BIF <- function(x, family, type) {
  x$as.matrix(family, type)
}

BIF$set("public", "as.matrix", function(family = NULL, type = c("rbif", "r2", "contrib", "coefficients")) {
  type <- match.arg(type)
  if (is.null(family)) {
    slot <- "BIF"
  } else {
    if (!family %in% colnames(private$data[["BIF"]]$rbif)) {
      stop("Unknown family name provided")
    } else {
      slot <- family
    }
  }
  res <- switch(type,
                rbif = private$data[[slot]]$rbif,
                r2 = private$data[[slot]]$r2,
                contrib = private$data[[slot]]$contrib,
                coefficients = private$data[[slot]]$coefficients)
  return(res)
})

# Locking class to disable method or field modification
BIF$lock()