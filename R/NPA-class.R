
#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Class providing object with methods for storing Network Perturbation amplitude computation results
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @return Object of \code{\link{R6Class}} named NPA containing NPA result data
#' @format \code{\link{R6Class}} object.
#' @field data A R list object containing NPA scoring results
#' @field network A NPAModel R6 class object

NPA <- R6Class("NPA",
    private = list(
        data = NULL,
        network = NA
    ),
    public = list(
        initialize = function(data, network) {
            private$data <- data
            private$network <- network
        },
        get_data = function() private$data
    ),
    lock_class = TRUE
)
setOldClass(c("NPA", "R6"))

# Enabling modification of class definition
NPA$unlock()

#==============================================================================
# Method: comparisons
#==============================================================================
setGeneric("comparisons",
           def = function(x) standardGeneric("comparisons")
           )

#' Retrieve comparisons names used for NPA computation
#' @param x NPA R6 class instance
#' @docType methods
#' @method comparisons NPA
#' @export
#' @return String vector. Comparisons names
setMethod("comparisons",
          signature(x = "R6"), function(x) {
    x$comparisons()
})

NPA$set("public", "comparisons", function() {
    "Returns the comparisons names of the NPA object"
    return(names(private$data$coefficients))
})

#==============================================================================
# Method: coefficients
#==============================================================================
#' Retrieve NPA coefficients
#' @exportMethod coefficients
#' @param x NPA R6 class instance
#' @param type Character vector, if set to `npa`, a numeric vector
#' of the NPA values for each comparison is returned, if set to `nodes`, a matrix
#' with NPA scores per network nodes (rows) for each comparison (columns).
#' Default value is `npa`
#' @docType methods
#' @return Numeric vector or matrix depending on `type` argument value.

setMethod("coefficients",
          signature(object = "R6"),
          function(object, type = c("npa", "nodes")) {
            type <- match.arg(type)
            object$coefficients(type)
          }
)

NPA$set("public", "coefficients", function(type = c("npa", "nodes")) {
    "Returns the coefficients values of the NPA object"
    type <- match.arg(type)
    if (type == 'npa') {
      return(private$data$coefficients)
    } else {
      return(private$data$nodes.coefficients)
    }
})


#==============================================================================
# Method: conf.int
#==============================================================================
#' Retrieve NPA confidence intervals
#' @exportMethod conf.int
#' @param x NPA R6 class instance
#' @param type Character vector. Returns a numeric matrix of confidence intervals
#' values, with columns: `down` and `up` confidence values. If type is set to `npa`, the
#' confidences values are given for each comparisons. If type is set to `nodes`,
#' the confidences values are related to the network backbone nodes. Default value
#' is `npa`.
#' @docType methods
#' @return Numeric vector or matrix depending on `type` argument value.
setGeneric("conf.int",
           def = function(object, ...) standardGeneric("conf.int")
)

setMethod("conf.int",
          signature(object = "R6"),
          function(object, type = c("npa", "nodes")) {
            type <- match.arg(type)
            object$conf.int(type)
            }
          )

NPA$set("public", "conf.int", function(type=c("npa", "nodes")) {
    "Returns the confidence intervals values of the NPA object"
    type <- match.arg(type)
    if (type == 'npa') {
      m <- matrix(c(private$data$ci.down, private$data$ci.up), length(private$data$ci.down), 2)
      rownames(m) <- names(private$data$ci.down)
      colnames(m) <- c("down", "up")
    } else {
      m <- cbind(
        private$data$nodes.coefficients.ci.down,
        private$data$nodes.coefficients.ci.up)
      colnames(m)[1:(ncol(m)/2)] <- paste0(colnames(m)[1:(ncol(m)/2)], " (down)")
      colnames(m)[-(1:(ncol(m)/2))] <- paste0(colnames(m)[-(1:(ncol(m)/2))], " (up)")
    }
    return(m)
})

#==============================================================================
# Method: summary
#==============================================================================
#' Returns a summary data.frame for a NPA object 
#' @exportMethod summary
#' @param x NPA R6 class instance
#' @docType methods
#' @return Numeric vector or matrix depending on `type` argument value.
setMethod("summary", 
          signature(object = "R6"),
          function(object) {
  summary.NPA(object)
})

summary.NPA <- function(x) {
  x$summary()
}


NPA$set("public", "summary", function() {
  "Returns a data.frame which summarizes the NPA object"
  df <- data.frame(
    coefficients=private$data$coefficients,
    ci.up=private$data$ci.up,
    ci.down=private$data$ci.down,
    pvperm.backbone=private$data$pvperm["backbone", ],
    pvperm.downstream=private$data$pvperm["downstream", ])
  t(df)
})

#==============================================================================
# Method: modules
#==============================================================================
#' Compute sub-graphs for dense leading nodes regions.
#' @exportMethod modules
#' @param x NPA R6 class instance
#' @param alpha A numerical value, scale the negative score of the non leading
#' nodes for dNetFind
#' @param p A numerical value. Threshold value, for leading nodes detection.
#' @docType methods
#' @include getNPAmodulesGlobal.R
#' @return S3 class object that stores module data.
setGeneric("modules",
           def = function(object, ...) standardGeneric("modules")
)

setMethod("modules",
          signature(object = "R6"),
          function(object, alpha = 0, p = 0.8) {
            object$modules(alpha = alpha, p = p)
          }
)

# S3 classes definitions for NPA modules types
NPAModules <- function(modulesData) {
  data <- modulesData
  structure(data, class = "NPAModules")
}

NPA$set("public", "modules", function(alpha = 0, p = p) {
  "S3 class object that stores module data."
  modules <- getNPAmodulesGlobal(private$data, alpha = alpha, p = p)
  modules$x <- 0 
  modules$y <- 0
  return(NPAModules(modules))
})


#==============================================================================
# R6 Method: Pretty print of the NPA object
#==============================================================================
NPA$set("public", "print", function(...) {
    "Pretty printing for a NPA R6 class object"
    s <- paste0("NPA Object - Nb comparisons: ", length(private$data$coefficients))
    print(s)
    invisible(self)
})

# Locking class to disable method or field modification
NPA$lock()


