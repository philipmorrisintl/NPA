#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Class providing object with methods for storing list of NPA results per
#' network
#'
#' @docType class
#' @importFrom R6 R6Class
#' @field data A R list object containing NPA scoring results
#' @include getBIF.R
#' @return Object of \code{\link{R6Class}} named NPAList containing NPA objects
#' @format \code{\link{R6Class}} object.

NPAList <- R6Class("NPAList",
    private = list(
        data = NULL,
        species = NA
    ),
    public = list(
        initialize = function(data, species=NA) {
            private$data <- data
            private$species <- species
        },
        networks = function() names(private$data),
        get_data = function() private$data,
        get_bif = function() BIF$new(getBIF(private$data))
    ),
    lock_class = TRUE
)
setOldClass(c("NPAList", "R6"))


# Enabling modification of class definition
NPAList$unlock()

#==============================================================================
# R6 Method: Pretty print of the NPAList object
#==============================================================================
NPAList$set("public", "print", function(...) {
    "Pretty printing for a NPAList R6 class object"
    s <- "NPAList Object:"
    if (!is.na(private$species)) {
        s <- paste(s, "- Species:", private$species)
    }
    s <- paste(s, "- Nb comparisons:", length(private$data[[1]]$coefficients))
    s <- paste(s, "- Nb networks:", length(private$data))
    print(s)
    invisible(self)
})

#==============================================================================
# Method: heatmap plot of the NPAList
#==============================================================================
#' Plot the heatmap of the NPAList object
#' @exportMethod plot
#' @param x NPA R6 class instance
#' @param type An integer value. Type of plot. If \code{type} is 1, default version
#' showing a summary heatmap showing leading nodes scores for the different
#' contrasts. If \code{type} is 2, a dynamic HTML/JavaScript page with network
#' graph showing nodes scores.
#' @param ... Additional parameters that can be passed to plotting function
#' @docType methods
#' @return An invisible R list object with extraction slots (CF, PV, cfall...)
#' @include extract_npas.R
setMethod("plot",
          signature(x = "NPAList", y = "integer"),
          function(x, y=NULL, ...) plot.NPAList(x, y=NULL, ...))

#' Plot the heatmap of the NPAList object
#' @param x NPA R6 class instance
#' @param type An integer value. Type of plot. If \code{type} is 1, default version
#' showing a summary heatmap showing leading nodes scores for the different
#' contrasts. If \code{type} is 2, a dynamic HTML/JavaScript page with network
#' graph showing nodes scores.
#' @param ... Additional parameters that can be passed to plotting function
#' @return An invisible R list object with extraction slots (CF, PV, cfall...)
plot.NPAList <- function(x, y=NULL, ...) {
  x$plot(y, ...)
}

NPAList$set("public", "plot", function(which=NULL, ...) {
  if (is.null(which)) {
    which <- 1:length(self$get_data()[[1]]$coefficients)
  }
  return(invisible(extractNPA2(self$get_data(), which = which, plot = TRUE, ...)))
})


NPAList$lock()

#' Compute list of NPA objects using several network models
#'
#' @param comparisons A list of data frames containing genes `nodeLabel`, `foldChange` and `t` statistic.
#' @param models A list of R6 class NPAModel objects
#' @param verbose A logical, if TRUE, messages are shown in the console
#'
#' @return A R6 class NPAList object
#' @export
#' @examples 
#' library(NPAModels)
#' data(COPD1)
#' #models <- load_models(species = 'Mm')
#' #npalist <- compute_npa_list(COPD1, models)
compute_npa_list <- function(comparisons, models,
                         verbose=FALSE) {
  npas <- list()
  for (model in models) {
    if (verbose) {
      message(paste0("Computing NPA for: ",
                     model$get_family(), ' - ', model$get_name()))
    }
    name <- paste0(model$get_family(), ' / ', model$get_name())
    npas[[name]] <- compute_npa(comparisons, model)$get_data()
  }
  return(NPAList$new(npas, model$get_species()))
}

#==============================================================================
# Method: bif
#==============================================================================
#' Compute the BIF object on a NPAList
#' @exportMethod get_bif
#' @param x NPAList R6 class instance
#' @docType methods
#' @return A BIF R6 class object.
#' @examples 
#' library(NPAModels)
#' data(COPD1)
#' #models <- load_models(species = 'Mm')
#' #npa_list <- compute_npa_list(COPD1, models)
#' #bif <- get_bif(npa_list)

setGeneric("get_bif",
           def = function(x) standardGeneric("get_bif")
)

#' Compute the BIF object on a NPAList
#' @param x NPAList R6 class instance
#' @return A BIF R6 class object.
setMethod("get_bif", c(x = "R6"), function(x) {
  x$get_bif()
})

