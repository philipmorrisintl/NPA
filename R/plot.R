#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

NPA$unlock()

setMethod("plot",
          signature(x = "NPA", y = "character"),
          function(x, y=c('heatmap', 'graph', 'graphjs'), ...) plot.NPA(x, y, ...))

#==============================================================================
# Method: plot for NPA object
#==============================================================================
#' Generate a NPA plot showing either a NPA summary heatmap or the network
#' graph with NPA scores depending of the type of type of plot
#' @param x NPA R6 class instance
#' @param type An character vector. Type of plot. If \code{type} is "heatmap", default version
#' showing a summary heatmap with leading nodes scores for the different
#' comparisons. If \code{type} is "graph", a graph is ploted showing the network and node scores barplots in nodes.
#' If \code{type} is "graphjs", a dynamic HTML/JavaScript page with network
#' graph showing nodes scores.
#' @param ... Additional parameters that can be passed to plotting function.
#' @docType methods
#' @include drawNPAjs.R
#' @include NPAheatmap.R
#' @include drawNPAmodule.R
#' @rdname plot.NPA
#' @method plot NPA
#' @return An invisible object, output of `heatmapNPA` function if `heatmap` type is used,
#' output of `drawNPAmodule` if `graph` is used, output of `drawNPAjs` for `graphjs` type.
#' @export
plot.NPA <- function(x, y="heatmap", ...) {
    x$plot(y, ...)
}

NPA$set("public", "plot", function(type=c('heatmap', 'graph', 'graphjs'), ...) {
    type <- match.arg(type)
    if (type=='heatmap') {
        return(invisible(heatmapNPA(private$data, ...)))
    } else if (type == 'graph') {
      return(invisible(drawNPAmodule(private$data, ...)))
    } else {
      return(invisible(drawNPAjs(private$data, ...)))
    }
})

NPA$lock()


BIF$unlock()
setMethod("plot",
          signature(x = "BIF", y = "character"),
          function(x, y = "networks" , ...) {
            plot.BIF(x, y = "networks", ...)
          })

#===============================================================================
# Method: plot for BIF object
#===============================================================================
#' Generate a BIF plot showing starplots per comparisons or per network families
#' @param x BIF R6 class instance
#' @param type An character vector. Type of plot. `type` is "network", default version
#' showing network contributions for each comparison. If `type` is "comparisons", startplots
#' figures of contributions of comparisons per network families is shown.
#' @param ... Additional parameters that can be passed to startplot functions (see
#' startplotBIFcontrasts and starplotBIFnetworks for details).
#' @docType methods
#' @include starplotBIFcontrasts.R
#' @include starplotBIFnetworks.R
#' @rdname plot.BIF
#' @method plot BIF
#' @return Either a list object form `startplotBIFcontrasts`` function or NULL value
#' if `starplotBIFnetworks`` is used.
#' @export
plot.BIF <- function(x, y="networks", ...) {
  x$plot(y, ...)
}

BIF$set("public", "plot", function(type="networks", ...) {
  if (type=="networks") {
    return(invisible(starplotBIFnetworks(private$data, ...)))
  } else {
    return(invisible(starplotBIFcontrasts(private$data, ...)))
  }
})

BIF$lock()


#===============================================================================
# Method: plot for NPA modules
#===============================================================================
#' Plot NPA modules as a network graph
#' @param x NPAModules S3 class object
#' @param type An character vector. Type of plot. `type` is "network", default version
#' showing network contributions for each comparison. If `type` is "comparisons", startplots
#' figures of contributions of comparisons per network families is shown.
#' @param ... Additional parameters that can be passed to plotNPAmodulesGlobal functions
#' @docType methods
#' @include plotNPAmodulesGlobal.R
#' @include NPA-class.R
#' @rdname plot.NPAModules
#' @method plot NPAModules
#' @return An invisible object, output of `plotNPAmodulesGlobal`` function.
#' @export
plot.NPAModules <- function(
  x,
  ...
  ) {
    x$x <- NULL
    x$y <- NULL
   return(invisible(plotNPAmodulesGlobal(
     npmodules = x,
     ...
     #main = main,
     #useLayout = useLayout,
     #title = title,
     #mode = mode , which.module = which.module, glayout = glayout, ...
     )))
}
