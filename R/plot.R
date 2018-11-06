#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

NPA$unlock()

#==============================================================================
# Method: plot for NPA object
#==============================================================================
#' Generate a NPA plot showing either a NPA summary heatmap or the network
#' graph with NPA scores depending of the type of type of plot
#' @exportMethod plot
#' @param x NPA R6 class instance
#' @param type An character vector. Type of plot. If \code{type} is "heatmap", default version
#' showing a summary heatmap with leading nodes scores for the different
#' comparisons. If \code{type} is "graph", a graph is ploted showing the network and node scores barplots in nodes.
#' If \code{type} is "drawjs", a dynamic HTML/JavaScript page with network
#' graph showing nodes scores.
#' @param ... Additional parameters that can be passed to plotting function.
#' @docType methods
#' @include drawNPAjs.R
#' @include NPAheatmap.R
#' @include drawNPAmodule.R
setMethod("plot",
          signature(x = "NPA", y = "character"),
          function(x, y=c('heatmap', 'graph', 'graphjs'), ...) plot.NPA(x, y, ...))

plot.NPA <- function(x, y, ...) {
    x$plot(y, ...)
}

NPA$set("public", "plot", function(type=c('heatmap', 'graph', 'graphjs'), ...) {
    type <- match.arg(type)
    if (type=='heatmap') {
        return(invisible(heatmapNPA(private$data)))
    } else if (type == 'graph') {
      return(invisible(drawNPAmodule(private$data)))
    } else {
      return(invisible(drawNPAjs(private$data)))
    }
})

NPA$lock()


BIF$unlock()
#===============================================================================
# Method: plot for BIF object
#===============================================================================
#' Generate a BIF plot showing starplots per comparisons or per network families
#' @exportMethod plot
#' @param x BIF R6 class instance
#' @param type An integer value. Type of plot. `type` is 1, default version
#' showing network contributions for each comparison. If `type` is 2, startplots
#' figures of contributions of comparisons per network families is shown.
#' @param ... Additional parameters that can be passed to startplot functions.
#' @docType methods
#' @include starplotBIFcontrasts.R
#' @include starplotBIFnetworks.R
setMethod("plot",
          signature(x = "BIF", y = "integer"),
          function(x, y = 1 , ...) plot.BIF(x, y = 1, ...))

plot.BIF <- function(x, y="networks", ...) {
  x$plot(y, ...)
}

BIF$set("public", "plot", function(type="networks", ...) {
  if (type=="networks") {
    return(invisible(starplotBIFnetworks(private$data)))
  } else {
    return(invisible(starplotBIFcontrasts(private$data)))
  }
})

BIF$lock()


#===============================================================================
# Method: plot for NPA modules
#===============================================================================

plot.NPAModules <- function(
  x,
  ...
  ) {
   return(invisible(plotNPAmodulesGlobal(
     npmodules = x
     #main = main,
     #useLayout = useLayout,
     #title = title,
     #type = type , which.module = which.module, glayout = glayout, ...
     )))
}
