#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

NPA$unlock()


#' Generate a NPA plot showing either a NPA summary heatmap or the network
#' graph with NPA scores depending of the type of type of plot
#' @param x NPA R6 class instance
#' @param y NULL value, Y axis is not used.
#' @param ... Additional parameters that can be passed to plotting function.
#' @include drawNPAjs.R
#' @include NPAheatmap.R
#' @include drawNPAmodule.R
#' @return An invisible object, output of `heatmapNPA` function if `heatmap` type is used,
#' output of `drawNPAmodule` if `graph` is used, output of `drawNPAjs` for `graphjs` type.
#' @export
setMethod("plot",
          signature(x = "NPA"),
          function(x, y=NULL, ...) plot.NPA(x, ...))

#==============================================================================
# Method: plot for NPA object
#==============================================================================
#' Generate a NPA plot showing either a NPA summary heatmap or the network
#' graph with NPA scores depending of the type of type of plot
#' @param x NPA R6 class instance
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
plot.NPA <- function(x, ...) {
    x$plot(...)
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
#' Generate a NPA plot showing either a NPA summary heatmap or the network
#' graph with NPA scores depending of the type of type of plot
#' @param x NPA R6 class instance
#' @param y NULL value, Y axis is not used.
#' @param ... Additional parameters that can be passed to startplot functions (see
#' startplotBIFcontrasts and starplotBIFnetworks for details).
#' @include drawNPAjs.R
#' @include NPAheatmap.R
#' @include drawNPAmodule.R
#' @return An invisible object, output of `heatmapNPA` function if `heatmap` type is used,
#' output of `drawNPAmodule` if `graph` is used, output of `drawNPAjs` for `graphjs` type.
#' @export
setMethod("plot",
          signature(x = "BIF"),
          function(x, ...) {
            plot.BIF(x, ...)
          })

#===============================================================================
# Method: plot for BIF object
#===============================================================================
#' Generate a BIF plot showing starplots per comparisons or per network families
#' @param x BIF R6 class instance
#' @param ... Additional parameters that can be passed to startplot functions (see
#' startplotBIFcontrasts and starplotBIFnetworks for details).
#' @docType methods
#' @include starplotBIFcontrasts.R
#' @include starplotBIFnetworks.R
#' @rdname plot.BIF
#' @method plot BIF
#' @return Either a list object from `startplotBIFcontrasts`` function or NULL value
#' if `starplotBIFnetworks` is used. This depends on `type` argument.
#' @export
plot.BIF <- function(x, ...) {
  x$plot(...)
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
