#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

NPA$unlock()
#==============================================================================
# Method: barplot
#==============================================================================
#' Generate a NPA barplot showing the NPA score per contrasts
#' @exportMethod barplot
#' @param height NPA R6 class instance.
#' @param type An integer value. Type of barplot. If \code{type} is 1, default version
#' showing a simple barplot with "O.K." statistics. \code{type} = 2,
#' wordclouds showing leading nodes are represented on the right side of the
#' barplot, this mode required a large figure. \code{type} = 3, a simplified
#' version of the barplot is generated using ggplot R package.
#' @param ... Additional parameters that can be passed to barplotNPA3 function.
#' @return A ggplot object if `type` is 3, otherwise, NULL is returned.
#' @docType methods
#' @include barplotNPA3.R
#' @include barplotNPAGG.R
setMethod("barplot",
          signature(height = "NPA"),
          function(height, type=seq(1,4)[1], ...) barplot.NPA(height, type, ...))

#' Generate a NPA barplot showing the NPA score per contrasts
#' @param height NPA R6 class instance.
#' @param type An integer value. Type of barplot. If \code{type} is 1, default version
#' showing a simple barplot with "O.K." statistics. \code{type} = 2,
#' wordclouds showing leading nodes are represented on the right side of the
#' barplot, this mode required a large figure. \code{type} = 3, a simplified
#' version of the barplot is generated using ggplot R package.
#' @param ... Additional parameters that can be passed to barplotNPA3 function.
#' @return A ggplot object if `type` is 3, otherwise, NULL is returned.
barplot.NPA <- function(height, type, ...) {
    height$barplot(type, ...)
}

NPA$set("public", "barplot", function(type, ...) {
    title <- paste0('NPA - ', private$network$get_name(), ' (', private$species, ')' )
    if (type < 3) {
      barplotNPA3(private$data, type=type, main=title, ...)
    } else {
      barplotNPAGG(private$data, ...)
    }
})
NPA$lock()


BIF$unlock()
#==============================================================================
# Method: barplot
#==============================================================================
#' Generate a BIF barplot
#' @exportMethod barplot
#' @param height BIF R6 class instance
#' @param type An integer value. Type of barplot. If `type` is set to 1 (default type),
#' pies are shown for each bar of the barplot. If `type` is set to 2, starplots are represented
#' below the bars.
#' @param ... Additional parameters that can be passed to barplotBIF function
#' @return An invisible list. Output of `barplotBIF`` function.
#' @docType methods
#' @include barplotBIF.R
setMethod("barplot",
          signature(height = "BIF"),
          function(height, type = seq(1, 2)[1], ...) barplot.BIF(height, type, ...))

#' Generate a BIF barplot
#' @param height BIF R6 class instance
#' @param type An integer value. Type of barplot. If `type` is set to 1 (default type),
#' pies are shown for each bar of the barplot. If `type` is set to 2, starplots are represented
#' below the bars.
#' @param ... Additional parameters that can be passed to barplotBIF function
#' @return An invisible list. Output of `barplotBIF`` function.
barplot.BIF <- function(height, type, ...) {
  #height$barplot_bif(type, ...)
  if (type == 1) {
    barplotBIF(height$get_data(), ...)  
  } else {
    barplotBIF(height$get_data(), pie = FALSE, starplot = TRUE, ...)
  }
}

BIF$set("public", "barplot_bif", function(type, ...) {
  if (type == 1) {
    barplotBIF(private$data, ...)  
  } else {
    barplotBIF(private$data, pie = FALSE, starplot = TRUE, ...)
  }
})
BIF$lock()

