#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Plots global NPA modules and optionally shows communities.
#'
#' @param npmodules output of getNPAmodulesGlobal.
#' @param main A character vector, title of the figure.
#' @param useLayout A logical. If \code{TRUE}, a layout is generated for displaying modules.
#' @param title  A logical. If \code{TRUE}, title is displayed.
#' @param type A character vector. "single" for showing modules in a global graph or "multiple" to show communities.
#' @param which.module A integer vector, indexes of selected modules. By default, `NULL` means all modules are used. 
#' @param ...  Other arguments to be passed to drawNPAmodule function.
#' @return An invisible list object with either communities and global modules output of
#' `drawNPAmodule` function if type is set to `multiple` or a single invisible list object with the output
#' of `drawNPAmodule` function.
#' @include utils.R
#' @include drawNPAmodule.R
#' @importFrom igraph V
#' @importFrom gplots textplot

plotNPAmodulesGlobal <-function(npmodules, main ="", useLayout = FALSE, title = FALSE,
                                type = c("single","multiple"), which.module = NULL,...){
    type <- match.arg(type)
    p <- npmodules$Global$p
    np <- npmodules$Global$np
    res <- NULL
    if(useLayout ==TRUE){
        layout(getPlotLayout(length(npmodules)-1))
    }
    if(type == "multiple" ){
        if(is.null(which.module)){
            which.module <- 1:length(npmodules)
        }
        res <- vector("list",length(which.module) )
        names(res) <- names(npmodules)[which.module]
        for(k in 1:length(which.module)){
            if(names(npmodules)[which.module[k]]!= "Global"){
                if(is.list(npmodules[[which.module[k]]])){
                    np0 <- np
                    np0$model$g <- npmodules[[which.module[k]]]$g
                    res[[k]] <- drawNPAmodule(np0, p = p,...)
                }else{
                    gplots::textplot("NPA not *O*K*", col = "grey", cex =  1)
                }
                if(title == TRUE){
                    title(main = paste(main, names(npmodules)[which.module[k]]))
                }

            }
        }
    }
    if(type == "single" ){
        np0 <- np
        np0$model$g <- npmodules$Global$g
        res <- drawNPAmodule(np0, p = p, ...)
        title(main = main)
    }
    return(invisible(res))
}
