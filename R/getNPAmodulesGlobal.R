#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Computes global NPA modules
#'
#' @param np Object from NPAIIfast
#' @param alpha A numerical value, scale the negative score of the non leading
#' nodes for dNetFind
#' @param p A numerical value. Threshold value, for leading nodes detection.
#' Used in getNPALE function.
#' @importFrom igraph upgrade_graph
#' @importFrom igraph induced_subgraph
#' @importFrom igraph infomap.community
#' @importFrom igraph E
#' @importFrom igraph V
#' @importFrom igraph as.undirected
#' @importFrom igraph get.data.frame
#' @importFrom Rgraphviz getNodeXY
#' @importFrom Rgraphviz agopen
#' @importFrom dnet dNetFind
#' @importFrom graph nodes
#' @importFrom methods as
#' @include getNPALE.R
#' @include colorscale.R
#' @include utils.R


getNPAmodulesGlobal <-function(np, alpha= 0, p = 0.8){
    LN <- getNPALE(np, prop = p)
    g <- igraph::upgrade_graph(np$model$g)
    igraph::E(g)$weight <- abs(igraph::E(g)$weight)
    
    gg <- as(getAdj(igraph::get.data.frame(g)[,1:2], symmetric = TRUE), "graphNEL")
    ## Bioconductor does not allow setting seed in the code
    ## Do it outside of the function call if needed.
    # set.seed(467563)
    glay <- do.call(cbind,Rgraphviz::getNodeXY(Rgraphviz::agopen(gg,"") ))
    rownames(glay) <- graph::nodes(gg)

    s <- sapply(1:length(np$coefficients), function(j){
        s <- LN[[j]]$x/max(LN[[j]]$x)
        s <- s[order(names(LN[[j]]$x))]
        if(all(np$pvperm[,j] < 0.05) & np$ci.down[j] >0){
            s[! names(s) %in% LN[[j]]$leadingNodes[,"GeneId"]] <- 0
        } else {
            s <- 0 * s
        }
        return(s)})
    colnames(s) <- names(np$coefficients)
    s <- rowSums(s)
    s[s==0] <- -max(s)*alpha
    mod <- dnet::dNetFind(g, s)
    com <- igraph::cluster_infomap(igraph::as.undirected(mod))
    modules <- vector("list", length(com))
    names(modules) <- paste("Module",1:length(com))
    for(h in 1:length(com)){
        g0  <- igraph::induced_subgraph(igraph::upgrade_graph(np$model$g), com[[h]])
        
        stopifnot(all(igraph::V(g0)$name %in% names(s)))

        tmp <- rep("FALSE", length(igraph::V(g0)$name))
        names(tmp) <- igraph::V(g0)$name
        tmp[names(tmp) %in% names(s)[s > 0]] <- "TRUE"
        tmp <- factor(tmp, levels = c("FALSE","TRUE"))
        cf <- np$nodes.coefficients[match(V(g0)$name, rownames(np$nodes.coefficients)),,drop=FALSE]
        col <- apply(cf,2, function(x) colorscale(x, signed = TRUE, maxx = max(abs(np$nodes.coefficients)),
                                                  minx = -max(abs(np$nodes.coefficients))))
        if(ncol(cf)==1){
            cf <- matrix(cf, ncol= 1)
        }
        if(nrow(cf)==1){
            cf <- matrix(cf, nrow= 1)
        }

        colnames(col) <- colnames(cf)
        rownames(col) <- rownames(cf)
        modules[[h]] <- list(g = g0, names = V(g0)$name,
                             layout =  glay[match(V(g0)$name, rownames(glay)),],
                             cf = cf,
                             col = col,
                             vshape = c("circle", "square")[unclass(tmp)],
                             LN = tmp)
    }
    g0 <- igraph::induced_subgraph(igraph::upgrade_graph(np$model$g), V(mod)$name)
    stopifnot(all(V(g0)$name %in% names(s)))
    tmp <- rep("FALSE", length(V(g0)$name))
    names(tmp) <- V(g0)$name
    tmp[names(tmp) %in% names(s)[s > 0]] <- "TRUE"
    tmp <- factor(tmp, levels = c("FALSE","TRUE"))
    cf <- np$nodes.coefficients[match(V(g0)$name, rownames(np$nodes.coefficients)),,drop=FALSE]
    col <- apply(cf,2, function(x) colorscale(
      x, signed = TRUE, maxx = max(abs(np$nodes.coefficients)),
      minx = -max(abs(np$nodes.coefficients))))
    if(ncol(cf)==1){
        cf <- matrix(cf, ncol= 1)
    }
    if(nrow(cf)==1){
        cf <- matrix(cf, nrow= 1)
    }

    colnames(col) <- colnames(cf)
    rownames(col) <- rownames(cf)
    modules$Global <- list(g = g0,
                           names = V(g0)$name,
                           layout =  glay[match(V(g0)$name, rownames(glay)),],
                           cf = cf,
                           col = col,
                           vshape = c("circle", "square")[unclass(tmp)],
                           LN = tmp,
                           alpha = alpha,
                           p = p,
                           np = np)
    return(modules)
}
