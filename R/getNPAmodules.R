#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Extract sub-graphs with dense leading nodes across all the comparison
#'
#' @param np NPA data (list object)
#' @export
#' @importFrom igraph upgrade_graph
#' @importFrom igraph induced_subgraph
#' @importFrom igraph cluster_infomap
#' @importFrom Rgraphviz getNodeXY
#' @importFrom Rgraphviz agopen
#' @importFrom dnet dNetFind
#' @importFrom methods as
#' @include getNPALE.R
#' @include colorscale.R


getNPAmodules <- function(np){
    LN <- getNPALE(np)
    g <- igraph::upgrade_graph(np$model$g)
    #E(g)$weight <- abs(E(g)$weight)

    gg <- as(getAdj(get.data.frame(g)[,1:2], symmetric = TRUE), "graphNEL")
    set.seed(467563)
    glay <- do.call(cbind,Rgraphviz::getNodeXY(Rgraphviz::agopen(gg,"") ))
    rownames(glay) <- nodes(gg)

    npmodules <- lapply(1:length(np$coefficients), function(j){
        if(all(np$pvperm[,j] < 0.05) & np$ci.down[j] >0){
            lnval <- sort(LN[[j]]$x, decreasing = TRUE)
            s <- lnval
            s[! names(s) %in% LN[[j]]$leadingNodes[,"GeneId"]] <- -1

            mod <- dnet::dNetFind(g, s)
            com <- igraph::cluster_infomap(mod, e.weight=abs(E(mod)$weight), modularity=FALSE)
            modules <- vector("list", length(com))
            names(modules) <- paste(names(np$coefficients)[j], "|| Module",1:length(com))
            for(h in 1:length(com)){
                g0  <- igraph::induced_subgraph(upgrade_graph(np$model$g), com[[h]])
                stopifnot(all(V(g0)$name %in% names(s)))
                tmp <- rep("FALSE", length(V(g0)$name))
                names(tmp) <- V(g0)$name
                tmp[names(tmp) %in% names(s)[s > 0]] <- "TRUE"
                tmp <- factor(tmp, levels = c("FALSE","TRUE"))
                cf <- np$nodes.coefficients[match(V(g0)$name, rownames(np$nodes.coefficients)),j]
                col <- colorscale(cf, signed = TRUE, maxx = max(abs(np$nodes.coefficients)),
                                              minx = -max(abs(np$nodes.coefficients)))
                names(col) <- names(cf)

                modules[[h]] <- list(g = g0, layout =  glay[match(V(g0)$name, rownames(glay)),],
                                    col = col, vshape = c("circle", "square")[unclass(tmp)], LN = tmp)
            }

            g0 <- igraph::induced_subgraph(upgrade_graph(np$model$g), V(mod)$name)
            stopifnot(all(V(g0)$name %in% names(s)))

            tmp <- rep("FALSE", length(V(g0)$name))
            names(tmp) <- V(g0)$name
            tmp[names(tmp) %in% names(s)[s > 0]] <- "TRUE"
            tmp <- factor(tmp, levels = c("FALSE","TRUE"))
            cf <- np$nodes.coefficients[match(V(g0)$name, rownames(np$nodes.coefficients)),j]
            col <- colorscale(cf, signed = TRUE, maxx = max(abs(np$nodes.coefficients)),
                                          minx = -max(abs(np$nodes.coefficients)))
            names(col) <- names(cf)

            modules$Global <- list(g = g0, layout =  glay[match(V(g0)$name, rownames(glay)),],
                                col = col, vshape = c("circle", "square")[unclass(tmp)], LN = tmp)

        }else{
            modules <- NA
        }
        return(modules)
    })
    names(npmodules) <- names(np$coefficients)
    attr(npmodules,"npa") <- np
    return(npmodules)
}

#' Plots NPA sub-graphs using graph or heatmap representation.
#'
#' @param npmodules List object from getNPAmodules function
#' @param type character vector, "graph" or "heatmap"
#' @param titleSuffix character vector, suffix for title
#' @importFrom reshape2 melt
#' @importFrom gplots textplot
#' @export
#' @include getsplit.R
#' @include imageplot_gg.R
#' @include visNet2.R
#' 
plotNPAmodules <-function(npmodules, type=c("graph", "heatmap"), titleSuffix = ""){
    np <- attr(npmodules,"npa")
    npmodules <- npmodules[sapply(npmodules, function(x) is.list(x))]
    if(type == "heatmap"){
        l0 <- lapply(npmodules, function(m) reshape2::melt(lapply(m, function(x) data.frame(
          ID= V(x$g)$name, isLN = x$LN)), id.vars = c("ID","isLN") ))
        l <- do.call(rbind,l0)
        colnames(l) <- c("ID", "isLN","ModuleName")
        l <- data.frame(l)
        l <- droplevels(l[l$isLN == "TRUE",])

        co <- as.matrix(table(l$ID, l$ModuleName))
        gr <- factor(getsplit(colnames(co), " ||",1, fixed = TRUE))

        for(k in 1:ncol(co)){
            z= co[,k]
            nodnp = np$nodes.coefficients[, as.character(gr[k])]
            z[z==1] = sign(nodnp)[match(names(z)[z==1],names(nodnp))]
            co[,k] =z
        }
        ImagePlotGG(co, group.col = gr, cluster = TRUE, cluster.col = FALSE, cex.labx =2,
                                 cex.laby = 1.3, title = paste(
                                   "Leading Nodes Modules", titleSuffix), key.title = "Node Sign")
        res <- list(data = co, group = gr)
    }
    if(type == "graph"){
        res <- NULL
        for(j in 1:length(npmodules)){
            ml <- npmodules[[j]]
            layout(getPlotLayout(length(ml)))
            for(k in 1:length(ml)){
                if(is.list(ml[[k]])){
                    visNet2(ml[[k]]$g, vertex.label.dist = 0.3, vertex.label.cex =1,
                            vertex.color = ml[[k]]$col, vertex.size = 12,
                            vertex.shape = ml[[k]]$vshape,glayout = ml[[k]]$layout)
                }else{
                    gplots::textplot("NPA not *O*K*", col = "grey", cex =  1)
                }
                title(main = paste(names(ml)[k], titleSuffix))
            }
        }
    }
    return(invisible(res))
}


# require(devtools);load_all()
#   load("np.rda") ; m = getNPAmodulesGlobal(np);
#    plotNPAmodulesGlobal(m, vertex.size =40, type ="single")

