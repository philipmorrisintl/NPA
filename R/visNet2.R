#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Draw a graph using edges (E) data.frame
#'
#' @param E A data.frame with 3 column: source node, sign, target node
#' @param newpage A logical for plotting on a new device 
#' @param glayout A function for generating the layout
#' @param vertex.frame.color A character vector, border color for nodes
#' @param vertex.size An optional integer for node size 
#' @param vertex.color An optional character vector, color for nodes. Default
#' is 'SkyBlue2'
#' @param vertex.shape An optional character vector, shape of nodes. Default is
#' 'circle'
#' @param vertex.label A character vector. Label for nodes
#' @param vertex.label.cex A numerical value for font size scaling. 
#' @param vertex.label.dist A numerical value for distance between label and
#' node
#' @param vertex.label.color A character vector, color for node label
#' @param edge.color A character vector, color for edges.
#' @param ... Additional optional parameters to be passed to plot.igraph
#' @export
#' @importFrom igraph igraph.from.graphNEL
#' @importFrom igraph list.vertex.attributes
#' @importFrom igraph plot.igraph
#' @importFrom igraph vcount
#' @include colorscale.R
#' 
visNet2 <- function(E, newpage=FALSE, glayout=layout.fruchterman.reingold,
    vertex.frame.color="black", vertex.size=NULL, vertex.color=NULL,
    vertex.shape=NULL, vertex.label=NULL, vertex.label.cex=NULL,
    vertex.label.dist=NULL, vertex.label.color="black", edge.color=NULL, ...) {

    set.seed(435)
    getSignedAdj <- function(E1) {
        nds <- sort(unique(as.vector(E1[, c(1, 3)])))
        A <- tapply(
            as.numeric(E1[, 2]),
            list(factor(E1[, 1], levels=nds),
                 factor(E1[, 3], levels=nds)),
            sum
        )
        A[is.na(A)] <- 0
        A[abs(A) > 1] <- sign(A[abs(A) > 1])
        return(A)
    }

    if(class(E) == "matrix") {
        if(ncol(E) == 2) {
            E <- cbind(E[, 1], rep("1", nrow(E)), E[, 2])
        }
        g <- as(abs(getSignedAdj(E)), "graphNEL")
    } else {
        g <- E
    }

    if (class(g) == "graphNEL") {
        ig <- igraph::igraph.from.graphNEL(g)
    } else {
        ig <- g
    }
    if (class(ig) != "igraph") {
        stop(
            "The function must apply to either 'igraph' or 'graphNEL' object.\n"
        )
    }
    if (is.null(V(ig)$name)) {
        V(ig)$name <- as.character(V(ig))
    }
    nsize <- igraph::vcount(ig)
    if (is.null(vertex.label)) {
        if ("geneSymbol" %in% list.vertex.attributes(ig)) {
            vertex.label <- V(ig)$geneSymbol
        } else {
            vertex.label <- V(ig)$name
        }
    }
    if(is.null(edge.color)){
        ecol <- rep("grey", length(unclass((E(ig)))))
        if(class(E) == "matrix"){
            d <- get.data.frame(ig)
            sgn <- rep(1, nrow(d))
            sgn[match(
                paste(E[, 1], E[, 3]),
                paste(d[, 1],d[, 2])
            )] <- as.numeric(E[, 2])
            ecol <- c("black", "grey")[unclass(factor(sgn, levels=c(-1, 1)))]
        }
        if(class(E) == "igraph") {
           if(!is.null(E(E)$weight)) {
               ecol <- c("black","grey")[unclass(
                            factor(
                                sign(E(E)$weight),
                                levels=c(-1,1)
                            )
                        )]
           }
        }
    } else {
        ecol <- edge.color
    }
    shapes <- rep("circle", igraph::vcount(ig))
    if (is.null(vertex.shape)) {
        if ("score" %in% igraph::list.vertex.attributes(ig)) {
            shapes[V(ig)$score < 0] <- "csquare"
        }
        vertex.shape <- shapes
    }
    if (is.null(vertex.color)) {
      vertex.color <- "SkyBlue2"
    }
    max.labels <- max(nchar(vertex.label))
    vertex.size2 <- 15
    vertex.label.dist2 <- 0
    vertex.label.cex2 <- 0.6
    if (nsize < 50) {
        if (max.labels > 2) {
            vertex.size2 <- 8
            vertex.label.dist2 <- 0.5
        }
    }
    if (nsize < 100 && nsize >= 50) {
        if (max.labels > 2) {
            vertex.size2 <- 8
            vertex.label.dist2 <- 0.5
        }
        vertex.label.cex2 <- 0.5
    }
    if (nsize >= 100) {
        if (max.labels > 3) {
            vertex.size2 <- 8
            vertex.label.dist2 <- 0.5
        }
        vertex.label.cex2 <- 0.4
    }
    if (is.null(vertex.size)) {
        vertex.size <- vertex.size2
    }
    if (is.null(vertex.label.dist)) {
        vertex.label.dist <- vertex.label.dist2
    }
    if (is.null(vertex.label.cex)) {
        vertex.label.cex <- vertex.label.cex2
    }
    if (newpage) {
        dev.new()
    }
    igraph::plot.igraph(
        ig, layout=glayout, vertex.frame.color=vertex.frame.color,
        vertex.size=vertex.size, vertex.color=vertex.color,
        vertex.shape=vertex.shape, vertex.label=vertex.label,
        vertex.label.cex=vertex.label.cex, vertex.label.dist=vertex.label.dist,
        vertex.label.color=vertex.label.color, vertex.label.family="sans",
        edge.color=ecol, ...
    )
    invisible()
}
