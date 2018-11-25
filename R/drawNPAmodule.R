#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Draw the graph of the network showing NPA scores
#'
#' @param np A list object, NPA data.
#' @param whichin A integer vector, indexes of selected comparisons.
#' @param cex.leg A numerical value, size of legend.
#' @param col.leg A character vector, color legend.
#' @param vertex.size A numerical, size vertex.
#' @param vertex.label.dist A numerical, distance of label to node center.
#' @param glayout A layout object for drawing graph.
#' @param colbg.nodes A character vector, color background for the nodes.
#' @param abbrev A logical. Should node name be shortened?
#' @param display.stat A logical value for displaying display stats or not.
#' @param lwdHighlight A numerical value, width of the highlighted nodes.
#' @param colHighlight A character vector, color of the hihlighted nodes.
#' @param vertexHighlight A string vector, names of vertexes to be highlighted.
#' @param p A numerical vector of length 2. Threshold values, for leading nodes
#' and leading genes detection.
#' @param showSignif A logical. If \code{TRUE}, "#" symbol is shown for nodes that
#' have signifiant NPA scores. 
#' @param ... any argument to be pased to visNet2 function.
#' @return A list object containing the graph object, layout and labels of the network.
#' @importFrom igraph induced_subgraph
#' @importFrom igraph layout.fruchterman.reingold
#' @importFrom igraph add.vertex.shape
#' @importFrom Rgraphviz getNodeXY
#' @importFrom Rgraphviz agopen
#' @importFrom igraph V
#' @importFrom methods as
#' @importFrom methods is
#' @include getNPALE.R
#' @include colorscale.R
#' @include visNet2.R

drawNPAmodule<- function (np, whichin = 1:length(np$coefficients), cex.leg = 1,
                          col.leg = "green3", vertex.size = 5, vertex.label.dist = NULL, vertex.set = NULL,
                          glayout = "dot", colbg.nodes = "grey90",
                          okonly = TRUE, abbrev = TRUE, display.stat = TRUE, lwdHighlight=1,
                          colHighlight = "black", vertexHighlight = NULL, p = 0.8,
                          showSignif = FALSE, ...)
{
    if (abbrev == TRUE) {
        short <- function(nm) {
            sapply(strsplit(nm, ""), function(x) {
                if (length(x) > 23) {
                    x <- paste(c(x[1:min(length(x), 23)], "..."),
                               collapse = "")
                }
                else {
                    x <- paste(x, collapse = "")
                }
                return(x)
            })
        }
    }else {
        short <- function(nm) {
            nm
        }
    }

    npSignif =  sapply(1:length(np$coefficients), function(j) {all(np$pvperm[,j] < 0.05) & np$ci.down[j] >0})
    stopifnot(sum(npSignif)>0)
    g <- np$model$g

    if(!is.null(vertex.set)){
        stopifnot(all(vertex.set %in% igraph::V(g)$name))
    }else{
        vertex.set <- intersect(rownames(np$nodes.coefficients),igraph::V(g)$name)
    }

    g <- igraph::induced_subgraph(g, match(vertex.set, igraph::V(g)$name))
    nm0 <- igraph::V(g)$name
    igraph::V(g)$name <- short(igraph::V(g)$name)

    #tmp <- np$nodes.coefficients[rownames(np$nodes.coefficients) %in% vertex.set, whichin, drop = FALSE]
    tmp <- np$nodes.coefficients
    rownames(tmp) <- short(rownames(tmp))

    bar.values <- lapply(1:nrow(tmp), function(i) tmp[i, ])
    names(bar.values) <- rownames(tmp)
    bar.values <- bar.values[match(igraph::V(g)$name, names(bar.values))]

    len <- getNPALE(np, prop = p )[whichin]
    text.nodes <- matrix("", nrow = nrow(np$nodes.coefficients), ncol = length(len))
    colnames(text.nodes) <- names(len)
    rownames(text.nodes) <- rownames(np$nodes.coefficients)

    for(h in 1:length(len)){
        sgn <- sign(np$nodes.coefficients.ci.up * np$nodes.coefficients.ci.down)
        text.nodes[rownames(text.nodes) %in% len[[h]]$leadingNodes[, "GeneId"],h] <- "*"
        if(showSignif ==  TRUE){
            text.nodes[rownames(text.nodes) %in%
                            intersect(len[[h]]$leadingNodes[, "GeneId"],rownames(sgn)[sgn[,h] == -1]),h] <- "#"
        }
    }

    if(okonly == TRUE){
        text.nodes[, which(!npSignif[whichin])] <- ""
    }
    rownames(text.nodes) <- short(rownames(text.nodes))
    myvertexplot <- function(coords, v = NULL, params) {
        vertex.size <- 1/200 * params("vertex", "size")
        if (length(vertex.size) != 1 && !is.null(v)) {
            vertex.size <- vertex.size[v]
        }
        bar <- params("vertex", "bar")
        nm <- params("vertex", "name")
        if (is.list(bar) && !is.null(v)) {
            bar <- bar[v]
        }
        else if (!is.null(v)) {
            bar <- list(bar)
        }
        c0 <- 0

        mapply(coords[, 1], coords[, 2], vertex.size * 2, bar,
               FUN = function(x, y, size, int) {
                   c0 <<- c0 + 1
                   stopifnot(is.vector(int))
                   names(int) <- NULL
                   if(nm0[c0] %in% vertexHighlight){
                       bdr <- colHighlight
                   }else{
                       bdr <- NA
                   }
                   nc <- length(int)
                   txt <- text.nodes[rownames(text.nodes) == nm[c0], ]
                   xc <- seq(x - size/2, x + size/2, length = nc + 1)
                   yc <- int/max(abs(int)) * size/2 + y

                   #colbar <- colorscale(int, signed = TRUE, minx = min(np$nodes.coefficients[rownames(np$nodes.coefficients) %in% vertex.set,whichin]),
                #                    maxx = max(np$nodes.coefficients[rownames(np$nodes.coefficients) %in% vertex.set,whichin]))
                   colbar <- colorscale(int, signed = TRUE, minx = min(np$nodes.coefficients),
                                        maxx = max(np$nodes.coefficients))
                   colbar[!npSignif[whichin]] <-"grey30"
                   rect(xc[1], y - size/1.9, xc[length(xc)], y + size/1.9,
                        col = colbg.nodes, border = bdr, lwd = lwdHighlight)
                   for (k in 1:length(xc)) {
                       rect(xc[k], y, xc[k + 1], yc[k], col = colbar[k])
                       if (display.stat == TRUE) {
                           if(k<=length(txt)){
                               text(xc[k]+ diff(xc)[1]/2, y, txt[k], cex = cex.leg, pos=1,
                                    col = col.leg)
                           }
                       }
                   }
               })
    }
    if (is(class(glayout), "function")) {
        ## Bioconductor does not allow setting seed in the code
        ## Do it outside of the function call if needed.
        # set.seed(seed)
        edges <- igraph::E(g)
        edges$weigths <- abs(edges$weight)
        igraph::E(g) <- edges
        glayout <- glayout(g)
        rownames(glayout) = igraph::V(g)$name
    }
    if(is.character(glayout)){
        if(glayout == "dot"){
            df <- get.data.frame(g)[,1:2]
            dv <- igraph::V(g)$name
            df2 <- cbind(dv, dv)
            colnames(df2) <- colnames(df)
            df <- rbind(df, df2)
            gg <- as(getAdj(df, symmetric = TRUE), "graphNEL")
            ## Bioconductor does not allow setting seed in the code
            ## Do it outside of the function call if needed.
            # set.seed(467563)
            glayout <- do.call(cbind, Rgraphviz::getNodeXY(Rgraphviz::agopen(gg, ""))) # DOT by default
            rownames(glayout) <- nodes(gg)
            glayout <- glayout[match(igraph::V(g)$name, rownames(glayout)),]
        }
    }
    if (is.null(vertex.label.dist)) {
        vertex.label.dist <- min(dist(glayout))/60
    }
    igraph::add.vertex.shape("bar", clip = igraph::vertex.shapes("circle")$clip,
                     plot = myvertexplot, parameters = list(vertex.bar = rep(0,
                                                                             length(whichin))))
    visNet2(g, vertex.shape = "bar", vertex.bar = bar.values,
            vertex.size = vertex.size, vertex.label.dist = vertex.label.dist,
            glayout = glayout)
    #visNet2(g, vertex.shape = "bar", vertex.bar = bar.values,
    #        vertex.size = vertex.size, vertex.label.dist = vertex.label.dist,
    #        glayout = glayout, ...)

    glayoutNormalized <- apply(glayout,2,function(x) 2*(x-min(x))/(max(x)-min(x))-1)
    rownames(glayoutNormalized) <- rownames(glayout)
    res <- list(g = g,
              layout =  glayoutNormalized,
              labels = igraph::V(g)$name)
    return(invisible(res))
}
