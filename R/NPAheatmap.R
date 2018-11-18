#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' heatmapNPA plot backbone differential values and indicate leading node and significance status
#'
#' @import ggplot2
#' @param npa A NPA object
#' @param clusterNodes Logical, perform a clustering?
#' @param cex.labx Size of the labels for contrast
#' @param cex.laby Size of the labels for node names
#' @param cex.main Size of the title
#' @param angle.facet.texty Angle of text in teh y-facets
#' @param cex.facety Size of the text in the y-facets
#' @param cex.facetx Size of the text in the y-facets
#' @param col.text Text color (black by default)
#' @param cex.txt Size of the symbols
#' @param show Logical, print the graph or return it as output?
#' @importFrom reshape2 melt 
#'
#' @return Invisible, a ggplot object
#'
heatmapNPA <- function(npa, clusterNodes = TRUE, cex.labx = 1.6, cex.laby = 1.6,
                       cex.main = 1.6,
                      angle.facet.texty = 90, cex.facety = 3, cex.facetx = 2,
                      col.text = "black",
                      cex.txt = 1, show = TRUE){

    X = npa$nodes.coefficients
    group2 = rep("Not *O*K*", ncol(X))
    group2[which(apply(npa$pvperm[1:2,],2,max) < 0.05 & npa$ci.down>0)]="*O*K*"

    #Get text matrix
    LN = getNPALEtable2(npa)
    LN = LN[match(rownames(X), rownames(LN)),]

    symb = c("Not Significant" = 4,
                "LN and Significant"=1,
                "LN and not Significant"=13)
    stopifnot(all(rownames(LN) == rownames(X)))

    txt = apply(LN,2, function(x){
        y = y2 = rep(0,length(x))
        y[grep("*",x, fixed=TRUE)] = symb["LN and Significant"]
        y2[grep("!",x, fixed=TRUE)] = symb["Not Significant"]
        y[y==symb["LN and Significant"] & y2==symb["Not Significant"]] = symb["LN and not Significant"]
        y[y==0 & y2==symb["Not Significant"]] = symb["Not Significant"]
        y[y==0]=NA
        return(y)
    })
    rownames(txt) = rownames(LN)

    #Define row groups
    group = rep("No LN", nrow(txt))
    group[apply(txt, 1, function(x) length(x[x%in%symb[2:3]])>0)] = "LN"
    group = factor(group)

    #Cluster rows within groups
    ord1 <- 1:nrow(X)
    Xclust = X
    Xclust[is.na(Xclust)] = 0
    ind.all <<- NULL
    if (clusterNodes == TRUE) {
        if (nlevels(group) == 1) {
            hc0 = hclust(dist(Xclust))
            ord1 = hc0$order
        } else {
            ind.all <<- NULL
            tmp2 = tapply(1:nrow(X), group, function(yy) {
                if (length(yy) >= 3) {
                    tmp <- X[yy, , drop = FALSE]
                    tmp[is.na(tmp)] <- 0
                    ind.1 <- hclust(dist(tmp))$order
                    ind.all <<- c(ind.all, yy[ind.1])
                }
                else {
                    ind.all <<- c(ind.all, yy)
                }
            })
            ord1 = ind.all
        }
    }
    #Re-order
    group = group[ord1]
    X = X[ord1, , drop = FALSE]
    txt = txt[ord1, , drop = FALSE]

    #Color scale
    colScale = c(RColorBrewer::brewer.pal(n = 9, "Blues")[rev(c(2, 5, 9))],
                  "white",
                 RColorBrewer::brewer.pal(n = 9, "YlOrRd")[c(2,5, 9)])
    # Symmetric scale
    max_val = 1.1 * max(abs(X), na.rm = TRUE)
    values = seq(-max_val, max_val, length = length(colScale) +  1)


    D = cbind(reshape2::melt(X), reshape2::melt(txt)[,"value"],
              reshape2::melt(apply(X, 2, function(x) as.character(group)))[,"value"],
              reshape2::melt(t(apply(X, 1, function(x) as.character(group2))))[,"value"])
    Variable <- RowName <- Value <- Text <- NULL
    colnames(D) =  c("RowName", "Variable", "Value", "Text", "Group", "Group2")

    D$Group <- factor(D$Group, levels = levels(group))
    D$Text = factor(D$Text, levels = as.vector(symb) )
    p = ggplot(D, aes(x = Variable, y = RowName)) + ggplot2::geom_tile(aes(fill = Value), colour = "grey70")
    p = p + ggplot2::scale_fill_gradientn(colours = colScale, name = "NPA coefficients",limits = c(-max_val, max_val),
                                 breaks = scales::cbreaks(c(-max_val, max_val) * 1.05, scales::pretty_breaks(5))$breaks) +
      ggplot2::labs(x = "", y = "", title = "NPA Backbone Differential Values") + ggplot2::scale_x_discrete(expand = c(0,0)) +
      ggplot2::scale_y_discrete(expand = c(0, 0))

    p = p + ggplot2::facet_grid(Group ~ Group2, scales = "free", space = "free")
    p = p + ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                  axis.text.x = ggplot2::element_text(size = 5 * cex.labx, angle = 90, hjust = 0, colour = "grey30"),
                  axis.text.y = ggplot2::element_text(size = 5 * cex.laby, colour = "grey30"),
                  plot.title = ggplot2::element_text(size = 12 * cex.main),
                  strip.text = ggplot2::element_text(color = "white"),
                  strip.text.x = ggplot2::element_text(angle = 0,  size = 5 * cex.facetx),
                  strip.text.y = ggplot2::element_text(angle = angle.facet.texty, size = 5 * cex.facety),
                  panel.border = ggplot2::element_rect(size = 1, colour = "grey60", fill = NA),
                  strip.background = ggplot2::element_rect(colour = "grey60", fill = "grey")) +
        # geom_text(aes(fill = Value, label = Text), colour = col.text, size = 5 * cex.txt)
      ggplot2::geom_point(aes(x = Variable, shape = factor(Text)), size = cex.txt, colour = col.text, data = D, fill = "black")+
      ggplot2::scale_shape_manual("LN & Stat", values = as.vector(symb), labels = names(symb))

    if(show == TRUE){
        print(p)
    }
    #p2 = barplotNPAGG(npa)

    #p0 = grid.arrange(p,p2, ncol=2)
    return(invisible(p))
}


