#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' barplotNPAGG: Yet another barplot of NPA object (using ggplot)
#'
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 position_dodge
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @param np A list object containing NPA data
#' @param main Title
#' @param cex.labx Size of labels
#' @param col Colors of the bars
#' @param txt.size Size of the *O*K* labels
#'
#' @return A ggplot object
#' @importFrom grDevices rainbow
#' @export

barplotNPAGG <- function(np,
                         main = "Network Perturbation Amplitude", cex.labx = 2,
                         col = rainbow(length(np$coefficients), s = 0.3, v = 0.6),
                         txt.size = 1) {

    D = data.frame(x = np$coefficients, ID = factor(names(np$coefficients), levels = names(np$coefficients)),
                   ci.up = np$ci.up, ci.down = np$ci.down)

    ci.up <- ci.down <- ID <- x <- NULL

    limits <- aes(ymax = ci.up, ymin = ci.down)
    dodge <- position_dodge(width = 0.9)

    txt = rep("", nrow(D))
    D$O = np$pvperm[1, ]
    D$K = np$pvperm[2, ]
    D$label = ""
    D$label[D$O < 0.1] = ".O"
    D$label[D$O < 0.05] = "*O"
    D$label[D$ci.down > 0] = paste(D$label[D$ci.down > 0], "*", sep = "")
    D$label[D$K < 0.05] = paste(D$label[D$K < 0.05], "K*", sep = "")
    D$label[D$K >= 0.05 & D$K < 0.1] = paste(D$label[D$K >= 0.05 & D$K < 0.1], "K.",
                                             sep = "")
    D$Olabel = rep(" ", nrow(D))
    D$Olabel[D$O < 0.1] = ".O"
    D$Olabel[D$O < 0.05] = "*O"
    D$Klabel = rep("", nrow(D))
    D$Klabel[D$K < 0.1] = "K."
    D$Klabel[D$K < 0.05] = "K*"
    D$starlabel = rep(" ", nrow(D))
    D$starlabel[np$ci.down > 0] = " * "
    col1 = col
    col[!(D$O < 0.05 & D$K < 0.05 & D$ci.down > 0)] = "#C3C3C3"
    names(col) = D$ID
    colK=rep("blue",nrow(D))
    colK[D$Klabel=="K."]="grey"
    colO=rep("green2",nrow(D))
    colO[D$Olabel==".O"]="grey"

    q0 <- ggplot(D, aes(x = ID, y = x, fill = ID)) +
        geom_bar(stat = "identity", colour = col1, position = dodge) +
        xlab("") + ylab("NPA") + ggtitle(main) +
        geom_errorbar(limits, position = dodge, width = 0.15, size = 1.1, colour = "grey40") +
        ggplot2::annotate("text", x = D$ID, y = D$x + (max(D$x) * 0.02), label = D$Olabel, colour = colO, hjust = 1.5, vjust = -0.5, size = 4*txt.size) +
        ggplot2::annotate("text",x = D$ID, y = D$x + (max(D$x) * 0.02), label = D$Klabel, colour = colK, hjust = -1.5, vjust = -0.5, size = 4*txt.size) +
        ggplot2::annotate("text", x = D$ID, y = D$x + (max(D$x) *0.02), label = D$starlabel, colour = "red2", hjust = 0, vjust = -0.5, size = 4*txt.size) +
        scale_fill_manual(values = col) +
        theme(legend.position = "none", axis.text.x = element_text(size = 5 * cex.labx, angle = 90, hjust = 0, colour = "grey30"))


    return(q0)
}
