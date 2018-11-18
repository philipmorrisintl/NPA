#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Generate heatmap for the provided data matrix
#'
#' This function provides a flexible interface to generate heatmaps for the provided data \code{X}.
#'
#' @param X A \code{matrix} of the data to be visualized.
#' @param group A \code{vector of factors} with the group assignments of the rows.
#' @param group.col A \code{vector of factors} with the group assignments of the columns
#' @param show.facet A \code{logical} specifying whether facets are shown.
#' @param show.facet.x A \code{logical} specifying whether facets are shown.
#' @param names.arg A \code{vector of characters} that is used as the column names. If \code{NULL} the original column names of \code{X} are used.
#' @param title A \code{character} with the plot title.
#' @param subtitle A \code{character} with the plot sub title.
#' @param cex.txt A \code{numeric} that specifies the scaling factor of the displayed text.
#' @param col.text Color of the text.
#' @param col.facet.text Color of the facet text.
#' @param cex.labx A \code{numeric} that specifies the scaling factor of the x-axis/column labels.
#' @param cex.laby A \code{numeric} that specifies the scaling factor of the y-axis/row labels.
#' @param col.lab Color of the axis labels.
#' @param col.group A \code{vector of characters} with the group colors.
#' @param col.scale Color vector used for the color scale.
#' @param cex.facet A \code{numeric} that specifies the scaling factor of the facet labels.
#' @param cex.facetx A \code{numeric} that specifies the scaling factor of the x-axis/column facet labels.
#' @param cex.facety A \code{numeric} that specifies the scaling factor of the y-axis/row labels.
#' @param cex.main A \code{numeric} that specifies the scaling factor of the title text.
#' @param zlim.max A \code{numeric} that specifies the maximum value of the color scale.
#' @param zlim.min A \code{numeric} that specifies the minimum value of the color scale.
#' @param angle.facet.textx A \code{numeric} that specifies the angle of the facet text of the x-axis/columns.
#' @param angle.facet.texty A \code{numeric} that specifies the angle of the facet text of the y-axis/rows.
#' @param BW A \code{logical} that specifies whether a black/white plot is generated.
#' @param textmat A \code{character matrix} with text that is displayed in the cells of the heatmap. Must have same dimension as \code{X}. If \code{useSymbols==TRUE} the text is interpreted as symbols/shapes. Multiple shapes can be displayed by concatenation (Shape1-Shape2-Shape3), but same number of shape symbols required across all cells.
#' @param shape_key_title  A \code{character} with the title of the shape key.
#' @param cluster A \code{logical} that specifies whether rows and columns of the matrix \code{X} should be clustered.
#' @param cluster.row A \code{logical} that specifies whether the rows of the matrix \code{X} should be clustered.
#' @param cluster.col A \code{logical} that specifies whether the columns of the matrix \code{X} should be clustered.
#' @param symmetric A \code{logical} that specifies whether the color scale should be symmetric.
#' @param dist.fun Distance \code{function} that is used for clustering of rows and columns.
#' @param dist.fun.row Distance \code{function} that is used for clustering of rows.
#' @param dist.fun.col Distance \code{function} that is used for clustering of columns.
#' @param dist.method Distance method that is used when \code{dist.fun} is not specified directly (default = euclidean).
#' @param panel.name Color panel name (for brewer.pal function) that is used for symmetric color scales when the color scale is not given.
#' @param key.title A \code{character} with the title of the key.
#' @param grid.color Color of the grid.
#' @param panelborder.color Color of the panel border.
#' @param panel.lwd A \code{numeric} which specifies the width of the panel borders.
#' @param strip.bg A \code{logical} which specifies whether the background should be stripped.
#' @param strip.border A \code{logical} which specifies whether the border should be stripped.
#' @param object.only A \code{logical} which specifies whether only the ggplot2 object should be provided, ie. the plot is not created.
#' @param newpage A \code{logical} which specifies whether a new page should be created during plotting.
#' @param useSymbols A \code{logical} which specifies whether symbols rather than text are displayed within each cell of the heatmap.
#' @param symbol_key_title A \code{character} with the title of the key for the symbols.
#' @param symbol_size A \code{numeric} with the size of the symbols.
#' @param symbol_labels A \code{character vector} with the labels of the symbols.
#' @param color_na_values A \code{character} specifying the color used for missing values in the matrix.
#'
#' @import ggplot2
#' @import reshape2
#' @import grid
#' @import stringr
#' @importFrom grDevices colorRampPalette
#' @importFrom stats dist
#' @importFrom stats hclust
#' @importFrom stats as.dist
#' @importFrom grDevices gray
#' @importFrom grDevices col2rgb
#' @include utils.R
#'
#' @return A ggplot2 object.
ImagePlotGG <- function(X,
                        group = factor(rep("", nrow(X))),
                        group.col = factor(rep("", ncol(X))),
                        show.facet = TRUE,
                        show.facet.x = show.facet,
                        names.arg = NULL,
                        title = "Heatmap",
                        subtitle = NULL,
                        cex.txt = 1,
                        col.text = "black",
                        col.facet.text = "white",
                        cex.labx = 1,
                        cex.laby = cex.labx,
                        col.lab = "grey40",
                        col.group = colorRampPalette(brewer.pal(n = 7,
                                                                "Spectral"))(nlevels(group)),
                        col.scale = NULL,
                        cex.facet = 2,
                        cex.facetx = cex.facet,
                        cex.facety = cex.facet,
                        cex.main = 2,
                        zlim.max = Inf,
                        zlim.min = -Inf,
                        angle.facet.textx = 0,
                        angle.facet.texty = -90,
                        BW = FALSE,
                        textmat = matrix("", nrow = nrow(X), ncol = ncol(X)),
                        shape_key_title = "",
                        cluster = FALSE,
                        cluster.row = cluster,
                        cluster.col = cluster,
                        symmetric = ifelse(sign(prod(range(X,
                                                           na.rm = TRUE))) == 1, FALSE, TRUE),
                        dist.fun = function(x) {
                          dist(x, method = dist.method)
                        },
                        dist.fun.row = dist.fun,
                        dist.fun.col = dist.fun,
                        dist.method = "euclidean",
                        panel.name = "RdYlBu",
                        key.title = "Value",
                        grid.color = "grey50",
                        panelborder.color = "grey50",
                        panel.lwd = 0,
                        strip.bg = "grey20",
                        strip.border = "grey40",
                        object.only = FALSE,
                        newpage = FALSE,
                        useSymbols = FALSE,
                        symbol_key_title = "",
                        symbol_size = 1,
                        symbol_labels = NULL,
                        color_na_values = "grey50") {
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  if (setequal(as.character(rownames(X)), as.character(1:nrow(X)))) {
    rownames(X) <- paste0("Row", rownames(X))
  }
  if (setequal(as.character(colnames(X)), as.character(1:ncol(X)))) {
    colnames(X) <- paste0("Col", colnames(X))
  }
  if (ncol(X) == 1) {
    cluster.col <- FALSE
  }
  if (nrow(X) == 1) {
    cluster.row <- FALSE
  }
  if (max(table(paste(colnames(X), group.col))) > 1) {
    t0 <- table(paste(colnames(X), group.col))
    stop(paste0(
      "Some colnames are not unique (within group.col, if any):",
      paste(names(t0)[t0 > 1], collapse = ", ")
    ))
  }
  if (max(table(paste(rownames(X), group))) > 1) {
    t0 <- table(paste(rownames(X), group))
    stop(paste0(
      "Some rownames are not unique (within group, if any):",
      paste(names(t0)[t0 > 1], collapse = ", ")
    ))
  }
  if (!is.factor(group)) {
    group <- factor(group)
  }
  if (!is.factor(group.col)) {
    group.col <- factor(group.col)
  }
  txt <- textmat
  if (!useSymbols) {
    txt[is.na(txt)] <- ""
  }
  if (is.null(rownames(X))) {
    rownames(X) <- paste0("Row", 1:nrow(X))
  }
  rownames(X) <- paste0(rownames(X), "")
  if (is.null(colnames(X))) {
    colnames(X) <- paste0("Col", 1:ncol(X))
  }
  colnames(X) <- paste0(colnames(X), "")
  if (!all(dim(txt) == dim(X))) {
    stop("textmat should have the same size as X")
  }
  flag.colnull <- 0
  if (is.null(col.scale)) {
    flag.colnull <- 1
    if (sign(prod(range(X, na.rm = TRUE))) >= 0 & symmetric ==
        FALSE) {
      col.scale <- c("white", brewer.pal(6, panel.name)[-1])
    } else {
      col.scale <- c(brewer.pal(n = 9, "Blues")[rev(c(2,
                                                      5, 9))],
                     "white",
                     brewer.pal(n = 9, "YlOrRd")[c(2,
                                                   5, 9)])
    }
  }
  ord1 <- 1:nrow(X)
  Xclust <- X
  Xclust[is.na(X)] <- 0
  ind.all <<- NULL
  if (cluster.row == TRUE) {
    if (nlevels(group) == 1) {
      hc0 <- hclust(as.dist(dist.fun(Xclust)))
      ord1 <- hc0$order
    } else {
      tmp2 <- tapply(1:nrow(X), group, function(yy) {
        if (length(yy) >= 3) {
          tmp <- X[yy, , drop = FALSE]
          tmp[is.na(tmp)] <- 0
          ind.1 <- hclust(dist.fun.row(tmp))$order
          ind.all <<- c(ind.all, yy[ind.1])
        } else {
          ind.all <<- c(ind.all, yy)
        }
      })
      ord1 <- ind.all
    }
    group <- group[ord1]
    X <- X[ord1, , drop = FALSE]
    txt <- txt[ord1, , drop = FALSE]
  }
  if (cluster.col == TRUE) {
    hc0 <- hclust(as.dist(dist.fun.col(t(Xclust))))
    ord2 <- hc0$order
    X <- X[, ord2, drop = FALSE]
    txt <- txt[, ord2, drop = FALSE]
    group.col <- group.col[ord2]
  }
  if (zlim.max != Inf) {
    X[X > zlim.max] <- zlim.max
  }
  if (zlim.min != -Inf) {
    X[X < zlim.min] <- zlim.min
  }
  if (zlim.max != Inf | zlim.min != -Inf) {
    txtsub <- paste0("(Data range truncated in ]", zlim.min,
                     ",", zlim.max, "[)")
    if (!is.null(subtitle)) {
      subtitle <- ifelse(subtitle == "", "", paste(subtitle,
                                                   txtsub))
    } else {
      subtitle <- txtsub
    }
  }
  group2 <- apply(X, 2, function(x)
    as.character(group))
  if (nrow(X) == 1) {
    group2 <- matrix(group2, nrow = 1)
  }
  group2.col <- t(apply(X, 1, function(x)
    as.character(group.col)))
  if (ncol(X) == 1) {
    group2.col <- t(group2.col)
  }
  rownames(txt) <-
    rownames(group2) <- rownames(group2.col) <- rownames(X)
  colnames(txt) <-
    colnames(group2) <- colnames(group2.col) <- colnames(X)
  plotlist <-
    list(
      coef = X,
      text = txt,
      group = group2,
      group.col = group2.col
    )
  if (!is.null(names.arg)) {
    if (length(names.arg) == ncol(X)) {
      colnames(X) <- names.arg
    } else {
      stop("names.arg must be of right length (ncol(X))")
    }
  }
  col3 <- col.scale
  if (length(col.group) == 1) {
    col.group <- rep(col.group, nlevels(group))
  }
  if (BW == TRUE) {
    col3 <- gray(colSums(col2rgb(col3)) / (3 * 255))
    col.group <- gray(colSums(col2rgb(col.group)) / (3 * 255))
    if (!col.text %in% c("black", "white")) {
      col.text <- "black"
    }
  }
  D <-
    cbind(
      reshape2::melt(plotlist$coef),
      reshape2::melt(plotlist$text),
      reshape2::melt(plotlist$group),
      reshape2::melt(plotlist$group.col)
    )
  D <- D[, c(2, 1, 3, 6, 9, 12)]
  Variable <- RowName <- Value <- Text <- Text1 <- NULL
  Shift1 <- Shift2 <- Text2 <- Shift3 <- Text3 <- Shift4 <- NULL
  Text4 <- Shift5 <- Text5 <- Group <- NULL
  colnames(D) <- c("Variable", "RowName", "Value", "Text",
                   "Group", "Group2")
  D$Group2 <- factor(D$Group2, levels = levels(group.col))
  D$Group <- factor(D$Group, levels = levels(group))
  D$facet_fill_color <- col.group[match(D$Group, levels(group))]
  p <-
    ggplot(D, aes(x = Variable, y = RowName)) + ggplot2::geom_tile(aes(fill = Value),
                                                          colour = grid.color)
  if (show.facet.x == FALSE) {
    p <-
      p + ggplot2::theme(strip.background = ggplot2::element_blank(),
                strip.text.x = ggplot2::element_blank())
  }
  if (symmetric == TRUE) {
    max_val <- 1.1 * max(abs(X), na.rm = TRUE)
    values <- seq(-max_val, max_val, length = length(col3) +
                    1)
    p <- p + ggplot2::scale_fill_gradientn(
      colours = col3,
      name = key.title,
      limits = c(-max_val, max_val),
      na.value = color_na_values
    ) + labs(x = "", y = "") +
      ggplot2::scale_x_discrete(expand = c(0, 0)) + ggplot2::scale_y_discrete(expand = c(0,
                                                                       0))
  } else {
    if (prod(range(X, na.rm = TRUE)) < 0) {
      n <- 10
      c0 <- abs(max(X, na.rm = TRUE)) / abs(min(X, na.rm = TRUE))
      val <- c(seq(min(X, na.rm = TRUE) * 1.1,-1e-10,
                   length = 50),
               0,
               seq(1e-10, max(X, na.rm = TRUE) *
                     1.1, length = 50))
      p <-
        p + ggplot2::scale_fill_gradientn(
          colours = col3,
          name = key.title,
          values = val,
          limits = range(X, na.rm = TRUE) *
            1.1,
          breaks = scales::cbreaks(range(X, na.rm = TRUE) *
                             1.05, scales::pretty_breaks(5))$breaks,
          rescaler = function(x,
                              ...)
            x,
          oob = identity,
          na.value = color_na_values
        ) + labs(x = "", y = "",
                 title = title) + ggplot2::scale_x_discrete(expand = c(0,
                      0)) + ggplot2::scale_y_discrete(expand = c(0, 0))
    } else {
      p <- p + ggplot2::scale_fill_gradientn(
        colours = col3,
        name = key.title,
        breaks = scales::cbreaks(range(X, na.rm = TRUE) * 1.05,
                                  scales::pretty_breaks(5))$breaks
      ) + labs(x = "", y = "",
               title = title) + ggplot2::scale_x_discrete(expand = c(0,
                                                            0)) + ggplot2::scale_y_discrete(expand = c(0, 0))
    }
  }
  if (is.character(subtitle)) {
    p <- p + ggplot2::ggtitle(bquote(atop(.(title), atop(italic(
      .(subtitle)
    ),
    ""))))
  } else {
    p <- p + ggplot2::ggtitle(bquote(.(title)))
  }

  if (nlevels(group) > 1 &
      nlevels(group.col) == 1 & !is.null(col.group)) {
    p <- p + ggplot2::facet_grid(Group ~ ., scales = "free", space = "free")
    p <- p + ggplot2::theme(strip.background = ggplot2::element_blank())
  }
  if (nlevels(group) > 1 &
      nlevels(group.col) == 1 & is.null(col.group)) {
    p <- p + ggplot2::facet_grid(Group ~ ., scales = "free", space = "free")
    p <-
      p + ggplot2::theme(strip.background = ggplot2::element_rect(colour = strip.border,
                                                fill = strip.bg))
  }
  if (nlevels(group) > 1 & nlevels(group.col) > 1) {
    p <- p + ggplot2::facet_grid(Group ~ Group2, scales = "free",
                        space = "free")
    p <-
      p + ggplot2::theme(strip.background = ggplot2::element_rect(colour = strip.border,
                                                fill = strip.bg))
  }
  if (nlevels(group) == 1 & nlevels(group.col) > 1) {
    p <- p + ggplot2::facet_grid(. ~ Group2, scales = "free", space = "free")
    if (show.facet.x == TRUE) {
      p <-
        p + ggplot2::theme(strip.background = ggplot2::element_rect(colour = strip.border,
                                                  fill = strip.bg))
    }
  }
  p <-
    p + ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        size = 5 *
          cex.labx,
        angle = 90,
        hjust = 0,
        colour = col.lab
      ),
      axis.text.y = ggplot2::element_text(size = 5 *
                                   cex.laby, colour = col.lab),
      plot.title = ggplot2::element_text(size = 12 *
                                  cex.main),
      strip.text = ggplot2::element_text(color = col.facet.text),
      strip.text.y = ggplot2::element_text(angle = angle.facet.texty,
                                  size = 5 * cex.facety),
      strip.text.x = ggplot2::element_text(angle = angle.facet.textx,
                                  size = 5 * cex.facetx),
      panel.border = ggplot2::element_rect(
        size = panel.lwd,
        colour = panelborder.color,
        fill = NA
      )
    )
  if (!useSymbols) {
    p <-
      p + ggplot2::geom_text(ggplot2::aes(fill = Value, label = Text),
                    colour = col.text,
                    size = 5 * cex.txt)
  } else {
    # interpret textmat as R symbols browser()
    D2 = D
    D2$Text = as.character(D2$Text)
    D2$Text = sapply(D2$Text, function(x) {
      if (is.na(x)) {
        return("NA")
      } else if (x == "") {
        return("NA")
      } else {
        x = gsub("-$", "-NA", gsub("^-", "NA-", gsub("--", "-NA-", x)))
        return(x)
      }
    })

    no_symbols = stringr::str_count(as.character(D2$Text)[1], "-") + 1
    all_symbols = c()
    for (i in 1:no_symbols) {
      D2[, paste0("Text", i)] = as.character(getsplit(as.character(D2$Text), "-", i))
      D2[, paste0("Text", i)][D2[, paste0("Text", i)] == "NA"] = NA
      all_symbols = c(all_symbols, unique(as.numeric(D2[, paste0("Text", i)])))
    }

    text_shifts = c()
    if (no_symbols == 1) {
      D2$Shift1 = 0
    } else if (no_symbols == 2) {
      D2$Shift1 = -0.2
      D2$Shift2 = 0.2
    } else if (no_symbols == 3) {
      D2$Shift1 = -0.3
      D2$Shift2 = 0
      D2$Shift3 = 0.3
    } else if (no_symbols == 4) {
      D2$Shift1 = -0.3
      D2$Shift2 = -0.1
      D2$Shift3 = 0.1
      D2$Shift4 = 0.3
    } else if (no_symbols == 5) {
      D2$Shift1 = -0.4
      D2$Shift2 = -0.2
      D2$Shift3 = 0
      D2$Shift4 = 0.2
      D2$Shift5 = 0.4
    } else {
      stop(paste0(no_symbols), " number of symbols not supported yet")
    }

    if (no_symbols == 1) {
      p2 = p + ggplot2::geom_point(
        ggplot2::aes(x = Variable, shape = factor(Text1)),
        size = symbol_size,
        colour = col.text,
        data = D2,
        fill = "black"
      )
    } else if (no_symbols > 1) {
      #equals else, but just to be clear
      p2 = p + ggplot2::geom_point(
        ggplot2::aes(
          x = as.numeric(Variable) + Shift1,
          shape = factor(Text1)
        ),
        size = symbol_size,
        colour = col.text,
        data = D2,
        fill = "black"
      )
      p2 = p2 + ggplot2::geom_point(
        ggplot2::aes(
          x = as.numeric(Variable) + Shift2,
          shape = factor(Text2)
        ),
        size = symbol_size,
        colour = col.text,
        data = D2,
        fill = "black"
      )
    }

    if (no_symbols > 2) {
      p2 = p2 + ggplot2::geom_point(
        ggplot2::aes(
          x = as.numeric(Variable) + Shift3,
          shape = factor(Text3)
        ),
        size = symbol_size,
        colour = col.text,
        data = D2,
        fill = "black"
      )
    }
    if (no_symbols > 3) {
      p2 = p2 + ggplot2::geom_point(
        ggplot2::aes(
          x = as.numeric(Variable) + Shift4,
          shape = factor(Text4)
        ),
        size = symbol_size,
        colour = col.text,
        data = D2,
        fill = "black"
      )
    }
    if (no_symbols > 4) {
      p2 = p2 + ggplot2::geom_point(
        ggplot2::aes(
          x = as.numeric(Variable) + Shift5,
          shape = factor(Text5)
        ),
        size = symbol_size,
        colour = col.text,
        data = D2,
        fill = "black"
      )
    }

    #add scale
    val_list <- all_symbols
    names(val_list) <- as.character(all_symbols)
    if (is.null(symbol_labels)) {
      symbol_labels <- val_list
    }
    p2 <-
      p2 + ggplot2::scale_shape_manual(symbol_key_title, values = val_list,
                              labels = symbol_labels)
    p = p2
  }
  if (nlevels(group) > 1 &
      nlevels(group.col) == 1 & !is.null(col.group)) {
    dummy <- ggplot2::ggplot(D, ggplot2::aes(x = Variable, y = RowName)) +
      ggplot2::facet_grid(Group ~ ., scales = "free", space = "free") +
      ggplot2::geom_rect(
        ggplot2::aes(fill = Group),
        xmin = -Inf,
        xmax = Inf,
        ymin = -Inf,
        ymax = Inf,
        color = "black"
      ) + ggplot2::scale_fill_manual("Group",
                            values = col.group) + ggplot2::theme(
                              strip.text = ggplot2::element_text(color = col.facet.text),
                              strip.text.y = ggplot2::element_text(angle = angle.facet.texty,
                                                          size = 5 * cex.facety),
                              strip.background = ggplot2::element_blank()
                            )
    g1 <- ggplot2::ggplotGrob(p)
    g2 <- ggplot2::ggplotGrob(dummy)
    gtable_select <- function(x, ...) {
      matches <- c(...)
      x$layout <- x$layout[matches, , drop = FALSE]
      x$grobs <- x$grobs[matches]
      x
    }
    panels <- grepl(pattern = "panel", g2$layout$name)
    strips <- grepl(pattern = "strip-right", g2$layout$name)
    g2$layout$r[panels] <- g2$layout$r[panels] + 1
    g2$layout$l[panels] <- g2$layout$l[panels] + 1
    new_strips <- gtable_select(g2, panels | strips)
    gtable_stack <- function(g1, g2) {
      z <- NULL
      g1$grobs <- c(g1$grobs, g2$grobs)
      g1$layout <- transform(g1$layout, z = z - max(z),
                             name = "g2")
      g1$layout <- rbind(g1$layout, g2$layout)
      g1
    }
    new_plot <- gtable_stack(g1, new_strips)
    p <- new_plot
    if (object.only == FALSE) {
      if (newpage == TRUE) {
        grid::grid.newpage()
      }
      p <- grid::grid.draw(new_plot)
      #print(p)
    }
  } else {
    if (object.only == FALSE) {
      if (newpage == TRUE) {
        grid::grid.newpage()
      }
      #print(p)
    }
  }
  return(invisible(p))
}
