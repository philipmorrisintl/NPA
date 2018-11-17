#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Extract NPA results summary and optionally display the heatmap
#'
#' @param npall0 A R list object. List of NPA objects for different networks
#' @param which An integer vector. Indexes of contrasts to use
#' @param prop.signif A numerical value. Threshold value for significant NPA to
#' keep if keep.all parameter is set to \code{FALSE}
#' @param lev A numerical value. P-value threshold
#' @param keep.all A Logical. Default is \code{FALSE}, only networks that have
#' a significant NPA are kept in the extraction.
#' @param plot A logical. Default is \code{TRUE}, figure of the extraction is
#' generated.
#' @param title A character vector. Title of the figure.
#' @param cex.txt A numerical value. Text size scaling.
#' @param cex.labx A \code{numeric} that specifies the scaling factor of the x-axis/column labels.
#' @param cex.laby A \code{numeric} that specifies the scaling factor of the y-axis/row labels.
#' @param cex.facet A \code{numeric} that specifies the scaling factor of the facet labels.
#' @param cex.main A \code{numeric} that specifies the scaling factor of the title text.
#' @param names.arg A string vector. Alternative names for contrasts.
#' @param col.text A character vector. Color for figure text.
#' @param okonly A logical. Default is \code{FALSE}. Not "O.K." NPA won't be
#' ignored.
#' @param simplified.text A logical. Default is \code{FALSE}. Advanced text
#' symbols is used.
#' @param normalize A logical. Default is \code{TRUE}, normalization is
#' performed using the maximum coefficient value.
#' @param BW A logical. Default is \code{FALSE}, colored figure is generated
#' instead of gray scale one.
#' @param panel.name Color panel name (for brewer.pal function) that is used for symmetric color scales when the color scale is not given.
#' @param symbol_size A \code{numeric} with the size of the symbols.
#' @param ... Additional parameters to be passed to ImagePlotGG
#' @include imageplot_gg.R
#' @return An invisible R list object with extraction slots (CF, PV, cfall...)

extractNPA2 <- function(npall0, which = 1:length(npall0[[1]]$coefficients), prop.signif = 1e-09,
                        lev = 0.05, keep.all = TRUE, plot = TRUE, title = "Normalized NPAs", cex.txt = 1,
                        cex.labx = 1.6, cex.laby = 2, cex.facet = 2, cex.main = 2, names.arg = NULL,
                        col.text = "#4D75E8", okonly = FALSE, simplified.text = FALSE, normalize = TRUE,
                        BW = FALSE, panel.name = "YlOrRd", symbol_size=3, ...) {

  which0 <- which00 <- which
  if (length(which00) == 1) {
    which0 <- c(which0, which0)
  }
  cf <- sapply(npall0, function(np) np$coefficients[which0])
  ci <- sapply(npall0, function(np) np$ci.down[which0])
  ci[ci == 0] <- -1e-10
  pvs2 <- sapply(npall0, function(np) np$pvperm[2, which0, drop = FALSE])
  pvs3 <- sapply(npall0, function(np) np$pvperm[1, which0, drop = FALSE])
  pvs <- sapply(npall0, function(np) apply(np$pvperm[1:2, which0, drop = FALSE],
                                          2, function(x) max(x, na.rm = TRUE)))
  pvs.tmp <- pvs + 1e-10
  p0 <- apply(pvs.tmp * sign(ci), 2, function(x) length(x[x < lev & x >= 0])/length(x))
  if (keep.all == FALSE) {
    in0 <- which(p0 >= prop.signif)
    names(in0) <- NULL
    if (length(in0) == 0) {
      stop(paste("None of the network meet the criterias..."))
    }
  } else {
    in0 <- 1:length(p0)
  }
  NP <- npall0[in0]
  if (normalize == TRUE) {
    CF <- sapply(NP, function(np) np$coefficients[which0])
    CFsc <- apply(CF, 2, function(x) x/max(x, na.rm = TRUE))
  } else {
    CF <- sapply(NP, function(np0) {
      mx <- getMaxNPA(np0)
      l2norm.basckbonescores <- colSums(np0$nodes.coefficients[, which0]^2)
      max.npa <- mx$value/np0$networkSize * l2norm.basckbonescores
      return(np0$coefficients[which0]/max.npa)
    })
    CFsc <- CF
  }
  PV <- sapply(NP, function(np) apply(np$pvperm[1:2, which0, drop = FALSE], 2, max))
  CI <- sapply(NP, function(np) np$ci.down[which0])
  if (length(which00) == 1) {
    CF <- CF[1, , drop = FALSE]
    CFsc <- CFsc[1, , drop = FALSE]
    PV <- PV[1, , drop = FALSE]
    CI <- CI[1, , drop = FALSE]
  }

  if (simplified.text == FALSE) {
    T0 <- matrix(NA, nrow(PV), ncol(PV))
    T0[PV < lev & CI > 0] <- "15"
    T0[PV < lev & CI <= 0] <- "7"

    T0[PV >= lev & PV < 2 * lev & CI > 0] <- "1"
    T0[PV >= lev & PV < 2 * lev & CI <= 0] <- "13"

    symlab <- c("OK pv <0.05 & CI>0","OK pv <0.05 & CI<0",
               expression("OK 0.05"<="pv <0.1 & CI>0"),expression("OK 0.05"<="pv <0.1 & CI<0"))
    names(symlab) <- c("15", "7", "1", "13")

    symlab <- symlab[c(1,3,2,4)]

  } else {
    T0 <- matrix(NA, nrow(PV), ncol(PV))
    T0[PV < lev & CI >= 0] <- "8"
    symlab <- c("*O*K*")
    names(symlab) <- "8"

  }
  rownames(T0) <- rownames(CFsc)
  colnames(T0) <- colnames(CFsc)
  bin <- 0 * pvs
  bin[pvs < lev & ci > 0] <- 1
  if (okonly == TRUE) {
    CFsc[is.na(T0)] <- NA
  }
  if (length(in0) >= 1) {
    plotlist <- list(coef = t(CFsc), text = t(T0))
    if (!is.null(names.arg)) {
      if (length(names.arg) == nrow(CFsc)) {
        rownames(CFsc) <- names.arg
      } else {
        stop("names.arg must be of right length")
      }
    }
    if (plot == TRUE) {
      nets <- getNets(rownames(plotlist$coef))
      ImagePlotGG(plotlist$coef, textmat = plotlist$text, group = nets$nets,
                  col.group = nets$col0, cex.txt = cex.txt, cex.labx = cex.labx, cex.laby = cex.labx,
                  cex.facet = cex.facet, title = title, cex.main = cex.main, col.text = "grey40",
                  panel.name = panel.name, BW = BW, cluster = FALSE, useSymbols = TRUE,
                  symbol_labels = symlab,symbol_size = symbol_size,symbol_key_title = "Statistics",
                  key.title="NPA", ...)
    }
  } else {
    if (plot == TRUE) {
      plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n",
           yaxt = "n", cex = 0)
      text(0.5, 0.5, "No significant \n network...", cex = 2)
    }
  }
  if (length(which00) == 1) {
    CF <- CF[1, , drop = FALSE]
    PV <- PV[1, , drop = FALSE]
    pvs <- pvs[1, , drop = FALSE]
    cf <- cf[1, , drop = FALSE]
    ci <- ci[1, , drop = FALSE]
    bin <- bin[1, , drop = FALSE]
  }
  res <- list(CF = CF, PV = PV, in0 = in0, prop.signif = prop.signif, pvall = pvs,
             cfall = cf, ci = ci, oks = pvs[, in0], plotlist = plotlist, bin = bin)
  return(invisible(res))
}
