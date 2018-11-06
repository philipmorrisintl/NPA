#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Get leading nodes table
#'
#' @param np A R list object. NPA scores results
#' @return A matrix of character vector. Contains leading node rank, +/- sign
#' and statisctics for each node as row and each contrast as column
#'
getNPALEtable2 <- function(np) {
    len <- getNPALE(np, type = "nodes", plotit = FALSE)
    fortab <- NULL
    for (k in 1:length(len)) {
        y <- sort(len[[k]]$x, decreasing = TRUE)
        rk <- 1:length(y)
        names(rk) <- names(y)
        y2 <- sign(np$nodes.coefficients[, k])
        names(y2) <- rownames(np$nodes.coefficients)
        yci <- sign(np$nodes.coefficients.ci.up[, k]) * sign(np$nodes.coefficients.ci.down[,
            k])
        names(yci) <- rownames(np$nodes.coefficients)
        y <- y[order(names(y))]
        yci <- yci[order(names(yci))]
        rk <- rk[order(names(rk))]
        y2 <- y2[order(names(y2))]
        rk <- sapply(rk, function(x) {
            if (nchar(x) < max(nchar(rk))) {
                y <- paste(paste(rep("0", max(nchar(rk)) - nchar(x)), collapse = ""),
                  x, sep = "")
            } else {
                y <- x
            }
            return(y)
        })
        ln0 <- rep(" ", length(y))
        names(ln0) <- names(y)
        if (!all(y == 0)) {
            ln0[names(y) %in% len[[k]]$leadingNodes[, 1]] <- "*"
            ln0[yci < 1] <- paste(ln0[yci < 1], "!", sep = "")  #not signif 95% confidence interval
        }
        if (!all(names(y) == names(y2))) {
            stop("Names do not match!")
        }
        tmp <- data.frame(ID = names(y), Rank = rk, LN = ln0, Sign = paste(" (",
            c("+", "-")[unclass(factor(y2, levels = c("1", "-1")))], ") ", sep = ""),
            Contribution = paste(round(100 * y, 2), "%", sep = ""))
        tmp$label <- apply(as.matrix(tmp)[, -1], 1, function(x) paste(x, collapse = ""))
        tmp$Contrast <- rep(names(len)[k], nrow(tmp))
        fortab <- rbind(fortab, tmp)
    }
    tab <- tapply(fortab$label, list(fortab$ID, fortab$Contrast), function(x) x)
    ord <- order(tapply(fortab$Rank, list(fortab$ID, fortab$Contrast), function(x) x)[,
        1])
    tab <- tab[ord, , drop = FALSE]
    tab <- tab[, match(names(np$coefficients), colnames(tab)), drop = FALSE]
    if (!all(colnames(tab) == names(np$coefficients))) {
        stop("Issue with leading node table column names")
    }
    return(tab)
}
