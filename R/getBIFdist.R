#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Compute distance based on L1-norm of leading node contributions
#'
#' @param npall A R list object. List of NPA objects for different networks.
#' @param which An integer vector. Indexes of contrasts to use.
#' @param names.arg A string vector. Alternative names for contrasts.
#' @param lev A numerical value. Levels at which the "OK: stats are significant, default value is 0.05. 
#' @param dist.only A logical. Only the distance matrix or the full set of results will be returned.
#' @param nets A string vector. Network family names to be used.
#' @param type A character vector. Type of BIF distance computed, default is `relative`.
#' @param ... Additional optional arguments to be passed. Not used yet.
#' @return A list object with computed distances on L1-norm, leading node contributions.
#'
getBIFdist <- function(npall, which = 1:length(npall[[1]]$coefficients), names.arg = names(npall[[1]]$coefficients),
    lev = 0.05, dist.only = FALSE, nets = factor(getNets(names(npall))$nets), type = c("relative",
        "absolute", "absoluteQ")[1], ...) {

    distMethod = c("manhattan", "euclidean", "euclidean")[match(type, c("relative", "absolute", "absoluteQ"))]
    p0 = c(1, 2, 2)[match(type, c("relative", "absolute", "absoluteQ"))]

    if (!all(sort(which) == c(1:length(npall[[1]]$coefficients)))) {
        npall <- lapply(npall, function(np) NPAsubset(np, which))
        names.arg <- names.arg[which]
    }
    sc0 <- C <- NULL
    npall <- NPAallrename(npall, names.arg)
    if (dist.only == FALSE) {
        bif <- getBIF(npall, ...)
        C <- getC(bif$BIF)
        sc0 <- lapply(npall, function(np) {
            w1 <- apply(np$pvperm[1:2, , drop = FALSE], 2, max)
            w <- 0 * w1 + 1
            w2 <- np$ci.down
            w[w1 > lev | w2 <= 0] <- 0
            np2 <- np$nodes.coefficients %*% diag(w, length(w), length(w))
            out <- t(np2) %*% np$Qbackbone %*% np2
            dd = diag(1/sqrt(diag(out)), length(diag(out)), length(diag(out)))
            out <- dd %*% out %*% dd
            out[is.na(out)] <- 0
            rownames(out) <- colnames(out) <- rownames(C)
            return(1 - out)
        })
    }

    if (type == "relative") {
        # Get contrib in %
        leContribs = lapply(npall, function(np) {
            ln = getNPALE(np, type = "nodes", plotit = FALSE)
            leContrib <- sapply(ln, function(l) l$x[order(names(l$x))])
            leContrib[, which(apply(np$pvperm, 2, max) > lev | np$ci.down <= 0)] <- 0
            return(leContrib)
        })
    } else if(type== "absolute") {
        # Multiply the contribution by NPAi/NPAmax
        # leContribs = lapply(npall, function(np) {
        #     ln = getNPALE(np, type = "nodes", plotit = FALSE)
        #     leContrib = sapply(ln, function(l) l$x[order(names(l$x))])
        #     leContrib[, which(apply(np$pvperm, 2, max) > lev | np$ci.down <= 0)] = 0
        #     res = leContrib %*% diag(np$coefficients, length(np$coefficients), length(np$coefficients))/max(np$coefficients)
        #     return(res)
        # })

        leContribs = lapply(npall, function(np){
            res =  np$nodes.coefficients %*% diag(np$coefficients, length(np$coefficients), length(np$coefficients))/max(np$coefficients)
            return(res)
        })

    } else if(type == "absoluteQ"){
        leContribs = lapply(npall, function(np){
            svQ=svd(np$Qbackbone)
            Qhalf = svQ$u%*% diag(sqrt(svQ$d))%*%t(svQ$v)
            res =  Qhalf %*% np$nodes.coefficients %*% diag(np$coefficients, length(np$coefficients), length(np$coefficients))/max(np$coefficients)
            return(res)
        })
    }

    leContribs <- lapply(leContribs, function(x) {
        y <- x
        colnames(y) <- gsub("\n", "", colnames(y), fixed = TRUE)
        return(y)
    })

    # An intermediate BIF for each BIG net nets = factor(getNets(names(npall))$nets)
    nets0 <- nets
    d00 <- x00 <- vector("list", nlevels(nets))
    names(d00) <- names(x00) <- levels(nets)
    for (k in 1:nlevels(nets)) {
        d00[[k]] <- as.matrix(dist(t(Reduce(rbind, leContribs[nets == levels(nets)[k]])), method = distMethod,
            diag = TRUE, upper = TRUE, p = p0))^p0

        rownames(d00[[k]]) <- colnames(d00[[k]]) <- names.arg
    }
    # if(!all(abs(Reduce('+',d00)-d0)<1e-12)){stop('Distance decomposition issue!')}
    d0 <- Reduce("+", d00)^ (1/p0)
    d00 <- lapply(d00, function(d) d^(1/p0))

    res <- list(dist = d0, alldist = d00, scalarproduct = sc0, level = lev, leContribs = leContribs,
        C = C)
    return(res)
}
