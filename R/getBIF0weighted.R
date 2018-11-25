#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Subfunction fo getBIF
#'
#' @param npall0 A R list object. List of NPA objects for different networks
#' @param which An integer vector. Indexes of contrasts to use
#' @param relativeto An integer value. Iindex of the NPA used as reference 
#' @param group.relativeMAX factor, for selecting comparisons used as reference
#' @param lev A numerical value. Levels at which the "OK: stats are significant, default value is 0.05.
#' @param refs Not used.
#' @param nets A string vector. Network family names to be used
#' @param ... Addtional optional parameters to be passed to function
#' @return A list with relative bif values, network contributions, bif value per comparisons.
#' @include NPAsubset.R


getBIF0weighted <- function(npall0, which = 1:length(npall0[[1]]$coefficients), relativeto = NULL,
    group.relativeMAX = NULL, lev = 0.05, refs = NULL, nets = NULL, ...) {
    flagN <- 0
    if (length(which) == 1) {
        which <- c(which, which)
        flagN <- 1
    }
    if (!setequal(which, 1:length(npall0[[1]]$coefficients))) {
        npall0 <- lapply(npall0, function(np) NPAsubset(np, which))
    }
    if (is.null(nets)) {
        nets <- getNets(names(npall0))$nets
    } else {
        nets <- factor(nets)
    }
    # Get all edges and their source and target backbone diff values
    E <- Reduce(rbind, lapply(npall0, function(np) np$edges))
    np <- npall0[[1]]
    k <- 0

    e1 <- Reduce(rbind, lapply(npall0, function(np) {
        k <<- k + 1
        w0 <- apply(np$pvperm[1:2, , drop = FALSE], 2, max)
        w2 <- np$ci.down
        w <- 0 * w0 + 1
        w[w0 >= lev | w2 <= 0] <- 0
        np2 <- np$nodes.coefficients[, , drop = FALSE] %*% diag(w, nrow = length(w),
            ncol = length(w))
        colnames(np2) <- colnames(np$nodes.coefficients)
        rnm <- rownames(np2)
        np2 <- data.frame(Network = rep(nets[k], nrow(np2)), Subnetwork = rep(names(npall0)[k],
            nrow(np2)), np2, check.names = FALSE)
        rownames(np2) <- rnm
        out <- cbind(np2[match(np$edges[, 1], rownames(np2)), ], np2[match(np$edges[,
            3], rownames(np2)), -1])
        return(out)
    }))
    E <- cbind(E, e1)
    wE <- t(apply(E[, c(1, 2, 3)], 1, sort))
    t0 <- table(paste(wE[, 1], wE[, 2], wE[, 3], sep = "////"))
    t1 <- table(nets)
    w1 <- 1/sqrt(t0[match(paste(wE[, 1], wE[, 2], wE[, 3], sep = "////"), names(t0))])  #e3dge coooc weighting, will be squared
    # w2 = 1/sqrt(maxNPA[match(E$Subnetwork, names(maxNPA))])#maxNPA downweight
    w3 <- 1/(t1[match(E$Network, names(t1))])  #1/number of subnet in net i will not be squared
    # Get solution for maximum NPA in the sense defined i getMaxNPA
    name.max <- "maxNPA3141_flmartin"
    k <- 0
    e2 <- Reduce(rbind, lapply(npall0, function(np0) {
        k <<- k + 1
        # l2=abs(np$L2) l3_2= - abs(np$L3) diag(l3_2)=diag(np$L3) l3inv=solve2(l3_2)
        # np2=(l3inv%*%t(l2))%*%rep(1,nrow(l2))
        np2 <- np0$max$vector
        colnames(np2) <- name.max  #name.max
        rnm <- rownames(np2)
        np2 <- data.frame(Network = rep(nets[k], nrow(np2)), Subnetwork = rep(names(npall0)[k],
            nrow(np2)), np2, check.names = FALSE)
        rownames(np2) <- rnm
        out <- cbind(np2[match(np0$edges[, 1], rownames(np2)), ], np2[match(np0$edges[,
            3], rownames(np2)), -1])
        return(out)
    }))
    # MAx NPA wieghts for each edge #Computes (f(x)+sign(x->y)%f(Y))^ 2 for f=f_max
    # (solution of max NPA) as well
    e2 <- cbind(E, e2)
    bifs.maxNPA <- (e2[, which(colnames(e2) == name.max)[1]] + as.numeric(e2[, "Direction"]) *
        e2[, which(colnames(e2) == name.max)[2]])^2
    bifs.maxNPA.weighted <- (e2[, which(colnames(e2) == name.max)[1]] + as.numeric(e2[,
        "Direction"]) * e2[, which(colnames(e2) == name.max)[2]])^2 * w1^2
    norm.network <- tapply(bifs.maxNPA.weighted, e2$Subnetwork, sum)
    w4 <- 1/sqrt(norm.network[match(E$Subnetwork, names(norm.network))])  # WeightedMAxNPA downweight, will be squared
    w0 <- as.vector(w1 * w4)
    E <- cbind(weight = w0, E)
    # Compute edges scores
    nm <- getNPAnames(npall0)
    forBIF <- E[, !colnames(E) %in% nm]
    bifs <- edges.scores <- NULL
    for (k in 1:length(nm)) {
        bifs <- cbind(bifs, (E[, which(colnames(E) == nm[k])[1]] + as.numeric(E[,
            "Direction"]) * E[, which(colnames(E) == nm[k])[2]])^2 * E[, "weight"]^2)
        edges.scores <- cbind(edges.scores, (E[, which(colnames(E) == nm[k])[1]] +
            as.numeric(E[, "Direction"]) * E[, which(colnames(E) == nm[k])[2]]) *
            E[, "weight"])
    }
    colnames(bifs) <- colnames(edges.scores) <- nm
    netBIF <- forBIF$Network
    #table(netBIF)
    # Summarize edges scores into bif and r2
    bifnet <- matrix(NA, nrow = nlevels(netBIF), ncol = length(nm))
    colnames(bifnet) <- colnames(bifs)
    rownames(bifnet) <- levels(netBIF)
    for (k in 1:nlevels(netBIF)) {
        bifnet[k, ] <- apply(bifs[netBIF == levels(netBIF)[k], , drop = FALSE], 2,
            function(y) sum(y * w3[netBIF == levels(netBIF)[k]]))
    }
    bif <- colSums(bifnet)
    # Get reference contrasts
    if (is.null(relativeto)) {
        if (is.null(group.relativeMAX)) {
            group.relativeMAX <- factor(rep("A", length(npall0[[1]]$coefficients)))
        } else {
            group.relativeMAX <- factor(group.relativeMAX)
        }
        relativeto <- rep(1, length(npall0[[1]]$coefficients))
        for (k in 1:nlevels(group.relativeMAX)) {
            tmp <- 0 * bif
            tmp[group.relativeMAX == levels(group.relativeMAX)[k]] <- bif[group.relativeMAX ==
                levels(group.relativeMAX)[k]]
            relativeto[group.relativeMAX == levels(group.relativeMAX)[k]] <- rep(which.max(tmp),
                length(bif[group.relativeMAX == levels(group.relativeMAX)[k]]))
        }
    }
    contrib <- bifnet %*% diag(1/bif, length(bif), length(bif))
    contrib[is.na(contrib)] <- 0
    colnames(contrib) <- nm
    bifrel=bif[relativeto]
    if(min(bifrel)==0){
        bifrel[bifrel==0]=1 #bifrel is the max anyway
        warning("Min BIF REF is 0!!!")
    }
    rbif <- sqrt(bif/bifrel)
    r2 <- diag(t(edges.scores) %*% edges.scores[, relativeto, drop = FALSE])/sqrt(colSums(edges.scores^2) *
        colSums(edges.scores[, relativeto, drop = FALSE]^2))
    net0 <- levels(netBIF)
    names(net0) <- net0
    r2net <- sapply(net0, function(n) diag(t(edges.scores[netBIF == n, , drop = FALSE]) %*%
        edges.scores[netBIF == n, relativeto, drop = FALSE])/sqrt(colSums(edges.scores[netBIF ==
        n, , drop = FALSE]^2) * colSums(edges.scores[netBIF == n, relativeto, drop = FALSE]^2)))
    r2 <- cbind(BIF = r2, r2net)
    r2[is.na(r2)] <- 0
    rbif <- cbind(BIF = rbif, t(bifnet))
    # rbif[,1]=rowSums(rbif[,-1]) r2[,1]=rowSums(r2[,-1])
    if (flagN == 1) {
        contrib <- contrib[, 1, drop = FALSE]
        rbif <- rbif[1, , drop = FALSE]
        r2 <- r2[1, , drop = FALSE]
    }
    return(list(rbif = rbif, r2 = as.matrix(r2), contrib = contrib, relativeto = relativeto,
        e2 = e2))
}
