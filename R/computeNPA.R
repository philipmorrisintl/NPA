#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Compute NPA on the given comparisons (contrasts) and network model
#'
#' @param comparisons A list with each slots containing data.frame of genes `nodeLabel`, `foldChange` and `t` statistic
#' @param network_model A R6 class NPAModel object created by load_model function from NPAModel package
#' @param b An integer value. Number of resampling performed
#' @param verbose A logical. If TRUE, progress printed in the console
#'
#' @return A R6 class NPA object
#' @export
#'
compute_npa <- function(comparisons, network_model, b = 500, verbose = FALSE) {
  np <- computeNPA(comparisons, network_model$get_data(), b = b, verbose = verbose)
  return(NPA$new(np, network_model))
}


#' Compute NPA scoring for a given dataset and model
#'
#' @param idmap A R list object. Contains data.frame for each contrast slots.
#' @param model A R list object. Contains slot model with nodes and edges
#' data.frame
#' @param verbose A logical. Default is \code{TRUE}, messages are displayed in
#' the console
#' @importFrom NPAModels getLQ
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @return A R list object containing the scoring metrics of the network
#'
#'

computeNPA <- function(dL, model, verbose=FALSE, b=500) {
    alpha <- 0.95
    if (!"Qbackbone" %in% names(model)) {
      warning(paste0("Models have not been preprocessed, ",
                     "this induces a much longer runtime ",
                     "for NPA computation. You can refer to ",
                     "NPAModels package documentation in order",
                     "preprocess networks"))
      if (verbose) {
        message("Preprocessing network model...")
      }
      model <- NPAModels::getLQ(model)
    }
    Qbackbone <- model$Qbackbone
    L3invtL2 <- model$L3invtL2
    L2 <- model$L2
    L3 <- model$L3
    if (verbose) {
       message("Preparing data...")
    }
    length0 <- length(dL)
    if (length(dL) == 1) {
        dL <- c(dL, dL)
    }
    # Make sure gene are exp(*)
    dL <- lapply(dL, function(x) {

        x$nodeLabel <- paste0("exp(", x$nodeLabel, ")")
        return(x)
    })
    # Check each entry ordered the same
    nm <- lapply(dL, function(G) { G$nodeLabel })
    ok <- sapply(nm, function(n) {
        ok <- TRUE
        if (length(n) != length(nm[[1]])) {
            ok <- FALSE
        } else {
            ok <- all(n == nm[[1]])
        }
        return(ok)
    })
    if (!all(ok == TRUE)) {
        dL <- lapply(dL, function(X) {
            Y <- X[order(X$nodeLabel), ]
            return(Y)
        })
    }
    Y <- sapply(dL, function(X) X$foldChange)
    V <- sapply(dL, function(G) {
        s <- G$foldChange/G$t
        s[G$t == 0] <- 0
        return(s) })^2  #Variance
    if (length(dL) == 1) {
        Y <- matrix(Y, ncol = 1)
        V <- matrix(V, ncol = 1)
        B <- matrix(B, ncol = 1)
    }
    colnames(Y) <- colnames(V) <- names(dL)
    rownames(Y) <- rownames(V) <- toupper(dL[[1]]$nodeLabel)
    if (max(table(toupper(rownames(Y)))) > 1) {
        stop("Some gene symbols are apperaring several times in the data!")
    }
    # Keep only gene in model
    gene <- toupper(colnames(L3invtL2))
    Y <- Y[rownames(Y) %in% gene, ]
    V <- V[rownames(V) %in% gene, ]
    V[is.na(V)] <- 0
   # Removee genes in model that have no data
    if (length(gene[!gene %in% rownames(Y)]) > 0) {
        genetodel <- gene[!gene %in% rownames(Y)]  #gene in model but not in data
        L2 <- L2[!rownames(L2) %in% genetodel, ]
        L3invtL2 <- L3invtL2[, !colnames(L3invtL2) %in% genetodel]
    }
    if (!setequal(colnames(L3invtL2), rownames(Y))) {
        stop("Gene symbol not matching between FC and L3invtL2")
    }
    Y <- Y[match(colnames(L3invtL2), rownames(Y)), ]
    V <- V[match(colnames(L3invtL2), rownames(V)), ]
    if (!all(rownames(Y) == toupper(colnames(L3invtL2)))) {
        stop("rownames Y and L3invtL2 do not match..")
    }
    if (!all(rownames(Y) == toupper(rownames(L2)))) {
        stop("rownames Y and L2 do not match..")
    }
    Var.fhat <- vector("list", ncol(Y))
    names(Var.fhat) <- colnames(Y)
    if (verbose == TRUE) {
        message("Computing differential backbone values...")
    }
    rg <- -L3invtL2 %*% Y
    nodes <- vector("list")
    nodes$ci.up <- nodes$ci.down <- NULL
    nodes$coefficients <- rg
    NetSize <- model$NetSize
    if (!all(rownames(Qbackbone) == rownames(rg))) {
        stop("Problem with backbone names")
    }

    # Computation of scores
    if (verbose == TRUE) {
        message("Computing amplitudes of perturbation...")
    }
    amplitude <- rep(NA, ncol(Y))
    names(amplitude) <- colnames(Y)
    amplitude <- diag(t(rg) %*% (Qbackbone %*% rg))/NetSize
    VarNPA <- 0
    var.coef <- NULL
    Var.fhat <- vector("list", ncol(Y))
    names(Var.fhat) <- colnames(Y)
    ciup <- cidown <- VarNPA <- rep(NA, ncol(Y))
    # Compute variances
    if (verbose == TRUE) {
        message("Computing variance...")
    }
    for (k in 1:ncol(Y)) {
        vs <- t(matrix(rep(sqrt(V[, k]), nrow(L3invtL2)),
                       byrow = TRUE, ncol = ncol(L3invtL2)))
        tmp2 <- vs * t(L3invtL2)
        Var.fhat[[k]] <- crossprod(tmp2)
        Q0 <- Qbackbone
        V0 <- Var.fhat[[k]]
        Q0V0 <- Q0 %*% V0
        tr.QVQV <- sum(diag(Q0V0 %*% Q0V0))
        # t(mu) QVQ mu
        mu <- matrix(rg[, k], ncol = 1)
        tmp <- t(mu) %*% Q0V0 %*% Q0 %*% mu
        VarNPA[k] <- 2 * tr.QVQV + 4 * tmp
    }
    var.coef <- sapply(Var.fhat, function(x) diag(x))
    nodes$ci.up <- rg + qnorm(1 - (1 - alpha)/2) * sqrt(var.coef)
    nodes$ci.down <- rg - qnorm(1 - (1 - alpha)/2) * sqrt(var.coef)
    nodes$p.value <- pnorm(abs(rg)/sqrt(var.coef), lower.tail = FALSE)
    set.seed(2674)
    # Downstream reshuffling
    if (verbose == TRUE) {
        message("Computing downstream reshuffling...")
    }
    perm <- function(x) {
      sample(x, length(x), replace = FALSE)
    }

    permV0 <- lapply(1:b, function(i) {
        set.seed(i + 241)
        return(-L3invtL2 %*% (Y[perm(1:nrow(Y)), ]))
    })
    permV0npa <- sapply(permV0, function(X) apply(X, 2, function(x) t(x) %*%
        Qbackbone %*% x))/NetSize
    permV02 <- permV02npa <- NULL
    # Backbone permutations
    if (verbose == TRUE) {
        message("Computing backbone reshuffling...")
    }
    W <- t(L2) %*% Y
    permBackbonenpa <- sapply(model$QbL3inv.perm, function(ql) colSums((ql %*%
        W)^2))/NetSize
    permBackbone <- NULL
    pvDown <- 1 - apply(cbind(amplitude, permV0npa), 1, function(x) length(
        x[x < x[1]])/length(x[-1]))
    pvBackbone <- 1 - apply(cbind(amplitude, permBackbonenpa), 1,
        function(x) length(x[x < x[1]])/length(x[-1]))
    test <- list(pv = rbind(downstream = pvDown, backbone = pvBackbone),
        downstream = permV0npa, backbone = permBackbonenpa,
        nodes.coefficients.perm.down = permV0,
        nodes.coefficients.perm.backbone = permBackbone)
    c0 <- qnorm(1 - (1 - alpha)/2)
    np <- list(coefficients = amplitude, coefficients.var = VarNPA/(NetSize^2),
        ci.up = amplitude + c0 * sqrt(VarNPA)/NetSize,
        ci.down = amplitude - c0 * sqrt(VarNPA)/NetSize,
        nodes.coefficients = rg, nodes.coefficients.ci.up = nodes$ci.up,
        nodes.coefficients.ci.down = nodes$ci.down,
        nodes.coefficients.pvalue = nodes$p.value,
        model = model[which(names(model) %in% c(
            "model", "startNodeDown", "g"))], networkSize = NetSize,
            signEdges = model$sgn, edges = model$backbone, L3invL2 = L3invtL2,
            L3 = L3, L2 = L2, Y = Y, Qbackbone = Qbackbone, test = test,
            pvperm = test$pv)
    np$max <- getMaxNPA(np)
    if (length0 == 1) {
        np <- NPAsubset(np, 1)
    }
    if (verbose) {
        message("Done.")
    }
    return(np)
}


#' Compute MAX of NPA
#'
#' @param np0 A R list containing scoring results
#' @include utils.R
#'
#' @return A list with value and vector slots for NPA maximum computed.
#'
getMaxNPA <- function(np0)
{
  l3inv2 <- solve2(np0$L3)
  D <- l3inv2 %*% np0$Qbackbone %*% l3inv2
  rownames(D) <- rownames(np0$Qbackbone)
  colnames(D) <- colnames(np0$Qbackbone)
  D.sub <- D[rownames(D) %in% names(np0$model$startNodeDown),
             colnames(D) %in% names(np0$model$startNodeDown)]
  D.sub2 <- solve(l3inv2[rownames(D) %in% names(np0$model$startNodeDown),
                        colnames(D) %in% names(np0$model$startNodeDown)])
  D.sub <- D.sub2 %*% D.sub
  eg.Dsub <- eigen(D.sub)
  maxlambda <- max(eg.Dsub$values)
  maxf <- eg.Dsub$vectors[, which.max(eg.Dsub$values)]
  all(abs(D.sub %*% maxf - maxlambda * maxf) < 1e-12)
  names(maxf) <- rownames(D.sub)
  f.extended <- rep(0, nrow(D))
  names(f.extended) <- rownames(D)
  f.extended[match(names(maxf), names(f.extended))] <- maxf
  max.sol.constrained <- l3inv2 %*% f.extended
  max.npa <- t(max.sol.constrained) %*% np0$Qbackbone %*% max.sol.constrained
  out <- list(value = max.npa, vector = max.sol.constrained)
  return(out)
}
