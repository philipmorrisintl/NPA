#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Get contrasts nmames from NPA objects
#'
#' @param npa A R list object. NPA scores results or NPAll object
#' @return A string vector. Names of contrasts used for this NPA or NPAll
#' object
getNPAnames<-function(npa){
    if(is.list(npa[[1]])){npa=npa[[1]]}
    return(names(npa$coefficients))
}
