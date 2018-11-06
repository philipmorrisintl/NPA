#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

.onLoad <- function(libname, pkgname) {
    # For S4 class dispathching...
    methods::setOldClass(c("NPA", "R6"))
}