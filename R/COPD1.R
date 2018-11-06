#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Comparisons dataset example
#'
#' This dataset comes from a study published in ArrayExpress [E-MTAB-2756]
#' This study was designed toidentify the onset of emphysema induced by exposure to cigarette
#' smoke. The mice were exposed to mainstream cigarette smoke from the Reference Cigarette 3R4F
#' through whole body exposure for up to 7 months. Additionaly, three cessation scenarios were
#' included to assess the impact of smoking cessation on the emphysema progression on C57BL/6 mice.

#'
#' @format A list object of 6 comparisons, each slots contain a dataframe that gives
#' for each gene its label (nodeLabel), its fold change value (foldChange) and
#' its `t` statistic value:
#' \describe{
#'   \item{3R4F-m5}{5 months exposure to 3R4F reference cigarette}
#'   \item{3R4F-m7}{7 months exposure to 3R4F reference cigarette}
#'   \item{Cessation2-m3}{}
#'   ...
#' }
"COPD1"