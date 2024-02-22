#' Estimates of mu from Normal-Inverse-Wishart (NIW) prior
#'
#' Estimates of mu from Normal-Inverse-Wishart (NIW) priors for example data
#' frame from
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html)
#' example
#'
#'
#' @format data frame containing 8,0000 rows and 7 variables
#'  \describe{
#'    \item{metric}{name of the metric extracted from `niw.post()`}
#'    \item{species}{species abbreviation}
#'    \item{sample_number}{sample number from 1-1000}
#'    \item{isotope}{column with isotope name}
#'    \item{mu_est}{estimate of mu produced from `niw.post()`}
#'    \item{element}{isotopic element used in labelling}
#'    \item{meutron}{neutron number used in labelling}
#' }
#'
"mu_est_long"


#' Estimates of sigma from Normal-Inverse-Wishart (NIW) prior
#'
#' Estimates of sigma from Normal-Inverse-Wishart (NIW) priors for example data
#' frame from
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html)
#' example
#'
#'
#' @format data frame containing 8,0000 rows and 7 variables
#'  \describe{
#'    \item{metric}{name of the metric extracted from `niw.post()`}
#'    \item{species}{species abbreviation}
#'    \item{isotope}{column with isotope name}
#'    \item{sample_number}{sample number from 1-1000}
#'    \item{d15n}{estimate of sigma for d15n produced from `niw.post()`}
#'    \item{d13c}{estimate of sigma for d13c produced from `niw.post()`}
#' }
#'
"sigma_est_wide"
