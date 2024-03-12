#' Estimates of μ from Normal-Inverse-Wishart (NIW) prior
#'
#' Estimates of μ from Normal-Inverse-Wishart (NIW) priors for example data
#' frame from
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#'
#' @format data frame containing 8,000 rows and 7 variables
#'  \describe{
#'    \item{metric}{name of the metric extracted from `niw.post()`}
#'    \item{species}{species abbreviation}
#'    \item{sample_number}{sample number from 1-1000}
#'    \item{isotope}{column with isotope name}
#'    \item{mu_est}{estimate of mu produced from `niw.post()`}
#'    \item{element}{isotopic element used in labelling}
#'    \item{neutron}{neutron number used in labelling}
#' }
#'
"mu_est_long"


#' Estimates of Σ from Normal-Inverse-Wishart (NIW) prior
#'
#' Estimates of Σ from Normal-Inverse-Wishart (NIW) priors for example data
#' frame from
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#'
#' @format data frame containing 8,000 rows and 6 variables
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

#' Estimates of μ and Σ from Normal-Inverse-Wishart (NIW) prior as `list`
#'
#' Estimates of μand Σ from Normal-Inverse-Wishart (NIW) priors for example data.
#' This `list` is produced using using the function `niw.post()` from
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#'
#'
#' @format A list with elements μ and Σ of sizes c(nsamples, length(lambda))
#'  and c(dim(Psi).
#'
"niw_fish_post"
