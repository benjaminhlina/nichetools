#' A `data.frame` containing posterior estimates of μ
#'
#' Posterior estimates of μ using `fish` data set from
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html),
#' using  Normal-Inverse-Wishart (NIW) priors.

#' @format `data.frame` containing 8,000 rows and 7 variables
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


#' A `data.frame` containing posterior estimates of Σ
#'
#' Posterior estimates of Σ using `fish` data set from
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html),
#' using Normal-Inverse-Wishart (NIW) priors
#'
#'
#' @format `data.frame` containing 8,000 rows and 6 variables
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

#' A `list` of the posterior estimates of μ and Σ
#'
#' Posterior estimates of μ and Σ using the `fish` data set from
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html),
#' using Normal-Inverse-Wishart (NIW) priors.
#' This `list` is produced  using the function `niw.post()` from
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#'
#'
#' @format A `list` with elements μ and Σ of sizes c(nsamples, length(lambda))
#'  and c(dim(Psi).
#'
"niw_fish_post"

#' A `data.frame` containing the estimates of percentage of overlap among groups
#'
#' Estimates of the percentage of overlap among example species used in
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#'
#' @format A `array`containing `matrices` of the percent overlap for each group
#' used in Bayesian estimates of μ and Σ using
#' Normal-Inverse-Wishart (NIW) priors calculated in `niw.post()`.
#'
"over_stat"
