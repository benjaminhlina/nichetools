#' A `data.frame` containing posterior estimates of \eqn{\mu}
#'
#' Posterior estimates of \eqn{\mu} using `fish` data set from
#' [{nicheROVER}](https://cran.r-project.org/package=nicheROVER),
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


#' A `data.frame` containing posterior estimates of \eqn{\Sigma}
#'
#' Posterior estimates of \eqn{\Sigma} using `fish` data set from
#' [{nicheROVER}](https://cran.r-project.org/package=nicheROVER),
#' using Normal-Inverse-Wishart (NIW) priors
#'
#'
#' @format `data.frame` containing 8,000 rows and 6 variables
#'  \describe{
#'    \item{metric}{name of the metric extracted from `niw.post()`}
#'    \item{species}{species abbreviation}
#'    \item{isotope}{column with isotope name}
#'    \item{sample_number}{sample number from 1-1000}
#'    \item{d15n}{estimate of \eqn{\Sigma} for d15n produced from `niw.post()`}
#'    \item{d13c}{estimate of \eqn{\Sigma} for d13c produced from `niw.post()`}
#' }
#'
"sigma_est_wide"

#' A `list` of the posterior estimates of \eqn{\mu} and \eqn{\Sigma} from `{nicheROVER}`
#'
#' Posterior estimates of \eqn{\mu} and \eqn{\Sigma} using the `fish` data set from
#' [{nicheROVER}](https://cran.r-project.org/package=nicheROVER),
#' using Normal-Inverse-Wishart (NIW) priors.
#' This `list` is produced  using the function `niw.post()` from
#' [{nicheROVER}](https://cran.r-project.org/package=nicheROVER).
#'
#'
#' @format A `list` with elements \eqn{\mu} and \eqn{\Sigma} of sizes c(nsamples, length(lambda))
#'  and c(dim(Psi)).
#'
"niw_fish_post"

#' A `data.frame` containing the estimates of percentage of overlap among groups
#'
#' Estimates of the percentage of overlap among example species used in
#' [{nicheROVER}](https://cran.r-project.org/package=nicheROVER).
#'
#' @format A `array`containing `matrices` of the percent overlap for each group
#' used in Bayesian estimates of \eqn{\mu} and \eqn{\Sigma} using
#' Normal-Inverse-Wishart (NIW) priors calculated in `niw.post()`.
#'
"over_stat"


#' A `list` of the posterior estimates of \eqn{\mu} and \eqn{\Sigma} from `{SIBER}`
#'
#' Posterior estimates of \eqn{\mu} and \eqn{\Sigma} using the `demo.siber.data`
#' data set from [{SIBER}](https://cran.r-project.org/package=SIBER).
#' This `list` is produced  using the function `siberMVN()` from
#' [{SIBER}](https://cran.r-project.org/package=SIBER).
#' @format A `list` with estimates of \eqn{\mu} and \eqn{\Sigma} for each species
#' and group.
"post_sam_siber"
