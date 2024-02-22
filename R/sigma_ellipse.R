#' Create ellipses based on Bayesian estimates
#'
#' This function allows the user to supply Bayesian estimates of mu and sigma to
#' create estimated Bayesian ellipse for niche region.
#'
#' @param dat_mu a data frame containing mu Bayesian estimates. This data frame
#' needs to be in long format with each mu estimate for each isotope stacked
#' on top of each each other.
#' @param dat_sigma a data frame containing sigma Bayesian estimates.
#' This data frame needs be in wide format that is sigma/covariance matrices stacked
#' onto of each other.
#' @param name character string that is the column name of each grouping used
#' (e.g, species) to create mu and sigma Bayesian estimates. This name is the same for
#' `dat_mu`and `dat_sigma`. Defaults to `"sample_name"`.
#' @param number character string that is the column name of each Bayesian estimate.
#' This name is the same for `dat_mu`and `dat_sigma`. Defaults to `"sample_number"`.
#' @param mu_name character string that is the column name of the mu estimate in
#' `dat_mu`. Defaults to `"mu_est"`.
#' @param isotope_a character string that is the column name of the first isotope.
#' This name is the same for `dat_mu`and `dat_sigma`. Defaults to `"cal_d15n"`.
#' @param isotope_b character string that is the column name of the first isotope.
#' This name is the same for `dat_mu`and `dat_sigma`. Defaults to `"cal_d13c"`.
#' @param p_ell is the confidence interval of each ellipse estimate.
#' Default is 0.95 (i.e., 95% confidence interval).
#'
#' @return a data frame containing 100 estimates for each `sample_number` of each
#' `sample_name`. As the function runs, it will produce progress bars and tell
#' the user what step it is on. After the last step it will complete a few more
#' processes and then return the total time the function ran.
#'
#' @examples
#' # ellipse <- sigma_ellipse()
#'
#' @import dplyr
#' @import ellipse
#' @import purrr
#' @import tibble
#' @import tidyr
#' @export


# ---- function ----
sigma_ellipse <- function(
    dat_mu,
    dat_sigma,
    name = "sample_name",
    number = "sample_number",
    mu_name = "mu_est",
    isotope_a = "cal_d15n",
    isotope_b ="cal_d13c",
    p_ell = NULL
) {
  start_time <- Sys.time()

  # # fix nameing if not supplid

  if(!any(names(dat_sigma) %in% c("sample_number",
                                  "sample_name",
                                  "cal_d15n",
                                  "cal_d13c"))) {
    start_time <- Sys.time()

    dat_sigma$sample_name <- dat_sigma[[name]]
    dat_sigma[[name]] <- NULL

    dat_sigma$sample_number <- dat_sigma[[number]]
    dat_sigma[[number]] <- NULL

    dat_sigma$cal_d15n <- dat_sigma[[isotope_a]]
    dat_sigma[[isotope_a]] <- NULL

    dat_sigma$cal_d13c <- dat_sigma[[isotope_b]]
    dat_sigma[[isotope_b]] <- NULL

  }
  if(!any(names(dat_mu) %in% c("sample_number",
                               "sample_name",
                               "mu_est",
                               "cal_d15n",
                               "cal_d13c"))) {

    dat_mu$sample_name <- dat_mu[[name]]
    dat_mu[[name]] <- NULL

    dat_mu$sample_number <- dat_mu[[number]]
    dat_mu[[number]] <- NULL

    dat_mu$cal_d15n <- dat_mu[[isotope_a]]
    dat_mu[[isotope_a]] <- NULL

    dat_mu$cal_d13c <- dat_mu[[isotope_b]]
    dat_mu[[isotope_b]] <- NULL

    dat_mu$mu_est <- dat_mu[[mu_name]]
    dat_mu[[mu_name]] <- NULL

  }
  # set p ellipse
  if(is.null(p_ell)) {
    p_ell <- 0.95
  }

  # prepare mu for ellipse
  mu <- dat_mu %>%
    dplyr::group_split(sample_name, sample_number) %>%
    purrr::map(~ .x %>%
                 dplyr::select(mu_est) %>%
                 .$mu_est, .progress = "Prepare mu for ellipse"
    )

  # preppare sigama for epplipse
  sigma <- dat_sigma %>%
    dplyr::group_split(sample_name, sample_number) %>%
    purrr::map(~ .x %>%
                 dplyr::select(cal_d15n, cal_d13c) %>%
                 as.matrix(2, 2, dimnames = c("cal_d15n", "cal_d13c"),
                 ), .progress = "Prepare sigma for ellipse"
    )


  # create ellipse
  ellipse_dat <- purrr::pmap(list(sigma, mu), function(first, second)
    ellipse::ellipse(x = first,
                     centre = second,
                     which = c(1, 2),
                     level = p_ell),
    .progress = "Create ellipses")

  # Converting ellipse estimates into tibble
  ellipse_dat <- ellipse_dat %>%
    purrr::map(~ .x %>%
                 tibble::as_tibble(),
               .progress = "Converting ellipse estimates into tibble"
    )
  # create names to join name each ellimate of the list
  list_names <- dat_sigma %>%
    dplyr::group_by(sample_name, sample_number) %>%
    dplyr::group_keys() %>%
    dplyr::mutate(sample_name_num = paste(sample_name, sample_number, sep = ":"))

  names(ellipse_dat) <- list_names$sample_name_num

  # bind and rename columns
  all_ellipses <- dplyr::bind_rows(ellipse_dat, .id = "ellipse_name") %>%
    dplyr::rename(
      isotope_a = x,
      isotope_b = y
    ) %>%
    tidyr::separate(ellipse_name,
                    into = c("sample_name", "sample_number"), sep = ":") %>%
    dplyr::mutate(
      sample_number = as.numeric(sample_number)
    )


  end_time <- Sys.time()

  time_spent <- round((end_time - start_time), digits = 2)

  message("Total time processing was ", time_spent, " mins")

  return(all_ellipses)

}
