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
#' @param p_ell is the confidence interval of each ellipse estimate.
#' Default is 0.95 (i.e., 95% confidence interval).
#' @param isotope_a character string that is the column name of the first
#' isotope used in `dat_sigma`. Defaults to `"d15n"`.
#' @param isotope_b character string that is the column name of the second
#' isotope used in `dat_sigma`. Defaults to `"  d13c"`.
#' @param message Control whether the time processing is displayed after the
#' end of the function. Default is `TRUE`.
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
niche_ellipse <- function(
    dat_mu,
    dat_sigma,
    isotope_a = NULL,
    isotope_b = NULL,
    p_ell = NULL
) {
  # options(error = recover)
  start_time <- Sys.time()

  # fix nameing if not supplid
  if(!any(names(dat_sigma) %in% c("d15n",
                                  "d13c"))) {
      # Check if isotope_a and isotope_b are specified
      if (!is.null(isotope_a) && !is.null(isotope_b)) {
        dat_sigma$d15n <- dat_sigma[[isotope_a]]
        dat_sigma[[isotope_a]] <- NULL

        dat_sigma$d13c <- dat_sigma[[isotope_b]]
        dat_sigma[[isotope_b]] <- NULL
      } else {
        # Handle the case where isotope_a or isotope_b is not specified
        cli::cli_abort("Both 'isotope_a' and 'isotope_b' must be specified when
                       dat_sigma 'd15n' and 'd13c' are named differently.")
      }
    }
#   if(!any(names(dat_mu) %in% c("sample_number",
#                                "sample_name",
#                                "mu_est",
#                                "cal_d15n",
#                                "cal_d13c"))) {
#
#     dat_mu$sample_name <- dat_mu[[name]]
#     dat_mu[[name]] <- NULL
#
#     dat_mu$sample_number <- dat_mu[[number]]
#     dat_mu[[number]] <- NULL
#
#     dat_mu$cal_d15n <- dat_mu[[isotope_a]]
#     dat_mu[[isotope_a]] <- NULL
#
#     dat_mu$cal_d13c <- dat_mu[[isotope_b]]
#     dat_mu[[isotope_b]] <- NULL
#
#     dat_mu$mu_est <- dat_mu[[mu_name]]
#     dat_mu[[mu_name]] <- NULL
#
#   } else {
#     # Handle the case where 'name' or 'number' is not defined
#     stop("Variables 'name' and 'number' must be defined.")
#   }
  # set p ellipse
  if(is.null(p_ell)) {
    p_ell <- 0.95
  }
  # Additional parameter validation, if needed
  if (!is.null(p_ell)) {
    if (!is.numeric(p_ell) || p_ell < 0 || p_ell > 1) {
      cli::cli_abort("Parameter '{.var}' must be a numeric value between 0 and 1.")
    }
  }
  # prepare mu for ellipse
  mu <- dat_mu |>
    dplyr::select(sample_name, sample_number, mu_est) |>
    dplyr::group_split(sample_name, sample_number) |>
    map(~ .x$mu_est,
        .progress = "Prepare mu for ellipse")

  # preppare sigama for epplipse
  #
  # Erroring at isotope names fix
  sigma <- dat_sigma |>
    dplyr::select(sample_name, sample_number, d15n, d13c) |>
    dplyr::group_split(sample_name, sample_number) |>
    purrr::map(~ cbind(.x$d15n, .x$d13c) |>
                 as.matrix(2, 2), .progress = "Prepare sigma for ellipse"
    )


  # create ellipses
  ellipse_dat <- purrr::pmap(list(sigma, mu), function(first, second)
    ellipse::ellipse(x = first,
                     centre = second,
                     which = c(1, 2),
                     level = p_ell),
    .progress = "Create ellipses")

  # Converting ellipse estimates into tibble
  ellipse_dat <- ellipse_dat |>
    purrr::map(~ .x |>
                 tibble::as_tibble(),
               .progress = "Converting ellipse estimates into tibble"
    )
  # create names to join name each ellimate of the list
  list_names <- dat_sigma |>
    dplyr::group_by(sample_name, sample_number) |>
    dplyr::group_keys() |>
    dplyr::mutate(sample_name_num = paste(sample_name, sample_number, sep = ":"))

  names(ellipse_dat) <- list_names$sample_name_num

  # bind and rename columns
  all_ellipses <- dplyr::bind_rows(ellipse_dat, .id = "ellipse_name") |>
    dplyr::rename(
      isotope_a = x,
      isotope_b = y
    ) |>
    tidyr::separate(ellipse_name,
                    into = c("sample_name", "sample_number"), sep = ":") |>
    dplyr::mutate(
      sample_number = as.numeric(sample_number)
    )


  end_time <- Sys.time()

  time_spent <- round((end_time - start_time), digits = 2)

  message("Total time processing was ", time_spent, " mins")

  return(all_ellipses)

}
