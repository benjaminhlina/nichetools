#' Create ellipses based on Bayesian estimates of \eqn{\mu} and \eqn{\Sigma}
#'
#' This function allows the user to supply Bayesian estimates of \eqn{\mu} and
#' \eqn{\Sigma} to create estimated Bayesian ellipse for niche region.
#'
#' @param dat_mu a `data.frame` containing \eqn{\mu} Bayesian estimates.
#' This `data.frame` needs to be in long format with each \eqn{\mu}
#' estimate for each isotope stacked on top of each other. This can be produced
#' using `extract_mu()`.
#' @param dat_sigma a `data.frame` containing \eqn{\Sigma} Bayesian estimates.
#' This `data.frame` needs be in wide format, that is \eqn{\Sigma} (covariance)
#' matrices stacked on top of each other. See example of how to convert to
#'  wide format. This can be produced using `extract_sigma()`.
#' @param p_ell is the confidence interval of each ellipse estimate.
#' Default is 0.95 (i.e., 95% confidence interval).
#' This value is bound by 0 and 1 and has to be a `numeric`.
#' @param isotope_a character string that is the column name of the first
#' isotope used in `dat_sigma`. Defaults to `"d13c"`.
#' @param isotope_b character string that is the column name of the second
#' isotope used in `dat_sigma`. Defaults to `"d15n"`.
#' @param random logical value indicating whether or not to randomly sample
#' posterior distributions for \eqn{\mu} and \eqn{\Sigma} to create a sub-sample
#' of ellipse. Default is `TRUE`.
#' @param set_seed numerical value to set seed for random sampling. Default is `4`.
#' @param n numerical value that controls the number of random samples.
#' Default is `10`.
#' @param message control whether the time processing is displayed after the
#' end of the function. Default is `TRUE`.
#'
#' @return A `tibble` containing, `sample_name`, `sample_number`, and the
#' isotopes that were used in the estimation of ellipse
#' (i.e.,  and `d13c` and `d15n`).
#'
#' @seealso [nicheROVER::niw.post()], [SIBER::siberMVN()], [extract_mu()],
#' and [extract_sigma()]
#' @examples
#' niche_ellipse(dat_mu = mu_est_long,
#'               dat_sigma = sigma_est_wide)
#' @import dplyr
#' @import ellipse
#' @import purrr
#' @importFrom rlang :=
#' @importFrom rlang sym
#' @importFrom rlang as_name
#' @import tibble
#' @import tidyr
#' @export


# ---- function ----
niche_ellipse <- function(
    dat_mu,
    dat_sigma,
    isotope_a = NULL,
    isotope_b = NULL,
    p_ell = NULL,
    random = NULL,
    set_seed = NULL,
    n = NULL,
    message = TRUE
) {
  # options(error = recover)
  start_time <- Sys.time()


  # Check if dat_mu is a data.frame
  if (!inherits(x = dat_mu, what = c("tbl_df", "tbl", "data.frame"))) {
    cli::cli_abort("Input 'dat_mu' must be class data.frame.")
  }
  # Check if dat_sigma is a data.frame
  if (!inherits(dat_sigma, what = c("tbl_df", "tbl", "data.frame"))) {
    cli::cli_abort("Input 'dat_sigma' must be class data.frame.")
  }


  if (is.null(isotope_a)) {
    isotope_a <- "d13c"
  }
  if (is.null(isotope_b)) {
    isotope_b <- "d15n"
  }
  # Validate if isotope_a is a character
  if (!is.character(isotope_a)) {
    cli::cli_abort("Argument 'isotope_a' must be a character.")
  }

  # Validate if isotope_b is a character
  if (!is.character(isotope_b)) {
    cli::cli_abort("Argument 'isotope_b' must be a character.")
  }


  if(is.null(p_ell)) {
    p_ell <- 0.95
  }
  # Additional parameter validation, if needed
  if (!is.null(p_ell)) {
    if (!is.numeric(p_ell) || p_ell < 0 || p_ell > 1) {
      cli::cli_abort("Parameter 'p_ell' must be a numeric value between 0 and 1.")
    }
  }

  if (is.null(random)) {
    random <- TRUE
  }

  if (!(random %in% c(TRUE, FALSE))) {

    cli::cli_abort("The 'random' is a logical that is TRUE or FALSE.")
  }


  # ---- put in random sample for 10 random samples

  if (random %in% TRUE) {
    if (is.null(set_seed)) {
      set_seed <- .Random.seed
    }
    if (!is.numeric(set_seed)) {
      cli::cli_abort("Argument 'set_seed' must be a numeric")
    }
    if (is.null(n)) {
      n <- 10
    }
    if (!is.numeric(n)) {
      cli::cli_abort("Argument 'n' must be a numeric")
    }
    set.seed(set_seed)
    sample_numbers <- sample(dat_mu$sample_number, n)
    # prepare mu for ellipse
    mu <- dat_mu |>
      dplyr::select(sample_name, sample_number, mu_est) |>
      filter(sample_number %in% sample_numbers) |>
      dplyr::group_split(sample_name, sample_number) |>
      purrr::map(~ .x$mu_est,
                 .progress = "Prepare mu for ellipse")

    # preppare sigama for epplipse
    isotope_a_sym <- rlang::sym(isotope_a)
    isotope_b_sym <- rlang::sym(isotope_b)
    #
    #
    # Erroring at isotope names fix will need make flexible qith {{{}}}
    sigma <- dat_sigma |>
      filter(sample_number %in% sample_numbers) |>
      dplyr::select(sample_name, sample_number, !!isotope_a_sym,
                    !!isotope_b_sym) |>
      dplyr::group_split(sample_name, sample_number) |>
      purrr::map(~ cbind(.x[[rlang::as_name(isotope_a_sym)]],
                         .x[[rlang::as_name(isotope_b_sym)]]) |>
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
      filter(sample_number %in% sample_numbers) |>
      dplyr::group_by(sample_name, sample_number) |>
      dplyr::group_keys() |>
      dplyr::mutate(sample_name_num = paste(sample_name, sample_number,
                                            sep = ":"))

    names(ellipse_dat) <- list_names$sample_name_num

    # bind and rename columns
    all_ellipses <- dplyr::bind_rows(ellipse_dat, .id = "ellipse_name") |>
      dplyr::rename(
        {{isotope_a}} := x,
        {{isotope_b}} := y
      ) |>
      tidyr::separate(ellipse_name,
                      into = c("sample_name", "sample_number"), sep = ":") |>
      dplyr::mutate(
        sample_number = as.numeric(sample_number)
      )


    end_time <- Sys.time()

    time_spent <- round((end_time - start_time), digits = 2)
    if (message) {
      cli::cli_alert(paste("Total time processing was", time_spent, units(time_spent),
                           sep = " "))
    }

    return(all_ellipses)
  }
  # ---- put in random sample for 10 random samples

  if (random %in% FALSE) {
    # prepare mu for ellipse
    mu <- dat_mu |>
      dplyr::select(sample_name, sample_number, mu_est) |>
      dplyr::group_split(sample_name, sample_number) |>
      purrr::map(~ .x$mu_est,
                 .progress = "Prepare mu for ellipse")

    # preppare sigama for epplipse
    #
    # Erroring at isotope names fix will need make flexible qith {{{}}}
    sigma <- dat_sigma |>
      dplyr::select(sample_name, sample_number, d13c, d15n) |>
      dplyr::group_split(sample_name, sample_number) |>
      purrr::map(~ cbind(.x$d13c, .x$d15n) |>
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
      dplyr::mutate(sample_name_num = paste(sample_name, sample_number,
                                            sep = ":"))

    names(ellipse_dat) <- list_names$sample_name_num

    # bind and rename columns
    all_ellipses <- dplyr::bind_rows(ellipse_dat, .id = "ellipse_name") |>
      dplyr::rename(
        {{isotope_a}} := x,
        {{isotope_b}} := y
      ) |>
      tidyr::separate(ellipse_name,
                      into = c("sample_name", "sample_number"), sep = ":") |>
      dplyr::mutate(
        sample_number = as.numeric(sample_number)
      )


    end_time <- Sys.time()

    time_spent <- round((end_time - start_time), digits = 2)
    if (message) {
      cli::cli_alert(paste("Total time processing was", time_spent, units(time_spent),
                           sep = " "))
    }

    return(all_ellipses)
  }

}
