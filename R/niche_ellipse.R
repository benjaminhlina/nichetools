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
#' @param isotope_n a `numeric` either `2` or `3` that is the number of isotopes
#' used in the analysis. Will default to `2`.
#' @param isotope_names is a vector of `character` string used change the column name
#' of isotopes used in the analysis. Defaults to `c("d13c", "d15n")`.
#' @param random logical value indicating whether or not to randomly sample
#' posterior distributions for \eqn{\mu} and \eqn{\Sigma} to create a sub-sample
#' of ellipse. Default is `TRUE`.
#' @param set_seed numerical value to set seed for random sampling. Default is
#' a random value. To consistently sample the same subsample, please supply
#' a numerical value (e.g., `4`). It is highly suggested to use set_seed to make
#' the function results when randomly sampling reproducible.
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
#' @importFrom stats runif
#' @import tibble
#' @import tidyr
#' @import tidyselect
#' @export


# ---- function ----
niche_ellipse <- function(
    dat_mu,
    dat_sigma,
    isotope_n = NULL,
    isotope_names = NULL,
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
  # create name vector that will be used to id isotopes.
  if (is.null(isotope_n)) {
    isotope_n <- 2
  }
  if (!is.numeric(isotope_n) || !(isotope_n %in% c(2, 3))) {
    cli::cli_abort("Argument 'isotope_n' must be a numeric value and either 2 or 3.")
  }

  if (isotope_n == 2) {
    if (is.null(isotope_names)) {
      isotope_names <- c("d13c", "d15n")
    }
    if (!is.character(isotope_names)) {
      cli::cli_abort("Argument 'isotope_names' must be a character.")
    }
  }
  if (isotope_n == 3) {
    if (is.null(isotope_names)) {
      isotope_names <- c("d13c", "d15n", "d34s")
    }
  }


  if (is.null(p_ell)) {
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

  if (!(is.logical(random) && random %in% c(TRUE, FALSE))) {

    cli::cli_abort("Argument 'random' is a logical that is TRUE or FALSE.")
  }


  # ---- put in random sample for 10 random samples

  if (random %in% TRUE) {
    if (is.null(set_seed)) {
      set_seed <- round(runif(1) * .Machine$integer.max)
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

    # set seed to only work locally within function not globally...clever
    # on.exit({.Random.seed <<- set_seed})

    set.seed(set_seed)

    if (isotope_n == 2) {
      sample_numbers <- sample(dat_mu$sample_number, n)

      isotope_a <- isotope_names[1]
      isotope_b <- isotope_names[2]
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
    if (isotope_n == 3) {

      # create sample numbers
      sample_numbers <- sample(dat_mu$sample_number, n)

      # ---- create every combo in table ----
      iso_combo <- tidyr::expand_grid(iso_a = isotope_names,
                                      iso_b = isotope_names) |>
        dplyr::filter(iso_a < iso_b) |>
        dplyr::mutate(
          iso_ab = paste(iso_a, iso_b, sep = "_")
        ) |>
        dplyr::distinct(iso_ab) |>
        tidyr::separate_wider_delim(iso_ab, names = c("iso_a", "iso_b"),
                                    delim = "_")

      # created id column
      iso_combo$id <- 1:nrow(iso_combo)

      # pivot longer to create vector that  can be used to filter combinations
      split_iso <- split(iso_combo, iso_combo$id) |>
        purrr::map(~ tidyr::pivot_longer(.x, cols = -id,
                                         values_to = "iso_name") |>
                     getElement("iso_name")
        )
      # ----- mu ----

      # select columns we need
      mu_select <- df_mu |>
        dplyr::select(sample_name, sample_number, isotope, mu_est) |>
        filter(sample_number %in% sample_numbers)

      # split into three different dataframes based on the iso combos
      mu <- split_iso |>
        purrr::map(~ mu_select |>
                     dplyr::filter(isotope %in% .x) |>
                     dplyr::group_split(sample_name, sample_number)
        )

      # extract the two est mu for each sample for each combo
      mu_2 <- mu |>
        purrr::map(~ purrr::map(.x, ~ .x$mu_est,
                                .progress = "Prepare mu for ellipse")
        )

      # ---- sigma -----

      # transform to long as it is easier to keep the format
      # required for the function
      # to be wide
      sigma_long <- df_sigma |>
        tidyr::pivot_longer(cols = -c(metric, sample_name, sample_number,
                                      isotope),
                            names_to = "id",
                            values_to = "est") |>
        dplyr::filter(sample_number %in% sample_numbers)

      # filter based on split and then split into each group
      sigma <- split_iso |>
        purrr::map(~ sigma_long |>
                     dplyr::filter(id %in% .x &
                                     isotope %in% .x) |>
                     tidyr::pivot_wider(names_from = "id",
                                        values_from = "est") |>
                     dplyr::group_split(sample_name, sample_number)
        )

      # grab sample name and number
      group_names <- split_iso |>
        purrr::map(~ sigma_long |>
                     dplyr::filter(id %in% .x &
                                     isotope %in% .x) |>
                     pivot_wider(names_from = "id",
                                 values_from = "est") |>
                     dplyr::group_by(sample_name, sample_number) |>
                     dplyr::group_keys() |>
                     dplyr::mutate(
                       id =  dplyr::row_number() |>
                         as.character()
                     )
        )

      # empty lists to dump mattrix
      sigma_list <- list()

      sm_list <- list()

      # for loop that converts every variance and covariance combo into
      # matricies for ellipse
      for (i in 1:length(sigma)) {

        st <- sigma[[i]]

        for (k in 1:length(st)) {

          sm <- st[[k]] |>
            dplyr:: select(tidyselect::any_of(split_iso[[i]])) |>
            as.matrix()

          sm_list[[k]] <- sm
        }
        sigma_list[[i]] <- sm_list
      }

      # ----- make ellipses -----

      ell_map <- map2(
        .x = sigma_list,
        .y = mu_2,
        .f = map2,
        function(x, y) ellipse::ellipse(x = x,
                                        centre = y,
                                        which = c(1, 2),
                                        level = p_ell) |>
          tibble::as_tibble(),
        .progress = "Create ellipses"
      )

      # ---- add in sample names ----
      ell_sam <- map2(
        .x = ell_map,
        .y = group_names,
        .f = function(x, y) x |>
          dplyr::bind_rows(.id = "id") |>
          dplyr::left_join(y, by = "id") |>
          dplyr::select(-id)
      )

      # convert id of iso_combo to character for joining
      iso_combo$id <- as.character(iso_combo$id)


      ell_final <- ell_sam |>
        dplyr::bind_rows(.id = "id") |>
        dplyr::left_join(iso_combo, by = "id") |>
        dplyr::select(-id) |>
        dplyr::mutate(
          iso_combos = paste(iso_a, iso_b, sep = " - ")
        ) |>
        dplyr::select(sample_name:iso_combos, x, y)

      end_time <- Sys.time()

      time_spent <- round((end_time - start_time), digits = 2)
      if (message) {
        cli::cli_alert(paste("Total time processing was", time_spent, units(time_spent),
                             sep = " "))
      }

      return(ell_final)


    }
  }

  # ---- put in random sample for 10 random samples

  if (random %in% FALSE) {
    # prepare mu for ellipse
    if (isotope_n == 2) {

      isotope_a <- isotope_names[1]
      isotope_b <- isotope_names[2]

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
        tidyr::separate_wider_delim(ellipse_name,
                                    names = c("sample_name", "sample_number"), delim = ":") |>
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
    if (isotope_n == 3) {

    # ---- create every combo in table ----
    iso_combo <- tidyr::expand_grid(iso_a = isotope_names,
                                    iso_b = isotope_names) |>
      dplyr::filter(iso_a < iso_b) |>
      dplyr::mutate(
        iso_ab = paste(iso_a, iso_b, sep = "_")
      ) |>
      dplyr::distinct(iso_ab) |>
      tidyr::separate_wider_delim(iso_ab, names = c("iso_a", "iso_b"),
                                  delim = "_")

    # created id column
    iso_combo$id <- 1:nrow(iso_combo)

    # pivot longer to create vector that  can be used to filter combinations
    split_iso <- split(iso_combo, iso_combo$id) |>
      purrr::map(~ tidyr::pivot_longer(.x, cols = -id,
                                       values_to = "iso_name") |>
                   getElement("iso_name")
      )
    # ----- mu ----

    # select columns we need
    mu_select <- df_mu |>
      dplyr::select(sample_name, sample_number, isotope, mu_est)

    # split into three different dataframes based on the iso combos
    mu <- split_iso |>
      purrr::map(~ mu_select |>
                   dplyr::filter(isotope %in% .x) |>
                   dplyr::group_split(sample_name, sample_number)
      )

    # extract the two est mu for each sample for each combo
    mu_2 <- mu |>
      purrr::map(~ purrr::map(.x, ~ .x$mu_est,
                              .progress = "Prepare mu for ellipse")
      )

    # ---- sigma -----

    # transform to long as it is easier to keep the format
    # required for the function
    # to be wide
    sigma_long <- df_sigma |>
      tidyr::pivot_longer(cols = -c(metric, sample_name, sample_number,
                                    isotope),
                          names_to = "id",
                          values_to = "est")

    # filter based on split and then split into each group
    sigma <- split_iso |>
      purrr::map(~ sigma_long |>
                   dplyr::filter(id %in% .x &
                                   isotope %in% .x) |>
                   tidyr::pivot_wider(names_from = "id",
                                      values_from = "est") |>
                   dplyr::group_split(sample_name, sample_number)
      )

    # grab sample name and number
    group_names <- split_iso |>
      purrr::map(~ sigma_long |>
                   dplyr::filter(id %in% .x &
                                   isotope %in% .x) |>
                   pivot_wider(names_from = "id",
                               values_from = "est") |>
                   dplyr::group_by(sample_name, sample_number) |>
                   dplyr::group_keys() |>
                   dplyr::mutate(
                     id =  dplyr::row_number() |>
                       as.character()
                   )
      )

    # empty lists to dump mattrix
    sigma_list <- list()

    sm_list <- list()

    # for loop that converts every variance and covariance combo into
    # matricies for ellipse
    for (i in 1:length(sigma)) {

      st <- sigma[[i]]

      for (k in 1:length(st)) {

        sm <- st[[k]] |>
          dplyr:: select(tidyselect::any_of(split_iso[[i]])) |>
          as.matrix()

        sm_list[[k]] <- sm
      }
      sigma_list[[i]] <- sm_list
    }

    # ----- make ellipses -----

    ell_map <- map2(
      .x = sigma_list,
      .y = mu_2,
      .f = map2,
      function(x, y) ellipse::ellipse(x = x,
                                      centre = y,
                                      which = c(1, 2),
                                      level = p_ell) |>
        tibble::as_tibble(),
      .progress = "Create ellipses"
    )

    # ---- add in sample names ----
    ell_sam <- map2(
      .x = ell_map,
      .y = group_names,
      .f = function(x, y) x |>
        dplyr::bind_rows(.id = "id") |>
        dplyr::left_join(y, by = "id") |>
        dplyr::select(-id)
    )

    # convert id of iso_combo to character for joining
    iso_combo$id <- as.character(iso_combo$id)


    ell_final <- ell_sam |>
      dplyr::bind_rows(.id = "id") |>
      dplyr::left_join(iso_combo, by = "id") |>
      dplyr::select(-id) |>
      dplyr::mutate(
        iso_combos = paste(iso_a, iso_b, sep = " - ")
      ) |>
      dplyr::select(sample_name:iso_combos, x, y)

    end_time <- Sys.time()

    time_spent <- round((end_time - start_time), digits = 2)
    if (message) {
      cli::cli_alert(paste("Total time processing was", time_spent, units(time_spent),
                           sep = " "))
    }

    return(ell_final)


      # return(all_ellipses)
    }

  }



}
