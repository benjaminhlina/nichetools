#' extract \eqn{\Sigma}
#'
#' Extract Bayesian estimates of \eqn{\Sigma} from data objects created by
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#' or [{SIBER}](https://cran.r-project.org/web/packages/SIBER/index.html).
#'
#' @param data a `list` created by the function `niw.post()` or `siberMVN()`
#' in the package
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html)
#' or [{SIBER}](https://cran.r-project.org/web/packages/SIBER/index.html), respectfully.
#' @param pkg a `character` string that is the name of the package that
#' you're using. Defaults to `"nicheROVER"`.
#' Alternatively the user can supply the argument with `"SIBER"`.
#' @param isotope_a a `character` string to change the column name
#' of the first isotope used in the analysis. Defaults to `"d15n"`.
#' @param isotope_b a `character` string to change the name of second isotope
#' used in the analysis. Defaults to `"d13c"`.
#' @param data_format a `character` string that decides whether the returned object is
#' in long or wide format. Default is `"wide"`, with the alternative supplied being `"long"`.
#'
#' @return Returns a `tibble` of extracted estimates of \eqn{\Sigma}
#' created by the function `niw.post()` or `siberMVN()` in the packages
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#' and [{SIBER}](https://cran.r-project.org/web/packages/SIBER/index.html).
#'
#' The returned object it will contain five columns in the
#' following order when `data_format` is set to `"wide"`,
#' `metric`, `id`, `sample_name`, `isotope`, `sample_number`,
#' and the posterior sample for \eqn{\Sigma} (i.e., `post_sample`).
#'
#' @seealso [nicheROVER::niw.post()] and [SIBER::siberMVN()]
#' @examples
#' extract_sigma(
#' data = niw_fish_post
#' )
#'
#' extract_sigma(
#' data = post_sam_siber,
#' pkg = "SIBER"
#' )
#'
#' @import dplyr
#' @import purrr
#' @import tibble
#' @import tidyr
#' @export

extract_sigma <-  function(data,
                           pkg = NULL,
                           isotope_a = NULL,
                           isotope_b = NULL,
                           data_format = NULL) {

  # Set pkg to "nicheROVER" if it is NULL
  if (is.null(pkg)) {
    pkg <- "nicheROVER"
  }

  # Check if pkg is one of the allowed values
  if (!(pkg %in% c("nicheROVER", "SIBER"))) {
    cli::cli_abort("Invalid characters for 'pkg'. Allowed character strings are
                   'nicheROVER' or 'SIBER'.")
  }

  # sett data formatt
  if(is.null(data_format)) {
      data_format <- "wide"
  }

  if (!(data_format %in% c("wide", "long"))) {
    cli::cli_abort("Invalid characters for 'data_format'. Allowed character
    strings are 'wide' or 'long'.")
  }

  # defaults of isotpoe a and b
    if (is.null(isotope_a)) {
      isotope_a <- "d15n"
    }

    if (is.null(isotope_b)) {
      isotope_b <- "d13c"
    }
    # Check if isotope_a is character
    if (!is.character(isotope_a)) {
      cli::cli_abort("The supplied argument for 'isotope_a' must be a character.")
    }

    # Check if isotope_b is character
    if (!is.character(isotope_b)) {
      cli::cli_abort("The supplied argument for 'isotope_b' must be a character.")
    }

  if (pkg %in% "nicheROVER") {
    # Check if data is a list
    if (!inherits(data, "list")) {
      cli::cli_abort("Input 'data' must be a list.")
    }
    # create name vector that will be used to id isotopes.
    id_isotope <- c(isotope_a, isotope_b)
    # extract sigma
    df_sigma <- purrr::map(data, purrr::pluck, 2) |>
      purrr::imap(~ tibble::as_tibble(.x) |>
                    dplyr::mutate(
                      metric = "sigma",
                      id = id_isotope,
                      sample_name = .y
                    )
      ) |>
      dplyr::bind_rows() |>
      tidyr::pivot_longer(cols = -c("id", "sample_name", "metric"),
                          names_to = "isotope",
                          values_to = "post_sample"
      ) |>
      tidyr::separate(isotope, into = c("isotope", "sample_number"),
                      sep = "\\.")

    if (data_format %in% "wide") {

      df_sigma <- df_sigma |>
        tidyr::pivot_wider(names_from = id,
                           values_from = post_sample)
      return(df_sigma)
    }
    if (data_format %in% "long") {
      return(df_sigma)

    }
  }

  if (pkg %in% "SIBER") {
    if (!inherits(data, "list")) {
      cli::cli_abort("Input 'data' must be a list.")
    }

    # create name vector that will be used to id isotopes.
    id_isotope <- c(isotope_a, isotope_b)


    df_sigma <- data |>
      purrr::map(~ {
        df <- .x[, 1:4] |>
          t() |>
          as.numeric() |>
          matrix(ncol = 2, byrow = T) |>
          as.data.frame() |>
          tibble::as_tibble()

        df <- df |>
          dplyr::mutate(
            sample_number = rep(1:ceiling(nrow(df) / 2),
                                each = 2,
                                length.out = nrow(df)),
            isotope = rep(id_isotope, times = nrow(df) / 2)
          )
      }
      ) |>
      dplyr::bind_rows(.id = "sample_name") |>
      dplyr::mutate(
        metric = "sigma",
      ) |>
      dplyr::rename(
        {{isotope_a}} := V1,
        {{isotope_b}} := V2
      ) |>
      dplyr::select(metric, sample_name, isotope, sample_number, {{isotope_a}},
                    {{isotope_b}})

    if (data_format %in% "wide") {

      return(df_sigma)
    }

    if (data_format %in% "long") {
      df_sigma <- df_sigma |>
        mutate(
          id = isotope
        ) |>
        dplyr::select(-isotope) |>
        tidyr::pivot_longer(cols = -c("metric", "sample_name", "id",
                                      "sample_number"),
                            names_to = "isotope",
                            values_to = "post_sample"

        ) |>
        dplyr::select(
          metric, id, sample_name, isotope, sample_number, post_sample
        ) |>
        arrange(id)


    }
  }
}
