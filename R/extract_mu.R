#' extract \eqn{\mu}
#'
#' Extract Bayesian estimates of \eqn{\mu} from data objects created by
#' [{nicheROVER}](https://cran.r-project.org/package=nicheROVER) or
#' [{SIBER}](https://cran.r-project.org/package=SIBER).
#'
#' @param data a `list` created by the function `niw.post()` or `siberMVN()`
#' in the package
#' [{nicheROVER}](https://cran.r-project.org/package=nicheROVER)
#' or [{SIBER}](https://cran.r-project.org/package=SIBER), respectfully.
#' @param pkg a `character` string that is the name of the package that
#' you're using. Defaults to `"nicheROVER"`.
#' Alternatively the user can supply the argument with `"SIBER"`.
#' @param isotope_a a `character` string to change the column name
#' of the first isotope used in the analysis. Defaults to `"d13c"`.
#' @param isotope_b a `character` string to change the name of second isotope
#' used in the analysis. Defaults to `"d15n"`.
#' @param data_format a `character` string that decides whether the returned object is
#' in long or wide format. Default is `"long"`, with the alternative supplied
#' being `"wide"`.
#' @param community_df a four column data frame. One of the columns has to be named
#' `community` and the data in the column will be `numeric` as a `character`
#' string(e.g., `"1", "2", "3"`). This is the order of the community names
#' and will be used to join the actual community names to the correct data.
#' These are the same class and values required by the function, `createSiberObject()`
#' from [{SIBER}](https://CRAN.R-project.org/package=SIBER).
#' The second column will be the names of the groups that are needed to supply
#' required by the function, `createSiberObject()`
#' from [{SIBER}](https://CRAN.R-project.org/package=SIBER).
#' The third and fourth columns contains the actual names of the communities
#' and groups the user is working with (e.g., `"region"`, `"common_name"`).
#'
#' @return Returns a `tibble` of extracted estimates of \eqn{\mu} created by the
#' function `niw.post()` or `siberMVN()` in the packages
#' [{nicheROVER}](https://cran.r-project.org/package=nicheROVER).
#' and [{SIBER}](https://cran.r-project.org/package=SIBER).
#'
#' The `tibble` will contain five columns in the following order, `metric`,
#' `sample_name`, `sample_number`, and the names of the isotope
#' columns supplied to `niw.post()` or  `siberMVN()` (e.g., `d13c` and  `d15n` ).
#'
#' @seealso [nicheROVER::niw.post()] and [SIBER::siberMVN()]
#' @examples
#' extract_mu(
#' data = niw_fish_post
#' )
#'
#' library(SIBER)
#'
#'# ---- create community names data frame ----
#' # uncomment to use
#' # str(demo.siber.data.2)
#'
#' demo.siber.data.2$group_name <- as.factor(demo.siber.data.2$group)
#'
#' demo.siber.data.2$group <- as.numeric(demo.siber.data.2$group_name) |>
#' as.character()
#'
#' demo.siber.data.2$community_name <- as.factor(demo.siber.data.2$community)
#'
#' demo.siber.data.2$community <- as.numeric(demo.siber.data.2$community_name) |>
#' as.character()
#'
#' cg_name <- demo.siber.data.2 |>
#' dplyr::distinct(community, group, community_name, group_name)
#'
#'
#' extract_mu(
#' data = post_sam_siber,
#' pkg = "SIBER",
#' community_df = cg_name
#' )
#'
#' @import dplyr
#' @import purrr
#' @import tibble
#' @import tidyr
#' @export


extract_mu <- function(data,
                       pkg = NULL,
                       isotope_a = NULL,
                       isotope_b = NULL,
                       data_format = NULL) {

  # Check if data is a list
  if (!inherits(data, "list")) {
    cli::cli_abort("Input 'data' must be a list.")
  }

  # Set pkg to "nicheROVER" if it is NULL
  if (is.null(pkg)) {
    pkg <- "nicheROVER"
  }

  # Check if pkg is one of the allowed values
  if (!(pkg %in% c("nicheROVER", "SIBER"))) {
    cli::cli_abort("Invalid characters for 'pkg'. Allowed character strings are
                   'nicheROVER' or 'SIBER'.")
  }
  # defaults of isotpoe a and b
  if (is.null(isotope_a)) {
    isotope_a <- "d13c"
  }

  if (is.null(isotope_b)) {
    isotope_b <- "d15n"
  }
  # Check if isotope_a is character
  if (!is.character(isotope_a)) {
    cli::cli_abort("The supplied argument for 'isotope_a' must be a character.")
  }

  # Check if isotope_b is character
  if (!is.character(isotope_b)) {
    cli::cli_abort("The supplied argument for 'isotope_b' must be a character.")
  }


  # sett data formatt
  if (is.null(data_format)) {
    data_format <- "long"
  }

  if (!(data_format %in% c("wide", "long"))) {
    cli::cli_abort("Invalid characters for 'data_format'. Allowed character
    strings are 'wide' or 'long'.")
  }
  if (pkg %in% "nicheROVER") {
    col_names <- purrr::map(data, purrr::pluck, 1) |>
      purrr::imap(~ tibble::as_tibble(.x)) |>
      dplyr::bind_rows() |>
      names()

    # extract mu
    df_mu <- purrr::map(data, purrr::pluck, 1) |>
      purrr::imap(~ tibble::as_tibble(.x) |>
                    dplyr::mutate(
                      metric = "mu",
                      sample_name = .y
                    )
      ) |>
      dplyr::bind_rows() |>
      dplyr::group_by(sample_name) |>
      dplyr::mutate(
        sample_number = 1:1000
      ) |>
      dplyr::ungroup()

    # reorder columns
    df_mu <- df_mu |>
      dplyr::select(metric:sample_number, all_of(col_names))


    if (data_format %in% "long") {
      df_mu <- df_mu |>
        tidyr::pivot_longer(cols = -c(metric, sample_name, sample_number),
                            names_to = "isotope",
                            values_to = "mu_est"
        )

      return(df_mu)
    }

    if (data_format %in% "wide") {
      return(df_mu)
    }
  }
  if (pkg %in% "SIBER") {

    if (!inherits(data, "list")) {
      cli::cli_abort("Input 'data' must be a list.")
    }


    id_isotope <- c(isotope_a, isotope_b)


    df_mu <- data |>
      purrr::map(~ {
        df <- .x[, 5:6] |>
          t() |>
          as.numeric() |>
          matrix(ncol = 1, byrow = T) |>
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
        metric = "mu"
      ) |>
      dplyr::rename(
        mu_est = V1
      ) |>
      dplyr::select(metric, sample_name,sample_number, isotope, mu_est)

    if (data_format %in% "long") {

      return(df_mu)
    }

    if (data_format %in% "wide") {
      df_mu |>
        pivot_wider(names_from = "isotope",
                    values_from = "mu_est")
    }
  }
}
