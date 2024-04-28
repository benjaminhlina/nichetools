#' extract Σ
#'
#' Extract Bayesian estimates of Σ from data objects created by either
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#' or [{SIBER}]().
#'
#' @param data a `list` created by the function `niw.post()` or `siberMVN()`
#' in the package
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html)
#' or [{SIBER}], respectfully.
#'  @param pkg character string of the package that the user is using. Defaults to
#' `"nicheROVER"` and will use methods to extract sigma values from `{nicherover}`
#' objects. Alternatively the user can supply the argument with `"SIBER"` to
#' extract sigma values from objectes created by `{SIBER}.`@param isotope_a `character` string to supply for the first
#' isotope used in `niw.post()`. Defaults to `"d15n"`. To be used if
#' @param isotope_b `character`string to supply for the second
#' isotope used in `niw.post()`. Defaults to `"d13c"`.
#'
#' @param data_format format a `character` that decided whether the returned object is
#' in long or wide format. Default is `"wide"`, with the alternative supplied
#' being `"long"`.
#' @param data_class controls the output returned when `pkg` is set to `"SIBER"`.
#' default is `"matrix"` but alternatively `"tibble"` can be supplied.
#'
#' @return Returns a `tibble` or ` of extracted estimates of Σ created by the
#' function `niw.post()` or `siberMVN()` in the packages
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#' and [{SIBER}]().
#'
#' If the returned object is a `tibble`, it will contain five columns in the
#' following order, `metric`, `id`, `sample_name`, `isotope`, `sample_number`, and the posterior sample for Σ
#' (i.e., `post_sample`).
#'
#' @seealso [nicheROVER::niw.post()]
#' @examples
#' extract_sigma(
#' data = niw_fish_post
#' )
#'
#' @import dplyr
#' @import purrr
#' @import tibble
#' @import tidyr
#' @export

extract_sigma <-  function(data,
                           isotope_a = NULL,
                           isotope_b = NULL,
                           format = "wide") {
  # Check if data is a list
  if (!inherits(data, "list")) {
    cli::cli_abort("Input 'data' must be a list.")
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
    tidyr::separate(isotope, into = c("isotope", "sample_number"), sep = "\\.")

  if (format %in% "wide") {

    df_sigma <- df_sigma |>
      tidyr::pivot_wider(names_from = id,
                         values_from = post_sample)
    return(df_sigma)
  }
  if (format %in% "long") {
    return(df_sigma)

  }
}
