#' \Sigma$ extract
#'
#' Extract Bayesian estimates of \Sigma$ from the function `niw.post()` in the
#' package
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#'
#' @param data a list object created by the function `niw.post()` in the package
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html)
#' @param isotope_a character string to supply for the first
#' isotope used in `niw.post()`. Defaults to `"d15n"`.
#' @param isotope_b character string to supply for the second
#' isotope used in `niw.post()`. Defaults to `"d13c"`.
#' @return Returns a `tibble` of extracted estimates of \Sigma$ created by the
#' function `niw.post()` in the pckage
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#' The tibble will contain five columns in the following order, `metric`, `id`,
#' `sample_name`, `isotope`, `sample_number`, and the posterior sample for \Sigma$
#' (i.e., `post_sample`).
#'
#' @seealso [nicheROVER::niw.post()]
#'
#' @examples
#' df_sigma <- sigma_extract(
#' data = niw_fish_post
#' )
#' # --- to use with `niche_ellipse()` we need to make into wide format ----
#' # we can do this by using tidyr's `pivot_wide()`. Uncomment below to run.
#' # df_sigma_wide <- df_sigma |>
#' # tiydr::pivot_wider(names_from = id,
#' #             values_from = post_sample)
#'
#'
#' # ---- To understand how niw_fish_post is being created ----
#' # ---- Uncomment the code below to create object from nicheROVER ----
#' # {
#' # library(dplyr)
#' # library(janitor)
#' # library(nicheROVER)
#' # library(nichetools)
#' # library(purrr)
#' # }
#' #
#' # gab fish data frame, and remove sulfur for the example
#' # df <- fish %>%
#' #   janitor::clean_names()
#' #
#' # create number of samples used in nicheROVER
#' # nsample <- 1000
#' #
#' # split the data frame by species and select isotopes of interest
#' # df_split <- df %>%
#' # split(.$species) %>%
#' # map(~ select(., d15n, d13c))
#' #
#' # extract the names of each list to name each object in list
#' # df_names <- df %>%
#' # group_by(species) %>%
#' # group_keys() %>%
#' # ungroup() %>%
#' # mutate(
#' # id = 1:nrow(.)
#' # )
#' #
#' # name each object in list
#' # names(df_split) <- df_names$species
#' #
#' # create niw posterior samples
#' # niw_fish_post <- df_split %>%
#' # map(~niw.post(nsample = nsample, X = .))
#' #
#' #
#' # we can then use sigma_extract() to extract mu from niw_fish_post
#' # df_sigmaa <- sigma_extract(
#' # data = niw_fish_post
#' # )
#'
#'\@import dplyr
#' @import purrr
#' @import tibble
#' @import tidyr
#' @export

sigma_extract <-  function(data,
                           isotope_a = NULL,
                           isotope_b = NULL) {
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

  return(df_sigma)
}
