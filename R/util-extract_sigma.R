#' extract Σ
#'
#' Extract Bayesian estimates of Σ from the function `niw.post()` in the
#' package
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#'
#' @param data a `list` created by the function `niw.post()` in the package
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html)
#' @param isotope_a `character` string to supply for the first
#' isotope used in `niw.post()`. Defaults to `"d15n"`.
#' @param isotope_b `character`string to supply for the second
#' isotope used in `niw.post()`. Defaults to `"d13c"`.
#' @param format format a `character` that decided whether the returned object is
#' in long or wide format. Default is `"wide"`, with the alternative supplied
#' being `"long"`.
#' @return Returns a `tibble` of extracted estimates of Σ created by the
#' function `niw.post()` in the package
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#' The tibble will contain five columns in the following order, `metric`, `id`,
#' `sample_name`, `isotope`, `sample_number`, and the posterior sample for Σ
#' (i.e., `post_sample`).
#'
#' @seealso [nicheROVER::niw.post()]
#' @examples
#' extract_sigma(
#' data = niw_fish_post
#' )
#' # ---- below will be turned into vignette when finished ----
#' \dontrun{
#' # load packages
#' {
#'   library(dplyr)
#'   library(janitor)
#'   library(nicheROVER)
#'   library(nichetools)
#'   library(purrr)
#'   }
#'
#' # ---- To understand how niw_fish_post is being created ----
#'
#' # get fish data frame, and remove sulfur for the example
#' df <- fish %>%
#'     janitor::clean_names()
#'
#' # create number of samples used in nicheROVER
#' nsample <- 1000
#'
#' # split the data frame by species and select isotopes of interest
#' df_split <- df %>%
#'   split(.$species) %>%
#'   map(~ select(., d15n, d13c))
#'
#' # extract the names of each list to name each object in list
#' df_names <- df %>%
#'    group_by(species) %>%
#'    group_keys() %>%
#'    ungroup() %>%
#'    mutate(
#'      id = 1:nrow(.)
#'      )
#'
#' # name each object in list
#' names(df_split) <- df_names$species
#'
#' # create niw posterior samples
#' niw_fish_post <- df_split %>%
#'   map(~niw.post(nsample = nsample, X = .))
#'
#' # we can then use extract_sigma() to extract sigma from niw_fish_post
#' df_sigma <- extract_sigma(
#'   data = niw_fish_post
#'   )
#'
#' # --- to use with `niche_ellipse()` we need to make into wide format ----
#' df_sigma_wide <- df_sigma |>
#' tidyr::pivot_wider(names_from = id,
#'             values_from = post_sample)
#' }
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
