#' extract niche size
#'
#' Extract niche size based on elliptical niche region of Bayesian estimates of
#' sigma created by function `niw.post()` in the package
#' [{nicheROVER}](https://cran.r-project.org/package=nicheROVER).
#' This function is a wrapper around `nicheROVER::niche.size`.
#'
#' @param data a `list` created by the function `niw.post()` in the package
#' [{nicheROVER}](https://cran.r-project.org/package=nicheROVER).
#' @param name a `character` string that will be assinged as the column name for
#' groups. Default is `sample_name`.
#' @param prob a `numeric` bound by 0 and 1 indicating the
#' probabilistic niche size. Default is 0.95.
#'
#' @return a `tibble` containing three rows, `sample_name`, `id`, and `niche_size`.
#'
#' @seealso [nicheROVER::niche.size()] and [nicheROVER::niw.post()]
#' @examples
#' extract_niche_size(data = niw_fish_post)
#'
#'
#' @import dplyr
#' @importFrom nicheROVER niche.size
#' @import tibble
#' @import tidyr
#' @export
extract_niche_size <- function(data,
                               name = NULL,
                               prob = NULL) {
  # Check if data is a list
  if (!inherits(data, "list")) {
    cli::cli_abort("Input 'data' must be a list.")
  }

  # default name
  if(is.null(name)) {
    name <- "sample_name"
  }
  # Validate if name is a character
  if (!is.character(name)) {
    cli::cli_abort("Argument 'name' must be a character.")
  }

  # prob
  if (is.null(prob)) {
    prob <- 0.95
  }
  #  parameter validation
  if (!is.null(prob)) {
    if (!is.numeric(prob) || prob < 0 || prob > 1) {
      cli::cli_abort("Parameter 'prob' must be a numeric value between 0 and 1.")
    }
  }

  # extract niche size
  niche_size <- purrr::map(data, ~ {
    purrr::map(apply(.x$Sigma, 3, function(x) niche.size(x, alpha = prob)), ~.)
    }) |>
    map(~ tibble::as_tibble_col(.x, column_name = "niche_size") |>
          tibble::rownames_to_column("id") |>
          tidyr::unnest(cols = "niche_size")) |>
    dplyr::bind_rows(.id = name)


  return(niche_size)
}
