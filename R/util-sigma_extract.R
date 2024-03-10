#' Sigma extract
#'
#' Extract estiamtes of sigma from functions from {nicheROVER}.
#'
#' @export

sigma_extract <-  function(data,
                           isotope_a = NULL,
                           isotope_b = NULL) {


  if (is.null(isotope_a)) {
    isotope_a <- "d15n"
  }

  if (is.null(isotope_b)) {
    isotope_b <- "d13c"
  }
  # Check if isotope_a is character
  if (!is.character(isotope_a)) {
    cli::cli_abort("Parameter 'isotope_a' must be a character vector.")
  }

  # Check if isotope_b is character
  if (!is.character(isotope_b)) {
    cli::cli_abort("Parameter 'isotope_b' must be a character vector.")
  }

  # extract sigma
  df_sigma <- purrr::map(data, pluck, 2) |>
    purrr::imap(~ tibble::as_tibble(.x) |>
           dplyr::mutate(
             metric = "sigma",
             id = c(isotope_a, isotope_b),
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
