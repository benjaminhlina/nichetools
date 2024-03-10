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
    isotope_a <- "d13c"
  }


  df_sigma <- map(data, pluck, 2) |>
    imap(~ as_tibble(.x) |>
           mutate(
             metric = "sigma",
             id = c(isotope_a, isotope_a),
             sample_name = .y
           )
    ) |>
    bind_rows() |>
    pivot_longer(cols = -c("id", "sample_name", "metric"),
                 names_to = "isotope",
                 values_to = "post_sample"
    ) |>
    separate(isotope, into = c("isotope", "sample_number"), sep = "\\.")
  return(df_sigma)
}
