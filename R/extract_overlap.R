#' extract overlap
#'
#' Extract Bayesian estimates of similarities among groups produced by the
#' following function `overlap()` in the package
#' [{nicheROVER}](https://cran.r-project.org/package=nicheROVER).
#'
#' @param data a `array` object containing `matrices` created by the function
#' `overlap()` in the package
#' [{nicheROVER}](https://cran.r-project.org/package=nicheROVER).
#' @param name_a character string to supply for the first
#' `sample_name` used in `overlap()`. Defaults to `"sample_name_a"`.
#' @param name_b character string to supply for the second
#' `sample_name` used in `overlap()`. Defaults to `"sample_name_b"`.
#'
#' @return A `tibble` containing five rows, `sample_name_a`, `id`,
#' `sample_name_b`, `sample_number`, and `niche_overlap`.
#'
#' @seealso [nicheROVER::overlap()]
#' @examples
#' extract_overlap(data = over_stat)
#'
#' @export


extract_overlap <- function(data,
                            name_a = NULL,
                            name_b = NULL) {

  # Check if data is a array
  if (!inherits(data, "array")) {
    cli::cli_abort("Input 'data' must be an array.")
  }

  # set name_a null
  if (is.null(name_a)){
    name_a <- "sample_name_a"
  }
  # set name_b null
  if (is.null(name_b)){
    name_b <- "sample_name_b"
  }

  # Check if name_a is character
  if (!is.character(name_a)) {
    cli::cli_abort("The supplied argument for 'name_a' must be a character.")
  }

  # Check if isotope_b is character
  if (!is.character(name_b)) {
    cli::cli_abort("The supplied argument for 'name_b' must be a character.")
  }


  # overlap extract
  overlap_df <- data %>%
    tibble::as_tibble(rownames = "species_a") %>%
    dplyr::mutate(
      id = 1:nrow(.)
    ) %>%
    tidyr::pivot_longer(cols = -c(id, species_a),
                        names_to = "species_b",
                        values_to = "niche_overlap") %>%
    tidyr::separate(species_b, into = c("species_b", "sample_number"),
                    sep = "\\.") %>%
    dplyr::rename(
      {{name_a}} := species_a,
      {{name_b}} := species_b
    )

  return(overlap_df)

}
