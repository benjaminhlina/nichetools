#' Overlap extract
#'
#' Extract Bayesian estimates of similarities among groups produced by the
#' following function `overlap()` in the package
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#'
#' @param data a list object created by the fucntion `overlap()` in the package
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
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
#' # overlap_extract()
#'
#'
#' @export


overlap_extract <- function(data,
                            name_a = NULL,
                            name_b = NULL) {

  if (is.null(name_a)){
    name_a <- "sample_name_a"
  }

  if (is.null(name_b)){
    name_b <- "sample_name_b"
  }

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
