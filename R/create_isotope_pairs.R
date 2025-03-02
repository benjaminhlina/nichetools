#' Create isotope pairs
#'
#' @param isotope_n number of isotopes
#' @param isotope_names names of isotopes
#'
#' @export

create_isotope_pairs <- function(isotope_n = NULL,
                                 isotope_names = NULL) {

  # create name vector that will be used to id isotopes.
  if (is.null(isotope_n)) {
    isotope_n <- 3
  }
  if (!is.numeric(isotope_n) || !(isotope_n %in% c(3))) {
    cli::cli_abort("Argument 'isotope_n' must 3.")
  }

  if (isotope_n == 3) {
    if (is.null(isotope_names)) {
      isotope_names <- c("d13c", "d15n", "d34s")
    }
  }
 if (!is.character(isotope_names)) {
      cli::cli_abort("Argument 'isotope_names' must be a character.")
    }

  # ---- create every combo in table ----
  iso_combo <- tidyr::expand_grid(iso_a = isotope_names,
                                  iso_b = isotope_names) |>
    dplyr::filter(iso_a < iso_b) |>
    dplyr::mutate(
      iso_ab = paste(iso_a, iso_b, sep = "_")
    ) |>
    dplyr::distinct(iso_ab) |>
    tidyr::separate_wider_delim(iso_ab, names = c("iso_a", "iso_b"),
                                delim = "_")

  # created id column
  iso_combo$id <- 1:nrow(iso_combo)

  # pivot longer to create vector that  can be used to filter combinations
  split_iso <- split(iso_combo, iso_combo$id) |>
    purrr::map(~ tidyr::pivot_longer(.x, cols = -id,
                                     values_to = "iso_name") |>
                 dplyr::mutate(
                   iso_name = factor(iso_name, level = c("d34s",
                                                         "d13c",
                                                         "d15n"))
                 ) |>
                 dplyr::arrange(iso_name) |>
                 getElement("iso_name")
    )
  return(split_iso)
}
