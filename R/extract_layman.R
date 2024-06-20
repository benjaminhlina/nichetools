#' extract Layman metrics
#'
#' Extract Bayesian estimates of six layman metrics from data objects created by
#' [{SIBER}](https://cran.r-project.org/web/packages/SIBER/index.html). These six
#' metric include the following Layman  of dX_range, dY_range, TA,
#' CD, MNND and SDNND
#'
#' @param data siber object
#' @param coummunity_df a two column dataframe one of the column names
#' has to be a number as a character which is the same community names requried
#' by `makeSiberObject()` function from {SIBER}. The other column is the actual
#' names of the communities that the user is working with (e.g., `"region"`)
#' @param community_name a character string that is the name of the second column
#' in community_df supplied (e.g., `"region"`).
#'
#' @import dplyr
#' @import purrr
#' @import tibble
#' @import tidyr
#' @export


extract_layman <- function(data,
                           community_df = NULL,
                           community_name = NULL,
                           data_format = NULL) {

  # sett data formatt
  if(is.null(data_format)) {
    data_format <- "long"
  }

  if (!(data_format %in% c("wide", "long"))) {
    cli::cli_abort("Invalid characters for 'data_format'. Allowed character
    strings are 'wide' or 'long'.")
  }

  if (is.null(data_format)) {
    data_format <- "long"
  }

  df_layman <- data |>
    purrr::map(~ as_tibble(.x)) |>
    dplyr::bind_rows(.id = "community") |>
    dplyr::left_join(community_df, by = "community")

  if (data_format %in% "long") {
    df_layman <- df_layman |>
      tidyr::pivot_longer(
      cols = -c({{community_name}}, community),
      names_to = "metric",
      values_to = "post_est"
    )

  return(df_layman)
  }
  if (data_format %in% "wide"){
    return(df_layman)
  }
}
