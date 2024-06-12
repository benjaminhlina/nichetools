extract_layman <- function(data,
                           community_df,
                           community_names = NULL,
                           data_format = NULL) {

  if (is.null(data_format)) {
    data_format <- "long"
  }

  df_layman <- data |>
    purrr::map(~ as_tibble(.x)) |>
    bind_rows(.id = "community") |>
    left_join(community_names, by = "community")

  if (data_format %in% "long") {
    df_layman <- df_layman |>
      tidyr::pivot_longer(
      cols = -c({{community_names}}, community),
      names_to = "metric",
      values_to = "post_est"
    )

  return(df_layman)
  }
  if (data_format %in% "wide"){
    return(df_layman)
  }
}
