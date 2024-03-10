#' Mu extract
#'
#' Extract estimates of mu from functions from {nicheROVER}.
#'
#' @export
mu_extract <- function(data) {

  df_mu <- map(data, pluck, 1) %>%
    imap(~ as_tibble(.x) %>%
           mutate(
             metric = "mu",
             sample_name = .y
           )
    ) %>%
    bind_rows() %>%
    group_by(sample_name) %>%
    mutate(
      sample_number = 1:1000
    ) %>%
    ungroup()
  return(df_mu)
}
