#' extract maximum-likelihood estimates for group metrics
#'
#' Extract group metrics within each community from a matrix object
#' that is produced by `groupMetricsML()` function from
#' [{SIBER}](https://CRAN.R-project.org/package=SIBER. These metrics
#' are the following   the convex hull total area (TA), Standard Ellipse Area (SEA), and the
#' corresponding small sample size corrected version SEAc based on the maximum likelihood
#' estimates of the means and covariance matrices of each group.
#'
#' @param data a `matrix` produced by the function `groupMetricsML()` in the
#' package [{SIBER}](https://CRAN.R-project.org/package=SIBER.
#' @param community_df a four column data frame. One of the columns has to be named
#' `community` and the data in the column will be `numeric` as a `character`
#' string(e.g., `"1", "2", "3"`). This is the order of the community names
#' and will be used to join the actual community names to the correct data.
#' These are the same class and values required by the function, `createSiberObject()`
#' from [{SIBER}](https://CRAN.R-project.org/package=SIBER).
#' The second column will be the names of the groups that are needed to supply
#' required by the function, `createSiberObject()`
#' from [{SIBER}](https://CRAN.R-project.org/package=SIBER).
#' The third and fourth columns contains the actual names of the communities
#' and groups the user is working with (e.g., `"region"`, `"common_name"`).
#' @param data_format a `character` string that decides whether the returned object is
#' in long or wide format. Default is `"long"`, with the alternative supplied being `"wide"`.
#'
#' @return A `tibble` containing four rows when `data_format` is set to its
#' default which is `long`. These four rows are the following, `community`,
#' `the_name_of_the_communities`, `metric` and `post_est`.
#' @examples
#' library(SIBER)
#'
#'# ---- create community names data frame ----
#' # uncomment to use
#' # str(demo.siber.data.2)
#'
#' demo.siber.data.2$group_name <- as.factor(demo.siber.data.2$group)
#'
#' demo.siber.data.2$group <- as.numeric(demo.siber.data.2$group_name) |>
#' as.character()
#'
#' demo.siber.data.2$community_name <- as.factor(demo.siber.data.2$community)
#'
#' demo.siber.data.2$community <- as.numeric(demo.siber.data.2$community_name) |>
#' as.character()
#'
#' cg_name <- demo.siber.data.2 |>
#' dplyr::distinct(community, group, community_name, group_name)
#'
#' # ---- create comparsions ----
#'
#' demo.siber.data.2 <- demo.siber.data.2[,1:4]
#'
#' siber_example <- createSiberObject(demo.siber.data.2)
#'
#' # extract group metrics
#' group_ml <- groupMetricsML(siber_example)
#'
#' group_convert <- extract_group_metrics(data = group_ml,
#'                                 community_df = cg_name)
#' @export
extract_group_metrics <- function(data = NULL,
                                  community_df = NULL,
                                  data_format = NULL) {


  # check if data is a matrix if not error
  if (!is.matrix(data)) {
    cli::cli_abort(c(
      "The `data` argument must be a matrix.",
      "i" = "Please provide data in matrix format."
    ))
  }


  # Check if `community_df` is a four-column data.frame
  if (!is.null(community_df)) {
    if (!is.data.frame(community_df) || ncol(community_df) != 4) {
      cli::cli_abort(c(
        "The `community_df` argument must be a data.frame with exactly four columns.",
        "i" = "Please provide a data.frame with four columns."
      ))
    }
  }

  # column names are community and group of community_df
  if (!any(c("community", "group") %in% colnames(community_df))) {
    cli::cli_abort("The data frame does not contain a column named
                   'community' and 'group'.")
  }


  # set data formatt
  if(is.null(data_format)) {
    data_format <- "long"
  }

  if (!(data_format %in% c("wide", "long"))) {
    cli::cli_abort("Invalid characters for 'data_format'. Allowed character
    strings are 'wide' or 'long'.")
  }
  df_group_ml <- data |>
    as.data.frame() |>
    tibble::rownames_to_column(var = "metric") |>
    tidyr::pivot_longer(cols = -metric,
                        names_to = "community_group",
                        values_to = "est") |>
    tidyr::separate_wider_delim(community_group, delim = ".",
                                names = c("community",
                                          "group")) |>
    dplyr::arrange(community, group, metric) |>
    dplyr::left_join(community_df, by = c("community", "group"))
  # dplyr::select(metric, community, group, {community})

  if (data_format %in% "long") {

    return(df_group_ml)
  }
  if (data_format %in% "wide") {

    df_group_ml <- df_group_ml |>
      tidyr::pivot_wider(names_from = "metric",
                         values_from = "est")

    return(df_group_ml)

  }
}
