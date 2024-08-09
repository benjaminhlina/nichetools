#' extract niche size
#'
#' Extract niche size based on elliptical niche region of Bayesian estimates of
#' sigma created by function `niw.post()` or `siberEllipses()` in the package
#' [{nicheROVER}](https://cran.r-project.org/package=nicheROVER) or
#' [{SIBER}](https://cran.r-project.org/package=SIBER), respectfully.
#' For [{nicheROVER}](https://cran.r-project.org/package=nicheROVER)
#' this function is a wrapper around `nicheROVER::niche.size`.
#'
#' @param data a `list` or `matrix` created by the function `niw.post()` or
#' `siberEllipses()` in the package
#' [{nicheROVER}](https://cran.r-project.org/package=nicheROVER) or
#' [{SIBER}](https://cran.r-project.org/package=SIBER), respectfully.
#' @param pkg a `character` string that is the name of the package that
#' you're using. Defaults to `"nicheROVER"`.
#' Alternatively the user can supply the argument with `"SIBER"`.
#' @param name a `character` string that will be assigned as the column name for
#' groups. Default is `sample_name`. Only to be used when `pkg` is set to
#' `"nicheROVER"`.
#' @param prob a `numeric` bound by 0 and 1 indicating the
#' probabilistic niche size. Default is 0.95. Only to be used when `pkg` is set to
#' `"nicheROVER"`.
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
#'
#' @return if `pkg` is set to `"nicheROVER"` then a `tibble` containing three rows, `sample_name`, `id`, and `niche_size` is returned.
#'
#' @seealso [nicheROVER::niche.size()], [nicheROVER::niw.post()],
#' [SIBER::siberEllipses()], and [SIBER::createSiberObject()]
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
                               pkg = NULL,
                               name = NULL,
                               prob = NULL,
                               community_df = NULL) {

  # set packages
  if (is.null(pkg)) {
    pkg <- "nicheROVER"
  }

  # Check if pkg is one of the allowed values
  if (!(pkg %in% c("nicheROVER", "SIBER"))) {
    cli::cli_abort("Invalid characters for 'pkg'. Allowed character strings are
                   'nicheROVER' or 'SIBER'.")
  }

  if (pkg %in% "nicheROVER") {
    # Check if data is a list
    if (!inherits(data, "list")) {
      cli::cli_abort("Input 'data' must be a list.")
    }

    # default name
    if (is.null(name)) {
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

  # niche size for siber extract
  if (pkg %in% "SIBER") {
    if (!inherits(data, "matrix")) {
      cli::cli_abort("Input 'data' must be a matrix.")
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

    # create naming varable for data
    grp <- paste(community_df$community, community_df$group, sep = ".") |>
      sort()

    # convert matrix to a data.cframe and assigna names
    seb_dat <- as.data.frame(data)
    colnames(seb_dat) <- grp

    seb_extract <- seb_dat |>
      dplyr::mutate(
        id = 1:nrow(data)
      ) |>
      tidyr::pivot_longer(cols = -id,
                          names_to = "community_group",
                          values_to = "sea") |>
      tidyr::separate_wider_delim(community_group, delim = ".",
                                  names = c("community",
                                            "group")) |>
      dplyr::arrange(community, group, id) |>
      dplyr::left_join(community_df, by = c("community", "group"))

    return(seb_extract)
  }
}
