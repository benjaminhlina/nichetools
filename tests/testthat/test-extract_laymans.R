library(SIBER)
# library(nichetools)
# library(testthat)
# create the siber object
# str(demo.siber.data.2)

demo.siber.data.2$community_names <- as.factor(demo.siber.data.2$community)
demo.siber.data.2$community <- as.numeric(demo.siber.data.2$community_names) |>
  as.character()

c_names <- demo.siber.data.2 |>
  dplyr::distinct(community, community_names)

demo_siber_data <- demo.siber.data.2 |>
  dplyr::select(iso1:community)
siber_example <- createSiberObject(demo_siber_data)

mu_post <- extractPosteriorMeans(siber_example, post_sam_siber)

# ---- layman metrics ----

layman_b <- bayesianLayman(mu.post = mu_post)

layman_ml <- communityMetricsML(siber_example)
# unit test for extract_sigma
test_that("test if it doesn't error with basic siber object ", {



  expect_no_error(
    df_laymen <- extract_layman(
      data = layman_b, community_df = c_names
    )

  )
})


# Test if data is not a list
test_that("data is not a list", {
  data_not_list <- 1:5
  cn <- data.frame(
    name = "1",
    community = "3"
  )
  expect_error(
    extract_layman(data = data_not_list,
                   type = "bay",
                   community_df = cn
    ), "The `data` argument must be a list.")
})

# Test if community_df is not a data.frame
test_that("community_df is not a data.frame", {
  data_list <- list(a = 1, b = 2)
  community_df_not_df <- matrix(1:10, nrow = 5, ncol = 2)
  expect_error(
    extract_layman(layman_b, community_df_not_df,
                   type = "bay"),
    "The `community_df` argument must be a data.frame.")
})

# Test if community_df does not have 2 columns
test_that("community_df does not have 2 columns", {
  data_list <- list(a = 1, b = 2)
  community_df_not_2_columns <- data.frame(x = 1:5)
  expect_error(
    extract_layman(layman_b, community_df = community_df_not_2_columns),
    "The `community_df` argument must be a data.frame with exactly two columns.")
})


# test if lenght of dataframe is correct for wide and long
test_that("test if lenght of wide and long  siber object ", {

  df_laymen <- extract_layman(
    data = layman_b, community_df = c_names
  )
  # check the type returned data frame should be data frame
  expect_s3_class(object = df_laymen, class =  "data.frame")
  # excreted dimensions
  expected_rows <- 48000
  expected_cols <- 5


  # Check the dimensions using expect_equal
  expect_equal(nrow(df_laymen), expected_rows,
               info = "Number of rows is not as expected.")
  expect_equal(ncol(df_laymen), expected_cols,
               info = "Number of columns is not as expected.")
})
# test if lenght of dataframe is correct for wide and long
test_that("test if lenght of wide and long  siber object ", {

  df_laymen <- extract_layman(
    data = layman_b, community_df = c_names, data_format = "wide"
  )
  # check the type returned data frame should be data frame
  expect_s3_class(object = df_laymen, class =  "data.frame")
  # excreted dimensions
  expected_rows <- 8000
  expected_cols <- 8


  # Check the dimensions using expect_equal
  expect_equal(nrow(df_laymen), expected_rows,
               info = "Number of rows is not as expected.")
  expect_equal(ncol(df_laymen), expected_cols,
               info = "Number of columns is not as expected.")
})



test_that("if type is supplid something other than ml or bay", {

  expect_error(
    extract_layman(
      data = layman_b,
      community_df = c_names,
      type = "bayes"
    ),
    regexp = "Invalid characters for 'type'. Allowed character strings are 'bay' or 'ml'.")
})

c_name_t <- demo.siber.data.2 |>
  dplyr::distinct(community, community_names) |>
  dplyr::rename(
    com = community
  )
test_that("if communioty_df has wrong name ", {


  expect_error(
    extract_layman(
      data = layman_b,
      community_df = c_name_t,
      type = "bay"
    ),
    regexp = "The data frame does not contain a column named 'community'.")
})


test_that("extract_layan throws an error when data is not a matrix", {
  # Test with a data frame (not a matrix)
  expect_error(
    extract_layman(data = data.frame(community = 1:3),
                   type = "ml",
                   # community_df = c_names
    ),
    "The `data` argument must be a matrix."
  )

  # Test with a numeric vector (not a matrix)
  expect_error(
    extract_layman(data = c(1, 2, 3), type = "ml",
                   # community_df = c_names
    ),
    "The `data` argument must be a matrix."
  )

  # Test with a matrix (should not throw an error)
  expect_silent(
    extract_layman(data = layman_ml, type = "ml",
                   community_df = c_names)
  )
})

# Write the test case
test_that("process_data produces correct output structure and transformations", {
  result <- extract_layman(
    data = layman_ml,
    type = "ml",
    community_df = c_names,
    isotope_x = "13",
    element_x = "C",
    isotope_y = "15",
    element_y = "N"
  )

  # Check that the result is a data frame
  expect_s3_class(result, "data.frame")

  # Check for the correct number of rows and columns
  expect_equal(nrow(result), nrow(layman_ml) * ncol(layman_ml))
  expect_true(all(c("metric", "community", "estimate", "labels") %in% names(result)))

  # Verify specific values and transformations
  expect_equal(result$metric[1], "dY_range")
  expect_equal(unique(result$labels),
               factor(c(paste0("\U03B4","<sup>", 15, "</sup>",
                               "N", "<br>Range"),
                        paste0("\U03B4","<sup>", 13, "</sup>",
                               "C", "<br>Range"),
                        "Total Area",
                        "Distance to<br>Centroid",
                        "Nearest<br>Neighbor<br>Distance",
                        "SD Nearest<br>Neighbor<br>Distance"),
    levels = c(paste0("\U03B4","<sup>", 13, "</sup>",
             "C", "<br>Range"),
      paste0("\U03B4","<sup>", 15, "</sup>",
             "N", "<br>Range"),
      "Total Area",
      "Distance to<br>Centroid",
      "Nearest<br>Neighbor<br>Distance",
      "SD Nearest<br>Neighbor<br>Distance"))
  )
})


