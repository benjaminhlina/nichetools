
test_that("Test if data object is list ", {
  df_mu <- extract_mu(
    data = niw_fish_post
  )

  expect_type(object = df_mu, type = "list")

})


test_that("error if data isn't a list", {
  dat <- data.frame(
    x = seq(0, 100, length.out = 10),
    y = seq(0, 0.1, length.out = 10)
  )

  expect_error(
    extract_mu(
      data = dat
    ), regexp = "Input 'data' must be a list."
  )

})


test_that("test that the object type and length are correct ", {
  df_mu_test <- extract_mu(
    data = niw_fish_post
  )
  # check the type returned data frame should be data frame
  expect_s3_class(object = df_mu_test, class =  "data.frame")
  # excreted dimensions
  expected_rows <- 8000
  expected_cols <- 5


  # Check the dimensions using expect_equal
  expect_equal(nrow(df_mu_test), expected_rows,
               info = "Number of rows is not as expected.")
  expect_equal(ncol(df_mu_test), expected_cols,
               info = "Number of columns is not as expected.")
})

test_that("test that the object type and length are correct ", {
  df_mu_test <- extract_mu(
    data = niw_fish_post, data_format = "wide"
  )
  # check the type returned data frame should be data frame
  expect_s3_class(object = df_mu_test, class =  "data.frame")
  # excreted dimensions
  expected_rows <- 4000
  expected_cols <- 5


  # Check the dimensions using expect_equal
  expect_equal(nrow(df_mu_test), expected_rows,
               info = "Number of rows is not as expected.")
  expect_equal(ncol(df_mu_test), expected_cols,
               info = "Number of columns is not as expected.")
})


test_that("Check if column names extracted are correct", {

  expected_names <- c("metric", "sample_name", "sample_number", "d13c", "d15n")

  df_mu_test <- extract_mu(
    data = niw_fish_post,
    data_format = "wide"
  )

  expect_equal(names(df_mu_test), expected_names)
})


test_that("Check if column order", {

  expected_names <- c("metric", "sample_name", "sample_number",
                      "isotope", "mu_est")

  df_mu_test_1 <- extract_mu(
    data = niw_fish_post
  )
  expect_equal(names(df_mu_test_1), expected_names)
})


library(SIBER)

demo.siber.data.2$group_name <- as.factor(demo.siber.data.2$group)

demo.siber.data.2$group <- as.numeric(demo.siber.data.2$group_name) |>
  as.character()

demo.siber.data.2$community_name <- as.factor(demo.siber.data.2$community)

demo.siber.data.2$community <- as.numeric(demo.siber.data.2$community_name) |>
  as.character()

cg_names <- demo.siber.data.2 |>
  dplyr::distinct(community, group, community_name, group_name)






# ---- siber ----
test_that("Test if data object is list ", {
  df_mu <- extract_mu(
    data = post_sam_siber,
    pkg = "SIBER",
    community_df = cg_names
  )

  expect_type(object = df_mu, type = "list")

})


test_that("error if data isn't a list", {
  dat <- data.frame(
    x = seq(0, 100, length.out = 10),
    y = seq(0, 0.1, length.out = 10)
  )

  expect_error(
    extract_mu(
      data = dat,
      pkg = "SIBER",
      community_df = cg_names

    ), regexp = "Input 'data' must be a list."
  )

})


test_that("test that the object type and length are correct ", {
  df_mu_test <- extract_mu(
    data = post_sam_siber,
    pkg = "SIBER",
    community_df = cg_names
  )
  # check the type returned data frame should be data frame
  expect_s3_class(object = df_mu_test, class =  "data.frame")
  # excreted dimensions
  expected_rows <- 40000
  expected_cols <- 9


  # Check the dimensions using expect_equal
  expect_equal(nrow(df_mu_test), expected_rows,
               info = "Number of rows is not as expected.")
  expect_equal(ncol(df_mu_test), expected_cols,
               info = "Number of columns is not as expected.")
})

test_that("test that the object type and length are correct ", {
  df_mu_test <- extract_mu(
    data = post_sam_siber,
    pkg = "SIBER",
    community_df = cg_names,
    data_format = "wide"
  )
  # check the type returned data frame should be data frame
  expect_s3_class(object = df_mu_test, class =  "data.frame")
  # excreted dimensions
  expected_rows <- 20000
  expected_cols <- 9


  # Check the dimensions using expect_equal
  expect_equal(nrow(df_mu_test), expected_rows,
               info = "Number of rows is not as expected.")
  expect_equal(ncol(df_mu_test), expected_cols,
               info = "Number of columns is not as expected.")
})


test_that("Check if column names extracted are correct", {

  expected_names <- c("metric",  "community",
                      "group", "sample_name", "sample_number", "community_name",
                      "group_name",  "d13c", "d15n")

  df_mu_test <- extract_mu(
    data = post_sam_siber,
    pkg = "SIBER",
    data_format = "wide",
    community_df = cg_names
  )

  expect_equal(names(df_mu_test), expected_names)
})


test_that("Check if column order", {

  expected_names <- c("metric",  "community",
                      "group", "sample_name", "sample_number", "community_name",
                      "group_name",
                      "isotope", "mu_est")

  df_mu_test_1 <- extract_mu(
    data = post_sam_siber,
    pkg = "SIBER",
    community_df = cg_names
  )
  expect_equal(names(df_mu_test_1), expected_names)
})

# Test 4: Invalid community_df (not a data.frame or not 4 columns)
test_that("extract_mu throws an error if community_df is not a valid data.frame", {
  invalid_community_df <- data.frame(community = c("A", "B"), group = c("G1", "G2"))
  expect_error(extract_mu(data = post_sam_siber,
                          pkg = "SIBER",
                          community_df = invalid_community_df),
               "The `community_df` argument must be a data.frame with exactly four columns.")
})

# Test 5: Missing 'community' or 'group' column in community_df
test_that("extract_mu throws an error if community_df isn't 4 columns", {
  invalid_community_df <- data.frame(
    col2 = c("A", "B", "C"),
    col3 = c(1, 2, 3),
    col4 = c(4, 5, 6)
  )

  expect_error(
    extract_mu(data = post_sam_siber,
               pkg = "SIBER",
               community_df = invalid_community_df),
    regexp = "The `community_df` argument must be a data.frame with exactly four columns.")

})
test_that("extract_mu throws an error if community_df is missing 'community' or 'group' columns", {


  invalid_community_df_2 <- data.frame(
    col2 = c("A", "B", "C"),
    col3 = c(1, 2, 3),
    col4 = c(4, 5, 6),
    col5 = c(4, 6, 3)
  )


  expect_error(
    extract_mu(data = post_sam_siber,
               pkg = "SIBER",
               community_df = invalid_community_df_2),
    regexp = "The data frame does not contain a column named 'community' and 'group'.")
})
