
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
  expected_rows <- 48000
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
  expected_rows <- 24000
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
