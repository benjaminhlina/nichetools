
test_that("Test if data object is list ", {
  df_mu <- mu_extract(
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
    mu_extract(
      data = dat
    ), regexp = "Input 'data' must be a list."
  )

})


test_that("test that the object type and length are correct ", {
  df_mu_test <- mu_extract(
    data = niw_fish_post
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
