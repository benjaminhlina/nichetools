
test_that("Test if object type returned is list ", {
  n_ellipse <- niche_ellipse(
    dat_mu = mu_est_long,
    dat_sigma = sigma_est_wide,
    message = FALSE
  )

  expect_type(object = n_ellipse, type = "list")

})
test_that("Test if object class returned is data.frame ", {
  n_ellipse_test <- niche_ellipse(
    dat_mu = mu_est_long,
    dat_sigma = sigma_est_wide,
    message = FALSE
  )
  expect_s3_class(object = n_ellipse_test, class = c("tbl_df", "tbl",
                                                       "data.frame")
  )
  # excreted dimensions
  expected_rows <- 400000
  expected_cols <- 4


  # Check the dimensions using expect_equal
  expect_equal(nrow(n_ellipse_test), expected_rows,
               info = "Number of rows is not as expected.")
  expect_equal(ncol(n_ellipse_test), expected_cols,
               info = "Number of columns is not as expected.")
})


test_that("Test if objects supplied are data.frame ", {
  # Test with invalid inputs (non-data.frames)
  invalid_dat_mu <- c(1, 2, 3, 4, 5)
  invalid_dat_sigma <- matrix(1:10, ncol = 2)

  expect_error(
    niche_ellipse(dat_mu = invalid_dat_mu, dat_sigma = sigma_est_wide)
    , "Input 'dat_mu' must be class data.frame.")

  expect_error(
    niche_ellipse(dat_mu = mu_est_long, dat_sigma = invalid_dat_sigma)
    , "Input 'dat_sigma' must be class data.frame.")

  expect_error(
    niche_ellipse(dat_mu = invalid_dat_mu, dat_sigma = invalid_dat_sigma)
  , "Input 'dat_mu' must be class data.frame.")

})
# test_that("check if no error are wrong", {
#   expect_no_error(
#     niche_ellipse(
#       dat_mu = mu_est_long,
#       dat_sigma = sigma_est_wide,
#       # isotope_a = "cal_d15n",
#       # isotope_b = "cal_d13c",
#     )
# )
# })
#
# test_that("check if it will take new names ", {
#
#   # sigma_est_wide$a <- sigma_est_wide$d15n
#   # sigma_est_wide$d15n <- NULL
#   # sigma_est_wide$b <- sigma_est_wide$d13c
#   #
#   #
#   # expect_error(
#   #   niche_ellipse(
#   #   dat_mu = mu_est_long,
#   #   dat_sigma = sigma_est_wide,
#   #   isotope_a = a,
#   #   isotope_b = b
#   # )
#   # )
# })
#
