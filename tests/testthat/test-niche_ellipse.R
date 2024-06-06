# packages

# {
# library(nichetools)
# library(testthat)
# }


test_that("Test if object type returned is list ", {
  n_ellipse <- niche_ellipse(
    dat_mu = mu_est_long,
    dat_sigma = sigma_est_wide,
    set_seed = 4,
    message = FALSE
  )

  expect_type(object = n_ellipse, type = "list")

})
test_that("Test if object class returned is data.frame ", {
  n_ellipse_test <- niche_ellipse(
    dat_mu = mu_est_long,
    dat_sigma = sigma_est_wide,
    set_seed = 4,
    message = FALSE
  )
  expect_s3_class(object = n_ellipse_test, class = c("tbl_df", "tbl",
                                                     "data.frame")
  )
  # excreted dimensions
  expected_rows <- 4000
  expected_cols <- 4
})
test_that("Test if object class returned is data.frame ", {
  n_ellipse_test <- niche_ellipse(
    dat_mu = mu_est_long,
    dat_sigma = sigma_est_wide,
    random = FALSE,
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


test_that("Check if naming works", {

  sigma_est_wide <- sigma_est_wide |>
    dplyr::rename(
      cal_d13c = d13c,
      cal_d15n = d15n
    )

  expect_no_error(
    n_ellipse_test <- niche_ellipse(
      dat_mu = mu_est_long,
      dat_sigma = sigma_est_wide,
      set_seed = 4,
      message = FALSE,
      isotope_a = "cal_d13c",
      isotope_b = "cal_d15n",
    )

  )
  expect_true("cal_d15n" %in% names(n_ellipse_test))
  expect_true("cal_d13c" %in% names(n_ellipse_test))

  expected_names <- c("sample_name", "sample_number",
                      "cal_d13c", "cal_d15n")


  expect_equal(names(n_ellipse_test), expected_names)
})


test_that("Test if isotope_a errors if not a charcters", {
  expect_error(niche_ellipse(
    dat_mu = mu_est_long,
    dat_sigma = sigma_est_wide,
    message = FALSE,
    isotope_a = 6), regexp = "Argument 'isotope_a' must be a character."
  )
})

test_that("Test if isotope_b errors if not a charcters", {
  expect_error(niche_ellipse(
    dat_mu = mu_est_long,
    dat_sigma = sigma_est_wide,
    set_seed = 4,
    message = FALSE,
    isotope_b = 4), regexp = "Argument 'isotope_b' must be a character."
  )
})


test_that("Parameter 'p_ell' is can take other values than 0.95", {
  # Test case 1: p_ell is 0.75,
  expect_no_error(
    n_ellipse_test <- niche_ellipse(
      dat_mu = mu_est_long,
      dat_sigma = sigma_est_wide,
      set_seed = 4,
      message = FALSE,
      p_ell = 0.75
    )
  )
  expect_equal(n_ellipse_test$d15n[1], 13.8, tolerance = 0.1)
  expect_equal(n_ellipse_test$d13c[1], -22.4, tolerance = 0.1)
})
test_that("Parameter 'p_ell' is set and validated correctly", {
  # Test case 1: p_ell is NULL, it should be set to 0.95
  expect_no_error(
    n_ellipse_test <- niche_ellipse(
      dat_mu = mu_est_long,
      dat_sigma = sigma_est_wide,
      set_seed = 4,
      message = FALSE,
    )
  )
  expect_equal(n_ellipse_test$d15n[1], 14.3, tolerance = 0.1)
  expect_equal(n_ellipse_test$d13c[1], -21.7, tolerance = 0.1)
})



test_that("p_ell errors when given a charcter  or value outside of range", {

  # Test case 3: p_ell is not numeric, it should raise an error
  expect_error(niche_ellipse(dat_mu = mu_est_long,
                             dat_sigma = sigma_est_wide,
                             set_seed = 4,
                             p_ell = "invalid"),
               "Parameter 'p_ell' must be a numeric value between 0 and 1.")

  # Test case 4: p_ell is numeric but outside the valid range,
  expect_error(niche_ellipse(dat_mu = mu_est_long,
                             dat_sigma = sigma_est_wide,
                             set_seed = 4,
                             p_ell = 1.5),
               "Parameter 'p_ell' must be a numeric value between 0 and 1.")
}
)


test_that("Test print out of message for run time", {

  expect_message(niche_ellipse(dat_mu = mu_est_long,
                               dat_sigma = sigma_est_wide,
                               set_seed = 4),
                 "Total time processing was")
})


test_that("test print out doesn't show when set to false", {

  expect_no_message(niche_ellipse(dat_mu = mu_est_long,
                                  dat_sigma = sigma_est_wide,
                                  set_seed = 4,
                                  message = FALSE)
  )
})
