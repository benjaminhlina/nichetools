

# ---- fish data frame from ----

test_that("check if no error are wrong", {
  expect_error(
    niche_ellipse(
      dat_mu = mu_est_long,
      dat_sigma = sigma_est_wide
    )
)
})

test_that("check if it will take new names ", {

  # sigma_est_wide$a <- sigma_est_wide$d15n
  # sigma_est_wide$d15n <- NULL
  # sigma_est_wide$b <- sigma_est_wide$d13c
  #
  #
  # expect_error(
  #   niche_ellipse(
  #   dat_mu = mu_est_long,
  #   dat_sigma = sigma_est_wide,
  #   isotope_a = a,
  #   isotope_b = b
  # )
  # )
})

