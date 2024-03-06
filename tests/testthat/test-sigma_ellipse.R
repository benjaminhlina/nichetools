

# ---- fish data frame from ----

test_that("check if names are wrong", {
  expect_error(
    sigma_ellipse(
      dat_mu = mu_est_long,
      dat_sigma = sigma_est_wide,
    )
)
})

