
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
