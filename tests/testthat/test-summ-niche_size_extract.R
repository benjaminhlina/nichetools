
test_that("test if list", {
  expect_no_error(
    test_1 <- niche_size_extract(
      data = niw_fish_post
    )

  )
  expect_type(object = test_1, type = "list")
})

test_that("error if data isn't a list", {
  dat <- data.frame(
    x = seq(0, 100, length.out = 10),
    y = seq(0, 0.1, length.out = 10)
  )

  expect_error(
    niche_size_extract(
      data = dat
    ), regexp = "Input 'data' must be a list."
  )
})




test_that("Parameter 'prob' is can take other values than 0.95", {
  # Test case 1: p_ell is 0.75,
  expect_no_error(
    niche_size_test <- niche_size_extract(
      data = niw_fish_post,
      prob = 0.75
    )
  )
  expect_equal(niche_size_test$niche_size[1], 1.6, tolerance = 0.1)
})
test_that("Parameter 'prob' is set and validated correctly", {
  # Test case 1: p_ell is NULL, it should be set to 0.95
  expect_no_error(
    niche_size_test <- niche_size_extract(
      data = niw_fish_post
    )
  )
  expect_equal(niche_size_test$niche_size[1], 2.8, tolerance = 0.1)
})
