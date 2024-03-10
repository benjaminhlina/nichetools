

# ---- fish data frame from ----

test_that("test if isotope names null ", {
  expect_no_error(
    sigma_extract(
      data = niw_fish_post
    )
  )
})

