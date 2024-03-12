
test_that("Test if data object is array", {
  df_over <- overlap_extract(
    data = over_stat
  )

  expect_type(object = df_over, type = "array")

})
