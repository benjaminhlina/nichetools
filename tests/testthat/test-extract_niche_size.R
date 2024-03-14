
test_that("test if list", {
  expect_no_error(
    test_1 <- extract_niche_size(
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
    extract_niche_size(
      data = dat
    ), regexp = "Input 'data' must be a list."
  )
})




test_that("Parameter 'prob' is can take other values than 0.95", {
  # Test case 1: p_ell is 0.75,
  expect_no_error(
    niche_size_test <- extract_niche_size(
      data = niw_fish_post,
      prob = 0.75
    )
  )
  expect_equal(niche_size_test$niche_size[1], 1.6, tolerance = 0.1)
})
test_that("Parameter 'prob' is set and validated correctly", {
  # Test case 1: p_ell is NULL, it should be set to 0.95
  expect_no_error(
    niche_size_test <- extract_niche_size(
      data = niw_fish_post
    )
  )
  expect_equal(niche_size_test$niche_size[1], 2.8, tolerance = 0.1)
})


test_that("Check if column names extracted are correct", {

  expected_names <- c("sample_name", "id", "niche_size")

  test_2 <- extract_niche_size(
    data = niw_fish_post
  )

  expect_equal(names(test_2), expected_names)
})


test_that("prob errors when given a charcter  or value outside of range", {

  # Test case 3: prob is not numeric, it should raise an error
  expect_error(extract_niche_size(
    data = niw_fish_post, prob = "invalid"),
    "Parameter 'prob' must be a numeric value between 0 and 1.")

  # Test case 4: prob is numeric but outside the valid range,
  expect_error(extract_niche_size(
    data = niw_fish_post, prob = 1.5),
    "Parameter 'prob' must be a numeric value between 0 and 1.")
}
)


test_that("if name errors if given number", {
  expect_error(extract_niche_size(
    data = niw_fish_post,
    name = 1
  ), "Argument 'name' must be a character.")


})

test_that("output data is the correct size and class", {

  test_3 <- extract_niche_size(
    data = niw_fish_post
  )

  # check the type returned data frame should be data frame
  expect_s3_class(object = test_3, class =  "data.frame")
  # excreted dimensions
  expected_rows <- 16000
  expected_cols <- 3


  # Check the dimensions using expect_equal
  expect_equal(nrow(test_3), expected_rows,
               info = "Number of rows is not as expected.")
  expect_equal(ncol(test_3), expected_cols,
               info = "Number of columns is not as expected.")
})
