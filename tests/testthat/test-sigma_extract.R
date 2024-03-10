
# unit test for sigma_extract
test_that("test if it doesn't error with basic niw object ", {
  expect_no_error(
    df_sigma_test <- sigma_extract(
      data = niw_fish_post
    )

  )
})
test_that("test if list", {
  expect_no_error(
    df_sigma_test <- sigma_extract(
      data = niw_fish_post
    )

  )
  expect_type(object = df_sigma_test, type = "list")
})

test_that("error if data isn't a list", {
  dat <- data.frame(
    x = seq(0, 100, length.out = 10),
    y = seq(0, 0.1, length.out = 10)
  )

  expect_error(
    sigma_extract(
      data = dat
    )
  )

})


test_that("test that the object type and length are correct ", {
  df_sigma_test <- sigma_extract(
    data = niw_fish_post
  )
  # check the type returned data frame should be data frame
  expect_s3_class(object = df_sigma_test, class =  "data.frame")
  # excreted dimensions
  expected_rows <- 16000
  expected_cols <- 6


  # Check the dimensions using expect_equal
  expect_equal(nrow(df_sigma_test), expected_rows,
               info = "Number of rows is not as expected.")
  expect_equal(ncol(df_sigma_test), expected_cols,
               info = "Number of columns is not as expected.")
})




test_that("Check if supplying isotope_a names works", {
  expect_no_error(df <- sigma_extract(
    data = niw_fish_post,
    isotope_a = "cal_d15n"
  )
  )

  expect_type(object = df$id, type = "character")

  expect_match(object = df$id, regexp = "cal_d15n", all = FALSE)

}
)

test_that("Check if supplying isotope_b names works", {
  df <- sigma_extract(
    data = niw_fish_post,
    isotope_b = "cal_d13c",

  )
 expect_type(object = df$id, type = "character")

  expect_match(object = df$id, regexp = "cal_d13c", all = FALSE)
}
)
test_that("that supplying both isotope names works ", {
  df <- sigma_extract(
    data = niw_fish_post,
    isotope_a = "cal_d15n",
    isotope_b = "cal_d13c"

  )
 expect_type(object = df$id, type = "character")

  expect_match(object = df$id, regexp = "cal_d13c", all = FALSE)
  expect_match(object = df$id, regexp = "cal_d15n", all = FALSE)

}
)
test_that("that isotope a and b will throw erros if charcter not supplied", {
  expect_error(df <- sigma_extract(
    data = niw_fish_post,
    isotope_a = 10,

  ), regexp = "The supplied argument for 'isotope_a' must be a character."
  )
  expect_error(df <- sigma_extract(
    data = niw_fish_post,
    isotope_b = 10,

  ), regexp = "The supplied argument for 'isotope_b' must be a character"
  )

}
)
