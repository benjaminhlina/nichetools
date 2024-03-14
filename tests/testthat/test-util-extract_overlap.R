
test_that("Test if data  is list", {
  df_over <- extract_overlap(
    data = over_stat
  )

  expect_type(object = df_over, type = "list")

})
test_that("error if data isn't a array", {
  dat <- data.frame(
    x = seq(0, 100, length.out = 10),
    y = seq(0, 0.1, length.out = 10)
  )

  expect_error(
    extract_overlap(
      data = dat
    ), regexp = "Input 'data' must be an array."
  )

})

test_that("test that the object type and length are correct ", {
  df_overlap_test <- extract_overlap(
    data = over_stat
  )
  # check the type returned data frame should be data frame
  expect_s3_class(object = df_overlap_test, class =  "data.frame")
  # excreted dimensions
  expected_rows <- 16000
  expected_cols <- 5


  # Check the dimensions using expect_equal
  expect_equal(nrow(df_overlap_test), expected_rows,
               info = "Number of rows is not as expected.")
  expect_equal(ncol(df_overlap_test), expected_cols,
               info = "Number of columns is not as expected.")
})
test_that("test that the object type and length are correct ", {
  df_overlap_test <- extract_overlap(
    data = over_stat,
  )
  # check the type returned data frame should be data frame
  expect_s3_class(object = df_overlap_test, class =  "data.frame")
  # excreted dimensions
  expected_rows <- 16000
  expected_cols <- 5


  # Check the dimensions using expect_equal
  expect_equal(nrow(df_overlap_test), expected_rows,
               info = "Number of rows is not as expected.")
  expect_equal(ncol(df_overlap_test), expected_cols,
               info = "Number of columns is not as expected.")
})

test_that("Check if column names extracted are correct", {

  expected_names <- c("sample_name_a", "id",
  "sample_name_b", "sample_number", "niche_overlap")

  df_overlap_test_1 <- extract_overlap(
    data = over_stat
  )

  expect_equal(names(df_overlap_test_1), expected_names)
})



test_that("Check if supplying sample_name_a  works", {
  expect_no_error(df <- extract_overlap(
    data = over_stat,
    name_a = "species_a"
  )
  )
  expect_type(object = df$species_a, type = "character")

}
)

test_that("Check if supplying sample_name_b  works", {
  expect_no_error(df <- extract_overlap(
    data = over_stat,
    name_b = "species_b"
  )
  )
  expect_type(object = df$species_b, type = "character")

}
)


test_that("that supplying both isotope names works ", {
  expect_no_error(df <- extract_overlap(
    data = over_stat,
    name_a = "species_a",
    name_b = "species_b"

  )
  )
  expect_type(object = df$species_a, type = "character")
  expect_type(object = df$species_b, type = "character")

}
)
test_that("that isotope a and b will throw erros if charcter not supplied", {
  expect_error(df <- extract_overlap(
    data = over_stat,
    name_a = 10,

  ), regexp = "The supplied argument for 'name_a' must be a character."
  )
  expect_error(df <- extract_overlap(
    data = over_stat,
    name_b = 10,

  ), regexp = "The supplied argument for 'name_b' must be a character"
  )

}
)
