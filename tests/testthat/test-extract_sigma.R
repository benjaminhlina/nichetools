

# unit test for extract_sigma
test_that("test if it doesn't error with basic niw object ", {
  expect_no_error(
    df_sigma_test <- extract_sigma(
      data = niw_fish_post
    )

  )
})
test_that("test if it wide-long 't error with basic niw object ", {
  expect_error(extract_sigma(
    data = niw_fish_post,
    data_format = "x"
  ), "Invalid characters for 'data_format'. Allowed character strings are 'wide' or 'long'.")


})
test_that("test if pkg 't error with basic niw object ", {
  expect_error(extract_sigma(
    data = niw_fish_post,
    pkg = "nicherover"
  ), "Invalid characters for 'pkg'. Allowed character strings are 'nicheROVER' or 'SIBER'.")


})
test_that("test if list", {
  expect_no_error(
    df_sigma_test <- extract_sigma(
      data = niw_fish_post,
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
    extract_sigma(
      data = dat
    ), regexp = "Input 'data' must be a list."
  )

})


test_that("test that the object type and length are correct ", {
  df_sigma_test <- extract_sigma(
    data = niw_fish_post,
    data_format = "long"
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


test_that("test that the object type and length are correct ", {
  df_sigma_test <- extract_sigma(
    data = niw_fish_post
  )
  # check the type returned data frame should be data frame
  expect_s3_class(object = df_sigma_test, class =  "data.frame")
  # excreted dimensions
  expected_rows <- 8000
  expected_cols <- 6


  # Check the dimensions using expect_equal
  expect_equal(nrow(df_sigma_test), expected_rows,
               info = "Number of rows is not as expected.")
  expect_equal(ncol(df_sigma_test), expected_cols,
               info = "Number of columns is not as expected.")
})



test_that("that supplying both isotope names works ", {
  df <- extract_sigma(
    data = niw_fish_post,
    isotope_names = c("cal_d13c","cal_d15n"),
    data_format = "long"

  )
  expect_type(object = df$id, type = "character")

  expect_match(object = df$id, regexp = "cal_d13c", all = FALSE)
  expect_match(object = df$id, regexp = "cal_d15n", all = FALSE)

}
)
test_that("that isotope a and b will throw erros if charcter not supplied", {
  expect_error(df <- extract_sigma(
    data = niw_fish_post,
    isotope_names = 10,

  ), regexp = "The supplied argument for 'isotope_names' must be a vector of characters"
  )
  expect_error(df <- extract_sigma(
    data = niw_fish_post,
    isotope_names = c(10, 11),

  ), regexp = "The supplied argument for 'isotope_names' must be a vector of characters."
  )

  expect_error(df <- extract_sigma(
    data = niw_fish_post,
    isotope_names = c("10"),

  ), regexp = "The 'isotope_names' vector must have exactly 2 elements, representing isotope_a and isotope_b."
  )

}
)

df <- nicheROVER::fish %>%
  janitor::clean_names()

## ----message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
nsample <- 1000

## ----message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
fish_par <- df %>%
  split(.$species) %>%
  map(~ select(., d13c, d15n, d34s)) %>%
  map(~ nicheROVER::niw.post(nsample = nsample, X = .))

test_that("test isotope_n to be 3", {
  ### test isotope_n
  expect_no_error(
    extract_sigma(
      data = fish_par,
      isotope_n = 3,
    )
  )

}
)

test_that("test error when isotope_n = 2 or 3", {

  expect_error(
    extract_sigma(
      data = fish_par,
      isotope_n = 2
    ),
    regex = "Argument 'isotope_n' does not match the number of isotopes being used."
  )
}
)
test_that("test error when isotope_n = 3 but names do not work", {

  expect_error(
    extract_sigma(
      data = fish_par,
      isotope_n = 3,
      isotope_names = c("d_13c", "d_15n")
    ),
    regex = "The 'isotope_names' vector must have exactly 3 elements, representing isotope_a, isotope_b, and isotope_c."
  )

  expect_error(
    extract_sigma(
      data = niw_fish_post,
      isotope_n = 3,
      isotope_names = c(1)
    ), "The supplied argument for 'isotope_names' must be a vector of characters."
  )
}
)
test_that("test error when isotope_n = 3 but names do not work", {

  expect_error(
    extract_sigma(
      data = niw_fish_post,
      isotope_n = 2,
      isotope_names = c("d_13c")
    ),
    regex = "The 'isotope_names' vector must have exactly 2 elements, representing isotope_a and isotope_b."
  )
  expect_error(
    extract_sigma(
      data = niw_fish_post,
      isotope_n = 2,
      isotope_names = c(1)
    ), "The supplied argument for 'isotope_names' must be a vector of characters."
  )

}
)



test_that("test error when isotope_n = 2 or 3", {

  expect_error(
    extract_sigma(
      data = niw_fish_post,
      isotope_n = 3
    ),
    regex = "Argument 'isotope_n' does not match the number of isotopes being used."
  )
}
)
test_that("test error when isotope_n = 2 or 3", {

  expect_error(
    extract_sigma(
      data = niw_fish_post,
      isotope_n = 4
    ),
    regex = "Argument 'isotope_n' must be a numeric value and either 2 or 3"
  )
}
)







test_that("test if it doesn't error with basic siber object ", {
  expect_no_error(
    df_sigma_test <- extract_sigma(
      data = post_sam_siber,
      pkg = "SIBER"
    )

  )
})
test_that("test if list", {
  expect_no_error(
    df_sigma_test <- extract_sigma(
      data = post_sam_siber,
      pkg = "SIBER"
    )

  )
  expect_type(object = df_sigma_test, type = "list")
})




test_that("test if list", {
  expect_error(extract_sigma(
      data = post_sam_siber,
      pkg = "SIBER",
      isotope_names = 1
    ), "The supplied argument for 'isotope_names' must be a vector of characters."
    )
  expect_error(extract_sigma(
      data = post_sam_siber,
      pkg = "SIBER",
      isotope_names = c("d13c")
    ), "The 'isotope_names' vector must have exactly 2 elements, representing isotope_a and isotope_b."
    )


})
test_that("error if data isn't a list", {
  dat <- data.frame(
    x = seq(0, 100, length.out = 10),
    y = seq(0, 0.1, length.out = 10)
  )

  expect_error(
    extract_sigma(
      data = dat,
      pkg = "SIBER"
    ), regexp = "Input 'data' must be a list."
  )

})



test_that("test that the object type and length are siber correct ", {
  df_sigma_test <- extract_sigma(
    data = post_sam_siber,
    pkg = "SIBER"
  )
  # check the type returned data frame should be data frame
  expect_s3_class(object = df_sigma_test, class =  "data.frame")
  # excreted dimensions
  expected_rows <- 40000
  expected_cols <- 6


  # Check the dimensions using expect_equal
  expect_equal(nrow(df_sigma_test), expected_rows,
               info = "Number of rows is not as expected.")
  expect_equal(ncol(df_sigma_test), expected_cols,
               info = "Number of columns is not as expected.")
})


test_that("test that the object type and length are correct ", {
  df_sigma_test <- extract_sigma(
    data = post_sam_siber,
    pkg = "SIBER",
    data_format = "long"
  )
  # check the type returned data frame should be data frame
  expect_s3_class(object = df_sigma_test, class =  "data.frame")
  # excreted dimensions
  expected_rows <- 80000
  expected_cols <- 6


  # Check the dimensions using expect_equal
  expect_equal(nrow(df_sigma_test), expected_rows,
               info = "Number of rows is not as expected.")
  expect_equal(ncol(df_sigma_test), expected_cols,
               info = "Number of columns is not as expected.")
})



test_that("that supplying both isotope names works ", {
  df <- extract_sigma(
    data = post_sam_siber,
    pkg = "SIBER",
    isotope_names = c("cal_d15n", "cal_d13c"),
    data_format = "long"

  )
  expect_type(object = df$id, type = "character")

  expect_match(object = df$id, regexp = "cal_d13c", all = FALSE)
  expect_match(object = df$id, regexp = "cal_d15n", all = FALSE)

}
)





