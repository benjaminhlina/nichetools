library(SIBER)
# library(testthat)
# library(nichetools)
# create the siber object
# str(demo.siber.data.2)

demo.siber.data.2$community_names <- as.factor(demo.siber.data.2$community)
demo.siber.data.2$community <- as.numeric(demo.siber.data.2$community_names) |>
  as.character()
demo.siber.data.2$group_names <- as.factor(demo.siber.data.2$group)
demo.siber.data.2$group <- as.numeric(demo.siber.data.2$group_names) |>
  as.character()

cg_names <- demo.siber.data.2 |>
  dplyr::distinct(community, group, community_names, group_names)
demo.siber.data.1 <- demo.siber.data.2[, 1:4]


siber_example <- createSiberObject(demo.siber.data.1)

group_ml <- groupMetricsML(siber_example)
# unit test for extract_sigma
test_that("test if it doesn't error with basic siber object ", {

  expect_no_error(
    group_extract <- extract_group_metrics(
      data = group_ml,
      community_df = cg_names
    )

  )
})
# library(testthat)
# library(dplyr)
# library(tidyr)
# library(tibble)
# library(cli)
#
# # Sample data for testing
# data_matrix <- matrix(
#   c(1, 2, 3, 4, 5, 6),
#   nrow = 2,
#   dimnames = list(
#     c("metric1", "metric2"),
#     c("A.G1", "B.G2", "C.G3")
#   )
# )
#
# community_df <- data.frame(
#   community = c("A", "B", "C"),
#   group = c("G1", "G2", "G3"),
#   col3 = c(1, 2, 3),
#   col4 = c(4, 5, 6)
# )



# Test 2: Valid matrix and community_df with 'wide' format
test_that("extract_group_metrics works with valid matrix and community_df in 'wide' format", {
  result <- extract_group_metrics(data = group_ml,
                                  community_df = cg_names,
                                  data_format = "wide"
  )

  # check the type returned data frame should be data frame
  expect_s3_class(object = result, class =  "data.frame")
  # excreted dimensions
  expected_rows <- 5
  expected_cols <- 7


  # Check the dimensions using expect_equal
  expect_equal(nrow(result), expected_rows,
               info = "Number of rows is not as expected.")
  expect_equal(ncol(result), expected_cols,
               info = "Number of columns is not as expected.")
  # Check for class type (as it's pivoted to wide format)
  # expect_true(is.data.frame(result))


  # expected_names <- c("community", "group", "SEA", "SEAc", "TA")
  #
  # expect_equal(names(result), expected_names)


})

# Test 3: Invalid data type (not a matrix)
test_that("extract_group_metrics throws an error if data is not a matrix", {
  expect_error(extract_group_metrics(data = data.frame(A = 1, B = 2)),
               "The `data` argument must be a matrix.")
})

# Test 4: Invalid community_df (not a data.frame or not 4 columns)
test_that("extract_group_metrics throws an error if community_df is not a valid data.frame", {
  invalid_community_df <- data.frame(community = c("A", "B"), group = c("G1", "G2"))
  expect_error(extract_group_metrics(data = group_ml,
                                     community_df = invalid_community_df),
               "The `community_df` argument must be a data.frame with exactly four columns.")
})

# Test 5: Missing 'community' or 'group' column in community_df
test_that("extract_group_metrics throws an error if community_df isn't 4 columns", {
  invalid_community_df <- data.frame(
    col2 = c("A", "B", "C"),
    col3 = c(1, 2, 3),
    col4 = c(4, 5, 6)
  )

  expect_error(
    extract_group_metrics(data = group_ml,
                          community_df = invalid_community_df),
    regexp = "The `community_df` argument must be a data.frame with exactly four columns.")

})
test_that("extract_group_metrics throws an error if community_df is missing 'community' or 'group' columns", {


  invalid_community_df_2 <- data.frame(
    col2 = c("A", "B", "C"),
    col3 = c(1, 2, 3),
    col4 = c(4, 5, 6),
    col5 = c(4, 6, 3)
  )


  expect_error(
    extract_group_metrics(data = group_ml,
                          community_df = invalid_community_df_2),
    regexp = "The data frame does not contain a column named 'community' and 'group'.")
})

# Test 6: Invalid data_format value
test_that("extract_group_metrics throws an error if data_format is not 'wide' or 'long'", {
  expect_error(
    extract_group_metrics(data = group_ml,
                          community_df = cg_names,
                          data_format = "invalid"),
    regexp = "Invalid characters for 'data_format'. Allowed character strings are 'wide' or 'long'.")
})

# # Test 7: Null community_df
# test_that("extract_group_metrics works when community_df is NULL", {
#   result <- extract_group_metrics(data = data_matrix, community_df = NULL)
#   expect_true(is.data.frame(result))
# })

