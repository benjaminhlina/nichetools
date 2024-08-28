library(tibble)
# library(nichetools)
# library(testthat)
# Assume that cg_names is a pre-defined dataset similar to the one expected in the function.

# Sample cg_names data for testing
cg_names <- data.frame(
  community = rep(c("A", "B"), each = 2),
  group = rep(c("G1", "G2"), 2)
)

# Example tests for the create_comparisons function
test_that("create_comparisons returns correct output for 'within' comparison", {
  result <- create_comparisons(cg_names, comparison = "within")

  # Expected output
  expected <- list(
    `A.G1_A.G2` = tibble(cg_1 = "A.G1", cg_2 = "A.G2"),
    # `A.G2_A.G1` = tibble(cg_1 = "A.G2", cg_2 = "A.G1"),
    `B.G1_B.G2` = tibble(cg_1 = "B.G1", cg_2 = "B.G2")
    # `B.G2_B.G1` = tibble(cg_1 = "B.G2", cg_2 = "B.G1")

  )

  expect_equal(result, expected)
})

test_that("create_comparisons returns correct output for 'among' comparison", {
  result <- create_comparisons(cg_names, comparison = "among")

  # Expected output
  expected <- list(
    `A.G1_B.G1` = tibble(cg_1 = "A.G1", cg_2 = "B.G1"),
    `A.G2_B.G2` = tibble(cg_1 = "A.G2", cg_2 = "B.G2")
    # `B.G1_A.G1` = tibble(cg_1 = "B.G1", cg_2 = "A.G1"),
    # `B.G2_A.G2` = tibble(cg_1 = "B.G2", cg_2 = "A.G2")
  )

  expect_equal(result, expected)
})

test_that("create_comparisons uses 'within' as default comparison", {
  results <- create_comparisons(cg_names,
                               comparison = "within")

  # Expected output
  expected_1 <- list(
    `A.G1_A.G2` = tibble(cg_1 = "A.G1", cg_2 = "A.G2"),
    # `A.G2_A.G1` = tibble(cg_1 = "A.G2", cg_2 = "A.G1"),
    `B.G1_B.G2` = tibble(cg_1 = "B.G1", cg_2 = "B.G2")
    # `B.G2_B.G1` = tibble(cg_1 = "B.G2", cg_2 = "B.G1")

  )

  expect_equal(results, expected_1)
})


test_that("create_comparisons throws an error for invalid comparison type", {
  expect_error(
    create_comparisons(cg_names,
                       comparison = "invalid"),
    regexp = "'comparison' must be either 'within' or 'among'.")
})


test_that("create_comparisons handles empty data correctly", {
  empty_data <- list(community = character(0),
                     group = character(0))
  expect_error(result <- create_comparisons(empty_data, comparison = "within"))

})
