
test_that("test if list", {
  expect_no_error(
    test_1 <- extract_niche_size(
      data = niw_fish_post
    )

  )
  expect_type(object = test_1, type = "list")
})

test_that("error if data isn't A LIST", {
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
      prob = 0.1
    )
  )
  expect_equal(niche_size_test$niche_size[1], 0.46, tolerance = 0.1)
})
test_that("Parameter 'prob' is set and validated correctly", {
  # Test case 1: p_ell is NULL, it should be set to 0.95
  expect_no_error(
    niche_size_test <- extract_niche_size(
      data = niw_fish_post
    )
  )
  expect_equal(niche_size_test$niche_size[1], 13.2, tolerance = 0.1)
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
  expected_rows <- 4000
  expected_cols <- 3


  # Check the dimensions using expect_equal
  expect_equal(nrow(test_3), expected_rows,
               info = "Number of rows is not as expected.")
  expect_equal(ncol(test_3), expected_cols,
               info = "Number of columns is not as expected.")
})


# ----- siber ----
library(SIBER)

# create the siber object
# str(demo.siber.data.2)

demo.siber.data.2$community_names <- as.factor(demo.siber.data.2$community)
demo.siber.data.2$community <- as.numeric(demo.siber.data.2$community_names) |>
  as.character()
demo.siber.data.2$group_names <- as.factor(demo.siber.data.2$group)
demo.siber.data.2$group <- as.numeric(demo.siber.data.2$group_name) |>
  as.character()

cg_names <- demo.siber.data.2 |>
  dplyr::distinct(group, community, group_names, community_names)

# demo_siber_data <- demo.siber.data.2 |>
#   dplyr::select(iso1:community)
# siber_example <- createSiberObject(demo_siber_data)
#
# # ---- create priors -----
# # options for running jags
# parms_1 <- list()
# parms_1$n.iter <- 2 * 10^4   # number of iterations to run the model for
# parms_1$n.burnin <- 1 * 10^3 # discard the first set of values
# parms_1$n.thin <- 10     # thin the posterior by this many
# parms_1$n.chains <- 2        # run this many chains
#
# # define the priors
# priors_1 <- list()
# priors_1$R <- 1 * diag(2)
# priors_1$k <- 2
# priors_1$tau.mu <- 1.0E-3

# ---- fit ellipse -----
# fit the ellipses which uses an Inverse Wishart prior
# on the covariance matrix Sigma, and a vague normal prior on the
# means. Fitting is via the JAGS method.
# ellipses_posterior <- siberMVN(post_sam_siber, parms_1, priors_1)


sea_b <- siberEllipses(corrected.posteriors = post_sam_siber)

test_that("output data is the correct size, class, and column names are valid", {

  test_3 <- extract_niche_size(
    data = sea_b,
    pkg = "SIBER",
    community_df = cg_names
  )

  # Check that column names are not NA or empty strings
  col_names <- colnames(test_3)

  expect_true(all(!is.na(col_names)), info = "There are NA values in column names.")
  expect_true(all(col_names != ""), info = "There are empty strings in column names.")
  # Check that the "ID" column exists
  expect_true("id" %in% colnames(test_3), info = "The 'id' column is missing in the output data.")

})
test_that("output pkg is correctly selected", {

  expect_error(extract_niche_size(
    data = sea_b,
    pkg = "siber",
    community_df = cg_names
  ))
})


test_that("error if data isn't A LIST", {
  dat <- data.frame(
    x = seq(0, 100, length.out = 10),
    y = seq(0, 0.1, length.out = 10)
  )

  expect_error(
    extract_niche_size(
      data = dat,
      pkg = "SIBER"
    ), regexp = "Input 'data' must be a matrix."
  )
})


test_that("output coilumn names", {

  test_6 <- extract_niche_size(
    data = sea_b,
    pkg = "SIBER",
    community_df = cg_names
  )

  col_names <- colnames(test_6)

  expect_true(any(col_names %in% c("id", "community", "group", "sea"
  )))



}
)
test_that("output coilumn number", {

  test_7 <- extract_niche_size(
    data = sea_b,
    pkg = "SIBER",
    community_df = cg_names
  )

  expected_cols <- 6
  expected_rows <- 20000

  # Check the dimensions using expect_equal
  expect_equal(nrow(test_7), expected_rows,
               info = "Number of rows is not as expected.")
  expect_equal(ncol(test_7), expected_cols,
               info = "Number of columns is not as expected.")
})


# Test 4: Invalid community_df (not a data.frame or not 4 columns)
test_that("extract_niche_size throws an error if community_df is not a valid data.frame", {
  invalid_community_df <- data.frame(community = c("A", "B"), group = c("G1", "G2"))
  expect_error(extract_niche_size(data = sea_b,
                                  pkg = "SIBER",
                                  community_df = invalid_community_df),
               "The `community_df` argument must be a data.frame with exactly four columns.")
})

# Test 5: Missing 'community' or 'group' column in community_df
test_that("extract_niche_size throws an error if community_df isn't 4 columns", {
  invalid_community_df <- data.frame(
    col2 = c("A", "B", "C"),
    col3 = c(1, 2, 3),
    col4 = c(4, 5, 6)
  )

  expect_error(
    extract_niche_size(data = sea_b,
                       pkg = "SIBER",
               community_df = invalid_community_df),
    regexp = "The `community_df` argument must be a data.frame with exactly four columns.")

})
test_that("extract_niche_size throws an error if community_df is missing 'community' or 'group' columns", {


  invalid_community_df_2 <- data.frame(
    col2 = c("A", "B", "C"),
    col3 = c(1, 2, 3),
    col4 = c(4, 5, 6),
    col5 = c(4, 6, 3)
  )


  expect_error(
    extract_niche_size(data = sea_b,
                       pkg = "SIBER",
               community_df = invalid_community_df_2),
    regexp = "The data frame does not contain a column named 'community' and 'group'.")
})
