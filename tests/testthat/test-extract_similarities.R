
# library(testthat)
# library(nichetools)
# library(purrr)
# library(dplyr)
library(SIBER)

demo.siber.data.2$group_name <- as.factor(demo.siber.data.2$group)

demo.siber.data.2$group <- as.numeric(demo.siber.data.2$group_name) |>
  as.character()

demo.siber.data.2$community_name <- as.factor(demo.siber.data.2$community)

demo.siber.data.2$community <- as.numeric(demo.siber.data.2$community_name) |>
  as.character()

cg_names <- demo.siber.data.2 |>
  dplyr::distinct(community, group, community_name, group_name)


demo_siber_data <- demo.siber.data.2 |>
  dplyr::select(iso1:community)

siber_example <- createSiberObject(demo_siber_data)
#
#
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
#
# # ---- fit ellipse -----
# # fit the ellipses which uses an Inverse Wishart prior
# # on the covariance matrix Sigma, and a vague normal prior on the
# # means. Fitting is via the JAGS method.
# ellipses_posterior <- siberMVN(siber_example, parms_1, priors_1)


# ---- create comparsions ----
cg_names_within_com <- create_comparisons(cg_names,
                                          comparison = "within")



ml_within_overlap <- cg_names_within_com %>%
  map(~ maxLikOverlap(.x$cg_1, .x$cg_2, siber_example,
                      p.interval = 0.95, n = 100))

bayes95_overlap <- cg_names_within_com %>%
  map(~ bayesianOverlap(.x$cg_1, .x$cg_2, post_sam_siber,
                        draws = 100, p.interval = 0.95,
                        n = 100)
  )


# Test cases
test_that("extract_similarities works for valid 'ml' type", {
  result <- extract_similarities(ml_within_overlap,
                                 type = "ml",
                                 community_df = cg_names)

  expect_true(is.data.frame(result))
  expect_true(all(c("community_id_1", "group_id_1", "community_1", "group_1",
                    "community_id_2", "group_id_2", "community_2", "group_2",
                    "area_1", "area_2", "prop_overlap") %in% colnames(result)))
  expect_equal(result$prop_overlap[1],
               (result$overlap[1] / (result$area_1[1] +
                                       result$area_2[1] - result$overlap[1]))
  ) # Check prop_overlap calculation
})
# 28.6 + 33.1 - 5.55e-17

test_that("extract_similarities works for valid 'bay' type", {
  result <-  extract_similarities(bayes95_overlap,
                                  type = "bay",
                                  community_df = cg_names)

  expect_true(is.data.frame(result))
  expect_true(all(c("community_id_1", "group_id_1", "community_1", "group_1",
                    "community_id_2", "group_id_2", "community_2", "group_2",
                    "area_1", "area_2", "prop_overlap") %in% colnames(result)))
  expect_equal(result$prop_overlap[1],
               (result$overlap[1] / (result$area_1[1] +
                                       result$area_2[1] - result$overlap[1]))
  )
  # Check prop_overlap calculation
})


test_that("extract_similarities fails with invalid 'type'", {
  expect_error(
    extract_similarities(ml_within_overlap,
                         type = "invalid",
                         community_df = community_df_example),
    regexp = "Invalid characters for 'type'. Allowed character strings are 'bay' or 'ml'.")
})

test_that("extract_similarities fails with invalid community_df", {
  # Incorrect number of columns
  invalid_df <- data.frame(community = c("comm1"),
                           group = c("group1"),
                           extra = c("extra"))
  expect_error(
    extract_similarities(ml_within_overlap,
                         type = "ml",
                         community_df = invalid_df),
    "The `community_df` argument must be a data.frame with exactly four columns.")

  # Missing required columns
  invalid_df2 <- data.frame(community = c("comm1"),
                            group = c("group1"),
                            community_name = c("Community 1"),
                            extra = c("extra"))
  expect_error(
    extract_similarities(
      ml_within_overlap,
      type = "ml",
      community_df = invalid_df2),
    "The data frame does not contain a column named 'community', 'group', 'community_name', or 'group_name'.")
})

test_that("extract_similarities doesn't work with NULL community_df", {
  # If community_df is NULL, function should still run
  expect_error(extract_similarities(
    ml_within_overlap,
    type = "ml",
    community_df = NULL),
    regexp = "The `community_df` argument needs to be supplied")

})
