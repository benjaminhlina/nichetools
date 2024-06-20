library(SIBER)

# create the siber object
# str(demo.siber.data.2)

demo.siber.data.2$community_names <- as.factor(demo.siber.data.2$community)
demo.siber.data.2$community <- as.numeric(demo.siber.data.2$community_names) |>
  as.character()

c_names <- demo.siber.data.2 |>
  dplyr::distinct(community, community_names)

siber_example <- createSiberObject(demo.siber.data)

mu_post <- extractPosteriorMeans(siber_example, post_sam_siber)

# ---- layman metrics ----

layman_b <- bayesianLayman(mu.post = mu_post)

# unit test for extract_sigma
test_that("test if it doesn't error with basic siber object ", {



  expect_no_error(
    df_laymen <- extract_layman(
      data = layman_b, community_df = c_names
    )

  )
})
