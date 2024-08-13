library(SIBER)

# create the siber object
# str(demo.siber.data.2)

demo.siber.data.2$community_names <- as.factor(demo.siber.data.2$community)
demo.siber.data.2$community <- as.numeric(demo.siber.data.2$community_names) |>
  as.character()

c_names <- demo.siber.data.2 |>
  dplyr::distinct(community, community_names)

demo_siber_data <- demo.siber.data.2 |>
  dplyr::select(iso1:community)
siber_example <- createSiberObject(demo_siber_data)

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


# Test if data is not a list
test_that("data is not a list", {
  data_not_list <- 1:5
  expect_error(extract_layman(data_not_list), "The `data` argument must be a list.")
})

# Test if community_df is not a data.frame
test_that("community_df is not a data.frame", {
  data_list <- list(a = 1, b = 2)
  community_df_not_df <- matrix(1:10, nrow = 5, ncol = 2)
  expect_error(
    extract_layman(layman_b, community_df_not_df),
    "The `community_df` argument must be a data.frame.")
})

# Test if community_df does not have 2 columns
test_that("community_df does not have 2 columns", {
  data_list <- list(a = 1, b = 2)
  community_df_not_2_columns <- data.frame(x = 1:5)
  expect_error(
  extract_layman(layman_b, community_df_not_2_columns),
              "The `community_df` argument must be a data.frame with exactly two columns.")
})


# test if lenght of dataframe is correct for wide and long
test_that("test if lenght of wide and long  siber object ", {

    df_laymen <- extract_layman(
      data = layman_b, community_df = c_names
    )
    # check the type returned data frame should be data frame
    expect_s3_class(object = df_laymen, class =  "data.frame")
    # excreted dimensions
    expected_rows <- 48000
    expected_cols <- 5


    # Check the dimensions using expect_equal
    expect_equal(nrow(df_laymen), expected_rows,
                 info = "Number of rows is not as expected.")
    expect_equal(ncol(df_laymen), expected_cols,
                 info = "Number of columns is not as expected.")
})
# test if lenght of dataframe is correct for wide and long
test_that("test if lenght of wide and long  siber object ", {

    df_laymen <- extract_layman(
      data = layman_b, community_df = c_names, data_format = "wide"
    )
    # check the type returned data frame should be data frame
    expect_s3_class(object = df_laymen, class =  "data.frame")
    # excreted dimensions
    expected_rows <- 8000
    expected_cols <- 8


    # Check the dimensions using expect_equal
    expect_equal(nrow(df_laymen), expected_rows,
                 info = "Number of rows is not as expected.")
    expect_equal(ncol(df_laymen), expected_cols,
                 info = "Number of columns is not as expected.")
})



