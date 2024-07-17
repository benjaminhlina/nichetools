library(SIBER)
library(nichetools)
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
