# extract similarities

Extract niche similarities from objects created by `{SIBER}`.

## Usage

``` r
extract_similarities(data, type = c("bay", "ml"), community_df = NULL)
```

## Arguments

- data:

  a `list` of results from either
  [`maxLikOverlap()`](https://rdrr.io/pkg/SIBER/man/maxLikOverlap.html)
  or
  [`bayesianOverlap()`](https://rdrr.io/pkg/SIBER/man/bayesianOverlap.html).

- type:

  a `character` that is either `"bay"` or `"ml"` which indicates whether
  the community metrics to be extracted are from a Bayesian analysis or
  a maximum-likelihood.

- community_df:

  a four column data frame. One of the columns has to be named
  `community` and the data in the column will be `numeric` as a
  `character` string(e.g., `"1", "2", "3"`). This is the order of the
  community names and will be used to join the actual community names to
  the correct data. These are the same class and values required by the
  function,
  [`createSiberObject()`](https://rdrr.io/pkg/SIBER/man/createSiberObject.html)
  from [SIBER](https://CRAN.R-project.org/package=SIBER). The second
  column will be the names of the groups that are needed to supply
  required by the function,
  [`createSiberObject()`](https://rdrr.io/pkg/SIBER/man/createSiberObject.html)
  from [SIBER](https://CRAN.R-project.org/package=SIBER). The third and
  fourth columns contains the actual names of the communities and groups
  the user is working with (e.g., `"region"`, `"common_name"`).

## Examples

``` r
library(purrr)
library(SIBER)

# ---- create community names data frame ----
# uncomment to use
# str(demo.siber.data.2)

demo.siber.data.2$group_name <- as.factor(demo.siber.data.2$group)

demo.siber.data.2$group <- as.numeric(demo.siber.data.2$group_name) |>
as.character()

demo.siber.data.2$community_name <- as.factor(demo.siber.data.2$community)

demo.siber.data.2$community <- as.numeric(demo.siber.data.2$community_name) |>
as.character()

cg_name <- demo.siber.data.2 |>
dplyr::distinct(community, group, community_name, group_name)

# ---- create comparsions ----
cg_names_within_c <- create_comparisons(cg_name,
                                        comparison = "within")

demo.siber.data.2 <- demo.siber.data.2[,1:4]

siber_example <- createSiberObject(demo.siber.data.2)

ml_within_overlap <- cg_names_within_c |>
map(~ maxLikOverlap(.x$cg_1, .x$cg_2, siber_example,
p.interval = NULL, n = 100), .progress = TRUE)

ml_95_within_com <- extract_similarities(ml_within_overlap, type = "ml",
community_df = cg_name)
```
