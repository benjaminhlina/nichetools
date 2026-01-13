# extract maximum-likelihood estimates for group metrics

Extract group metrics within each community from a matrix object that is
produced by
[`groupMetricsML()`](https://rdrr.io/pkg/SIBER/man/groupMetricsML.html)
function from [SIBER](https://CRAN.R-project.org/package=SIBER). These
metrics are the following the convex hull total area (TA), Standard
Ellipse Area (SEA), and the corresponding small sample size corrected
version SEAc based on the maximum likelihood estimates of the means and
covariance matrices of each group.

## Usage

``` r
extract_group_metrics(data = NULL, community_df = NULL, data_format = NULL)
```

## Arguments

- data:

  a `matrix` produced by the function
  [`groupMetricsML()`](https://rdrr.io/pkg/SIBER/man/groupMetricsML.html)
  in the package [SIBER](https://CRAN.R-project.org/package=SIBER).

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

- data_format:

  a `character` string that decides whether the returned object is in
  long or wide format. Default is `"long"`, with the alternative
  supplied being `"wide"`.

## Value

A `tibble` containing four rows when `data_format` is set to its default
which is `long`. These four rows are the following, `community`,
`the_name_of_the_communities`, `metric` and `post_est`.

## Examples

``` r
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

demo.siber.data.2 <- demo.siber.data.2[,1:4]

siber_example <- createSiberObject(demo.siber.data.2)

# extract group metrics
group_ml <- groupMetricsML(siber_example)

group_convert <- extract_group_metrics(data = group_ml,
                                community_df = cg_name)
```
