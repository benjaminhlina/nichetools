# extract Layman metrics

Extract Bayesian estimates for the following six layman metrics,
\\\delta^{13}\\C range, \\\delta^{15}\\N range, total area (TA),
distance to centroid (CD), distance to the nearest neighbour (NND), and
the standard deviation of the distance to the nearest neighbour (SDNND)
from data objects created by
[SIBER](https://CRAN.R-project.org/package=SIBER). To learn more about
the following metrics please review [Layman et al.
(2008)](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1890/0012-9658%282007%2988%5B42%3ACSIRPF%5D2.0.CO%3B2).

## Usage

``` r
extract_layman(
  data,
  type = NULL,
  community_df = NULL,
  data_format = NULL,
  isotope_x = NULL,
  isotope_y = NULL,
  element_x = NULL,
  element_y = NULL
)
```

## Arguments

- data:

  a `list` created by the function
  [`bayesianLayman()`](https://rdrr.io/pkg/SIBER/man/bayesianLayman.html)
  from the package [SIBER](https://CRAN.R-project.org/package=SIBER).

- type:

  a `character` that is either `"bay"` or `"ml"` which indicates whether
  the community metrics to be extracted are from a Bayesian analysis or
  a maximum-likelihood.

- community_df:

  a two column data frame. One of the columns has to be named
  `community` and the data in the column will be `numerics` as a
  `character` string(e.g., `"1", "2", "3"`). This is the order of the
  community names and will be used to join the actual community names to
  the correct data. These are the same class and values required by the
  function,
  [`createSiberObject()`](https://rdrr.io/pkg/SIBER/man/createSiberObject.html)
  from [SIBER](https://CRAN.R-project.org/package=SIBER). The second
  column contains the actual names of the communities that the user is
  working with (e.g., `"region"`).

- data_format:

  a `character` string that decides whether the returned object is in
  long or wide format. Default is `"long"`, with the alternative
  supplied being `"wide"`.

- isotope_x:

  a `numeric` that will be used in the labeling processes for the range
  of the x. Default is `13` (e.g., \\\delta\\^13 C). This will create a
  column called `labels`, that will only be created when `data_format`
  is set to `long`.

- isotope_y:

  a `numeric` that will be used in the labeling processes for the range
  of the y isotope. Default is `15` (e.g., \\\delta\\^15 N). \#' This
  will create a column called `labels`, that will only be created when
  `data_format` is set to `long`.

- element_x:

  a `character` that will be used in the labeling process for the range
  of the x isotope. Default is `C` (e.g., \\\delta\\^13 C). This will
  create a column called `labels`, that will only be created when
  `data_format` is set to `long`.

- element_y:

  a `character` that will be used in the labeling process for the range
  of the y isotope. Default is `N` (e.g., \\\delta\\^13 N). \#' This
  will create a column called `labels`, that will only be created when
  `data_format` is set to `long`.

## Value

A `tibble` containing four rows when `data_format` is set to its default
which is `long`. These four rows are the following, `community`,
`the_name_of_the_communities`, `metric` and `post_est`.

## See also

[`SIBER::bayesianLayman()`](https://rdrr.io/pkg/SIBER/man/bayesianLayman.html)
and
[`SIBER::createSiberObject()`](https://rdrr.io/pkg/SIBER/man/createSiberObject.html)

## Examples

``` r
library(SIBER)

# ---- bring in SIBER demo data ----
# uncomenet to use
# str(demo.siber.data)

# ---- create community names data frame ----
# uncomment to use
# str(demo.siber.data.2)

demo.siber.data.2$group_name <- as.factor(demo.siber.data.2$group)

demo.siber.data.2$group <- as.numeric(demo.siber.data.2$group_name) |>
as.character()

demo.siber.data.2$community_names <- as.factor(demo.siber.data.2$community)

demo.siber.data.2$community <- as.numeric(demo.siber.data.2$community_names) |>
as.character()
c_names <- demo.siber.data.2 |>
dplyr::distinct(community, community_names)


demo.siber.data_2 <- demo.siber.data.2[,1:4]
# ---- create the siber object ----
siber.example <- createSiberObject(demo.siber.data_2)

# ---- view Bayesian estimates of mu and sigma produced by SIBER ---
# uncomment to use
# str(post_sam_siber)

# ---- extract posterior estimates of mu -----

mu_post <- extractPosteriorMeans(siber.example, post_sam_siber)

# ---- Bayesian estimates of layman metrics using SIBER ----

layman_b <- bayesianLayman(mu.post = mu_post)

# ---- use nichetools to extract Bayesian estimates of Layman metrics ----

layman_be <- extract_layman(layman_b, community_df = c_names)

layman_be
#> # A tibble: 48,000 × 5
#>    community community_names metric   post_est labels                           
#>    <chr>     <fct>           <chr>       <dbl> <fct>                            
#>  1 1         dublin          dY_range     6.71 δ<sup>15</sup>N<br>Range         
#>  2 1         dublin          dX_range     8.14 δ<sup>13</sup>C<br>Range         
#>  3 1         dublin          TA          13.7  Total Area                       
#>  4 1         dublin          CD           4.27 Distance to<br>Centroid          
#>  5 1         dublin          NND          5.08 Nearest<br>Neighbor<br>Distance  
#>  6 1         dublin          SDNND        3.57 SD Nearest<br>Neighbor<br>Distan…
#>  7 1         dublin          dY_range     6.20 δ<sup>15</sup>N<br>Range         
#>  8 1         dublin          dX_range     8.19 δ<sup>13</sup>C<br>Range         
#>  9 1         dublin          TA          10.6  Total Area                       
#> 10 1         dublin          CD           4.19 Distance to<br>Centroid          
#> # ℹ 47,990 more rows
```
