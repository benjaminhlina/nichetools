# extract \\\mu\\

Extract Bayesian estimates of \\\mu\\ from data objects created by
[nicheROVER](https://cran.r-project.org/package=nicheROVER) or
[SIBER](https://cran.r-project.org/package=SIBER).

## Usage

``` r
extract_mu(
  data,
  pkg = NULL,
  isotope_names = NULL,
  data_format = NULL,
  community_df = NULL
)
```

## Arguments

- data:

  a `list` created by the function `niw.post()` or
  [`siberMVN()`](https://rdrr.io/pkg/SIBER/man/siberMVN.html) in the
  package [nicheROVER](https://cran.r-project.org/package=nicheROVER) or
  [SIBER](https://cran.r-project.org/package=SIBER), respectfully.

- pkg:

  a `character` string that is the name of the package that you're
  using. Defaults to `"nicheROVER"`. Alternatively the user can supply
  the argument with `"SIBER"`.

- isotope_names:

  is a vector of `character` string used change the column name of
  isotopes used in the analysis. Defaults to `c("d13c", "d15n")`.

- data_format:

  a `character` string that decides whether the returned object is in
  long or wide format. Default is `"long"`, with the alternative
  supplied being `"wide"`.

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

## Value

Returns a `tibble` of extracted estimates of \\\mu\\ created by the
function `niw.post()` or
[`siberMVN()`](https://rdrr.io/pkg/SIBER/man/siberMVN.html) in the
packages [nicheROVER](https://cran.r-project.org/package=nicheROVER).
and [SIBER](https://cran.r-project.org/package=SIBER).

The `tibble` will contain five columns in the following order, `metric`,
`sample_name`, `sample_number`, and the names of the isotope columns
supplied to `niw.post()` or
[`siberMVN()`](https://rdrr.io/pkg/SIBER/man/siberMVN.html) (e.g.,
`d13c` and `d15n` ).

## See also

[`nicheROVER::niw.post()`](https://rdrr.io/pkg/nicheROVER/man/niw.post.html)
and [`SIBER::siberMVN()`](https://rdrr.io/pkg/SIBER/man/siberMVN.html)

## Examples

``` r
extract_mu(
data = niw_fish_post
)
#> # A tibble: 8,000 × 5
#>    metric sample_name sample_number isotope mu_est
#>    <chr>  <chr>               <int> <chr>    <dbl>
#>  1 mu     ARCS                    1 d13c     -23.8
#>  2 mu     ARCS                    1 d15n      12.6
#>  3 mu     ARCS                    2 d13c     -23.7
#>  4 mu     ARCS                    2 d15n      12.6
#>  5 mu     ARCS                    3 d13c     -24.2
#>  6 mu     ARCS                    3 d15n      12.4
#>  7 mu     ARCS                    4 d13c     -24.1
#>  8 mu     ARCS                    4 d15n      12.7
#>  9 mu     ARCS                    5 d13c     -23.9
#> 10 mu     ARCS                    5 d15n      12.6
#> # ℹ 7,990 more rows

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


extract_mu(
data = post_sam_siber,
pkg = "SIBER",
community_df = cg_name
)
#> # A tibble: 40,000 × 9
#>    metric community group sample_name sample_number community_name group_name
#>    <chr>  <chr>     <chr> <chr>               <int> <fct>          <fct>     
#>  1 mu     1         1     1.1                     1 dublin         city      
#>  2 mu     1         1     1.1                     1 dublin         city      
#>  3 mu     1         1     1.1                     2 dublin         city      
#>  4 mu     1         1     1.1                     2 dublin         city      
#>  5 mu     1         1     1.1                     3 dublin         city      
#>  6 mu     1         1     1.1                     3 dublin         city      
#>  7 mu     1         1     1.1                     4 dublin         city      
#>  8 mu     1         1     1.1                     4 dublin         city      
#>  9 mu     1         1     1.1                     5 dublin         city      
#> 10 mu     1         1     1.1                     5 dublin         city      
#> # ℹ 39,990 more rows
#> # ℹ 2 more variables: isotope <chr>, mu_est <dbl>
```
