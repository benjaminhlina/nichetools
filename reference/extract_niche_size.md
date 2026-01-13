# extract niche size

Extract niche size based on elliptical niche region of Bayesian
estimates of sigma created by function `niw.post()` or
[`siberEllipses()`](https://rdrr.io/pkg/SIBER/man/siberEllipses.html) in
the package [nicheROVER](https://cran.r-project.org/package=nicheROVER)
or [SIBER](https://cran.r-project.org/package=SIBER), respectfully. For
[nicheROVER](https://cran.r-project.org/package=nicheROVER) this
function is a wrapper around
[`nicheROVER::niche.size`](https://rdrr.io/pkg/nicheROVER/man/niche.size.html).

## Usage

``` r
extract_niche_size(
  data,
  pkg = NULL,
  name = NULL,
  prob = NULL,
  community_df = NULL
)
```

## Arguments

- data:

  a `list` or `matrix` created by the function `niw.post()` or
  [`siberEllipses()`](https://rdrr.io/pkg/SIBER/man/siberEllipses.html)
  in the package
  [nicheROVER](https://cran.r-project.org/package=nicheROVER) or
  [SIBER](https://cran.r-project.org/package=SIBER), respectfully.

- pkg:

  a `character` string that is the name of the package that you're
  using. Defaults to `"nicheROVER"`. Alternatively the user can supply
  the argument with `"SIBER"`.

- name:

  a `character` string that will be assigned as the column name for
  groups. Default is `sample_name`. Only to be used when `pkg` is set to
  `"nicheROVER"`.

- prob:

  a `numeric` bound by 0 and 1 indicating the probabilistic niche size.
  Default is 0.95. Only to be used when `pkg` is set to `"nicheROVER"`.

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

if `pkg` is set to `"nicheROVER"` then a `tibble` containing three rows,
`sample_name`, `id`, and `niche_size` is returned.

## See also

[`nicheROVER::niche.size()`](https://rdrr.io/pkg/nicheROVER/man/niche.size.html),
[`nicheROVER::niw.post()`](https://rdrr.io/pkg/nicheROVER/man/niw.post.html),
[`SIBER::siberEllipses()`](https://rdrr.io/pkg/SIBER/man/siberEllipses.html),
and
[`SIBER::createSiberObject()`](https://rdrr.io/pkg/SIBER/man/createSiberObject.html)

## Examples

``` r
extract_niche_size(data = niw_fish_post)
#> # A tibble: 4,000 × 3
#>    sample_name id    niche_size
#>    <chr>       <chr>      <dbl>
#>  1 ARCS        1           13.6
#>  2 ARCS        2           14.8
#>  3 ARCS        3           13.4
#>  4 ARCS        4           13.6
#>  5 ARCS        5           13.7
#>  6 ARCS        6           13.2
#>  7 ARCS        7           18.3
#>  8 ARCS        8           13.9
#>  9 ARCS        9           14.3
#> 10 ARCS        10          14.3
#> # ℹ 3,990 more rows

```
