# extract \\\Sigma\\

Extract Bayesian estimates of \\\Sigma\\ from data objects created by
[nicheROVER](https://cran.r-project.org/package=nicheROVER) or
[SIBER](https://cran.r-project.org/package=SIBER).

## Usage

``` r
extract_sigma(
  data,
  pkg = NULL,
  isotope_n = NULL,
  isotope_names = NULL,
  data_format = NULL
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

- isotope_n:

  a `numeric` either `2` or `3` that is the number of isotopes used in
  the anlsysis. Will default to `2`.

- isotope_names:

  is a vector of `character` string used change the column name of
  isotopes used in the analysis. Defaults to `c("d13c", "d15n")`.

- data_format:

  a `character` string that decides whether the returned object is in
  long or wide format. Default is `"wide"`, with the alternative
  supplied being `"long"`.

## Value

Returns a `tibble` of extracted estimates of \\\Sigma\\ created by the
function `niw.post()` or
[`siberMVN()`](https://rdrr.io/pkg/SIBER/man/siberMVN.html) in the
packages [nicheROVER](https://cran.r-project.org/package=nicheROVER).
and [SIBER](https://cran.r-project.org/package=SIBER).

The returned object will contain five columns in the following order
when `data_format` is set to `"wide"`, `metric`, `id`, `sample_name`,
`isotope`, `sample_number`, and the posterior sample for \\\Sigma\\
(e.g., `d13c` and `d15n`).

## See also

[`nicheROVER::niw.post()`](https://rdrr.io/pkg/nicheROVER/man/niw.post.html)
and [`SIBER::siberMVN()`](https://rdrr.io/pkg/SIBER/man/siberMVN.html)

## Examples

``` r
extract_sigma(
data = niw_fish_post
)
#> # A tibble: 8,000 × 6
#>    metric sample_name isotope sample_number  d13c  d15n
#>    <chr>  <chr>       <chr>   <chr>         <dbl> <dbl>
#>  1 sigma  ARCS        d13c    1             1.07  0.305
#>  2 sigma  ARCS        d15n    1             0.305 0.571
#>  3 sigma  ARCS        d13c    2             1.01  0.221
#>  4 sigma  ARCS        d15n    2             0.221 0.657
#>  5 sigma  ARCS        d13c    3             1.21  0.486
#>  6 sigma  ARCS        d15n    3             0.486 0.616
#>  7 sigma  ARCS        d13c    4             1.14  0.352
#>  8 sigma  ARCS        d15n    4             0.352 0.570
#>  9 sigma  ARCS        d13c    5             0.788 0.376
#> 10 sigma  ARCS        d15n    5             0.376 0.849
#> # ℹ 7,990 more rows

extract_sigma(
data = post_sam_siber,
pkg = "SIBER"
)
#> # A tibble: 40,000 × 6
#>    metric sample_name isotope sample_number   d13c   d15n
#>    <chr>  <chr>       <chr>           <int>  <dbl>  <dbl>
#>  1 sigma  1.1         d13c                1  1.58  -0.808
#>  2 sigma  1.1         d15n                1 -0.808  2.22 
#>  3 sigma  1.1         d13c                2  1.49  -0.990
#>  4 sigma  1.1         d15n                2 -0.990  3.64 
#>  5 sigma  1.1         d13c                3  1.60  -1.30 
#>  6 sigma  1.1         d15n                3 -1.30   3.78 
#>  7 sigma  1.1         d13c                4  1.61  -1.48 
#>  8 sigma  1.1         d15n                4 -1.48   3.69 
#>  9 sigma  1.1         d13c                5  1.54  -0.892
#> 10 sigma  1.1         d15n                5 -0.892  2.58 
#> # ℹ 39,990 more rows
```
