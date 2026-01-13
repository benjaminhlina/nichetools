# extract overlap

Extract Bayesian estimates of similarities among groups produced by the
following function `overlap()` in the package
[nicheROVER](https://cran.r-project.org/package=nicheROVER).

## Usage

``` r
extract_overlap(data, name_a = NULL, name_b = NULL)
```

## Arguments

- data:

  a `array` object containing `matrices` created by the function
  `overlap()` in the package
  [nicheROVER](https://cran.r-project.org/package=nicheROVER).

- name_a:

  character string to supply for the first `sample_name` used in
  `overlap()`. Defaults to `"sample_name_a"`.

- name_b:

  character string to supply for the second `sample_name` used in
  `overlap()`. Defaults to `"sample_name_b"`.

## Value

A `tibble` containing five rows, `sample_name_a`, `id`, `sample_name_b`,
`sample_number`, and `niche_overlap`.

## See also

[`nicheROVER::overlap()`](https://rdrr.io/pkg/nicheROVER/man/overlap.html)

## Examples

``` r
extract_overlap(data = over_stat)
#> # A tibble: 16,000 × 6
#>    sample_name_a    id sample_name_b sample_number niche_overlap
#>    <chr>         <int> <chr>         <chr>                 <dbl>
#>  1 ARCS              1 ARCS          1                    NA    
#>  2 ARCS              1 BDWF          1                     0.073
#>  3 ARCS              1 LKWF          1                     0.787
#>  4 ARCS              1 LSCS          1                     0.737
#>  5 ARCS              1 ARCS          2                    NA    
#>  6 ARCS              1 BDWF          2                     0.049
#>  7 ARCS              1 LKWF          2                     0.484
#>  8 ARCS              1 LSCS          2                     0.671
#>  9 ARCS              1 ARCS          3                    NA    
#> 10 ARCS              1 BDWF          3                     0.095
#> # ℹ 15,990 more rows
#> # ℹ 1 more variable: niche_overlap_perc <dbl>
```
