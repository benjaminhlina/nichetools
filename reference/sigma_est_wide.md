# A `data.frame` containing posterior estimates of \\\Sigma\\

Posterior estimates of \\\Sigma\\ using `fish` data set from
[nicheROVER](https://cran.r-project.org/package=nicheROVER), using
Normal-Inverse-Wishart (NIW) priors

## Usage

``` r
sigma_est_wide
```

## Format

`data.frame` containing 8,000 rows and 6 variables

- metric:

  name of the metric extracted from `niw.post()`

- species:

  species abbreviation

- isotope:

  column with isotope name

- sample_number:

  sample number from 1-1000

- d15n:

  estimate of \\\Sigma\\ for d15n produced from `niw.post()`

- d13c:

  estimate of \\\Sigma\\ for d13c produced from `niw.post()`
