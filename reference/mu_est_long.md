# A `data.frame` containing posterior estimates of \\\mu\\

Posterior estimates of \\\mu\\ using `fish` data set from
[nicheROVER](https://cran.r-project.org/package=nicheROVER), using
Normal-Inverse-Wishart (NIW) priors.

## Usage

``` r
mu_est_long
```

## Format

`data.frame` containing 8,000 rows and 7 variables

- metric:

  name of the metric extracted from `niw.post()`

- species:

  species abbreviation

- sample_number:

  sample number from 1-1000

- isotope:

  column with isotope name

- mu_est:

  estimate of mu produced from `niw.post()`

- element:

  isotopic element used in labelling

- neutron:

  neutron number used in labelling
