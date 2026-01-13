# create comparisons

Creates a list with all of the comparisons needed to create Bayesian and
maximum-likelihood estimates for proportion of niche similarities.

## Usage

``` r
create_comparisons(data, comparison = c("within", "among"))
```

## Arguments

- data:

  a `data.frame` that is the names of the community and group names

- comparison:

  a `character`that is either `"within"` or `"among"` indicating whether
  the comparisons are within a community and between groups or among
  communities for the same groups.

## Examples

``` r
# ---- load siber ----
library(SIBER)

# ---- create community names data frame ----
# uncomment to use
# str(demo.siber.data.2)

demo.siber.data.2$group_name <- as.factor(demo.siber.data.2$group)

demo.siber.data.2$group <- as.numeric(demo.siber.data.2$group_name) |>
as.character()

demo.siber.data.2$community_names <- as.factor(demo.siber.data.2$community)

demo.siber.data.2$community <- as.numeric(demo.siber.data.2$community_names) |>
as.character()

cg_names <- demo.siber.data.2 |>
dplyr::distinct(community, group, community_names, group_name)

# ---- create comparsions ----
create_comparisons(cg_names,
                  comparison = "within")
#> $`1.1_1.2`
#> # A tibble: 1 × 2
#>   cg_1  cg_2 
#>   <chr> <chr>
#> 1 1.1   1.2  
#> 
#> $`1.1_1.3`
#> # A tibble: 1 × 2
#>   cg_1  cg_2 
#>   <chr> <chr>
#> 1 1.1   1.3  
#> 
#> $`1.2_1.3`
#> # A tibble: 1 × 2
#>   cg_1  cg_2 
#>   <chr> <chr>
#> 1 1.2   1.3  
#> 
#> $`2.1_2.4`
#> # A tibble: 1 × 2
#>   cg_1  cg_2 
#>   <chr> <chr>
#> 1 2.1   2.4  
#> 
```
