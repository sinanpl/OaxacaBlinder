
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OaxacaBlinder

<!-- badges: start -->
<!-- badges: end -->

This is an R implementation of the bare-bones version of
`OaxacaBlinder`. It aims to provide a more intuitive interface in R

## Installation

You can install the development version of OaxacaBlinder like so:

``` r
remotes::install_github("SinanPolatoglu/OaxacaBlinder")
```

## Example

``` r
library(OaxacaBlinder)
dplyr::glimpse(glassdoor_gpg)
#> Rows: 1,000
#> Columns: 10
#> $ JobTitle  <chr> "Graphic Designer", "Software Engineer", "Warehouse Associat…
#> $ Gender    <chr> "Female", "Male", "Female", "Male", "Male", "Female", "Femal…
#> $ Age       <dbl> 18, 21, 19, 20, 26, 20, 20, 18, 33, 35, 24, 18, 19, 30, 35, …
#> $ PerfEval  <dbl> 5, 5, 4, 5, 5, 5, 5, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, …
#> $ Education <chr> "College", "College", "PhD", "Masters", "Masters", "PhD", "C…
#> $ Dept      <chr> "Operations", "Management", "Administration", "Sales", "Engi…
#> $ Seniority <dbl> 2, 5, 5, 4, 5, 4, 4, 5, 5, 5, 5, 3, 3, 5, 4, 3, 5, 5, 5, 5, …
#> $ BasePay   <dbl> 42363, 108476, 90208, 108080, 99464, 70890, 67585, 97523, 11…
#> $ Bonus     <dbl> 9938, 11128, 9268, 10154, 9319, 10126, 10541, 10240, 9836, 9…
#> $ Gender_i  <dbl> 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, …
```

The formula interface is similar to the `oaxaca` package. It should be
specified as
`dependent_var ~ x_var1 + x_var1 + ... + x_varK | group_var`. Note that
the interface supports supplying factor variables to the formula and
`x_var*` can represent a nominal variable such as further in the
document.

### Twofold decomposition

``` r
twofold = OaxacaBlinderDecomp(BasePay ~ Dept + Age | Gender_i, glassdoor_gpg, type = "twofold")
summary(twofold)
#> Oaxaca Blinder Decomposition model
#> ----------------------------------
#> Type: twofold
#> Formula: BasePay ~ Dept + Age | Gender_i
#> Data: glassdoor_gpg
#> 
#> Avg Group 1: 98457.55
#> Avg Group 2: 89942.82
#> 
#> Gap: 8514.73
#> % Diff: 8.65%
#>               coefficient      %
#> explained         -808.97  -9.5%
#> unexplained       9323.70 109.5%
#> unexplained_a        0.00  -0.0%
#> unexplained_b     9323.70 109.5%
```

In addition, coefficients can be extracted vor the variable-level
estimates

``` r
coef(twofold)
#>                  explained unexplained unexplained_a unexplained_b
#> (Intercept)        0.00000   6411.0695   -1429.53485    7840.60432
#> DeptEngineering   16.57809    879.1701     420.57923     458.59083
#> DeptManagement    88.91066   -523.5248    -253.15830    -270.36653
#> DeptOperations   -23.28850   -811.3901    -390.81885    -420.57129
#> DeptSales        -67.03145   -197.2291     -98.30487     -98.92424
#> Age             -824.14018   3565.6027    1751.23764    1814.36502
```

### Threefold decomposition

``` r
threefold = (OaxacaBlinderDecomp(BasePay ~ Dept + Age | Gender_i, glassdoor_gpg, type = "threefold"))
summary(threefold)
#> Oaxaca Blinder Decomposition model
#> ----------------------------------
#> Type: threefold
#> Formula: BasePay ~ Dept + Age | Gender_i
#> Data: glassdoor_gpg
#> 
#> Avg Group 1: 98457.55
#> Avg Group 2: 89942.82
#> 
#> Gap: 8514.73
#> % Diff: 8.65%
#>              coefficient      %
#> endowments       -737.44  -8.7%
#> coefficients     9387.36 110.2%
#> interaction     -3588.14 -42.1%
coef(threefold)
#>                  endowments coefficients interaction
#> (Intercept)        0.000000    6411.0695 -6411.06948
#> DeptEngineering    8.287274     871.7015 -4583.77511
#> DeptManagement   121.996735    -495.9225  2667.74352
#> DeptOperations    -4.512995    -794.6885  3874.11548
#> DeptSales        -74.624013    -205.4014   951.74424
#> Age             -788.586795    3600.6049   -86.89869
```
