
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OaxacaBlinder

<!-- badges: start -->
<!-- badges: end -->

This is an R implementation of the bare-bones version of
`OaxacaBlinder`. It aims to provide a more intuitive interface in R

## Installation

You can install the development version of OaxacaBlinder like so:

``` r
# install.packages("remotes")
remotes::install_github("SinanPolatoglu/OaxacaBlinder")
```

## Example

``` r
library(OaxacaBlinder)
dplyr::glimpse(chicago_long)
#> Rows: 712
#> Columns: 7
#> $ female       <int> 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, …
#> $ male         <dbl> 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, …
#> $ foreign_born <int> 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, …
#> $ age          <int> 52, 46, 31, 35, 19, 50, 33, 43, 39, 22, 28, 31, 30, 20, 6…
#> $ education    <chr> "high.school", "high.school", "high.school", "high.school…
#> $ real_wage    <dbl> 8.500000, NA, 12.179999, 15.000001, 8.000000, NA, 10.0000…
#> $ ln_real_wage <dbl> 2.140066, NA, 2.499795, 2.708050, 2.079442, NA, 2.302585,…
```

The formula interface is similar to the `oaxaca` package. It should be
specified as
`dependent_var ~ x_var1 + x_var1 + ... + x_varK | group_var`.

### Twofold decomposition

``` r
twofold = OaxacaBlinderDecomp(real_wage ~ age | female, chicago_long, type = "twofold")
summary(twofold)
#> Oaxaca Blinder Decomposition model
#> ----------------------------------
#> Type: twofold
#> Formula: real_wage ~ age | female
#> Data: chicago_long
#> 
#> Descriptives
#>             n    %n mean(real_wage)
#> female==0 412 57.9%           17.52
#> female==1 300 42.1%           13.69
#> 
#> Gap: 3.83
#> % Diff: 21.88%
#>               coefficient   % of gap
#> explained            0.19       5.0%
#> unexplained          3.64      95.0%
#> unexplained_a        0.00       0.0%
#> unexplained_b        3.64      95.0%
```

In addition, coefficients can be extracted vor the variable-level
estimates

``` r
coef(twofold)
#>             explained unexplained unexplained_a unexplained_b
#> (Intercept) 0.0000000   3.5092069   -0.06015326    3.56936019
#> age         0.1918078   0.1333213    0.06015326    0.07316808
```

The interface supports supplying factor variables to the formula and
`x_var*` can represent a nominal variable, such as `education` below.

``` r
twofold = OaxacaBlinderDecomp(real_wage ~ age + education | female, chicago_long, type = "twofold")
coef(twofold)
#>                         explained unexplained unexplained_a unexplained_b
#> (Intercept)            0.00000000  12.3638252     4.1730103     8.1908149
#> age                    0.20776033   1.0908790     0.6372495     0.4536294
#> educationcollege       0.04105593  -0.7408281    -0.4046387    -0.3361894
#> educationhigh.school  -0.40051435  -2.9743002    -1.6272463    -1.3470539
#> educationLTHS         -1.53612163  -3.1102927    -1.7512426    -1.3590501
#> educationsome.college  1.09557809  -2.2027054    -1.0271322    -1.1755733
```

### Threefold decomposition

``` r
threefold = OaxacaBlinderDecomp(real_wage ~ age | female, chicago_long, type = "threefold")
summary(threefold)
#> Oaxaca Blinder Decomposition model
#> ----------------------------------
#> Type: threefold
#> Formula: real_wage ~ age | female
#> Data: chicago_long
#> 
#> Descriptives
#>             n    %n mean(real_wage)
#> female==0 412 57.9%           17.52
#> female==1 300 42.1%           13.69
#> 
#> Gap: 3.83
#> % Diff: 21.88%
#>              coefficient   % of gap
#> endowments          0.19       4.9%
#> coefficients        3.64      94.9%
#> interaction         0.01       0.1%
```
