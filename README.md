
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OaxacaBlinder

<!-- badges: start -->

[![experimental](http://badges.github.io/stability-badges/dist/experimental.svg)](http://github.com/badges/stability-badges)

<!-- badges: end -->

This is an R implementation of `OaxacaBlinder` decomposition that comes
with

- correction for dummy encoding baseline with multiple factors
- more convenient output formatting

Please also check the stable `oaxaca` package on CRAN.

## Installation

You can install the development version of `OaxacaBlinder` like so:

``` r
# install.packages("remotes")
remotes::install_github("SinanPolatoglu/OaxacaBlinder")
```

## Example

Using the chicago dataset from the `oaxaca` package

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

### Twofold decomposition

- The formula interface is similar to the `oaxaca` package: it should be
  specified as
  `dependent_var ~ x_var1 + x_var1 + ... + x_varK | group_var`.
- OaxacaBlinder is capable of correcting for the ommitted baseline bias
  for multipe sets of factor variables with the
  `baseline_invariant=TRUE`
- The `pooled` argument sets what reference $\beta_R$:
  - `neumark`: The parameters of a model **excluding** the group
    variable
  - `jann`: The parameters of a model **including** the group variable

``` r
twofold = OaxacaBlinderDecomp(
  formula = real_wage ~ age + education | female,
  data = chicago_long,
  type = "twofold",
  baseline_invariant = TRUE,
  n_bootstraps = 100
)
summary(twofold)
#> Oaxaca Blinder Decomposition model
#> ----------------------------------
#> Type: twofold
#> Formula: real_wage ~ age + education | female
#> Data: chicago_long
#> 
#> Descriptives
#>             n    %n mean(real_wage)
#> female==0 412 57.9%           17.52
#> female==1 300 42.1%           13.69
#> 
#> Gap: 3.83
#> % Diff: 21.88%
#>               coefficient   % of gap      2.5%     97.5%
#> explained           -0.53     -13.9% -1.306213 0.1681933
#> unexplained          4.37     113.9%  3.232576 5.5662780
#> unexplained_a        1.89      49.2%  1.402721 2.5663579
#> unexplained_b        2.48      64.6%  1.777999 3.1122698
```

In addition, coefficients can be extracted vor the variable-level
estimates

``` r
coef(twofold)
#>                          explained unexplained unexplained_a unexplained_b
#> (Intercept)            0.000000000  5.00836516    2.61888654    2.38947862
#> age                    0.220418162  1.07822114    0.31640221    0.76181894
#> educationcollege      -0.009283372  0.01142439   -0.01935324    0.03077763
#> educationhigh.school  -0.107295348 -0.52124445   -0.35213923   -0.16910523
#> educationLTHS         -0.560862567 -1.15597710   -0.68768187   -0.46829523
#> educationsome.college  0.184695669 -0.40644640   -0.15524704   -0.25119935
#> education.baseline    -0.260197078  0.35251787    0.16750479    0.18501308
```

Or with bootstrapped confidence intervals for variable level results

``` r
coef(twofold, ci=TRUE)
#>        coef_type                  term  coefficient         2.5%       97.5%
#> 1      explained           (Intercept)  0.000000000  0.000000000  0.00000000
#> 2      explained                   age  0.220418162 -0.003446369  0.53534373
#> 3      explained    education.baseline -0.260197078 -0.657812690  0.06859416
#> 4      explained      educationcollege -0.009283372 -0.151655711  0.12650843
#> 5      explained  educationhigh.school -0.107295348 -0.403230839  0.34095018
#> 6      explained         educationLTHS -0.560862567 -1.190555209 -0.06628040
#> 7      explained educationsome.college  0.184695669  0.008586773  0.41180056
#> 8    unexplained           (Intercept)  5.008365163  1.300805150  8.41320062
#> 9    unexplained                   age  1.078221143 -1.846739705  4.19573495
#> 10   unexplained    education.baseline  0.352517873 -0.133223881  0.75388814
#> 11   unexplained      educationcollege  0.011424394 -0.547359476  0.46466839
#> 12   unexplained  educationhigh.school -0.521244454 -1.676171714  0.55148598
#> 13   unexplained         educationLTHS -1.155977097 -1.919934983 -0.29237190
#> 14   unexplained educationsome.college -0.406446395 -1.243927239  0.48374409
#> 15 unexplained_a           (Intercept)  2.618886538  0.556656099  4.71643644
#> 16 unexplained_a                   age  0.316402205 -1.059082761  1.90174874
#> 17 unexplained_a    education.baseline  0.167504793 -0.044985535  0.36371764
#> 18 unexplained_a      educationcollege -0.019353236 -0.259729988  0.22277605
#> 19 unexplained_a  educationhigh.school -0.352139226 -1.054489794  0.18717027
#> 20 unexplained_a         educationLTHS -0.687681871 -1.176104919 -0.22955356
#> 21 unexplained_a educationsome.college -0.155247041 -0.624632761  0.26228669
#> 22 unexplained_b           (Intercept)  2.389478624  0.488537938  4.54667043
#> 23 unexplained_b                   age  0.761818938 -0.827969222  2.56148546
#> 24 unexplained_b    education.baseline  0.185013080 -0.097748156  0.39335298
#> 25 unexplained_b      educationcollege  0.030777630 -0.303367434  0.28215788
#> 26 unexplained_b  educationhigh.school -0.169105228 -0.731720008  0.36530685
#> 27 unexplained_b         educationLTHS -0.468295226 -0.800785060 -0.03573617
#> 28 unexplained_b educationsome.college -0.251199354 -0.735602991  0.24180657
```

### Threefold decomposition

``` r
threefold = OaxacaBlinderDecomp(
  real_wage ~ age + education | female, chicago_long,
  type = "twofold",
  pooled = "jann",
  baseline_invariant = TRUE
)
summary(threefold)
#> Oaxaca Blinder Decomposition model
#> ----------------------------------
#> Type: twofold
#> Formula: real_wage ~ age + education | female
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
#> explained           -0.59     -15.4%
#> unexplained          4.43     115.4%
#> unexplained_a        0.00       0.0%
#> unexplained_b        4.43     115.4%
coef(threefold)
#>                          explained unexplained unexplained_a unexplained_b
#> (Intercept)            0.000000000  5.00836516    0.30167882    4.70668634
#> age                    0.207760327  1.09087898    0.63724953    0.45362944
#> educationcollege      -0.009142399  0.01128342   -0.01545726    0.02674068
#> educationhigh.school  -0.110733093 -0.51780671   -0.29583600   -0.22197071
#> educationLTHS         -0.584635143 -1.13220452   -0.58369823   -0.54850629
#> educationsome.college  0.171472514 -0.39322324   -0.19756114   -0.19566210
#> education.baseline    -0.266963834  0.35928463    0.15362427    0.20566036
```
