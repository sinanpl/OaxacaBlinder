
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OaxacaBlinder

<!-- badges: start -->

[![experimental](http://badges.github.io/stability-badges/dist/experimental.svg)](http://github.com/badges/stability-badges)
[![R-CMD-check](https://github.com/sinanpl/OaxacaBlinder/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sinanpl/OaxacaBlinder/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This repository is no longer being maintained. Please use
<https://github.com/davidskalinder/OaxacaBlinder> instead.

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
twofold <- OaxacaBlinderDecomp(
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
#>               coefficient   % of gap        se      2.5%     97.5% pval
#> explained           -0.53     -13.9% 0.4122770 -1.180545 0.2278868 0.26
#> unexplained          4.37     113.9% 0.6087611  3.240732 5.5661360 0.00
#> unexplained_a        1.89      49.2% 0.2887906  1.380282 2.4521890 0.00
#> unexplained_b        2.48      64.6% 0.3421788  1.866660 3.1136341 0.00
```

In addition, coefficients can be extracted vor the variable-level
estimates

``` r
coef(twofold)
#>                             explained unexplained unexplained_a unexplained_b
#> (Intercept)               0.000000000  5.00836516    2.61888654    2.38947862
#> age                       0.220418162  1.07822114    0.31640221    0.76181894
#> educationadvanced.degree -0.260197078  0.35251787    0.16750479    0.18501308
#> educationcollege         -0.009283372  0.01142439   -0.01935324    0.03077763
#> educationhigh.school     -0.107295348 -0.52124445   -0.35213923   -0.16910523
#> educationLTHS            -0.560862567 -1.15597710   -0.68768187   -0.46829523
#> educationsome.college     0.184695669 -0.40644640   -0.15524704   -0.25119935
```

Or with bootstrapped confidence intervals for variable level results

``` r
coef(twofold, ci = TRUE)
#>        coef_type                     term  coefficient         se         2.5%
#> 1      explained              (Intercept)  0.000000000 0.00000000  0.000000000
#> 2      explained                      age  0.220418162 0.14769579 -0.059591156
#> 3      explained educationadvanced.degree -0.260197078 0.20525755 -0.626713783
#> 4      explained         educationcollege -0.009283372 0.07159522 -0.154005008
#> 5      explained     educationhigh.school -0.107295348 0.20575285 -0.414935269
#> 6      explained            educationLTHS -0.560862567 0.27265132 -1.070658507
#> 7      explained    educationsome.college  0.184695669 0.10535330 -0.001978705
#> 8    unexplained              (Intercept)  5.008365163 1.82883225  1.356173015
#> 9    unexplained                      age  1.078221143 1.70895891 -1.546499592
#> 10   unexplained educationadvanced.degree  0.352517873 0.26147630 -0.128444179
#> 11   unexplained         educationcollege  0.011424394 0.32621063 -0.634284843
#> 12   unexplained     educationhigh.school -0.521244454 0.48150982 -1.410001762
#> 13   unexplained            educationLTHS -1.155977097 0.48209552 -2.087042856
#> 14   unexplained    educationsome.college -0.406446395 0.44545743 -1.248365304
#> 15 unexplained_a              (Intercept)  2.618886538 1.04626610  0.458208282
#> 16 unexplained_a                      age  0.316402205 0.84675375 -0.943723945
#> 17 unexplained_a educationadvanced.degree  0.167504793 0.11320126 -0.041031671
#> 18 unexplained_a         educationcollege -0.019353236 0.15479741 -0.299398808
#> 19 unexplained_a     educationhigh.school -0.352139226 0.28200658 -1.008012016
#> 20 unexplained_a            educationLTHS -0.687681871 0.28550883 -1.284580378
#> 21 unexplained_a    educationsome.college -0.155247041 0.22006110 -0.581722778
#> 22 unexplained_b              (Intercept)  2.389478624 0.91592100  0.382865480
#> 23 unexplained_b                      age  0.761818938 0.95947907 -0.787774465
#> 24 unexplained_b educationadvanced.degree  0.185013080 0.14996956 -0.087590702
#> 25 unexplained_b         educationcollege  0.030777630 0.18081621 -0.354584082
#> 26 unexplained_b     educationhigh.school -0.169105228 0.26464354 -0.732591979
#> 27 unexplained_b            educationLTHS -0.468295226 0.22650803 -0.912673995
#> 28 unexplained_b    educationsome.college -0.251199354 0.25380668 -0.665969808
#>          97.5% pval
#> 1   0.00000000 0.00
#> 2   0.50224690 0.10
#> 3   0.13011911 0.26
#> 4   0.10258954 0.98
#> 5   0.40945553 0.70
#> 6  -0.03536842 0.04
#> 7   0.38520822 0.06
#> 8   7.96884579 0.02
#> 9   5.65616923 0.46
#> 10  0.93229447 0.12
#> 11  0.66026442 0.84
#> 12  0.53185592 0.18
#> 13 -0.34201624 0.00
#> 14  0.33240011 0.44
#> 15  4.42728201 0.00
#> 16  2.15763770 0.66
#> 17  0.42375242 0.12
#> 18  0.28012057 0.80
#> 19  0.16279972 0.14
#> 20 -0.27955566 0.00
#> 21  0.23573060 0.48
#> 22  3.79379661 0.02
#> 23  2.91650468 0.34
#> 24  0.50393374 0.14
#> 25  0.38084125 0.98
#> 26  0.32397155 0.38
#> 27 -0.07636008 0.00
#> 28  0.19618562 0.36
```

### Threefold decomposition

``` r
threefold <- OaxacaBlinderDecomp(
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
#>                             explained unexplained unexplained_a unexplained_b
#> (Intercept)               0.000000000  5.00836516    0.30167882    4.70668634
#> age                       0.207760327  1.09087898    0.63724953    0.45362944
#> educationadvanced.degree -0.266963834  0.35928463    0.15362427    0.20566036
#> educationcollege         -0.009142399  0.01128342   -0.01545726    0.02674068
#> educationhigh.school     -0.110733093 -0.51780671   -0.29583600   -0.22197071
#> educationLTHS            -0.584635143 -1.13220452   -0.58369823   -0.54850629
#> educationsome.college     0.171472514 -0.39322324   -0.19756114   -0.19566210
```
