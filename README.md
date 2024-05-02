
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OaxacaBlinder

<!-- badges: start -->

[![experimental](http://badges.github.io/stability-badges/dist/experimental.svg)](http://github.com/badges/stability-badges)
[![R-CMD-check](https://github.com/sinanpl/OaxacaBlinder/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sinanpl/OaxacaBlinder/actions/workflows/R-CMD-check.yaml)
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
remotes::install_github("sinanpl/OaxacaBlinder")
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
- OaxacaBlinder is capable of correcting for the omitted baseline bias
  for multiple sets of factor variables with the
  `baseline_invariant=TRUE`
- The `pooled` argument sets what reference $\beta_R$:
  - `neumark`: The parameters of a model **excluding** the group
    variable
  - `jann`: The parameters of a model **including** the group variable

``` r
set.seed(1973)
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
#>               coefficient   % of gap        se      2.5%     97.5%
#> explained           -0.53     -13.9% 0.4180845 -1.434944 0.2887765
#> unexplained          4.37     113.9% 0.6292984  3.252398 5.6389801
#> unexplained_a        1.89      49.2% 0.2942611  1.411671 2.4729207
#> unexplained_b        2.48      64.6% 0.3537071  1.859117 3.2261392
```

In addition, coefficients can be extracted for the variable-level
estimates

``` r
coef(twofold)
#>                          explained unexplained unexplained_a unexplained_b
#> age                    0.220418162  1.07822114    0.31640221    0.76181894
#> education.baseline    -0.260197078  0.35251787    0.16750479    0.18501308
#> educationcollege      -0.009283372  0.01142439   -0.01935324    0.03077763
#> educationhigh.school  -0.107295348 -0.52124445   -0.35213923   -0.16910523
#> educationLTHS         -0.560862567 -1.15597710   -0.68768187   -0.46829523
#> educationsome.college  0.184695669 -0.40644640   -0.15524704   -0.25119935
#> (Intercept)            0.000000000  5.00836516    2.61888654    2.38947862
```

Or with bootstrapped confidence intervals for variable level results

``` r
coef(twofold, ci = TRUE)
#>        coef_type                  term  coefficient         se         2.5%
#> 1      explained           (Intercept)  0.000000000 0.00000000  0.000000000
#> 2      explained                   age  0.220418162 0.17417049 -0.091410524
#> 3      explained    education.baseline -0.260197078 0.21524096 -0.773532447
#> 4      explained      educationcollege -0.009283372 0.07474587 -0.132752600
#> 5      explained  educationhigh.school -0.107295348 0.19045141 -0.586152875
#> 6      explained         educationLTHS -0.560862567 0.25074354 -0.939880049
#> 7      explained educationsome.college  0.184695669 0.12875609 -0.003837003
#> 8    unexplained           (Intercept)  5.008365163 2.20539880  1.082182846
#> 9    unexplained                   age  1.078221143 1.75125140 -2.158891075
#> 10   unexplained    education.baseline  0.352517873 0.25192252 -0.097228220
#> 11   unexplained      educationcollege  0.011424394 0.27418584 -0.468845582
#> 12   unexplained  educationhigh.school -0.521244454 0.52009114 -1.379697757
#> 13   unexplained         educationLTHS -1.155977097 0.40936979 -1.924458790
#> 14   unexplained educationsome.college -0.406446395 0.45779201 -1.204477533
#> 15 unexplained_a           (Intercept)  2.618886538 1.22525976  0.545658939
#> 16 unexplained_a                   age  0.316402205 0.92630813 -1.588336437
#> 17 unexplained_a    education.baseline  0.167504793 0.10923229 -0.020622568
#> 18 unexplained_a      educationcollege -0.019353236 0.12660152 -0.239781100
#> 19 unexplained_a  educationhigh.school -0.352139226 0.31068609 -0.935464044
#> 20 unexplained_a         educationLTHS -0.687681871 0.23997114 -1.174277347
#> 21 unexplained_a educationsome.college -0.155247041 0.21803016 -0.551545319
#> 22 unexplained_b           (Intercept)  2.389478624 1.10725926  0.548002919
#> 23 unexplained_b                   age  0.761818938 0.91009894 -0.985627670
#> 24 unexplained_b    education.baseline  0.185013080 0.14536079 -0.070810349
#> 25 unexplained_b      educationcollege  0.030777630 0.15900823 -0.267248679
#> 26 unexplained_b  educationhigh.school -0.169105228 0.26146368 -0.635785709
#> 27 unexplained_b         educationLTHS -0.468295226 0.20801790 -0.861886628
#> 28 unexplained_b educationsome.college -0.251199354 0.26699278 -0.674083534
#>          97.5%
#> 1   0.00000000
#> 2   0.65279282
#> 3   0.14749455
#> 4   0.20092160
#> 5   0.22638256
#> 6  -0.08627095
#> 7   0.47558496
#> 8   9.19828855
#> 9   4.10080406
#> 10  0.89887391
#> 11  0.54474731
#> 12  0.45293495
#> 13 -0.46003370
#> 14  0.57630707
#> 15  5.18077724
#> 16  1.89272482
#> 17  0.41442471
#> 18  0.22033196
#> 19  0.16458701
#> 20 -0.30903780
#> 21  0.27713761
#> 22  4.89176209
#> 23  2.26706136
#> 24  0.48785069
#> 25  0.33624334
#> 26  0.33882796
#> 27 -0.16394466
#> 28  0.33717668
```

### Threefold decomposition

``` r
threefold <- OaxacaBlinderDecomp(
  real_wage ~ age + education | female, chicago_long,
  type = "threefold",
  baseline_invariant = TRUE
)
summary(threefold)
#> Oaxaca Blinder Decomposition model
#> ----------------------------------
#> Type: threefold
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
#>              coefficient   % of gap
#> endowments         -0.41     -10.7%
#> coefficients        4.57     119.1%
#> interaction        -0.32      -8.3%
coef(threefold)
#>                         endowments coefficients   interaction
#> age                    0.189129042   1.06573867  0.0437715888
#> education.baseline    -0.199562539   0.43417646 -0.1422931256
#> educationcollege      -0.008208598   0.01072411 -0.0003744928
#> educationhigh.school  -0.096298746  -0.49974365 -0.0324974106
#> educationLTHS         -0.422071444  -0.99876035 -0.2960078660
#> educationsome.college  0.124886299  -0.45496110  0.1083240704
#> (Intercept)            0.000000000   5.00836516  0.0000000000
```

## Contributing

Any contribution is appreciated. In case you notice any typos or
opportunities for improvement, please open an issue or pull request.

For developers: this project is set up for using pre-commit. Find out
more [here](https://lorenzwalthert.github.io/precommit/) and [get
started](https://lorenzwalthert.github.io/precommit/articles/precommit.html).
