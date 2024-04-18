test_that("bootstrapped gaps haven't changed", {
  set.seed(1973)
  obd <- OaxacaBlinderDecomp(
    formula = ln_real_wage ~ age + education | female,
    data = chicago_long,
    baseline_invariant = TRUE,
    n_bootstraps = 10
  )
  testthat::expect_snapshot(obd$bootstraps$gaps)
})
