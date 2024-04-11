test_that("sums check when some bootstraps have 0 variance", {
  most_foreign_advdeg <-
    chicago_long$foreign_born &
    chicago_long$education == "advanced.degree" &
    chicago_long$age < 49 # all but 3
  chicago_long_mod <-
    chicago_long[!most_foreign_advdeg, ]

  set.seed(1973)
  testthat::expect_no_warning(
    # This should have 0-variance levels in 3 of 10 runs
    obd <-
      OaxacaBlinderDecomp(
        formula = ln_real_wage ~ education | foreign_born,
        data = chicago_long_mod,
        type = "threefold",
        n_bootstraps = 10
      ),
    message = "bootstrap runs and were discarded."
  )
})
