test_that("0-variance dummy IV results match Stata", {
  chicago_mod <- chicago
  chicago_mod$too_young <- chicago_mod$age < 19
  names(chicago_mod) <- gsub("\\.", "_", names(chicago_mod))

  fmla_tooyoung_dum <-
    ln_real_wage ~
    LTHS + some_college + college + high_school |
    too_young

  obd_tooyoung <-
    OaxacaBlinderDecomp(
      fmla_tooyoung_dum,
      chicago_mod,
      type = "threefold"
    )

  # Get the same thing from saved Stata baseline results
  stata_dir <-
    testthat::test_path("fixtures", "stata_results", "tooyoung_dum")
  tooyoung_stata <- readRDS(file.path(stata_dir, "tooyoung.rds"))

  testthat::expect_equal(
    obd_tooyoung$varlevel,
    tooyoung_stata
  )
})

test_that("0-variance baseline-adjusted IV results match Stata", {
  chicago_long_mod <- chicago_long
  chicago_long_mod$too_young <- chicago_long_mod$age < 19
  baseline_cat <- levels(as.factor(chicago_long_mod$education))[1]
  baseline_rowname <- gsub("\\.", "_", baseline_cat)

  obd <-
    OaxacaBlinderDecomp(
      ln_real_wage ~ education | too_young,
      chicago_long_mod,
      baseline_invariant = TRUE,
      type = "threefold"
    )
  obd_ests <- obd$varlevel
  rownames(obd_ests) <- gsub("education", "", rownames(obd_ests))
  rownames(obd_ests) <-
    gsub(".baseline", baseline_rowname, rownames(obd_ests))
  rownames(obd_ests) <- gsub("\\.", "_", rownames(obd_ests))
  obd_ests <- obd_ests[order(rownames(obd_ests)), ]

  # Get the same thing from saved Stata baseline results
  stata_dir <-
    testthat::test_path(
      "fixtures", "stata_results", "tooyoung_baseline_invariant"
    )
  stata_obd <-
    readRDS(file.path(stata_dir, "tooyoung_baseline_invariant.rds"))
  stata_obd_ests <- stata_obd[order(rownames(stata_obd)), ]

  testthat::expect_equal(
    obd$varlevel,
    stata_obd
  )
})

