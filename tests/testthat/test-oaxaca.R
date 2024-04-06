test_that("decomp with zero-variance IVs matches Stata", {
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
  tooyoung_stata <-
    readRDS(
      testthat::test_path("fixtures", "stata_results", "tooyoung.rds")
    )

  testthat::expect_equal(
    obd_tooyoung$varlevel,
    tooyoung_stata
  )
})
