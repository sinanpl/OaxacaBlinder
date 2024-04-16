test_that("baseline-adjusted-IV threefold results match Stata", {
  chicago_long_mod <- chicago_long
  baseline_cat <- levels(as.factor(chicago_long_mod$education))[1]
  baseline_rowname <- gsub("\\.", "_", baseline_cat)

  obd <-
    OaxacaBlinderDecomp(
      ln_real_wage ~ age + education | female,
      chicago_long_mod,
      baseline_invariant = TRUE,
      type = "threefold"
    )
  # Match and sort rownames
  obd_ests <- obd$varlevel
  rownames(obd_ests) <- gsub("education", "", rownames(obd_ests))
  rownames(obd_ests) <-
    gsub(".baseline", baseline_rowname, rownames(obd_ests))
  rownames(obd_ests) <- gsub("\\.", "_", rownames(obd_ests))
  obd_ests <- obd_ests[order(rownames(obd_ests)), ]

  # Get the same thing from saved Stata baseline results
  stata_dir <-
    testthat::test_path(
      "fixtures", "stata_results", "basic_3f"
    )
  stata_obd <-
    readRDS(file.path(stata_dir, "basic_3f.rds"))
  stata_obd_ests <- stata_obd[order(rownames(stata_obd)), ]

  testthat::expect_equal(
    obd_ests,
    stata_obd_ests
  )
})
