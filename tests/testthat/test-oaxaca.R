testthat::test_that("threefold matches other calcs", {
  # Setup ----
  fmla_lm_dum <-
    ln.real.wage ~
    LTHS + some.college + college + advanced.degree
  fmla_foreign_dum <-
    ln.real.wage ~
    LTHS + some.college + college + advanced.degree |
    foreign.born

  # Calc by hand ----
  foreign1 <- chicago[chicago$foreign.born == 1 ,]
  foreign0 <- chicago[chicago$foreign.born == 0 ,]
  fit_foreign1 <- lm(fmla_lm_dum, data = foreign1)
  fit_foreign0 <- lm(fmla_lm_dum, data = foreign0)
  EX_foreign1 <- apply(model.matrix(fit_foreign1), 2, mean)
  EX_foreign0 <- apply(model.matrix(fit_foreign0), 2, mean)
  B_foreign1 <- fit_foreign1$coefficients
  B_foreign0 <- fit_foreign0$coefficients
  manual_3f_terms <-
    data.frame(
      # naive calcs since no terms drop
      endowments = (EX_foreign0 - EX_foreign1) * B_foreign1,
      coefficients = EX_foreign1 * (B_foreign0 - B_foreign1),
      interaction =
        (EX_foreign0 - EX_foreign1) * (B_foreign0 - B_foreign1)
    )
  manual_3f_terms <- manual_3f_terms[order(manual_3f_terms[, 1]), ]

  # Calc with OaxacaBlinderDecomp() ----
  obd3 <-
    OaxacaBlinderDecomp(
      formula = fmla_foreign_dum,
      data = chicago,
      type = "threefold"
    )
  obd_3f_terms <-
    obd3$varlevel[order(obd3$varlevel[, 1]), ]

  # Calc with oaxaca::oaxaca() ----
  oax <-
    oaxaca::oaxaca(
      fmla_foreign_dum,
      data = chicago,
      R = NULL
    )
  oax_3f_var <-
    as.data.frame(oax$threefold$variables)[c(1, 3, 5)] |>
    `names<-`(names(obd3$varlevel))
  oax_3f_terms <-
    oax_3f_var[order(oax_3f_var[, 1]), ]

  # Confirm manual and oaxaca::oaxaca()'s results match ----
  testthat::expect_equal(
    manual_3f_terms,
    oax_3f_terms
  )

  # Confirm function results match ----
  testthat::expect_equal(
    obd_3f_terms,
    oax_3f_terms
  )

})

testthat::test_that("neumark twofold matches manual calcs", {
  # Setup ----
  fmla_lm_dum <-
    ln.real.wage ~
    LTHS + some.college + college + advanced.degree
  fmla_foreign_dum <-
    ln.real.wage ~
    LTHS + some.college + college + advanced.degree |
    foreign.born

  # Calc by hand ----
  foreign1 <- chicago[chicago$foreign.born == 1 ,]
  foreign0 <- chicago[chicago$foreign.born == 0 ,]
  fit_foreign1 <- lm(fmla_lm_dum, data = foreign1)
  fit_foreign0 <- lm(fmla_lm_dum, data = foreign0)
  fit_pooled <- lm(fmla_lm_dum, data = chicago)
  EX_foreign1 <- apply(model.matrix(fit_foreign1), 2, mean)
  EX_foreign0 <- apply(model.matrix(fit_foreign0), 2, mean)
  EX_pooled <- apply(model.matrix(fit_pooled), 2, mean)
  B_foreign1 <- fit_foreign1$coefficients
  B_foreign0 <- fit_foreign0$coefficients
  B_pooled <- fit_pooled$coefficients
  manual_2f_terms <-
    data.frame(
      # naive calcs since no terms drop
      explained = (EX_foreign0 - EX_foreign1) * B_pooled,
      unexplained_a = EX_foreign0 * (B_foreign0 - B_pooled),
      unexplained_b = EX_foreign1 * (B_pooled - B_foreign1)
    )
  manual_2f_terms$unexplained <-
    manual_2f_terms$unexplained_a + manual_2f_terms$unexplained_b
  manual_2f_terms <-
    manual_2f_terms[
      order(manual_2f_terms[, 1]),
      c("explained", "unexplained", "unexplained_a", "unexplained_b")
    ]

  # Calc with OaxacaBlinderDecomp() ----
  obd2 <-
    OaxacaBlinderDecomp(
      formula = fmla_foreign_dum,
      data = chicago,
      type = "twofold",
      pooled = "neumark"
    )
  obd_2f_terms <-
    obd2$varlevel[order(obd2$varlevel[, 1]), ]

  testthat::expect_equal(
    obd_2f_terms,
    manual_2f_terms
  )

})

testthat::test_that("categorical and dummy results match", {
  run_educ_catg_and_dum <-
    function(
      data_catg,
      data_dum,
      fmla_catg,
      fmla_dum,
      obd_type
    ) {
      obd_catg <-
        OaxacaBlinderDecomp(formula = fmla_catg,
                            data = data_catg,
                            type = obd_type)
      rownames(obd_catg$varlevel) <-
        gsub("education", "", rownames(obd_catg$varlevel))
      obd_catg$varlevel <-
        obd_catg$varlevel[order(rownames(obd_catg$varlevel)), ]
      obd_catg$meta <- NULL

      obd_dum <-
        OaxacaBlinderDecomp(formula = fmla_dum,
                            data = data_dum,
                            type = obd_type)
      obd_dum$varlevel <-
        obd_dum$varlevel[order(rownames(obd_dum$varlevel)), ]
      obd_dum$meta <- NULL

      out <- list(obd_catg = obd_catg, obd_dum = obd_dum)
      out
    }

  # Set up long and dummy datasets and formulae ----

  chicago_long_mod <- chicago_long
  chicago_long_mod$education <-
    as.factor(chicago_long_mod$education) |>
    relevel(ref = "LTHS") |>
    relevel(ref = "advanced.degree") # force in spite of sorting
  chicago_long_mod$too_young <- chicago_long_mod$age < 19

  chicago_mod <- chicago
  chicago_mod$too_young <- chicago_mod$age < 19

  fmla_foreign_catg <- ln_real_wage ~ education | foreign_born
  fmla_tooyoung_catg <- ln_real_wage ~ education | too_young

  fmla_foreign_dum <-
    ln.real.wage ~
    LTHS + some.college + college + high.school |
    foreign.born
  fmla_tooyoung_dum <-
    ln.real.wage ~
    LTHS + some.college + college + high.school |
    too_young

  # Test without dropped items ----
  no_drops_3f <-
    run_educ_catg_and_dum(
      data_catg = chicago_long_mod,
      data_dum = chicago_mod,
      fmla_catg = fmla_foreign_catg,
      fmla_dum = fmla_foreign_dum,
      obd_type = "threefold"
    )
  testthat::expect_equal(
    no_drops_3f$obd_catg,
    no_drops_3f$obd_dum
  )

  no_drops_2f <-
    run_educ_catg_and_dum(
      data_catg = chicago_long_mod,
      data_dum = chicago_mod,
      fmla_catg = fmla_foreign_catg,
      fmla_dum = fmla_foreign_dum,
      obd_type = "twofold"
    )
  testthat::expect_equal(
    no_drops_2f$obd_catg_varlevels,
    no_drops_2f$obd_dum_varlevels
  )

  # Test with dropped terms ----
  with_drops_3f <-
    run_educ_catg_and_dum(
      data_catg = chicago_long_mod,
      data_dum = chicago_mod,
      fmla_catg = fmla_tooyoung_catg,
      fmla_dum = fmla_tooyoung_dum,
      obd_type = "threefold"
    )
  testthat::expect_equal(
    with_drops_3f$obd_catg,
    with_drops_3f$obd_dum
  )

  with_drops_2f <-
    run_educ_catg_and_dum(
      data_catg = chicago_long_mod,
      data_dum = chicago_mod,
      fmla_catg = fmla_tooyoung_catg,
      fmla_dum = fmla_tooyoung_dum,
      obd_type = "threefold"
    )
  testthat::expect_equal(
    with_drops_2f$obd_catg,
    with_drops_2f$obd_dum
  )

})

test_that("0-variance dummy IV results match Stata", {
  chicago_mod <- chicago
  chicago_mod$too_young <- chicago_mod$age < 19
  names(chicago_mod) <- gsub("\\.", "_", names(chicago_mod))

  fmla_tooyoung_dum <-
    ln_real_wage ~
    LTHS + some_college + college + high_school |
    too_young

  obd <-
    OaxacaBlinderDecomp(
      fmla_tooyoung_dum,
      chicago_mod,
      type = "threefold"
    )
  obd_ests <- obd$varlevel[order(rownames(obd$varlevel)), ]

  # Get the same thing from saved Stata baseline results
  stata_dir <-
    testthat::test_path("fixtures", "stata_results", "tooyoung_dum")
  stata_obd <- readRDS(file.path(stata_dir, "tooyoung.rds"))
  stata_obd_ests <- stata_obd[order(rownames(stata_obd)), ]

  testthat::expect_equal(
    obd_ests,
    stata_obd_ests
  )
})

test_that("0-variance categorical IV results match Stata", {
  chicago_long_mod <- chicago_long
  chicago_long_mod$education <-
    as.factor(chicago_long_mod$education) |>
    relevel(ref = "LTHS") |>
    relevel(ref = "advanced.degree") # force in spite of sorting
  chicago_long_mod$too_young <- chicago_long_mod$age < 19

  obd <-
    OaxacaBlinderDecomp(
      formula = ln_real_wage ~ education | too_young,
      data = chicago_long_mod,
      type = "threefold"
    )
  obd_ests <- obd$varlevel
  rownames(obd_ests) <- gsub("education", "", rownames(obd_ests))
  rownames(obd_ests) <- gsub("\\.", "_", rownames(obd_ests))
  obd_ests <- obd_ests[order(rownames(obd_ests)), ]

  # Get the same thing from saved Stata baseline results
  stata_dir <-
    testthat::test_path("fixtures", "stata_results", "tooyoung_dum")
  stata_obd <- readRDS(file.path(stata_dir, "tooyoung.rds"))
  stata_obd_ests <- stata_obd[order(rownames(stata_obd)), ]

  testthat::expect_equal(
    obd_ests,
    stata_obd_ests
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
      "fixtures", "stata_results", "tooyoung_baseline_invariant"
    )
  stata_obd <-
    readRDS(file.path(stata_dir, "tooyoung_baseline_invariant.rds"))
  stata_obd_ests <- stata_obd[order(rownames(stata_obd)), ]

  testthat::expect_equal(
    obd_ests,
    stata_obd_ests
  )
})
