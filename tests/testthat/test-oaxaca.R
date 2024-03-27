testthat::test_that("categorical and dummy results match", {
  run_catg_and_dum <-
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
      obd_catg$varlevel <-
        obd_catg$varlevel[order(obd_catg$varlevel[, 1]), ]
      rownames(obd_catg$varlevel) <- NULL
      obd_catg$meta <- NULL

      obd_dum <-
        OaxacaBlinderDecomp(formula = fmla_dum,
                            data = data_dum,
                            type = obd_type)
      obd_dum$varlevel <-
        obd_dum$varlevel[order(obd_dum$varlevel[, 1]), ]
      rownames(obd_dum$varlevel) <- NULL
      obd_dum$meta <- NULL

      out <- list(obd_catg = obd_catg, obd_dum = obd_dum)
      out
    }

  # Set up long and dummy datasets and formulae ----

  chicago_long_mod <- chicago_long
  chicago_long_mod$education <-
    as.factor(chicago_long_mod$education) |>
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
    run_catg_and_dum(
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
    run_catg_and_dum(
      data_catg = chicago_long_mod,
      data_dum = chicago_mod,
      fmla_catg = fmla_foreign_catg,
      fmla_dum = fmla_foreign_dum,
      obd_type = "twofold"
    )
  testthat::expect_equal(
    no_drops_3f$obd_catg_varlevels,
    no_drops_3f$obd_dum_varlevels
  )

  # Test with dropped terms ----
  with_drops_3f <-
    run_catg_and_dum(
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
    run_catg_and_dum(
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
