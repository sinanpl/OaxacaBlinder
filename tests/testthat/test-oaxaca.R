testthat::test_that("categorical and dummy results match", {
  run_catg_and_dum <-
    function(
      data_catg,
      data_dum,
      fmla_catg,
      fmla_dum,
      obd_type
    ) {
      obd3_catg <-
        OaxacaBlinderDecomp(formula = fmla_catg,
                            data = data_catg,
                            type = obd_type)
      obd3_catg_varlevels <-
        obd3_catg$varlevel[order(rownames(obd3_catg$varlevel)),]
      obd3_dum <-
        OaxacaBlinderDecomp(formula = fmla_dum,
                            data = data_dum,
                            type = obd_type)
      obd3_dum_varlevels <-
        obd3_dum$varlevel[order(rownames(obd3_dum$varlevel)),]
      rownames(obd3_dum_varlevels) <-
        rownames(obd3_catg_varlevels)
      out <-
        list(obd3_catg_varlevels = obd3_catg_varlevels,
             obd3_dum_varlevels = obd3_dum_varlevels)
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
  foreigns_3f <-
    run_catg_and_dum(
      data_catg = chicago_long_mod,
      data_dum = chicago_mod,
      fmla_catg = fmla_foreign_catg,
      fmla_dum = fmla_foreign_dum,
      obd_type = "threefold"
    )
  testthat::expect_equal(
    foreigns_3f$obd3_catg_varlevels,
    foreigns_3f$obd3_dum_varlevels
  )

  foreigns_2f <-
    run_catg_and_dum(
      data_catg = chicago_long_mod,
      data_dum = chicago_mod,
      fmla_catg = fmla_foreign_catg,
      fmla_dum = fmla_foreign_dum,
      obd_type = "twofold"
    )
  testthat::expect_equal(
    foreigns_3f$obd3_catg_varlevels,
    foreigns_3f$obd3_dum_varlevels
  )

  # Test with dropped terms ----
  tooyoungs_3f <-
    run_catg_and_dum(
      data_catg = chicago_long_mod,
      data_dum = chicago_mod,
      fmla_catg = fmla_tooyoung_catg,
      fmla_dum = fmla_tooyoung_dum,
      obd_type = "threefold"
    )
  testthat::expect_equal(
    tooyoungs_3f$obd3_catg_varlevels,
    tooyoungs_3f$obd3_dum_varlevels
  )

  tooyoungs_2f <-
    run_catg_and_dum(
      data_catg = chicago_long_mod,
      data_dum = chicago_mod,
      fmla_catg = fmla_tooyoung_catg,
      fmla_dum = fmla_tooyoung_dum,
      obd_type = "threefold"
    )
  testthat::expect_equal(
    tooyoungs_2f$obd3_catg_varlevels,
    tooyoungs_2f$obd3_dum_varlevels
  )

})
