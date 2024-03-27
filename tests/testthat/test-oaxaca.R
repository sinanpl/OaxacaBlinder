testthat::test_that("categorical and dummy results match", {
  run_long_and_dum <-
    function(
      data_long,
      data_dum,
      fmla_long,
      fmla_dum
    ) {
      obd3_long <-
        OaxacaBlinderDecomp(formula = fmla_long,
                            data = data_long,
                            type = "threefold")
      obd3_long_varlevels <-
        obd3_long$varlevel[order(rownames(obd3_long$varlevel)),]
      obd3_dum <-
        OaxacaBlinderDecomp(formula = fmla_dum,
                            data = data_dum,
                            type = "threefold")
      obd3_dum_varlevels <-
        obd3_dum$varlevel[order(rownames(obd3_dum$varlevel)),]
      rownames(obd3_dum_varlevels) <-
        rownames(obd3_long_varlevels)
      out <-
        list(obd3_long_varlevels = obd3_long_varlevels,
             obd3_dum_varlevels = obd3_dum_varlevels)
      out
    }

  # Set up long and dummy datasets and formulae ----
  chicago_long_mod <- chicago_long
  chicago_long_mod$too_young <- chicago_long_mod$age < 19
  chicago_mod <- chicago
  chicago_mod$too_young <- chicago_mod$age < 19

  fmla_foreign_long <- ln_real_wage ~ education | foreign_born
  fmla_tooyoung_long <- ln_real_wage ~ education | too_young
  fmla_foreign <-
    ln.real.wage ~
    LTHS + some.college + college + high.school |
    foreign.born
  fmla_tooyoung <-
    ln.real.wage ~
    LTHS + some.college + college + high.school |
    too_young

  # Test without dropped items ----
  foreigns <-
    run_long_and_dum(
      data_long = chicago_long_mod,
      data_dum = chicago_mod,
      fmla_long = fmla_foreign_long,
      fmla_dum = fmla_foreign
    )
  testthat::expect_equal(
    foreigns$obd3_long_varlevels,
    foreigns$obd3_dum_varlevels
  )

  # Test version with dropped terms ----
  tooyoungs <-
    run_long_and_dum(
      data_long = chicago_long_mod,
      data_dum = chicago_mod,
      fmla_long = fmla_tooyoung_long,
      fmla_dum = fmla_tooyoung
    )
  testthat::expect_equal(
    tooyoungs$obd3_long_varlevels,
    tooyoungs$obd3_dum_varlevels
  )

})
