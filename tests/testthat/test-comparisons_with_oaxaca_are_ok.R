# This file tests equality (approximate) between results of the
# oaxaca::oaxaca and OaxacaBlinder::OaxacaBlinderDecomp package


# fit several testing models ----------------------------------------------

formula = real_wage ~ age | male
dataset = OaxacaBlinder::chicago_long

# 5 fits are needed
# 4x (v1) Oaxacadecomp with twofold/threefold AND type=neumark/jann as options
# 1x (v2) oaxaca::oaxaca - this contains several runs with alternating options

modv1_twofold_neumark = OaxacaBlinder::OaxacaBlinderDecomp(
  formula=formula,
  data = dataset,
  type = 'twofold',
  pooled = 'neumark',
  baseline_invariant = FALSE,
  n_bootstraps = NULL
)
modv1_twofold_jann = OaxacaBlinder::OaxacaBlinderDecomp(
  formula=formula,
  data = dataset,
  type = 'twofold',
  pooled = 'jann',
  baseline_invariant = FALSE,
  n_bootstraps = NULL
)
modv1_threefold_neumark = OaxacaBlinder::OaxacaBlinderDecomp(
  formula=formula,
  data = dataset,
  type = 'threefold',
  pooled = 'neumark',
  baseline_invariant = FALSE,
  n_bootstraps = NULL
)
modv1_threefold_jann = OaxacaBlinder::OaxacaBlinderDecomp(
  formula=formula,
  data = dataset,
  type = 'threefold',
  pooled = 'jann',
  baseline_invariant = FALSE,
  n_bootstraps = NULL
)

modv2 = oaxaca::oaxaca(formula = formula, data = dataset, R=NULL)


# helper functions for comparison -----------------------------------------
# on overall / coefficient level
compare_v1v2_overall = function(modv1, modv2=modv2, type='neumark'){

  if(type=='neumark'){
    index=5
  } else if (type == "jann"){
    index=6
  }

  results = c(
    all.equal(abs(modv1$overall$explained)      , abs(modv2$twofold$overall[index, 'coef(explained)'])),
    all.equal(abs(modv1$overall$unexplained)    , abs(modv2$twofold$overall[index, 'coef(unexplained)'])),
    all.equal(abs(modv1$overall$unexplained_a)  , abs(modv2$twofold$overall[index, 'coef(unexplained B)'])),
    all.equal(abs(modv1$overall$unexplained_b)  , abs(modv2$twofold$overall[index, 'coef(unexplained A)']))
  )
  all(results)
}
compare_v1v2_coeflevel = function(modv1, modv2=modv2, type='jann'){
  if(type=='neumark'){
    index=5
  } else if (type == "jann"){
    index=6
  }
  cols_to_select = c(
    "coef(explained)",
    "coef(unexplained)",
    "coef(unexplained B)",
    "coef(unexplained A)"
  )
  df1 = abs(as.matrix(modv2$twofold$variables[[index]][, cols_to_select]))
  df2 = abs(as.matrix(modv1$varlevel))

  attributes(df1) = NULL
  attributes(df2) = NULL

  all.equal(df1, df2)
}


# execute comparisons -----------------------------------------------------

testthat::test_that("equal_results_twofold_neumark_twofold", {
  testthat::expect_true(compare_v1v2_overall(  modv1_twofold_neumark, modv2 = modv2, type='neumark'))
  testthat::expect_true(compare_v1v2_coeflevel(modv1_twofold_neumark, modv2 = modv2, type='neumark'))
})

# commented tests that do not work yet due to
# - name change level a / b
# - threefold not implemented
testthat::test_that("equal_results_twofold_jann_twofold", {
  testthat::expect_true(compare_v1v2_overall(modv1_twofold_jann, modv2=modv2, type='jann'))
  testthat::expect_true(compare_v1v2_coeflevel(modv1_twofold_jann, modv2=modv2, type='jann'))
})
testthat::test_that("equal_results_threefold_neumark_threefold", {
  testthat::expect_true(compare_v1v2_overall(modv1_threefold_neumark, modv2=modv2, type='neumark'))
  testthat::expect_true(compare_v1v2_coeflevel(modv1_threefold_neumark, modv2=modv2, type='neumark'))
})
testthat::test_that("equal_results_twofold_jann_threefold", {
  testthat::expect_true(compare_v1v2_overall(modv1_threefold_jann, modv2=modv2, type='jann'))
  testthat::expect_true(compare_v1v2_coeflevel(modv1_threefold_jann, modv2=modv2, type='jann'))
})

