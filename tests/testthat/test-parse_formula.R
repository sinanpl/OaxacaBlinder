# example test
testthat::test_that("parse_formula works", {
  fml = OaxacaBlinder:::parse_formula(
    data=OaxacaBlinder::chicago_long,
    formula=real_wage ~ age + education | gender
  )

  testthat::expect_equal(fml,
                         list(
                           dep_var = 'real_wage',
                           indep_var = 'age+education',
                           group_var = 'gender',
                           ref_level = "female"
                         ))
})
