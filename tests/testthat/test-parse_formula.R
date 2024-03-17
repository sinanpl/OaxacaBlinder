# example test
test_that("parse_formula works", {
  fml = OaxacaBlinder:::parse_formula(real_wage ~ age + education | female)

  testthat::expect_equal(fml,
                         list(
                           dep_var = 'real_wage',
                           indep_var = 'age+education',
                           group_var = 'female'
                         ))
})
