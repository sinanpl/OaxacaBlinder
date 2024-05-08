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

test_that("baseline-adjusted-IV Jann twofold results match Stata", {
  chicago_long_mod <- chicago_long
  baseline_cat <- levels(as.factor(chicago_long_mod$education))[1]
  baseline_rowname <- gsub("\\.", "_", baseline_cat)

  obd <-
    OaxacaBlinderDecomp(
      ln_real_wage ~ age + education | female,
      chicago_long_mod,
      baseline_invariant = TRUE,
      type = "twofold",
      pooled = "jann"
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
      "fixtures", "stata_results", "basic_2f"
    )
  stata_obd <-
    readRDS(file.path(stata_dir, "basic_2f.rds"))
  stata_obd_ests <- stata_obd[order(rownames(stata_obd)), ]

  testthat::expect_equal(
    obd_ests[1:2],
    stata_obd_ests
  )
})

test_that("threefold results with bootstraps haven't changed", {
  set.seed(1973)
  threefold <- OaxacaBlinderDecomp(
    formula = ln_real_wage ~ age + education | female,
    data = chicago_long,
    type = "threefold",
    baseline_invariant = TRUE,
    n_bootstraps = 10
  )
  # testthat::expect_snapshot(summary(threefold))
  # testthat::expect_snapshot(coef(threefold, ci = TRUE))
})

test_that("twofold results with bootstraps haven't changed", {
  set.seed(1973)
  twofold <- OaxacaBlinderDecomp(
    formula = ln_real_wage ~ age + education | female,
    data = chicago_long,
    type = "twofold",
    pooled = "jann",
    baseline_invariant = TRUE,
    n_bootstraps = 10
  )
  x <- coef(twofold, ci = TRUE)
  x["coefficient"] <- round(x["coefficient"], 3)
  x["se"] <- round(x["se"], 3)
  x["2.5%"] <- round(x[, "2.5%"], 3)
  x["97.5%"] <- round(x[, "97.5%"], 3)
  rounded_coefs <- x
  testthat::expect_snapshot(rounded_coefs)
})

test_that("sums check when some bootstraps have 0 variance", {
  most_foreign_advdeg <-
    chicago_long$foreign_born &
      chicago_long$education == "advanced.degree" &
      chicago_long$age < 49 # all but 3
  chicago_long_mod <-
    chicago_long[!most_foreign_advdeg, ]

  set.seed(1973)
  testthat::expect_no_warning(
    # This should have 0-variance levels in 3 of 10 runs
    obd <-
      OaxacaBlinderDecomp(
        formula = ln_real_wage ~ education | foreign_born,
        data = chicago_long_mod,
        type = "threefold",
        n_bootstraps = 10
      ),
    message = "bootstrap runs and were discarded."
  )
})

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
  foreign1 <- chicago[chicago$foreign.born == 1, ]
  foreign0 <- chicago[chicago$foreign.born == 0, ]
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
  foreign1 <- chicago[chicago$foreign.born == 1, ]
  foreign0 <- chicago[chicago$foreign.born == 0, ]
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

testthat::test_that("threefold categ. and dummy results match", {
  conform_educ_results <- function(obd) {
    rownames(obd$varlevel) <-
      gsub("education", "", rownames(obd$varlevel))
    obd$varlevel <-
      obd$varlevel[order(rownames(obd$varlevel)), ]
    obd$meta <- NULL
    obd
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
  no_drops_3f_catg <-
    OaxacaBlinderDecomp(
      formula = fmla_foreign_catg,
      data = chicago_long_mod,
      type = "threefold"
    ) |>
    conform_educ_results()
  no_drops_3f_dum <-
    OaxacaBlinderDecomp(
      formula = fmla_foreign_dum,
      data = chicago_mod,
      type = "threefold"
    ) |>
    conform_educ_results()

  testthat::expect_equal(
    no_drops_3f_catg,
    no_drops_3f_dum
  )

  # Test with dropped terms ----
  with_drops_3f_catg <-
    OaxacaBlinderDecomp(
      formula = fmla_tooyoung_catg,
      data = chicago_long_mod,
      type = "threefold"
    ) |>
    conform_educ_results()
  with_drops_3f_dum <-
    OaxacaBlinderDecomp(
      formula = fmla_tooyoung_dum,
      data = chicago_mod,
      type = "threefold"
    ) |>
    conform_educ_results()

  testthat::expect_equal(
    with_drops_3f_catg,
    with_drops_3f_dum
  )
})

testthat::test_that("twofold categ. and dummy results match", {
  conform_educ_results <- function(obd) {
    rownames(obd$varlevel) <-
      gsub("education", "", rownames(obd$varlevel))
    obd$varlevel <-
      obd$varlevel[order(rownames(obd$varlevel)), ]
    obd$meta <- NULL
    obd
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
  no_drops_2f_catg <-
    OaxacaBlinderDecomp(
      formula = fmla_foreign_catg,
      data = chicago_long_mod,
      type = "twofold"
    ) |>
    conform_educ_results()
  no_drops_2f_dum <-
    OaxacaBlinderDecomp(
      formula = fmla_foreign_dum,
      data = chicago_mod,
      type = "twofold"
    ) |>
    conform_educ_results()

  testthat::expect_equal(
    no_drops_2f_catg,
    no_drops_2f_dum
  )

  # Test with dropped terms ----
  with_drops_2f_catg <-
    OaxacaBlinderDecomp(
      formula = fmla_tooyoung_catg,
      data = chicago_long_mod,
      type = "twofold"
    ) |>
    conform_educ_results()
  with_drops_2f_dum <-
    OaxacaBlinderDecomp(
      formula = fmla_tooyoung_dum,
      data = chicago_mod,
      type = "twofold"
    ) |>
    conform_educ_results()

  testthat::expect_equal(
    with_drops_2f_catg,
    with_drops_2f_dum
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

test_that("strange categ. level names don't change results", {
  chicago_long_mod <- chicago_long
  chicago_long_mod$education <-
    gsub(
      "some.college",
      "some college'd",
      chicago_long_mod$education
    )

  obd_sensible_level <-
    OaxacaBlinderDecomp(
      ln_real_wage ~ education | foreign_born,
      chicago_long,
      baseline_invariant = TRUE,
      type = "threefold"
    )
  obd_sensible_level$meta <- NULL

  obd_silly_level <-
    OaxacaBlinderDecomp(
      ln_real_wage ~ education | foreign_born,
      chicago_long_mod,
      baseline_invariant = TRUE,
      type = "threefold"
    )
  obd_silly_level$meta <- NULL
  rownames(obd_silly_level$varlevel) <-
    gsub("ege\\.d", "ege", rownames(obd_silly_level$varlevel))

  testthat::expect_equal(
    obd_silly_level,
    obd_sensible_level
  )
})

test_that("bootstrapped gaps haven't changed", {
  set.seed(1973)
  obd <- OaxacaBlinderDecomp(
    formula = ln_real_wage ~ age + education | female,
    data = chicago_long,
    baseline_invariant = TRUE,
    n_bootstraps = 10
  )
  testthat::expect_snapshot(obd$bootstraps$gaps)
})

test_that("threefold results with flipped non-factor viewpoint match Stata", {
  chicago_long_mod <- chicago_long
  baseline_cat <- levels(as.factor(chicago_long_mod$education))[1]
  baseline_rowname <- gsub("\\.", "_", baseline_cat)

  obd <-
    OaxacaBlinderDecomp(
      ln_real_wage ~ age + education | female,
      chicago_long_mod,
      baseline_invariant = TRUE,
      viewpoint_group = 0,
      type = "threefold"
    )
  # Match and sort rownames
  obd_ests <- obd$varlevel[c("endowments", "coefficients", "interaction")]
  rownames(obd_ests) <- gsub("education", "", rownames(obd_ests))
  rownames(obd_ests) <-
    gsub(".baseline", baseline_rowname, rownames(obd_ests))
  rownames(obd_ests) <- gsub("\\.", "_", rownames(obd_ests))
  obd_ests <- obd_ests[order(rownames(obd_ests)), ]

  # Get the same thing from saved Stata baseline results
  stata_dir <-
    testthat::test_path(
      "fixtures", "stata_results", "flipped_viewpoint_3f"
    )
  stata_obd <-
    readRDS(file.path(stata_dir, "flipped_viewpoint_3f.rds"))
  stata_obd_ests <- stata_obd[order(rownames(stata_obd)), ]

  testthat::expect_equal(
    obd_ests,
    stata_obd_ests
  )
})

test_that("threefold results with flipped factor viewpoint match Stata", {
  chicago_long_mod <- chicago_long
  chicago_long_mod$female <-
    factor(chicago_long_mod$female, c("0", "1"), c("male", "female"))
  baseline_cat <- levels(as.factor(chicago_long_mod$education))[1]
  baseline_rowname <- gsub("\\.", "_", baseline_cat)

  obd <-
    OaxacaBlinderDecomp(
      ln_real_wage ~ age + education | female,
      chicago_long_mod,
      baseline_invariant = TRUE,
      viewpoint_group = "male",
      type = "threefold"
    )
  # Match and sort rownames
  obd_ests <- obd$varlevel[c("endowments", "coefficients", "interaction")]
  rownames(obd_ests) <- gsub("education", "", rownames(obd_ests))
  rownames(obd_ests) <-
    gsub(".baseline", baseline_rowname, rownames(obd_ests))
  rownames(obd_ests) <- gsub("\\.", "_", rownames(obd_ests))
  obd_ests <- obd_ests[order(rownames(obd_ests)), ]

  # Get the same thing from saved Stata baseline results
  stata_dir <-
    testthat::test_path(
      "fixtures", "stata_results", "flipped_viewpoint_3f"
    )
  stata_obd <-
    readRDS(file.path(stata_dir, "flipped_viewpoint_3f.rds"))
  stata_obd_ests <- stata_obd[order(rownames(stata_obd)), ]

  testthat::expect_equal(
    obd_ests,
    stata_obd_ests
  )
})

test_that("threefold results with auto-flipped factor viewpoint match Stata", {
  chicago_long_mod <- chicago_long
  chicago_long_mod$female <-
    factor(chicago_long_mod$female, c("1", "0"), c("female", "male"))
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
  obd_ests <- obd$varlevel[c("endowments", "coefficients", "interaction")]
  rownames(obd_ests) <- gsub("education", "", rownames(obd_ests))
  rownames(obd_ests) <-
    gsub(".baseline", baseline_rowname, rownames(obd_ests))
  rownames(obd_ests) <- gsub("\\.", "_", rownames(obd_ests))
  obd_ests <- obd_ests[order(rownames(obd_ests)), ]

  # Get the same thing from saved Stata baseline results
  stata_dir <-
    testthat::test_path(
      "fixtures", "stata_results", "flipped_viewpoint_3f"
    )
  stata_obd <-
    readRDS(file.path(stata_dir, "flipped_viewpoint_3f.rds"))
  stata_obd_ests <- stata_obd[order(rownames(stata_obd)), ]

  testthat::expect_equal(
    obd_ests,
    stata_obd_ests
  )
})

test_that("Jann twofold results with flipped non-factor viewpoint match Stata", {
  chicago_long_mod <- chicago_long
  baseline_cat <- levels(as.factor(chicago_long_mod$education))[1]
  baseline_rowname <- gsub("\\.", "_", baseline_cat)

  obd <-
    OaxacaBlinderDecomp(
      ln_real_wage ~ age + education | female,
      chicago_long_mod,
      baseline_invariant = TRUE,
      viewpoint_group = 0,
      type = "twofold",
      pooled = "jann"
    )
  # Match and sort rownames
  obd_ests <-
    obd$varlevel[c("explained", "unexplained", "unexplained_a", "unexplained_b")]
  rownames(obd_ests) <- gsub("education", "", rownames(obd_ests))
  rownames(obd_ests) <-
    gsub(".baseline", baseline_rowname, rownames(obd_ests))
  rownames(obd_ests) <- gsub("\\.", "_", rownames(obd_ests))
  obd_ests <- obd_ests[order(rownames(obd_ests)), ]

  # Get the same thing from saved Stata baseline results
  stata_dir <-
    testthat::test_path(
      "fixtures", "stata_results", "flipped_viewpoint_2f"
    )
  stata_obd <-
    readRDS(file.path(stata_dir, "flipped_viewpoint_2f.rds"))
  stata_obd_ests <- stata_obd[order(rownames(stata_obd)), ]

  testthat::expect_equal(
    obd_ests[1:2],
    stata_obd_ests
  )
})
