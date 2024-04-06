varlevel <-
  read_stata_estimates(
    testthat::test_path("fixtures", "stata_baselines", "tooyoung.xlsx")
  )
saveRDS(varlevel, testthat::test_path("fixtures", "stata_baselines", "tooyoung.rds"))
