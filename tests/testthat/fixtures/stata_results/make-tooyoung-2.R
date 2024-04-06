varlevel <-
  read_stata_estimates(
    testthat::test_path("fixtures", "stata_results", "tooyoung.xlsx")
  )
saveRDS(varlevel, testthat::test_path("fixtures", "stata_results", "tooyoung.rds"))
