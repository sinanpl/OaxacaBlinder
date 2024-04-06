varlevel <-
  read_stata_estimates(
    testthat::test_path("fixtures", "tooyoung.xlsx")
  )
saveRDS(varlevel, testthat::test_path("fixtures", "tooyoung.rds"))
