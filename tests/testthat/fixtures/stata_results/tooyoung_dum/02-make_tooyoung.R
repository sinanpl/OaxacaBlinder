stata_dir <-
  testthat::test_path("fixtures", "stata_results", "tooyoung_dum")
varlevel <-
  read_stata_estimates(
    file.path(stata_dir, "tooyoung.xlsx")
  )
saveRDS(varlevel, file.path(stata_dir, "tooyoung.rds"))
