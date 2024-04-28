stata_dir <-
  testthat::test_path("fixtures", "stata_results", "basic_2f")
varlevel <-
  OaxacaBlinder:::read_stata_estimates(
    file.path(stata_dir, "basic_2f.xlsx"),
    type = "twofold"
  )
saveRDS(varlevel, file.path(stata_dir, "basic_2f.rds"))
