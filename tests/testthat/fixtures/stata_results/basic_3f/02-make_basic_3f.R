stata_dir <-
  testthat::test_path("fixtures", "stata_results", "basic_3f")
varlevel <-
  OaxacaBlinder:::read_stata_estimates(
    file.path(stata_dir, "basic_3f.xlsx"),
    type = "threefold"
  )
saveRDS(varlevel, file.path(stata_dir, "basic_3f.rds"))
