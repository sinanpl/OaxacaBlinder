stata_dir <-
  testthat::test_path("fixtures", "stata_results", "flipped_viewpoint_3f")
varlevel <-
  OaxacaBlinder:::read_stata_estimates(
    file.path(stata_dir, "flipped_viewpoint_3f.xlsx"),
    type = "threefold"
  )
saveRDS(varlevel, file.path(stata_dir, "flipped_viewpoint_3f.rds"))
