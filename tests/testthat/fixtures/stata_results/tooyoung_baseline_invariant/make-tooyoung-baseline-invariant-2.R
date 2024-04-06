stata_dir <-
  testthat::test_path(
    "fixtures", "stata_results", "tooyoung_baseline_invariant"
    )
varlevel <-
  read_stata_estimates(
    file.path(stata_dir, "tooyoung_baseline_invariant.xlsx")
  )
saveRDS(
  varlevel,
  file.path(stata_dir, "tooyoung_baseline_invariant.rds")
)
