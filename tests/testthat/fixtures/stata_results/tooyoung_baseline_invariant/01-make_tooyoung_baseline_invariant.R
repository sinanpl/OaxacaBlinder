chicago_mod <- OaxacaBlinder::chicago
chicago_mod$unwage <- -chicago_mod$ln.real.wage
chicago_mod$too_young <- chicago_mod$age < 19
chicago_mod$native <- !chicago_mod$foreign.born

names(chicago_mod) <- gsub("\\.", "_", names(chicago_mod))

stata_dir <-
  testthat::test_path(
    "fixtures", "stata_results", "tooyoung_baseline_invariant"
  )

dta_path <- file.path(stata_dir, "chicago_mod.dta")

chicago_mod |> haven::write_dta(dta_path)

command <-
  paste(
    "oaxaca unwage",
    "normalize(LTHS some_college college high_school advanced_degree)",
    "normalize(foreign_born native),",
    "by(too_young) relax swap"
  )

make_decomp_dofile(
  command = command,
  do_path = file.path(stata_dir, "tooyoung_baseline_invariant.do"),
  dta_file = basename(dta_path),
  est_file = "tooyoung_baseline_invariant"
)
