chicago_mod <- OaxacaBlinder::chicago
chicago_mod$too_young <- chicago_mod$age < 19

names(chicago_mod) <- gsub("\\.", "_", names(chicago_mod))

stata_dir <-
  testthat::test_path("fixtures", "stata_results", "tooyoung_dum")

dta_path <- file.path(stata_dir, "chicago_mod.dta")

chicago_mod |> haven::write_dta(dta_path)

command <-
  paste(
    "oaxaca ln_real_wage LTHS some_college college high_school,",
    "by(too_young) relax"
  )

make_decomp_dofile(
  command = command,
  do_path = file.path(stata_dir, "tooyoung.do"),
  dta_file = basename(dta_path),
  est_file = "tooyoung"
)
