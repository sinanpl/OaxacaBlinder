chicago_mod <- OaxacaBlinder::chicago
chicago_mod$native <- !chicago_mod$foreign.born

names(chicago_mod) <- gsub("\\.", "_", names(chicago_mod))

stata_dir <-
  testthat::test_path("fixtures", "stata_results", "basic_3f")

dta_path <- file.path(stata_dir, "chicago_mod.dta")

chicago_mod |> haven::write_dta(dta_path)

command <-
  paste(
    "oaxaca ln_real_wage", # Wide data only has ln_
    "age normalize(LTHS some_college college high_school advanced_degree)",
    "normalize(foreign_born native),",
    "by(female)"
  )

OaxacaBlinder:::make_decomp_dofile(
  command = command,
  do_path = file.path(stata_dir, "basic_3f.do"),
  dta_file = basename(dta_path),
  est_file = "basic_3f"
)
#
