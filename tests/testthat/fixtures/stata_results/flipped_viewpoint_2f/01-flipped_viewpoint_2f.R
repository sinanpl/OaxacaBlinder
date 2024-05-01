chicago_mod <- OaxacaBlinder::chicago

names(chicago_mod) <- gsub("\\.", "_", names(chicago_mod))

stata_dir <-
  testthat::test_path("fixtures", "stata_results", "flipped_viewpoint_2f")

dta_path <- file.path(stata_dir, "chicago_mod.dta")

chicago_mod |> haven::write_dta(dta_path)

command_2f <-
  paste(
    "oaxaca ln_real_wage", # Wide data only has ln_
    "age normalize(LTHS some_college college high_school advanced_degree),",
    "by(female) pooled swap"
  )

OaxacaBlinder:::make_decomp_dofile(
  command = command_2f,
  do_path = file.path(stata_dir, "flipped_viewpoint_2f.do"),
  dta_file = basename(dta_path),
  est_file = "flipped_viewpoint_2f"
)
#
