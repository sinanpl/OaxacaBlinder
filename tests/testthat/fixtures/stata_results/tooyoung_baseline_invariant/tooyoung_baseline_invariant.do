clear all
use chicago_mod.dta
oaxaca unwage normalize(LTHS some_college college high_school advanced_degree) normalize(foreign_born native), by(too_young) relax swap
etable, cstat(_r_b, nformat(%8.0g)) export(tooyoung_baseline_invariant.xlsx, replace)
