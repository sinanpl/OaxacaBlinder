clear all
use chicago_mod.dta
oaxaca ln_real_wage normalize(LTHS some_college college high_school advanced_degree), by(too_young) relax
etable, cstat(_r_b, nformat(%8.0g)) export(tooyoung_baseline_invariant.xlsx, replace)
