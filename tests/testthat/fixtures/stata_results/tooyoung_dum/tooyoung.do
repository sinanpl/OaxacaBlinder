clear all
use chicago_mod.dta
oaxaca ln_real_wage LTHS some_college college high_school, by(too_young) relax
etable, cstat(_r_b, nformat(%8.0g)) export(tooyoung.xlsx, replace)
