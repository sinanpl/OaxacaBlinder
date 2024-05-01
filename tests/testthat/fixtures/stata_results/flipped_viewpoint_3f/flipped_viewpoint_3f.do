clear all
use chicago_mod.dta
oaxaca ln_real_wage age normalize(LTHS some_college college high_school advanced_degree), by(female) swap
etable, cstat(_r_b, nformat(%8.0g)) export(flipped_viewpoint_3f.xlsx, replace)
