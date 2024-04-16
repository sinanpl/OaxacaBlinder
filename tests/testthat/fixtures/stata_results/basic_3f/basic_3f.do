clear all
use chicago_mod.dta
oaxaca ln_real_wage age normalize(LTHS some_college college high_school advanced_degree), by(female)
etable, cstat(_r_b, nformat(%8.0g)) export(basic_3f.xlsx, replace)
