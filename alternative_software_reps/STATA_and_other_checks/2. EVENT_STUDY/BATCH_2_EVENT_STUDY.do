clear all
set more off

use "C:\Users\Stoycho.rusinov\Documents\GitHub\Estimating_price_elasticity\Statistical_documentation_in_R\main_results\Stata_replications\2. EVENT_STUDY\panel_main.dta"

* ------------------------------------------------------------
* Prepare variables
* ------------------------------------------------------------

encode Domicile_named_country, gen(country)

gen cf = country
replace cf = 0 if treated_factor == "CONTROL"

gen et = Year - 2020

quietly summarize et
local emin = r(min)
local base = 0 - `emin'

gen etk = et - `emin'

* ------------------------------------------------------------
* Crosswalk cf to country name
* ------------------------------------------------------------

preserve
    keep cf Domicile_named_country
    keep if cf != 0
    duplicates drop
    tempfile xwalk
    save `xwalk'
restore

* ------------------------------------------------------------
* Estimate model
* ------------------------------------------------------------

reghdfe log_app ib0.cf#ib`base'.etk [aw=regression_weight], absorb(country Year)

local df = e(df_r)

* ------------------------------------------------------------
* Harvest coefficients
* ------------------------------------------------------------

matrix b = e(b)
matrix V = e(V)

local names : colnames b
local k = colsof(b)

clear
set obs `k'

gen str120 parm = ""
gen double raw_coefficient = .
gen double raw_se = .

forvalues j = 1/`k' {
    local nm : word `j' of `names'
    replace parm = "`nm'" in `j'
    replace raw_coefficient = b[1, `j'] in `j'
    replace raw_se = sqrt(V[`j', `j']) in `j'
}

* ------------------------------------------------------------
* Export raw coefficients so we can see what Stata named them
* ------------------------------------------------------------

export excel using ///
"C:\Users\Stoycho.rusinov\Documents\GitHub\Estimating_price_elasticity\Statistical_documentation_in_R\main_results\Stata_replications\2. EVENT_STUDY\raw_stata_coefficients.xlsx", ///
firstrow(variables) replace

list parm raw_coefficient raw_se, abbreviate(30)

* ------------------------------------------------------------
* Parse coefficient names safely
* ------------------------------------------------------------

gen str120 clean_parm = parm

replace clean_parm = subinstr(clean_parm, "bn.", ".", .)
replace clean_parm = subinstr(clean_parm, "b.",  ".", .)
replace clean_parm = subinstr(clean_parm, "o.",  ".", .)

gen byte is_event_coef = regexm(clean_parm, "^[0-9]+\.cf#[0-9]+\.etk$")

keep if is_event_coef == 1

gen cf = real(regexs(1)) if regexm(clean_parm, "^([0-9]+)\.cf")
gen etk_v = real(regexs(1)) if regexm(clean_parm, "#([0-9]+)\.etk$")

gen event_time = etk_v + `emin'

drop if cf == 0

* ------------------------------------------------------------
* Re-anchor each country to event_time 0 equals zero
* ------------------------------------------------------------

bysort cf (event_time): egen anchor0 = max(cond(event_time == 0, raw_coefficient, .))

gen coefficient = raw_coefficient - anchor0
gen std_error = raw_se

* ------------------------------------------------------------
* Merge country names
* ------------------------------------------------------------

merge m:1 cf using `xwalk', nogen

* ------------------------------------------------------------
* Add inference
* ------------------------------------------------------------

gen double t_stat = coefficient / std_error
gen double p_value = 2 * ttail(`df', abs(t_stat))
gen double ci_low  = coefficient - invttail(`df', 0.025) * std_error
gen double ci_high = coefficient + invttail(`df', 0.025) * std_error

* ------------------------------------------------------------
* Clean final table
* ------------------------------------------------------------

keep Domicile_named_country cf event_time coefficient std_error t_stat p_value ci_low ci_high raw_coefficient anchor0 parm clean_parm
order Domicile_named_country cf event_time coefficient std_error t_stat p_value ci_low ci_high raw_coefficient anchor0 parm clean_parm
sort Domicile_named_country event_time

rename Domicile_named_country country_name

export excel using ///
"C:\Users\Stoycho.rusinov\Documents\GitHub\Estimating_price_elasticity\Statistical_documentation_in_R\main_results\Stata_replications\2. EVENT_STUDY\event_study_coefficients.xlsx", ///
firstrow(variables) replace