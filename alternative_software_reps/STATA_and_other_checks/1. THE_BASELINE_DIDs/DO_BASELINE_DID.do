clear all
set more off

* ============================================================
* Stata replication of baseline saturated DiD
* ============================================================

* Go to the folder containing the Stata database
cd "C:\Users\Stoycho.rusinov\Documents\GitHub\Estimating_price_elasticity\Statistical_documentation_in_R\main_results\Stata_replications\The_baseline_DID"

* Load final database exported from R
use "final_regression_database_stata.dta", clear


describe
summarize

capture confirm numeric variable Year
if _rc {
    destring Year, replace
}

capture confirm numeric variable Applicants
if _rc {
    destring Applicants, replace
}

capture confirm numeric variable regression_weight
if _rc {
    destring regression_weight, replace
}

tab Year
tab Treated
tab Post

summ Applicants regression_weight


* Create log outcome and panel ID

capture drop ln_applicants
gen ln_applicants = log(Applicants)

capture drop country_id
encode Domicile_named_country, gen(country_id)

xtset country_id Year

* Construct country-specific post-treatment dummies
* This replicates fixest::i(treated_factor, Post, ref = "CONTROL")

capture drop post_c*

levelsof country_id if Treated == 1, local(treated_ids)

foreach id of local treated_ids {
    gen post_c`id' = (country_id == `id' & Post == 1)
}

ds post_c*

**# Model 1

* Equivalent to R:
* log(Applicants) ~ i(treated_factor, Post, ref = "CONTROL") |
* Domicile_named_country + Year

reg ln_applicants post_c* i.country_id i.Year [aw=regression_weight], vce(cluster country_id)
estimates store m_dummyFE_twfe

* Absorbed FE version
capture which reghdfe
if _rc {
    ssc install ftools, replace
    ssc install reghdfe, replace
}

reghdfe ln_applicants post_c* [aw=regression_weight], ///
    absorb(country_id Year) ///
    vce(cluster country_id)

estimates store m_reghdfe_twfe

**# Model 2
* Country FE only
* Equivalent to:
* log(Applicants) ~ i(treated_factor, Post, ref = "CONTROL") |
* Domicile_named_country

reghdfe ln_applicants post_c* [aw=regression_weight], ///
    absorb(country_id) ///
    vce(cluster country_id)

estimates store m_reghdfe_country

**# Model 3
* No fixed effects
* Equivalent to:
* log(Applicants) ~ i(treated_factor, Post, ref = "CONTROL")


reg ln_applicants post_c* [aw=regression_weight], vce(cluster country_id)
estimates store m_noFE

**# DK standard errors specification

*Before we close this session we will try to replicate the standard errors exactly

xtset country_id Year

xtscc ln_applicants post_c* i.Year [aw=regression_weight], fe