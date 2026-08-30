*******************************************************
* Brexit paper
* Ground-zero Stata replication from panel_wb.xlsx
* Current starting file has 468 observations
* 52 countries x 9 years, 2016-2024
*******************************************************

clear all
set more off

*******************************************************
* 1. Import starting file
*******************************************************

import excel "panel_wb.xlsx", sheet("Sheet 1") firstrow clear

describe
count
tab Year

*******************************************************
* 2. Panel identifier and basic checks
*******************************************************

capture drop DMC
encode Domicile_named_country, gen(DMC)

xtset DMC Year
isid DMC Year

assert Post == (Year >= 2021)
assert did == Treated*Post

count if missing(log_app, regression_weight, did, GDPpc, Pop15_64, YouthUnempl)
summ regression_weight

*******************************************************
* 3. Recreate the R transformations exactly
*    Unweighted centering over the 468-row estimation sample
*******************************************************

capture drop log_GDPpc c_log_GDPpc
gen double log_GDPpc = ln(GDPpc)
quietly summarize log_GDPpc, meanonly
gen double c_log_GDPpc = log_GDPpc - r(mean)

capture drop log_Pop15_64 c_log_Pop15_64
gen double log_Pop15_64 = ln(Pop15_64)
quietly summarize log_Pop15_64, meanonly
gen double c_log_Pop15_64 = log_Pop15_64 - r(mean)

capture drop c_YouthUnempl
quietly summarize YouthUnempl, meanonly
gen double c_YouthUnempl = YouthUnempl - r(mean)

summ c_log_GDPpc c_log_Pop15_64 c_YouthUnempl

*******************************************************
* 4. Model V interaction terms
*******************************************************

capture drop did_GDP did_Pop did_YU
gen double did_GDP = did*c_log_GDPpc
gen double did_Pop = did*c_log_Pop15_64
gen double did_YU  = did*c_YouthUnempl

*******************************************************
* 5. Explicit fixed-effect dummies
*    We use pooled xtscc with explicit country and year FE
*    so the coefficient objective is weighted LSDV
*******************************************************

capture drop FE_C*
capture drop FE_Y*

tab DMC, gen(FE_C)
tab Year, gen(FE_Y)

* FE_C1 and FE_Y1 are reference categories
* The current file has exactly 52 countries and 9 years

*******************************************************
* 6. Install commands if needed
*******************************************************

capture which xtscc
if _rc != 0 {
    ssc install xtscc
}

capture which esttab
if _rc != 0 {
    ssc install estout
}

*******************************************************
* 7. Table 3 models II-V
*
* Model I in the manuscript uses 520 observations,
* 2016-2025. It cannot be reproduced from this 468-row
* 2016-2024 starting file.
*******************************************************

estimates clear

* Model II
xtscc log_app did c_log_GDPpc FE_C2-FE_C52 FE_Y2-FE_Y9 [aw=regression_weight], pooled lag(1)
estimates store M2

* Model III
xtscc log_app did c_log_GDPpc c_log_Pop15_64 FE_C2-FE_C52 FE_Y2-FE_Y9 [aw=regression_weight], pooled lag(1)
estimates store M3

* Model IV
xtscc log_app did c_log_GDPpc c_log_Pop15_64 c_YouthUnempl FE_C2-FE_C52 FE_Y2-FE_Y9 [aw=regression_weight], pooled lag(1)
estimates store M4

* Model V
xtscc log_app did c_log_GDPpc c_log_Pop15_64 c_YouthUnempl did_GDP did_Pop did_YU FE_C2-FE_C52 FE_Y2-FE_Y9 [aw=regression_weight], pooled lag(1)
estimates store M5

esttab M2 M3 M4 M5, ///
    keep(did c_log_GDPpc c_log_Pop15_64 c_YouthUnempl did_GDP did_Pop did_YU) ///
    b(3) se(3) ///
    star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
    stats(N r2, fmt(0 3)) ///
    mtitles("II" "III" "IV" "V")

*******************************************************
* 8. Saturated country-specific DiD variables
*    Use ISO3 codes for readable and stable variable names
*******************************************************

capture drop SAT_*

levelsof iso3c if treated_factor != "CONTROL", local(eu_iso)

foreach c of local eu_iso {
    gen double SAT_`c' = Post*(iso3c == "`c'")
    quietly levelsof Domicile_named_country if iso3c == "`c'", local(country_name) clean
    label variable SAT_`c' "`country_name' x Post"
}

*******************************************************
* 9. Saturated models S1-S4
*******************************************************

local SATVARS
foreach c of local eu_iso {
    local SATVARS `SATVARS' SAT_`c'
}

* S1 saturated DiD
xtscc log_app `SATVARS' FE_C2-FE_C52 FE_Y2-FE_Y9 [aw=regression_weight], pooled lag(1)
estimates store S1

* S2 plus GDPpc
xtscc log_app `SATVARS' c_log_GDPpc FE_C2-FE_C52 FE_Y2-FE_Y9 [aw=regression_weight], pooled lag(1)
estimates store S2

* S3 plus GDPpc and population
xtscc log_app `SATVARS' c_log_GDPpc c_log_Pop15_64 FE_C2-FE_C52 FE_Y2-FE_Y9 [aw=regression_weight], pooled lag(1)
estimates store S3

* S4 plus GDPpc, population, and youth unemployment
xtscc log_app `SATVARS' c_log_GDPpc c_log_Pop15_64 c_YouthUnempl FE_C2-FE_C52 FE_Y2-FE_Y9 [aw=regression_weight], pooled lag(1)
estimates store S4

esttab S1 S2 S3 S4, ///
    keep(`SATVARS' c_log_GDPpc c_log_Pop15_64 c_YouthUnempl) ///
    label ///
    b(3) se(3) ///
    star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
    stats(N r2, fmt(0 3)) ///
    mtitles("S1 Saturated DiD" "S2 + GDPpc" "S3 + GDPpc + Pop" "S4 + GDPpc + Pop + YouthUnempl")

*******************************************************
* 10. Optional point-estimate verification against
*     ordinary weighted LSDV for Model V and S4
*******************************************************

reg log_app did c_log_GDPpc c_log_Pop15_64 c_YouthUnempl did_GDP did_Pop did_YU FE_C2-FE_C52 FE_Y2-FE_Y9 [aw=regression_weight]
estimates store M5_WLS_check

reg log_app `SATVARS' c_log_GDPpc c_log_Pop15_64 c_YouthUnempl FE_C2-FE_C52 FE_Y2-FE_Y9 [aw=regression_weight]
estimates store S4_WLS_check

*******************************************************
* Expected key S4 point estimates from R
*
* Austria      about -0.504
* Belgium      about -0.575
* Bulgaria     about -1.781
* Croatia      about -1.529
* Lithuania    about -2.164
* Poland       about -1.524
* Romania      about -1.597
* Slovenia     about -1.177
* Sweden       about -0.927
*
* Control coefficients in S4
* c_log_GDPpc       about 0.376
* c_log_Pop15_64    about 1.617
* c_YouthUnempl     about 0.002
*******************************************************
