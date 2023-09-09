*******************
****General Setup**
*******************

clear  

set more off 

cap log close 

pwd  

cd "/Users/NicoleOY/Desktop/PS3"

log using hw3.log, text replace 

*****************
***load data*****
*****************

use judicature_2010.dta, clear

*append the judicature data sets
append using judicature_2012.dta

append using judicature_2014.dta

append using judicature_2016.dta

*merge the judicature data with provincial characteristics in 2010
merge m:1 hk_now_prov using province.dta

drop _merge

*generate the variable of reform timing for all individual

gen treat_t=.

replace treat_t=2014 if hk_now_prov==22 | hk_now_prov==31 | hk_now_prov==42 | hk_now_prov==44 | hk_now_prov==46 | hk_now_prov==52 | hk_now_prov==63

replace treat_t=2015 if hk_now_prov==14 | hk_now_prov==15 | hk_now_prov==23 | hk_now_prov==32 | hk_now_prov==33 | hk_now_prov==34 | hk_now_prov==35 | hk_now_prov==37 | hk_now_prov==50 | hk_now_prov==53 | hk_now_prov==64

replace treat_t=2016 if hk_now_prov==11 | hk_now_prov==12 | hk_now_prov==13 | hk_now_prov==21 | hk_now_prov==36 | hk_now_prov==41 | hk_now_prov==43 | hk_now_prov==45 | hk_now_prov==51 | hk_now_prov==54 | hk_now_prov==61 | hk_now_prov==62 | hk_now_prov==65

*present and sort the panel variable and time variable

xtset pid year 
sort pid year 

*******************************************
***Define the treatment dummy, post dummy, and interaction term********
*******************************************

gen Treat=0

replace Treat=1 if hk_now_prov==22 | hk_now_prov==31 | hk_now_prov==42 | hk_now_prov==44 | hk_now_prov==46 | hk_now_prov==52 | hk_now_prov==63 | hk_now_prov==14 | hk_now_prov==15 | hk_now_prov==23 | hk_now_prov==32 | hk_now_prov==33 | hk_now_prov==34 | hk_now_prov==35 | hk_now_prov==37 | hk_now_prov==50 | hk_now_prov==53 | hk_now_prov==64 | hk_now_prov==11 | hk_now_prov==12 | hk_now_prov==13 | hk_now_prov==21 | hk_now_prov==36 | hk_now_prov==41 | hk_now_prov==43 | hk_now_prov==45 | hk_now_prov==51 | hk_now_prov==54 | hk_now_prov==61 | hk_now_prov==62 | hk_now_prov==65

gen Post=0

replace Post=1 if year>=treat_t

gen Treat_Post=Treat*Post

*******************************************
***Validity test of DID********
*******************************************

*check determinants of reform in 2014
gen period1=0

replace period1=1 if treat_t==2014

by pid, sort: egen period2=max(period1) //by pid: Repeat Stata command on subsets (personal id) of the data

by pid: gen reform_14=period2

reg reform_14 population revenue expenditure highschool rural_inc urban_inc if year==2010, cluster(pid)
outreg2 using table1, excel dec(3) replace

*define Y2= timing of reform
gen timing=treat_t-2014

***determinants of reform timing

*provincial population
reg timing population if year==2010, cluster(pid)
outreg2 using table1, excel dec(3)

*government revenue
reg timing population revenue if year==2010, cluster(pid)
outreg2 using table1, excel dec(3)

*government expenditure
reg timing population revenue expenditure if year==2010, cluster(pid)
outreg2 using table1, excel dec(3)

*number of high school students
reg timing population revenue expenditure highschool if year==2010, cluster(pid)
outreg2 using table1, excel dec(3)

*rural per capita income
reg timing population revenue expenditure highschool rural_inc if year==2010, cluster(pid)
outreg2 using table1, excel dec(3)

*urban disposable income
reg timing population revenue expenditure highschool rural_inc urban_inc if year==2010, cluster(pid)
outreg2 using table1, excel dec(3)

*************************
***DID Regression********
*************************

//general DID specification
xi: xtreg govnwork Treat_Post i.year, fe cluster(pid)
outreg2 using table2, drop(_Iyear*)excel dec(3) replace


//Construct f(Wi, t): use second-order polynomial function of time
gen year2=year-2000

gen xa1=population*year2 //first-order
gen xa2=population*year2*year2 //second-order

gen xb1=revenue*year2
gen xb2=revenue*year2*year2

gen xc1=expenditure*year2
gen xc2=expenditure*year2*year2

gen xd1=highschool*year2
gen xd2=highschool*year2*year2

gen xe1=rural_inc*year2
gen xe2=rural_inc*year2*year2

gen xf1=urban_inc*year2
gen xf2=urban_inc*year2*year2

*DID specification controlling for significant determinants of reform
xi: xtreg govnwork Treat_Post population revenue expenditure highschool rural_inc urban_inc i.year, fe cluster(pid)
outreg2 using table2, drop(_Iyear*)excel dec(3)

*DID specification controlling for significant determinants of reform with time trend
xi: xtreg govnwork Treat_Post xa* xb* xc* xd* xe* xf* i.year, fe cluster(pid)
outreg2 using table2, drop(_Iyear* xa* xb* xc* xd* xe* xf*) excel dec(3)

*DID specification controlling for linear province time trend 
xi: xtreg govnwork Treat_Post i.year i.hk_now_prov*year2, fe cluster(pid)
outreg2 using table2, drop(_Iyear*)excel dec(3)


*DID specification controlling for quadratic province time trend 
gen year2_2=year2^2
xi: xtreg govnwork Treat_Post i.year i.hk_now_prov*year2_2, fe cluster(pid)
outreg2 using table2, drop(_Iyear*)excel dec(3)


*********************************************
***Check the common trends assumption********
*********************************************

//Generate Treat*Year dummy to replace Treat*Post
gen lag1=treat_t-2 //Pevious two years before the reform

gen prey2=0 //generate a dummy for being previous two years of reform
replace prey2=1 if year==lag1 

gen lag2=treat_t-4 //generate a dummy for being previous four years of reform

gen prey4=0
replace prey4=1 if year==lag2

gen prey0=0 //generate a dummy for being the year of reform
replace prey0=1 if year==treat_t

gen ahead1=treat_t+2 //generate a dummy for being post two years of reform

gen post1=0 //generate a dummy for being after one year of reform
replace post1=1 if year>=ahead1


//generate treatment * Year dummy 
gen tretprey2=Treat*prey2 
gen tretprey4=Treat*prey4
gen tretprey0=Treat*prey0
gen tretpost1=Treat*post1

//regressing using Treat*Year dummy
xi: xtreg govnwork tretprey4 tretprey2 tretprey0 tretpost1 xa* xb* xc* xd* xe* xf* i.year, fe cluster(pid)
outreg2 using table3, drop(_Iyear* xa* xb* xc* xd* xe* xf*) excel dec(3) replace

//Test that the post-treatment coefficients (not 0) are significantly different from pre-treatment coefficients (0)
test tretprey4=tretprey2=tretprey0=tretpost1

cap log close

