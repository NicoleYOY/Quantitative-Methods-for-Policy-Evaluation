*******************
****General Setup**
*******************

clear  

set more off 

cap log close 

pwd  

cd "/Users/NicoleOY/Desktop/ECO3211 PS4"


log using hw4.log, text replace 

*****************
***load data*****
*****************

use "/Users/NicoleOY/Desktop/ECO3211 PS4/CFPS 2010/ecfps2010adult_202008.dta", clear

*******************************************************************************
// 1. Merge the datasets
*******************************************************************************
use "/Users/NicoleOY/Desktop/ECO3211 PS4/CFPS 2010/ecfps2010adult_202008.dta",clear
merge 1:m pid fid using "/Users/NicoleOY/Desktop/ECO3211 PS4/CFPS 2010/ecfps2010famconf_202008.dta"
keep if _merge==3
sort qa1y_best
gen gqa1y_best =group(10) // divide the sample into 10 groups
tab gqa1y_best
drop if gqa1y_best==1 | gqa1y_best==10 // drop the bottom and top 10%

*******************************************************************************
// 2. Define the interested variables
*******************************************************************************
gen policy_year=.
decode provcd,gen(provcd_name)
replace policy_year=1969 if provcd_name=="Shanghai" |provcd_name=="Zhejiang"|provcd_name=="Jiangxi"|provcd_name=="Sichuan"
replace policy_year=1970 if provcd_name=="Beijing" |provcd_name=="Tianjin"|provcd_name=="Hebei"|provcd_name=="Shanxi" |provcd_name=="Liaoning"|provcd_name=="Jilin"|provcd_name=="Heilongjiang"|provcd_name=="Jiangsu"|provcd_name=="Shandong"|provcd_name=="Henan"|provcd_name=="Hubei"|provcd_name=="Guangdong"|provcd_name=="Chongqing"|provcd_name=="Yunnan"
replace policy_year=1971 if provcd_name=="Anhui" |provcd_name=="Guizhou"|provcd_name=="Shaanxi"
replace policy_year=1972 if provcd_name=="Fujian" 
replace policy_year=1974 if provcd_name=="Gansu" 
replace policy_year=1975 if provcd_name=="Hunan" |provcd_name=="Guangxi Zhuang Autonomous Region"
tab policy_year

gen month_year1=ym( qa1y_best , qa1m )
replace month_year1=. if qa1y_best<0
format month_year1 %tm
gen x=month_year1 // define the year month of birth as the running varibale

gen month=9
gen cutoff=ym( policy_year , month ) // Setting the cutoff point
format cutoff %tm
replace x=x-cutoff // define the distance of the running variable 

gen d=0
replace d=1 if x>=0 // generate the location dummy
gen dx=d*x  // the interaction term

*******************************************************************************
// 3. Validity test of RDD
*******************************************************************************
* The compulsory education is a sharp RDD. After the cutoff point, the probability of treatment is 1 and before the cutoff point, the probability is 0. No need to check the first stage discontinuity

* Density test of x: histogram
histogram x, discrete width(1) addplot (pci 0 0 0.005 0) legend(col(2)) xtitle("Birth Cohort")
graph save "/Users/NicoleOY/Desktop/ECO3211 PS4/density_histogram1",replace

*Density test: DCdensity
DCdensity x, breakpoint(0) generate(xj yj r0 fhat se_fhat)
graph save "/Users/NicoleOY/Desktop/ECO3211 PS4/density_DCdensity1",replace

*Density test of x :regression test 

// Generate the number of observations
bys x:gen number=_N

// Optimal bandwidth for reduce form regression
rdbwselect number x, all //use rdbwselect to calculate optimal bandwidth (h) for number
gen bw_reduce_number_CCT=round(e(h_CCT)) //save CCT h for number
gen bw_reduce_number_IK=round(e(h_IK)) //save IK h for number
gen bw_reduce_number_CV=round(e(h_CV)) //save CV h for number

//First stage
reg number d x dx if abs(x)<=bw_reduce_number_CCT, cluster(x) //rectangle kernal weight
outreg2 using Table_density, excel dec(3) drop(_I*) replace

predict hat_number if e(sample) //local linear fit
predict sd_number if e(sample), stdp //standard errors of local linear fit
gen ub_number=hat_number+1.96*sd_number //95% confidence interval
gen lb_number=hat_number-1.96*sd_number
by x, sort: egen mean_number=mean(number) //mean outcome in each bin

//Graph
preserve
keep if abs(x)<=bw_reduce_number_CCT+2
twoway 	(scatter mean_number x , mcolor(navy navy navy) msize(medium medium medium) msymbol(O)) ///
(rline ub_number lb_number x if x<0, sort lpattern(dash)) ///
(rline ub_number lb_number x if x>=0, sort lpattern(dash)) ///
(line hat_number x if  x<0, sort lcolor(black) lwidth(medthick)) ///
(line hat_number x if  x>=0, sort lcolor(black) lwidth(medthick)), ///
ytitle(" ") xtitle(distance to the cutoff) xline(0) xlabel(-8[2]8) ///
title("number", size(small))
graph save "/Users/NicoleOY/Desktop/ECO3211 PS4/density_Regression",replace
restore

**smoothness of predetermined characteristics: gender, urban hukou3, urban hukou12, Han group, father's education, mother's education

// gender
gen male=.
replace male=1 if gender==1
replace male=0 if gender==0|gender==5

rdbwselect male x, all //use rdbwselect to calculate optimal bandwidth (h) for gender 
gen bw_reduce_male_CCT=round(e(h_CCT)) //save CCT h for male
gen bw_reduce_male_IK=round(e(h_IK)) //save IK h for male
gen bw_reduce_male_CV=round(e(h_CV)) //save CV h for male

*reduce form
reg male d x dx if abs(x)<=bw_reduce_male_CCT, cluster(x) //rectangle kernal weight
outreg2 using Table_smoothness, excel dec(3) drop(_I*) replace

predict hat_male if e(sample) //local linear fit
predict sd_male if e(sample), stdp //standard errors of local linear fit
gen ub_male=hat_male+1.96*sd_male //95% confidence interval
gen lb_male=hat_male-1.96*sd_male

by x, sort: egen mean_male=mean(male) //mean outcome in each bin

*Graph
preserve
keep if abs(x)<=bw_reduce_male_CCT+2
twoway 	(scatter mean_male x , mcolor(navy navy navy) msize(medium medium medium) msymbol(O)) ///
(rline ub_male lb_male x if x<0, sort lpattern(dash)) ///
(rline ub_male lb_male x if x>=0, sort lpattern(dash)) ///
(line hat_male x if  x<0, sort lcolor(black) lwidth(medthick)) ///
(line hat_male x if  x>=0, sort lcolor(black) lwidth(medthick)), ///
ytitle(" ") xtitle(distance to the cutoff) xline(0) xlabel(-90[7]90) ///
title("male", size(small))
graph save "/Users/NicoleOY/Desktop/ECO3211 PS4/Smoothness_gender",replace
restore

// urban hukou3
tab qa302
label list qa302
gen hukou3=.
replace hukou3=1 if qa302==3
replace hukou3=0 if qa302==1 | qa302==5| qa302==79

rdbwselect hukou3 x, all 
gen bw_reduce_hukou3_CCT=round(e(h_CCT)) 
gen bw_reduce_hukou3_IK=round(e(h_IK)) 
gen bw_reduce_hukou3_CV=round(e(h_CV)) 

*reduce form
reg hukou3 d x dx if abs(x)<=bw_reduce_hukou3_CCT, cluster(x) //rectangle kernal weight
outreg2 using Table_smoothness, excel dec(3) drop(_I*)

predict hat_hukou3 if e(sample) //local linear fit
predict sd_hukou3 if e(sample), stdp //standard errors of local linear fit
gen ub_hukou3=hat_hukou3+1.96*sd_hukou3 //95% confidence interval
gen lb_hukou3=hat_hukou3-1.96*sd_hukou3

by x, sort: egen mean_hukou3=mean(hukou3) //mean outcome in each bin

*Graph
preserve
keep if abs(x)<=bw_reduce_hukou3_CCT+2
twoway 	(scatter mean_hukou3 x , mcolor(navy navy navy) msize(medium medium medium) msymbol(O)) ///
(rline ub_hukou3 lb_hukou3 x if x<0, sort lpattern(dash)) ///
(rline ub_hukou3 lb_hukou3 x if x>=0, sort lpattern(dash)) ///
(line hat_hukou3 x if  x<0, sort lcolor(black) lwidth(medthick)) ///
(line hat_hukou3 x if  x>=0, sort lcolor(black) lwidth(medthick)), ///
ytitle(" ") xtitle(distance to the cutoff) xline(0) xlabel(-60[4]60) ///
title("hukou3", size(small))
graph save "/Users/NicoleOY/Desktop/ECO3211 PS4/Smoothness_hukou3",replace
restore

// urban hukou12
tab qa402
label list qa402
gen hukou12=.
replace hukou12=1 if qa402==3
replace hukou12=0 if qa402==1 | qa402==5| qa402==79

rdbwselect hukou12 x, all 
gen bw_reduce_hukou12_CCT=round(e(h_CCT)) 
gen bw_reduce_hukou12_IK=round(e(h_IK)) 
gen bw_reduce_hukou12_CV=round(e(h_CV)) 

reg hukou12 d x dx if abs(x)<=bw_reduce_hukou12_CCT, cluster(x) //rectangle kernal weight
outreg2 using Table_smoothness, excel dec(3) drop(_I*) 

predict hat_hukou12 if e(sample) //local linear fit
predict sd_hukou12 if e(sample), stdp //standard errors of local linear fit
gen ub_hukou12=hat_hukou12+1.96*sd_hukou12 //95% confidence interval
gen lb_hukou12=hat_hukou12-1.96*sd_hukou12

by x, sort: egen mean_hukou12=mean(hukou12) //mean outcome in each bin

*Graph
preserve
keep if abs(x)<=bw_reduce_hukou12_CCT+2
twoway 	(scatter mean_hukou12 x , mcolor(navy navy navy) msize(medium medium medium) msymbol(O)) ///
(rline ub_hukou12 lb_hukou12 x if x<0, sort lpattern(dash)) ///
(rline ub_hukou12 lb_hukou12 x if x>=0, sort lpattern(dash)) ///
(line hat_hukou12 x if  x<0, sort lcolor(black) lwidth(medthick)) ///
(line hat_hukou12 x if  x>=0, sort lcolor(black) lwidth(medthick)), ///
ytitle(" ") xtitle(distance to the cutoff) xline(0) xlabel(-60[4]60) ///
title("hukou12", size(small))
graph save "/Users/NicoleOY/Desktop/ECO3211 PS4/Smoothness_hukou12",replace
restore

//Han groups
tab qa5code
label list qa5code
gen Han=.
replace Han=1 if qa5code==1
replace Han=0 if qa5code !=-10 & qa5code !=-9& qa5code !=-8 &qa5code !=-2&qa5code !=-1 &qa5code !=1

rdbwselect Han x, all  
gen bw_reduce_Han_CCT=round(e(h_CCT)) 
gen bw_reduce_Han_IK=round(e(h_IK)) 
gen bw_reduce_Han_CV=round(e(h_CV)) 

*reduce form
reg Han d x dx if abs(x)<=bw_reduce_Han_CCT, cluster(x) //rectangle kernal weight
outreg2 using Table_smoothness, excel dec(3) drop(_I*)

predict hat_Han if e(sample) //local linear fit
predict sd_Han if e(sample), stdp //standard errors of local linear fit
gen ub_Han=hat_Han+1.96*sd_Han //95% confidence interval
gen lb_Han=hat_Han-1.96*sd_Han

by x, sort: egen mean_Han=mean(Han) //mean outcome in each bin

*Graph
preserve
keep if abs(x)<=bw_reduce_Han_CCT+2
twoway 	(scatter mean_Han x , mcolor(navy navy navy) msize(medium medium medium) msymbol(O)) ///
(rline ub_Han lb_Han x if x<0, sort lpattern(dash)) ///
(rline ub_Han lb_Han x if x>=0, sort lpattern(dash)) ///
(line hat_Han x if  x<0, sort lcolor(black) lwidth(medthick)) ///
(line hat_Han x if  x>=0, sort lcolor(black) lwidth(medthick)), ///
ytitle(" ") xtitle(distance to the cutoff) xline(0) xlabel(-70[5]70) ///
title("Han", size(small))
graph save "/Users/NicoleOY/Desktop/ECO3211 PS4/Smoothness_Han",replace
restore


// Father's education
ta feduc
label list feduc
gen faeduc=. 
replace faeduc=feduc if feduc>=1 & feduc<=8

rdbwselect faeduc x, all  
gen bw_reduce_f_CCT=round(e(h_CCT)) 
gen bw_reduce_f_IK=round(e(h_IK)) 
gen bw_reduce_f_CV=round(e(h_CV)) 

*reduce form
reg faeduc d x dx if abs(x)<=bw_reduce_f_CCT, cluster(x) //rectangle kernal weight
outreg2 using Table_smoothness, excel dec(3) drop(_I*)

predict hat_f if e(sample) //local linear fit
predict sd_f if e(sample), stdp //standard errors of local linear fit
gen ub_f=hat_f+1.96*sd_f //95% confidence interval
gen lb_f=hat_f-1.96*sd_f

by x, sort: egen mean_f=mean(faeduc) //mean outcome in each bin

*Graph
preserve
keep if abs(x)<=bw_reduce_f_CCT+2
twoway 	(scatter mean_f x , mcolor(navy navy navy) msize(medium medium medium) msymbol(O)) ///
(rline ub_f lb_f x if x<0, sort lpattern(dash)) ///
(rline ub_f lb_f x if x>=0, sort lpattern(dash)) ///
(line hat_f x if  x<0, sort lcolor(black) lwidth(medthick)) ///
(line hat_f x if  x>=0, sort lcolor(black) lwidth(medthick)), ///
ytitle(" ") xtitle(distance to the cutoff) xline(0) xlabel(-90[7]90) ///
title("Feducation", size(small))
graph save "/Users/NicoleOY/Desktop/ECO3211 PS4/Smoothness_feduc",replace
restore

// Mother's education
ta meduc
label list meduc
gen moeduc=. 
replace moeduc=meduc if meduc>=1 & meduc<=8

rdbwselect moeduc x, all  
gen bw_reduce_m_CCT=round(e(h_CCT)) 
gen bw_reduce_m_IK=round(e(h_IK)) 
gen bw_reduce_m_CV=round(e(h_CV)) 

*reduce form
reg moeduc d x dx if abs(x)<=bw_reduce_m_CCT, cluster(x) //rectangle kernal weight
outreg2 using Table_smoothness, excel dec(3) drop(_I*)

predict hat_m if e(sample) //local linear fit
predict sd_m if e(sample), stdp //standard errors of local linear fit
gen ub_m=hat_m+1.96*sd_m //95% confidence interval
gen lb_m=hat_m-1.96*sd_m

by x, sort: egen mean_m=mean(moeduc) //mean outcome in each bin

*Graph
preserve
keep if abs(x)<=bw_reduce_m_CCT+2
twoway 	(scatter mean_m x , mcolor(navy navy navy) msize(medium medium medium) msymbol(O)) ///
(rline ub_m lb_m x if x<0, sort lpattern(dash)) ///
(rline ub_m lb_m x if x>=0, sort lpattern(dash)) ///
(line hat_m x if  x<0, sort lcolor(black) lwidth(medthick)) ///
(line hat_m x if  x>=0, sort lcolor(black) lwidth(medthick)), ///
ytitle(" ") xtitle(distance to the cutoff) xline(0) xlabel(-65[5]65) ///
title("Meducation", size(small))
graph save "/Users/NicoleOY/Desktop/ECO3211 PS4/Smoothness_meduc",replace
restore

*******************************************************************************
// 4. Ouctome
*******************************************************************************
ta cfps2010eduy_best
gen eduy=cfps2010eduy_best
replace eduy=. if cfps2010eduy_best<0

*optimal bandwidth for reduce form regression
rdbwselect eduy x, all 
gen bw_reduce_eduy_CCT=round(e(h_CCT)) 
gen bw_reduce_eduy_IK=round(e(h_IK)) 
gen bw_reduce_eduy_CV=round(e(h_CV)) 

*reduce form
reg eduy d x dx if abs(x)<=bw_reduce_eduy_CCT, cluster(x) //rectangle kernal weight
outreg2 using Table_ouctome, excel dec(3) drop(_I*) replace

predict hat_eduy if e(sample) //local linear fit
predict sd_eduy if e(sample), stdp //standard errors of local linear fit
gen ub_eduy=hat_eduy+1.96*sd_eduy //95% confidence interval
gen lb_eduy=hat_eduy-1.96*sd_eduy

by x, sort: egen mean_eduy=mean(eduy) //mean outcome in each bin

*Graph
preserve
keep if abs(x)<=bw_reduce_eduy_CCT+2
twoway 	(scatter mean_eduy x , mcolor(navy navy navy) msize(medium medium medium) msymbol(O)) ///
(rline ub_eduy lb_eduy x if x<0, sort lpattern(dash)) ///
(rline ub_eduy lb_eduy x if x>=0, sort lpattern(dash)) ///
(line hat_eduy x if  x<0, sort lcolor(black) lwidth(medthick)) ///
(line hat_eduy x if  x>=0, sort lcolor(black) lwidth(medthick)), ///
ytitle(" ") xtitle(distance to the cutoff) xline(0) xlabel(-60[5]60) ///
title("eduy", size(small))
graph save "/Users/NicoleOY/Desktop/ECO3211 PS4/educoutome1",replace
restore

*******************************************************************************
// 5. Robustness check
*******************************************************************************
* Varying the bandwidth
// Choose IK
reg eduy d x dx if abs(x)<=bw_reduce_eduy_IK, cluster(x) //rectangle kernal weight
outreg2 using Table_ouctome, excel dec(3) drop(_I*) 

predict hat_eduy2 if e(sample) //local linear fit
predict sd_eduy2 if e(sample), stdp //standard errors of local linear fit
gen ub_eduy2=hat_eduy2+1.96*sd_eduy2 //95% confidence interval
gen lb_eduy2=hat_eduy2-1.96*sd_eduy2

by x, sort: egen mean_eduy2=mean(eduy) //mean outcome in each bin

*Graph
preserve
keep if abs(x)<=bw_reduce_eduy_IK+2
twoway 	(scatter mean_eduy2 x , mcolor(navy navy navy) msize(medium medium medium) msymbol(O)) ///
(rline ub_eduy2 lb_eduy2 x if x<0, sort lpattern(dash)) ///
(rline ub_eduy2 lb_eduy2 x if x>=0, sort lpattern(dash)) ///
(line hat_eduy2 x if  x<0, sort lcolor(black) lwidth(medthick)) ///
(line hat_eduy2 x if  x>=0, sort lcolor(black) lwidth(medthick)), ///
ytitle(" ") xtitle(distance to the cutoff) xline(0) xlabel(-85[5]85) ///
title("educoutcome", size(small))
graph save "/Users/NicoleOY/Desktop/ECO3211 PS4/educoutcome2",replace
restore

// Choose CV
reg eduy d x dx if abs(x)<=bw_reduce_eduy_CV, cluster(x) //rectangle kernal weight
outreg2 using Table_ouctome, excel dec(3) drop(_I*) 

predict hat_eduy3 if e(sample) //local linear fit
predict sd_eduy3 if e(sample), stdp //standard errors of local linear fit
gen ub_eduy3=hat_eduy3+1.96*sd_eduy3 //95% confidence interval
gen lb_eduy3=hat_eduy3-1.96*sd_eduy3

by x, sort: egen mean_eduy3=mean(eduy) //mean outcome in each bin

*Graph
preserve
keep if abs(x)<=bw_reduce_eduy_CV+2
twoway 	(scatter mean_eduy3 x , mcolor(navy navy navy) msize(medium medium medium) msymbol(O)) ///
(rline ub_eduy3 lb_eduy3 x if x<0, sort lpattern(dash)) ///
(rline ub_eduy3 lb_eduy3 x if x>=0, sort lpattern(dash)) ///
(line hat_eduy3 x if  x<0, sort lcolor(black) lwidth(medthick)) ///
(line hat_eduy3 x if  x>=0, sort lcolor(black) lwidth(medthick)), ///
ytitle(" ") xtitle(distance to the cutoff) xline(0) xlabel(-80[5]80) ///
title("educoutcome3", size(small))
graph save "/Users/NicoleOY/Desktop/ECO3211 PS4/educoutcome3",replace
restore

*parametric estimation
gen x2=x*x
gen dx2=d*x2
reg eduy d x dx x2 dx2, cluster(x)
outreg2 using Table_ouctome, excel dec(3) drop(_I*)

predict hat_eduy4 if e(sample) //quadratic fit
predict sd_eduy4 if e(sample), stdp //standard errors of quadratic fit
gen ub_eduy4=hat_eduy4+1.96*sd_eduy4 //95% confidence interval
gen lb_eduy4=hat_eduy4-1.96*sd_eduy4

by x, sort: egen mean_eduy4=mean(eduy) //mean outcome in each bin

*Graph
twoway 	(scatter mean_eduy4 x , mcolor(navy navy navy) msize(medium medium medium) msymbol(O)) ///
(rline ub_eduy4 lb_eduy4 x if x<0, sort lpattern(dash)) ///
(rline ub_eduy4 lb_eduy4 x if x>=0, sort lpattern(dash)) ///
(line hat_eduy4 x if  x<0, sort lcolor(black) lwidth(medthick)) ///
(line hat_eduy4 x if  x>=0, sort lcolor(black) lwidth(medthick)), ///
ytitle(" ") xtitle(distance to the cutoff) xline(0) xlabel(-400[30]250) ///
title("eduy4", size(small))
graph save "/Users/NicoleOY/Desktop/ECO3211 PS4/Parametric",replace


save "ps4.dta",replace 
cap log close
