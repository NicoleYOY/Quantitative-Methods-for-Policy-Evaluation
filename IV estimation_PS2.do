// clean up and setup//
capture clear
capture log close
set more off

//bootstrap data preparation
* path to working directory

cd "/Users/NicoleOY/Desktop/ECO 3211 PS 2/CFPS 2010/PS 2" 

use "/Users/NicoleOY/Desktop/ECO 3211 PS 2/CFPS 2010/ecfps2010adult_202008.dta", clear //load data

*see picture 1:m merge way 

merge 1:m pid fid using "/Users/NicoleOY/Desktop/ECO 3211 PS 2/CFPS 2010/ecfps2010adult_202008.dta" // the using file 

drop if _merge==2 // drop observations 
drop _merge // drop _merge column 

//var define
*year of birth
gen yob=qa1y_best 
*list qa1y_best if qa1y_best<1900 
replace yob=. if qa1y_best<0


*month of birth
gen mob=qa1m 

*list qa1m if qa1m > 12 | qa1m < 1 
*sum qa1m if qa1m > 12 | qa1m < 1     obs = 282 

replace mob=. if qa1m<0  //(282 real changes made, 282 to missing) 


//use a variable to define whether interviewee was born in the 1st half of birth year or the 2nd half. 
gen half=.
replace half=1 if mob>=1 & mob<9
replace half=2 if mob>=9 & mob<=12 


gen birth_half=yh(yob, half) //yh() function output:h1 h2 
format birth_half %th //Description: convenience function to make typing half-yearly dates in expressions easier


*years of schooling
ta cfps2010eduy_best

. ta cfps2010eduy_best //method 2 to check irregular observations
/*
      Years of |
  education in |
      CFPS2010 |      Freq.     Percent        Cum.
---------------+-----------------------------------
       Unknown |          4        0.01        0.01
             0 |      7,988       23.78       23.79
             1 |        119        0.35       24.14
             2 |        225        0.67       24.81
             3 |        326        0.97       25.78
             4 |        382        1.14       26.92
             5 |        347        1.03       27.95
             6 |      5,762       17.15       45.10
             7 |        575        1.71       46.81
             8 |        654        1.95       48.76
             9 |      9,212       27.42       76.18
            10 |        433        1.29       77.47
            11 |        327        0.97       78.44
            12 |      4,487       13.35       91.79
            13 |        197        0.59       92.38
            14 |        135        0.40       92.78
            15 |      1,501        4.47       97.25
            16 |        852        2.54       99.79
            17 |         12        0.04       99.82
            18 |          2        0.01       99.83
            19 |         52        0.15       99.98
            22 |          6        0.02      100.00
---------------+-----------------------------------
         Total |     33,598      100.00
*/
gen eduy=cfps2010eduy_best
replace eduy=. if cfps2010eduy_best<0

*birth place-province code
gen birth_prov=qa102acode
replace birth_prov=. if qa102acode<0

*residing place at 12-province code
gen twelve_prov=qa401acode
replace twelve_prov=. if qa401acode<0
replace twelve_prov=birth_prov if qa4==1 //lived in the birth place at age 12

*birth place-county/prefectural code
gen birth_county=qa102c_code
replace birth_county=. if qa102c_code<0

*residing place at 12-county/prefectural code
gen twelve_county=qa401c_code
replace twelve_county=. if qa401c_code<0
replace twelve_county=birth_county if qa4==1 //lived in the birth place at age 12

drop if twelve_prov==.|twelve_county==.


//health outcomes: overall
*self-perceived health outcome
gen health=.
replace health=1 if qp3==5
replace health=2 if qp3==4
replace health=3 if qp3==3
replace health=4 if qp3==2
replace health=5 if qp3==1

//physical health measurements
*bmi 
gen bmi=bmivalue
replace bmi=. if qp1<0
replace bmi=. if qp2<0

*overweight
gen overweight=0
replace overweight=1 if bmi>25
replace overweight=. if bmi==.

*felt physical discomfort in past two weeks
gen discomfort=qp4
replace discomfort=. if qp4<0
 
*chronic diease
gen chronic=qp5
replace chronic=. if qp5<0

*hospitalized last year 
gen hospitalized=qp6
replace hospitalized=. if qp6<0

*index: larger value, physically unhealthier
egen physical_index=rowmean(overweight discomfort chronic hospitalized)
egen float zphysical_index= std(physical_index), mean(0) std(1)

//mental health outcomes
*depressed
label list qq601

gen depressed=.
replace depressed=1 if qq601==5
replace depressed=2 if qq601==4
replace depressed=3 if qq601==3
replace depressed=4 if qq601==2
replace depressed=5 if qq601==1

*stressed
gen stressed=.
replace stressed=1 if qq602==5
replace stressed=2 if qq602==4
replace stressed=3 if qq602==3
replace stressed=4 if qq602==2
replace stressed=5 if qq602==1

*upset
gen upset=.
replace upset=1 if qq603==5
replace upset=2 if qq603==4
replace upset=3 if qq603==3
replace upset=4 if qq603==2
replace upset=5 if qq603==1

*hopeless
gen hopeless=.
replace hopeless=1 if qq604==5
replace hopeless=2 if qq604==4
replace hopeless=3 if qq604==3
replace hopeless=4 if qq604==2
replace hopeless=5 if qq604==1

*difficult
gen difficult=.
replace difficult=1 if qq605==5
replace difficult=2 if qq605==4
replace difficult=3 if qq605==3
replace difficult=4 if qq605==2
replace difficult=5 if qq605==1

*meaningless
gen meaningless=.
replace meaningless=1 if qq606==5
replace meaningless=2 if qq606==4
replace meaningless=3 if qq606==3
replace meaningless=4 if qq606==2
replace meaningless=5 if qq606==1

*generate a new health index: larger value, mentally unhealthier
egen mental_index=rowmean(depressed stressed upset hopeless difficult meaningless)
egen float zmental_indexl= std(mental_index), mean(0) std(1)

local outcomes "depressed stressed upset hopeless difficult meaningless"
foreach x of local outcomes {
egen float z`x'= std(`x'), mean(0) std(1)	
}

keep if yob>=1965 & yob<1985 //Keep adult individuals born between 1965 and 1985
// done with washing data here 





//graph the first stage & reduce form
by birth_half, sort: egen qeduy=mean(eduy) 
by birth_half, sort: egen qphysical=mean(physical_index) 
by birth_half, sort: egen qmental=mean(mental_index) 

twoway (connected qeduy birth_half)  // connected is the form. qeduy on y-aixs. birth_half on x-axis. 
graph save Graph_firststage, replace /*save the graph */
graph export "Graph_firststage.pdf", as(pdf) replace /*save the graph as PDF format */

twoway (connected qphysical birth_half) 
graph save Graph_reduceform1, replace 
graph export "Graph_reduceform1.pdf", as(pdf) replace

twoway (connected qmental birth_half) 
graph save Graph_reduceform2, replace 
graph export "Graph_reduceform2.pdf", as(pdf) replace 


// IV=I[qob>=9]
tab half, gen(halfdummy)

//preserve results in three files seperately called ols, iv, validity  
//1. regular OLS estimation 
xi: reg zmental_index eduy i.yob i.twelve_prov, cluster(twelve_county)
outreg2 using ols, excel dec(3) drop(_I* o._I*) replace
xi: reg zphysical_index eduy i.yob i.twelve_prov, cluster(twelve_county)
outreg2 using ols, excel dec(3) drop(_I* o._I*)


//2.1 first stage
xi: reg eduy halfdummy2 i.yob i.twelve_prov, cluster(twelve_county)
outreg2 using iv, excel dec(3) drop(_I* o._I*) replace

//2.2 reduced form
xi: reg zmental_index halfdummy2 i.yob i.twelve_prov, cluster(twelve_county)
outreg2 using iv, excel dec(3) drop(_I* o._I*) 
xi: reg zphysical_index halfdummy2 i.yob i.twelve_prov, cluster(twelve_county)
outreg2 using iv, excel dec(3) drop(_I* o._I*) 

//2.3 iv estimation: ivreg2 
xi: ivreg2 zmental_index (eduy=halfdummy2) i.yob i.twelve_prov, cluster(twelve_county)
outreg2 using iv, excel dec(3) drop(_I* o._I*) 
xi: ivreg2 zphysical_index (eduy=halfdummy2) i.yob i.twelve_prov, cluster(twelve_county)
outreg2 using iv, excel dec(3) drop(_I* o._I*) 

//3. falsification test: conditional on endogeneous treatment
xi: reg zmental_index halfdummy2 eduy i.yob i.twelve_prov, cluster(twelve_county)
outreg2 using validity, excel dec(3) drop(_I* o._I*) replace
xi: reg zphysical_index halfdummy2 eduy i.yob i.twelve_prov, cluster(twelve_county)
outreg2 using validity, excel dec(3) drop(_I* o._I*) 




//heterogeneous impacts across gender
ta gender
label list gender
/*        -10 Undecidable
          -9 Missing
          -8 Not applicable
          -2 Refuse
          -1 Unknown
           0 Female
           1 Male
           5 Female
*/
gen male=.
replace male=1 if gender==1
replace male=0 if gender==0|gender==5

ta male

// run on male subset 
xi: ivreg2 zmental_index (eduy=halfdummy2) i.yob i.twelve_prov if male==1, cluster(twelve_county)
outreg2 using gender, excel dec(3) drop(_I* o._I*) replace
xi: ivreg2 zphysical_index (eduy=halfdummy2) i.yob i.twelve_prov if male==1, cluster(twelve_county)
outreg2 using gender, excel dec(3) drop(_I* o._I*) 

// run on female subset 
xi: ivreg2 zmental_index (eduy=halfdummy2) i.yob i.twelve_prov if male==0, cluster(twelve_county)
outreg2 using gender, excel dec(3) drop(_I* o._I*) 
xi: ivreg2 zphysical_index (eduy=halfdummy2) i.yob i.twelve_prov if male==0, cluster(twelve_county)
outreg2 using gender, excel dec(3) drop(_I* o._I*) 

gen eduy_male=eduy*male
gen halfdummy2_male=halfdummy2*male

// run on all observations but using interation term to achieve the same goal as seperating the dataset by gender 
// test for diff using interaction term between treatment and male
xi: ivreg2 zmental_index (eduy eduy_male=halfdummy2 halfdummy2_male) i.yob*male i.twelve_prov*male, cluster(twelve_county)
outreg2 using gender, excel dec(3) drop(_I* o._I*) //check the significance of iv estimate in front of eduy_male

xi: ivreg2 zphysical_index (eduy eduy_male=halfdummy2 halfdummy2_male) i.yob*male i.twelve_prov*male, cluster(twelve_county)
outreg2 using gender, excel dec(3) drop(_I* o._I*)

//heterogeneous impacts across father's education
ta feduc
label list feduc 

// only use samples whose father are literate 
gen literate_f=(feduc>=2) // value 2 here is picked according to lables of feduc. 
replace literate_f=. if feduc==0|feduc<0 

//ivreg2 
xi: ivreg2 zmental_index (eduy=halfdummy2) i.yob i.twelve_prov if literate_f==1, cluster(twelve_county)
outreg2 using fedu, excel dec(3) drop(_I* o._I*) replace
xi: ivreg2 zphysical_index (eduy=halfdummy2) i.yob i.twelve_prov if literate_f==1, cluster(twelve_county)
outreg2 using fedu, excel dec(3) drop(_I* o._I*) 

//ivreg2 
xi: ivreg2 zmental_index (eduy=halfdummy2) i.yob i.twelve_prov if literate_f==0, cluster(twelve_county)
outreg2 using fedu, excel dec(3) drop(_I* o._I*) 
xi: ivreg2 zphysical_index (eduy=halfdummy2) i.yob i.twelve_prov if literate_f==0, cluster(twelve_county)
outreg2 using fedu, excel dec(3) drop(_I* o._I*) 

gen eduy_literate_f=eduy*literate_f
gen halfdummy2_literate_f=halfdummy2*literate_f

//similar to the above 
*test for diff using interaction term between treatment and literate_f
xi: ivreg2 zmental_index (eduy eduy_literate_f=halfdummy2 halfdummy2_literate_f) i.yob*literate_f i.twelve_prov*literate_f, cluster(twelve_county)
outreg2 using fedu, excel dec(3) drop(_I* o._I*) //check the significance of iv estimate in front of eduy_literate_f

xi: ivreg2 zphysical_index (eduy eduy_literate_f=halfdummy2 halfdummy2_literate_f) i.yob*literate_f i.twelve_prov*literate_f, cluster(twelve_county)
outreg2 using fedu, excel dec(3) drop(_I* o._I*)


//heterogeneous impacts across mother's education
// totaly same logic as played in father's edu 
ta meduc
label list meduc

gen literate_m=(meduc>=2)
replace literate_m=. if meduc==0|meduc<0

xi: ivreg2 zmental_index (eduy=halfdummy2) i.yob i.twelve_prov if literate_m==1, cluster(twelve_county)
outreg2 using medu, excel dec(3) drop(_I* o._I*) 
xi: ivreg2 zphysical_index (eduy=halfdummy2) i.yob i.twelve_prov if literate_m==1, cluster(twelve_county)
outreg2 using medu, excel dec(3) drop(_I* o._I*) 

xi: ivreg2 zmental_index (eduy=halfdummy2) i.yob i.twelve_prov if literate_m==0, cluster(twelve_county)
outreg2 using medu, excel dec(3) drop(_I* o._I*) 
xi: ivreg2 zphysical_index (eduy=halfdummy2) i.yob i.twelve_prov if literate_m==0, cluster(twelve_county)
outreg2 using medu, excel dec(3) drop(_I* o._I*) 




*test for diff
gen eduy_literate_m = eduy*literate_m
gen halfdummy2_literate_m = halfdummy2*literate_m

*test for diff using interaction term between treatment and literate_m 
xi: ivreg2 zmental_index (eduy eduy_literate_m=halfdummy2 halfdummy2_literate_m) i.yob*literate_m i.twelve_prov*literate_m, cluster(twelve_county)
outreg2 using medu, excel dec(3) drop(_I* o._I*) //check the significance of iv estimate in front of eduy_literate_m

xi: ivreg2 zphysical_index (eduy eduy_literate_m=halfdummy2 halfdummy2_literate_m) i.yob*literate_m i.twelve_prov*literate_m, cluster(twelve_county)
outreg2 using medu, excel dec(3) drop(_I* o._I*)
