//ECO3211 NicoleOY PS1


capture clear 

capture log close 

set more off 


cd "/Users/NicoleOY/Desktop/ECO3211_PS1/PS1_Data" 

log using PS1.log, text replace 

use "/Users/NicoleOY/Desktop/ECO3211_PS1/PS1_Data/CEPS_wave1/cepsw1studentE> N.dta", clear 



//data merge

merge 1:1 ids using "/Users/NicoleOY/Desktop/ECO3211_PS1/PS1_Data/CEPS_wave> 1/cepsw1parentEN.dta", nogen

merge m:1 schids using"/Users/NicoleOY/Desktop/ECO3211_PS1/PS1_Data/CEPS_wa> ve1/cepsw1principalEN.dta", nogen

merge m:1 clsids schids using "/Users/NicoleOY/Desktop/ECO3211_PS1/PS1_Data> /CEPS_wave1/cepsw1teacherEN.dta", nogen


// keep if are randomly assigned to class in grade 7

keep if ple1503==1

drop if ple16==1 & ple17~=5

save step1finished.dta


use "/Users/NicoleOY/Desktop/ECO3211_PS1/PS1_Data/CEPS_wave1/step1finished.dta"

// Define Treatment Group

decode matb01, generate(MathteacherGender)

decode hra01, generate(HeadSubject)

decode hrc01, generate(HeadGender)

replace MathteacherGender = HeadGender if HeadSubject == "Math"

drop if MathteacherGender==""


gen treated=1

replace treated=0 if MathteacherGender=="Male"


gen FemaleMathTeacher=1

replace FemaleMathTeacher=0 if MathteacherGender=="Male"

// Managing the variables
// Female Student
decode a01, generate(Stu_Sex)

gen FemaleStudent=1

replace FemaleStudent=0 if Stu_Sex=="Male"

// Ethnicity Group
decode a03, generate(EthnicityGroup)

gen HanEthnicity=1

replace HanEthnicity=0 if EthnicityGroup== "Other"

// Local Residents
decode a04, generate(ResidentsType)

gen LocalResidents=1

replace LocalResidents=0 if ResidentsType=="Not in the local county/district"

// Only child
decode b01, generate(NumOfChild)

gen OnlyChild=1

replace OnlyChild=0 if NumOfChild=="No, I am not"

// Preschool Education
decode c01, generate(PreschoolED)

gen HadPreEd=1

replace HadPreEd=0 if PreschoolED=="No, I haven't"

drop if PreschoolED==""

// Tranform the education to schooling years


gen schoolingYearsOfFather=.
replace schoolingYearsOfFather=0 if b07==1
replace schoolingYearsOfFather=6 if b07==2
replace schoolingYearsOfFather=9 if b07==3
replace schoolingYearsOfFather=8 if b07==4
replace schoolingYearsOfFather=11 if b07==5
replace schoolingYearsOfFather=12 if b07==6
replace schoolingYearsOfFather=15 if b07==7
replace schoolingYearsOfFather=16 if b07==8
replace schoolingYearsOfFather=19 if b07==9

gen schoolingYearsOfMother=.
replace schoolingYearsOfMother=0 if b06==1
replace schoolingYearsOfMother=6 if b06==2
replace schoolingYearsOfMother=9 if b06==3
replace schoolingYearsOfMother=8 if b06==4
replace schoolingYearsOfMother=11 if b06==5
replace schoolingYearsOfMother=12 if b06==6
replace schoolingYearsOfMother=15 if b06==7
replace schoolingYearsOfMother=16 if b06==8
replace schoolingYearsOfMother=19 if b06==9


// Dealing with missing values

drop if b07== .

drop if b06== .

drop if tr_mat== .

save step2finished.dta

// Balancing Test

iebaltab FemaleStudent HanEthnicity LocalResidents OnlyChild schoolingYearsOfFather schoolingYearsOfMother HadPreEd tr_mat, grpvar(treated) save(BalancingTest) vce(cluster clsids) fixedeffect(schids)  //with cluster for class and fixed effects of school

// Outcomes related to math study between treatment group and control group

reg stdmat treated FemaleStudent HanEthnicity LocalResidents OnlyChild schoolingYearsOfFather schoolingYearsOfMother tr_mat, cluster (clsids) 


//heterogenous effect

gen FemaleTeaFemaleStu = FemaleMathTeacher*FemaleStudent


reg stdmat FemaleMathTeacher FemaleStudent FemaleTeaFemaleStu HanEthnicity LocalResidents OnlyChild schoolingYearsOfFather schoolingYearsOfMother tr_mat , cluster(clsids)




