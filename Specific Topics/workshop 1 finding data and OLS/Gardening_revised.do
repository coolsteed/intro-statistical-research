//Understand the social patterning of gardening practices

//outcome = gardening

//what influences gardening?

//standard variables = gender, race, age, income

// base model how is gender associated to race, gender, age and income

// setting the working environment 
clear

// set the working directory
cd "~\Dropbox\working space\Statistic Consultant\SOCG206\workshop 1\"

// read the data 
use "./Gardening.dta", replace

// Y = gardening, X_i = sex, rincome, race, age

regress garden sex rincome race age

//ooops, sex and race are not continuous 

regress garden i.sex i.race rincome age //<--Second model

//gardening is a more conservative practice

regress garden i.sex i.race rincome age polviews //<-- Third model

//if we compare adjusted R2, the adj R2 of the third model is the same as our second model

regress garden i.sex i.race rincome age polviews
predict resid
