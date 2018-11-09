**************************************************************************************
*STATA solutions for VAM RDD session 5/11/18 -exercises by Salomo Hirvonen.
*If you have any questions do not hesitate contact me at salomo.hirvonen@bristol.ac.uk 
**************************************************************************************
clear all
clear mata
graph drop _all
mata: mata set matastrict off, permanently
program drop _all
macro drop _all
matrix drop _all

*install rdrobust package
net install rdrobust, from(https://sites.google.com/site/rdpackages/rdrobust/stata) replace

*Change the correct directory.
cd

*Use the dataset.
use rdrobust_senate.dta, clear





*****************************************************
*Exercise 1
*With rdrobust\_senate dataset plot your data (vote as dependent variable and margin as forcing variable with rdplot command) using both evenly-spaced and 
*quantile-spaced mimicking variance bin choice (Hint: You need specify binselect option). Can you spot differences?
*****************************************************

*Command for evenly-spaced mimimc variance bin select option. The default option of the rdplot command and arguably the preferred one as tries to visualise
*the underlying variability of the data in an evenly-spaced manner (i.e. bin length in terms of forcing variable is similar across bins (in respective sides of the cut-off).
rdplot vote margin,  binselect(esmv)
*Command for evenly-spaced IMSE-optimal bin select option. This bin select method, compared to the previous one, tries to capture the underlying function better
*as bins are constructed by in (Integrated) Mean Square Error-optimal way i.e. trading off with variance and bias in order to select the number of observation for a bin.
*As you can see: has much fever bins than the mimic-variance option.
rdplot vote margin,  binselect(es)

*****************************************************
*Exercise 2
*Plot your data using both evenly-spaced and quantile-spaced IMSE-optimal bin choice. Can you spot differences?
*****************************************************

*Command for quantile-spaced mimimc variance bin select option. Quantile-spacing means that bin length is select in a way that number of observations in bins are similar across
*bins (in respctive sides of the cut-off). Thus there are more bins where data is more dense and less bins where there is less observations.
rdplot vote margin,  binselect(qsmv)
*Command for quantile-spaced IMSE-optimal bin select option. Again fewer bins compared to the mimic variance option.
rdplot vote margin,  binselect(qs)

*Important thing with the RD graphs is that they should visualy support your formal analysis. As usual plotting the data is a great starting point for an analysis.



*****************************************************
*Exercise 3
*We want to find what is the effect of incumbency on the vote share in next election.
*Use a global polynomial RDD specification and do this for quadratic, cubic and quartic function 
*(Hint: Create a new variable for treatment status from election margin) without using the rdrobust command.
*****************************************************

*generating a binary treatment variable, which takes value of 1 if margin is  greater or equal to than 0 (i.e. democratic party is incumbent) and 0 otherwise. 
gen treated=margin>=0

/*
Global quadratic polynomial regression. c. specifies that margin is a continious variable and ## creates interaction (including all underlying terms) between variables. We want
to estimate the jump at 0, so the coefficient of interest is _treated_ as by interacting with second order of margin (plus underlying terms) we fit second order polynomial 
regressions on both sides of the cut-off and coefficient of treated gives the difference of these when margin is equal to zero.
*/
reg vote c.margin##c.margin##treated

*Same for cubic
reg vote c.margin##c.margin##c.margin##treated
*Same for quartic
reg vote c.margin##c.margin##c.margin##c.margin##treated


*NOTE: Fitting global polynomials here was given an exercise about thinking how rd actaully works and how you are able to estimate "the jump", but it is known both
*theoretically and from empirical settings that global polynomials have poor properties in terms of recovering the actual parameter of interest and thus in your
*actual analysis you should NOT use these, but instead rely on non-parametric local regressions! 


*****************************************************
*Exercise 4
*Now using a non-parametric specification redo your analysis using local linear estimation within 15\% bandwidth.
*Run regressions separately for non-treated and treated units and compute the estimate for treatment effect.
*****************************************************

*Regressing vote on margin when absolute value of margin is smaller or equal to 15 and treated is equal to zero i.e. we look only observations on the left side of the cut-off.
reg vote margin if abs(margin)<=15 & treated==0
*We store the constant as this is the (predicted) value when margin is equal to zero.
scalar define b1 = _b[_cons]
*Regressing vote on margin when absolute value of margin is smaller or equal to 15 and treated is equal to one i.e. we look only observations on the right side of the cut-off.
reg vote margin if abs(margin)<=15 & treated==1
*We store the constant as this is the (predicted) value when margin is equal to zero.
scalar define b2 = _b[_cons]
*The estimated rd effect is the difference between treated and non-treated predicted value when margin is equal to zero:
di b2-b1

*6.9638374

*****************************************************
*Exercise 5
*Repeat Exercise 4 but now conduct the analysis in one regression instead of two separate ones. 
*Make sure that your estimate is the same as computed in the previous exercise.
*****************************************************

/*
regressing vote on margin*treated margin treated when absolute value of margins is smaller or equal to 15 (i.e. we are within the bandwidth).
As with the global polynomials coefficient on treated gives us the estimated difference and it is equal to 6.963837 which is same what we computed
in previous exercise. 
*/
reg vote c.margin##treated if abs(margin)<=15

*****************************************************
*Exercise 6
*Up until this point we have given equal weight for all observations (within the bandwidth). Redo Exercise 5 with a triangular kernel.
*****************************************************

/*
A triangular kernel would give observations at the cut-off value weight of 1 and then (linearly) decreasing weghts within the bandwidth
so that observations at the bandwidth boundary (and outside of the bandwidth) get weight of 0. There are many programing ways to achieve this
but maybe the simplest one is the following: take a maximum of zero and 1-(absolute value of margin/bandwidth). This gives weight zero for all observations
greater than 15 and smaller than -15 (as the second term in the max formula is negative) and weights between 1 and 0 within the bandwidth.
*/
gen weights=max(0, (1-abs(margin)/15))

*Now we run the same regrssion as in last exercise, but with specifying the weighting option.
reg vote c.margin##treated [pw=weights] if abs(margin)<=15


*****************************************************
*Exercise 7
*Replicate your result from previous exercise using rdrobust package.
*****************************************************

*We have to specify the bandwidth to be equal to 15 (h option), triangular kernel to be used (kernel option) and order of the polynomial (p option)
rdrobust vote margin, h(15) kernel(tri) p(1)

*****************************************************
*Exercise 8
*Now use MSE-optimal bandwidth selection in order to choose the bandwidth together with a local linear specification. 
*Is the robust bias corrected confidence interval larger or smaller compared to the conventional one? You can test different order of polynomials.
*****************************************************

/*
In order to choose mse-optimal bandwidth we use the bwselect option mserd (this is the default option of the command). In output you can
see the robust bias corrected confidence intervals in the second row, these should be used for the inference and those are strictly larger
than traditional CI, but note that your point estimate is not in the middle of this CI.
*/

rdrobust vote margin, kernel(tri) p(1) bwselect(mserd)

*Same as the previous one but now using rho==1 option meaning that the pilot bandwidth (i.e. bias correction bandwidth) is equal to the main bandwidth.
rdrobust vote margin, rho(1) kernel(tri) p(1) bwselect(mserd)

*Same as the previous one, but now with uniform kernel option (i.e. all observations in the bandwidth gets a weight of 1). This is to illustrate a question raised during the session: weighting has an effect on the size of 
*the mse optimal bandwidth.
rdrobust vote margin, rho(1) kernel(uni) p(1) bwselect(mserd)


*****************************************************
*Exercise 9
*Conduct a covariate smoothness test using available variable(s). What do you conclude in terms of falsification of the RDD?  
*****************************************************

*Important thing for selecting covariates that they have to be measured before the treatment assignment. We use the state population and run our rd analysis.
rdrobust population margin, rho(1) kernel(tri) p(1) bwselect(mserd)

*We fail to reject the null i.e. no discontinuity at the cut-off. This is evidence for the validity of our design as the pre-treatment population should be same for treated and
*non-treated at the cut-off.

*****************************************************
*Exercise 10
*Run placebo cut-off tests. Do your findings support the validity of our design?  
*****************************************************

*Summarise margin for both treated and non-treated. For example pick 50th percentile as a placebo cut-off.
bys treated: sum margin, detail

*Conduct placebo cut-off tests using option for cut-off (i.e. c):
rdrobust vote margin, rho(1) bwselect(mserd) p(1)  c(-14.34493)

rdrobust vote margin, rho(1) bwselect(mserd) p(1) c(20.43995)
*We don't find discontinuities at the placebo cut-offs. This is exactly what we wanted to see.

*****************************************************
*Exercise 11
*Using rddensity package, find out whether there is evidence for manipulation at the cut-off.
*****************************************************

*Install rddensity package
net install rddensity, from(https://sites.google.com/site/rdpackages/rddensity/stata) replace 

*Run the manipulation test for our forcing variable margin:
rddensity margin

*We fail to reject the null, so we don't find evidence for manipulation. This is evidence for our rd design.
