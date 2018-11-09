#*************************************************************************************
# R solutions for VAM RDD session 5/11/18 - exercises by Salomo Hirvonen.
# If you have any questions do not hesitate contact me at salomo.hirvonen@bristol.ac.uk 
#*************************************************************************************
  
# Download the relevant packages:
# [1] Useful to organise your data and generate new variables (see also VAM session 1 on the "dplyr" package on Moodle)
#install.packages("tidyverse")
library(dplyr)
# [2] In order to import the Stata (.dta) files with the data, we need this package
#install.packages("readstata13")
library(readstata13)
# [3] To run the RD, we will use the RD robust package by Calonico, Cattaneo, Farrell and Titiunik (2014b, 2018)
#install.packages("rdrobust")
library(rdrobust)
# [4] To check the distribution of forcing variable observations around the cut-off, we will use the RD density package 
# by Calonico, Cattaneo, Farrell and Titiunik (2014b, 2018)
#install.packages("rddensity")
library(rddensity)

# We use data from Cattaneo, Frandsen and Titiunik (2015), JCI, "Randomization Inference in the Regression - Discontinuity Design: An Application to Party
# Advantages in the U.S. Senate"
# Their paper can be found under: http://www-personal.umich.edu/~cattaneo/papers/Cattaneo-Frandsen-Titiunik_2015_JCI.pdf
# The replication data (data-senate) is on: https://sites.google.com/site/rdpackages/rdrobust
# But we provide you with the replication data on Github (so no need to download it from the online source)
mydata <- read.dta13("./rdrobust_senate.dta")

# Have a look at the data
mydata %>% View()
# Check the variable names in mydata
names(mydata)

#****************************************************
# Exercise 1
# With rdrobust\_senate dataset plot your data (vote as dependent variable and margin as forcing variable with rdplot command) using both evenly-spaced and 
# quantile-spaced mimicking variance bin choice (Hint: You need specify binselect option). Can you spot differences?
#****************************************************

# We attach the data, so we do not have to specify each time which dataframe we use (e.g. we can write 
# summary(margin) instead of summary(mydata$margin))
attach(mydata)
# Next, we want to determine the outcome y and the forcing variable x
# Define outcome variable
y = vote
# Define forcing variable
x = margin
# Plot 1
# Set the names for the plot's title
outnm = "Vote"
forcnm = "Margin"
sample = 'US_Senate'
tit = paste('RD effect on Democratic vote share in t + 1', sep='')
# and under which name the plot will be saved in your folder (as .pdf)
path = "./"
file = paste(path,'/RDplot-', forcnm, '-', outnm, '-', sample, '_esmv.pdf', sep='')
pdf(file)
# Command for evenly-spaced mimimc variance bin select option. The default option of the rdplot command and arguably the 
# preferred one as tries to visualise the underlying variability of the data in an evenly-spaced manner 
# (i.e. bin length in terms of forcing variable is similar across bins (in respective sides of the cut-off).
rdplot(y = y, x = x, binselect = "esmv", title = tit, x.label = "Dem margin of victory in t",
       y.label = "Dem vote share in t + 1", 
       x.lim = c(-30,30), y.lim = c(0,100))
# Make the cutoff line red
abline(v=0, lwd=2, col='red')
# Turn device off (i.e. close "the project" of plot 1, so you can generate plot 2)
dev.off()
# Plot 2
# and under which name the plot will be saved in your folder (as .pdf)
path = "./"
file = paste(path,'/RDplot-', forcnm, '-', outnm, '-', sample, '_es.pdf', sep='')
pdf(file)
# Command for evenly-spaced IMSE-optimal bin select option. This bin select method, compared to the previous one, 
# tries to capture the underlying function better as bins are constructed by in (Integrated) Mean Square Error-optimal 
# way i.e. trading off with variance and bias in order to select the number of observation for a bin.
# As you can see: has much fever bins than the mimic-variance option.
rdplot(y = y, x = x, binselect = "es", title = tit, x.label = "Dem margin of victory in t",
       y.label = "Dem vote share in t + 1", 
       x.lim = c(-30,30), y.lim = c(0,100))
# Make the cutoff line red
abline(v=0, lwd=2, col='red')
# Turn device off (i.e. close "the project" of plot 2)
dev.off()
detach(mydata)

#****************************************************
# Exercise 2
# Plot your data using both evenly-spaced and quantile-spaced IMSE-optimal bin choice. Can you spot differences?
#*****************************************************

attach(mydata)
y = vote
x = margin
# Plot 1
outnm = "Vote"
forcnm = "Margin"
sample = 'US_Senate'
tit = paste('RD effect on Democratic vote share in t + 1', sep='')
path = "./"
file = paste(path,'/RDplot-', forcnm, '-', outnm, '-', sample, '_qsmv.pdf', sep='')
pdf(file)
# Command for quantile-spaced mimimc variance bin select option. Quantile-spacing means that bin length is select in a way 
# that number of observations in bins are similar across bins (in respctive sides of the cut-off). Thus there are more bins 
# where data is more dense and less bins where there is less observations.
rdplot(y = y, x = x, binselect = "qsmv", title = tit, x.label = "Dem margin of victory in t",
       y.label = "Dem vote share in t + 1", 
       x.lim = c(-30,30), y.lim = c(0,100))
abline(v=0, lwd=2, col='red')
dev.off()
# Plot 2
path = "./"
file = paste(path,'/RDplot-', forcnm, '-', outnm, '-', sample, '_qs.pdf', sep='')
pdf(file)
# Command for quantile-spaced IMSE-optimal bin select option. Again fewer bins compared to the mimic variance option.
rdplot(y = y, x = x, binselect = "qs", title = tit, x.label = "Dem margin of victory in t",
       y.label = "Dem vote share in t + 1", 
       x.lim = c(-30,30), y.lim = c(0,100))
abline(v=0, lwd=2, col='red')
dev.off()
detach(mydata)

# Important thing with the RD graphs is that they should visualy support your formal analysis. 
# As usual plotting the data is a great starting point for an analysis.

#****************************************************
# Exercise 3
# We want to find what is the effect of incumbency on the vote share in next election.
# Use a global polynomial RDD specification and do this for quadratic, cubic and quartic function 
# (Hint: Create a new variable for treatment status from election margin) without using the rdrobust command.
#****************************************************

# Generating a binary treatment variable, which takes value of 1 if margin is  greater or equal to than 0 
# (i.e. democratic party is incumbent) and 0 otherwise.
mydata = mydata %>% mutate(treated = ifelse(margin >= 0,1,0))

# Global quadratic polynomial regression. c. specifies that margin is a continious variable and ## creates interaction (including all underlying terms) between variables. We want
# to estimate the jump at 0, so the coefficient of interest is _treated_ as by interacting with second order of margin (plus underlying terms) we fit second order polynomial 
# regressions on both sides of the cut-off and coefficient of treated gives the difference of these when margin is equal to zero.
mydata = mydata %>% mutate(margin2 = margin*margin)
pooling <- lm(vote ~ margin*treated + margin2*treated, data=mydata)
summary(pooling)

# Same for cubic
mydata = mydata %>% mutate(margin3 = margin*margin*margin)
pooling <- lm(vote ~ margin*treated + margin2*treated + margin3*treated, data=mydata)
summary(pooling)

# Same for quartic
mydata = mydata %>% mutate(margin4 = margin2*margin2)
pooling <- lm(vote ~ margin*treated + margin2*treated + margin3*treated + margin4*treated, data=mydata)
summary(pooling)

# NOTE: Fitting global polynomials here was given an exercise about thinking how rd actaully works and how you are able to estimate "the jump", but it is known both
# theoretically and from empirical settings that global polynomials have poor properties in terms of recovering the actual parameter of interest and thus in your
# actual analysis you should NOT use these, but instead rely on non-parametric local regressions! 

#****************************************************
# Exercise 4
# Now using a non-parametric specification redo your analysis using local linear estimation within 15\% bandwidth.
# Run regressions separately for non-treated and treated units and compute the estimate for treatment effect.
#****************************************************
  
mydata15 = mydata %>% filter(margin <= 15 & margin >= 0)
pooling <- lm(vote ~ margin, data=mydata15)
summary(pooling)
b1 = coef(pooling)[1]
mydata15 = mydata %>% filter(margin >= -15 & margin <= 0)
pooling <- lm(vote ~ margin, data=mydata15)
summary(pooling)
b2 = coef(pooling)[1]
b1-b2
#6.9638374

#****************************************************
# Exercise 5
# Repeat Exercise 4 but now conduct the analysis in one regression instead of two separate ones. 
# Make sure that your estimate is the same as computed in the previous exercise.
#****************************************************
  
# Regressing vote on margin*treated margin treated when absolute value of margins is smaller or equal to 15 (i.e. we are within the bandwidth).
# As with the global polynomials coefficient on treated gives us the estimated difference and it is equal to 6.963837 which is same what we computed
# in previous exercise. 

mydata15 = mydata %>% filter(margin >= -15 & margin <= 15)
pooling <- lm(vote ~ margin*treated, data=mydata15)
summary(pooling)

#****************************************************
# Exercise 6
# Up until this point we have given equal weight for all observations (within the bandwidth). Redo Exercise 5 with a triangular kernel.
#****************************************************
  
# A triangular kernel would give observations at the cut-off value weight of 1 and then (linearly) decreasing weghts within the bandwidth
# so that observations at the bandwidth boundary (and outside of the bandwidth) get weight of 0. There are many programing ways to achieve this
# but maybe the simplest one is the following: take a maximum of zero and 1-(absolute value of margin/bandwidth). This gives weight zero for all observations
# greater than 15 and smaller than -15 (as the second term in the max formula is negative) and weights between 1 and 0 within the bandwidth.
  
mydata = mydata %>% mutate(weight=ifelse((1-abs(margin)/15)>0,(1-abs(margin)/15),0))
# Have a look at the weight for each margin
mydata %>% select(margin,weight) %>% View()

# Now we run the same regrssion as in last exercise, but with specifying the weighting option.
pooling <- lm(vote ~ margin*treated, data=mydata, weights=weight)
summary(pooling)

#****************************************************
# Exercise 7
# Replicate your result from previous exercise using rdrobust package.
#****************************************************
  
# Running the RD to estimate the LATE of the Democratic winning margin in t on the party's vote share in t+1 (US Senate Elections)
attach(mydata)
# We look at the sample distribution of the forcing variable (margin in t) and the outcome variables (vote in t+1)
summary(vote)
summary(margin)
# Next, we want to determine the outcome y and the forcing variable x
# Define outcome variable
y = vote
# Define forcing variable
x = margin
# Define cut-off (though if it is zero, you actually do not have to, because it is the default)
c = 0
rd = rdrobust(y,x,c, h=15, kernel="triangular", p=1)
summary(rd)
detach(mydata)

#****************************************************
# Exercise 8
# Now use MSE-optimal bandwidth selection in order to choose the bandwidth together with a local linear specification. 
# Is the robust bias corrected confidence interval larger or smaller compared to the conventional one? You can test 
# different order of polynomials.
#****************************************************

# In order to choose mse-optimal bandwidth we use the bwselect option mserd (this is the default option of the command). 
# In output you can see the robust bias corrected confidence intervals in the second row, these should be used for the 
# inference and those are strictly larger than traditional CI, but note that your point estimate is not in the middle 
# of this CI.

# Running the RD to estimate the LATE of the Democratic winning margin in t on the party's vote share in t+1 
# (US Senate Elections)
attach(mydata)
# Next, we want to determine the outcome y and the forcing variable x
# Define outcome variable
y = vote
# Define forcing variable
x = margin
# Define cut-off (though if it is zero, you actually do not have to, because it is the default)
c = 0
rd = rdrobust(y,x,c, h=15, kernel="triangular", p=1, bwselect="mserd")
summary(rd)

# Same as the previous one but now using rho=1 option meaning that the pilot bandwidth (i.e. bias correction bandwidth) 
# is equal to the main bandwidth.
rd = rdrobust(y,x,c, h=15, rho=1, kernel="triangular", p=1, bwselect="mserd")
summary(rd)

# Same as the previous one, but now with uniform kernel option (i.e. all observations in the bandwidth gets a weight of 1). 
# This is to illustrate a question raised during the session: weighting has an effect on the size of the mse optimal bandwidth.
rd = rdrobust(y,x,c, h=15, kernel="uniform", p=1, bwselect="mserd")
summary(rd)

#****************************************************
# Exercise 9
# Conduct a covariate smoothness test using available variable(s). What do you conclude in terms of falsification of the RDD?  
#****************************************************

# Important thing for selecting covariates that they have to be measured before the treatment assignment. 
# We use the state population and run our rd analysis.
# Define new dependent variable:
y_new = population
rd = rdrobust(y_new,x,c, h=15, rho=1, kernel="triangular", p=1, bwselect="mserd")
summary(rd)

# We fail to reject the null i.e. no discontinuity at the cut-off. This is evidence for the validity of our design as the 
# pre-treatment population should be same for treated and
# non-treated at the cut-off.

#****************************************************
# Exercise 10
# Run placebo cut-off tests. Do your findings support the validity of our design?  
#****************************************************

# Summarise margin for both treated and non-treated. For example pick 50th percentile (median) as a placebo cut-off.
mydata %>% select(margin) %>% filter(treated == 0) %>% summary()
mydata %>% select(margin) %>% filter(treated == 1) %>% summary()

# Define placebo cut-off:
c_placebo = -14.34493
rd = rdrobust(y,x,c_placebo, h=15, rho=1, kernel="triangular", p=1, bwselect="mserd")
summary(rd)

c_placebo = 20.43995
rd = rdrobust(y,x,c_placebo, h=15, rho=1, kernel="triangular", p=1, bwselect="mserd")
summary(rd)
detach(mydata)
# We don't find discontinuities at the placebo cut-offs. This is exactly what we wanted to see.

# ****************************************************
# Exercise 11
# Using rddensity package, find out whether there is evidence for manipulation at the cut-off.
# ****************************************************

# Run the manipulation test for our forcing variable margin:
attach(mydata)
rdens = rddensity(margin)
summary(rdens)
detach(mydata)
# We fail to reject the null (check the p-value), so we don't find evidence for manipulation. This is evidence for our rd design.
