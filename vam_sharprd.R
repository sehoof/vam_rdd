# Download the relevant packages:
# [1] Useful to organise your data and generate new variables (see also VAM session 1 on the "dplyr" package on Moodle):
#install.packages("tidyverse")
library(dplyr)
# [2] In order to import the Stata (.dta) files with the data, we need this package:
#install.packages("readstata13")
library(readstata13)
# [3] To run the RD, we will use the RD robust package by Calonico, Cattaneo, Farrell and Titiunik (2014b, 2018):
#install.packages("rdrobust")
library(rdrobust)
# and the package by Matthieu Stigler from GitHub (Useful to check out: https://github.com/MatthieuStigler/RDDtools):
library(devtools)
install_github(repo = "RDDtools", username = "MatthieuStigler", subdir = "RDDtools")
library(RDDtools)

# We use data from Cattaneo, Frandsen and Titiunik (2015), JCI, "Randomization Inference in the Regression - Discontinuity Design: An Application to Party
# Advantages in the U.S. Senate"
# Their paper can be found under: http://www-personal.umich.edu/~cattaneo/papers/Cattaneo-Frandsen-Titiunik_2015_JCI.pdf
# The replication data (data-senate) is on: https://sites.google.com/site/rdpackages/rdrobust
# But we provide you with the replication data on Github (so no need to download it from the online source):
mydata <- read.dta13("./rdrobust_senate.dta")

# Have a look at the data:
mydata %>% View()
# Check the variable names in mydata:
names(mydata)

# [1] DIY RD:
mydata = mydata %>% mutate(treated = ifelse(margin >= 0,1,0))
mydata = mydata %>% mutate(margin2 = margin*margin)
pooling <- lm(vote ~ margin*treated + margin2*treated, data=mydata)
summary(pooling)
mydata = mydata %>% mutate(margin3 = margin*margin*margin)
pooling <- lm(vote ~ margin*treated + margin2*treated + margin3*treated, data=mydata)
summary(pooling)
mydata = mydata %>% mutate(margin4 = margin2*margin2)
pooling <- lm(vote ~ margin*treated + margin2*treated + margin3*treated + margin4*treated, data=mydata)
summary(pooling)

mydata15 = mydata %>% filter(margin <= 15 & margin >= 0)
pooling <- lm(vote ~ margin, data=mydata15)
summary(pooling)
b1 = coef(pooling)[1]
mydata15 = mydata %>% filter(margin >= -15 & margin <= 0)
pooling <- lm(vote ~ margin, data=mydata15)
summary(pooling)
b2 = coef(pooling)[1]
b1-b2
mydata15 = mydata %>% filter(margin >= -15 & margin <= 15)
pooling <- lm(vote ~ margin*treated, data=mydata15)
summary(pooling)

mydata = mydata %>% mutate(weight=ifelse((1-abs(margin)/15)>0,(1-abs(margin)/15),0))
mydata %>% select(margin,weight) %>% View()

pooling <- lm(vote ~ margin*treated, data=mydata, weights=weight)
summary(pooling)

# [2] rdrobust package:
# Running the RD to estimate the LATE of the Democratic winning margin in t on the party's vote share in t+1 (US Senate Elections):
# We attach the data, so we do not have to specify each time which dataframe we use (e.g. we can write 
# summary(margin) instead of summary(mydata$margin)):
attach(mydata)
# We look at the sample distribution of the forcing variable (margin in t) and the outcome variables (vote in t+1):
summary(vote)
summary(margin)

# Next, we want to determine the outcome y and the forcing variable x:
# Define outcome variable:
y = vote
# Define forcing variable:
x = margin
# Define cut-off (though if it is zero, you actually do not have to, because it is the default):
c = 0
outnm = "Vote"
forcnm = "Margin"
sample = 'US_Senate'
tit = paste('RD effect on Democratic vote share in t + 1', sep='')
tit = ''
path = "./"
file = paste(path,'/RDplot-', forcnm, '-', outnm, '-', sample, 'Sample.pdf', sep='')
pdf(file)
rd = rdrobust(y,x,c, h=15, kernel=triangular, p=1)
summary(rd)
rdplot(y = y, x = x, c = c, binselect = "esmv",title = tit, x.label = "Dem margin of victory in t",
       y.label = "Dem vote share in t + 1", 
       x.lim = c(-30,30), y.lim = c(0,100))
abline(v=0, lwd=2, col='red')
dev.off()
# McCrary Test for no discontinuity of density around cutpoint:
file = paste(path,'/McCrary-', forcnm, '-', outnm, '-', sample, 'Sample.pdf', sep='')
pdf(file)
sharprd = RDDdata(y = vote, x = margin, cutpoint = 0)
dens_test(sharprd)
dev.off()
detach(mydata)
