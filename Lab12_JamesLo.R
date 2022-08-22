#1
library(lavaan)
library(tidyverse)
library(MASS)

#2
# input the correlations in lower diagonal form 
lower = '
1.00
.77 1.00
.59 .81 1.00
.50 .72 .89 1.00
.48 .69 .84 .91 1.00
.46 .68 .80 .88 .93 1.00
.50 .46 .36 .26 .28 .28 1.00 '
# name the variables and convert to full correlation matrix

cor = getCov(lower, names = c("trial1","trial2","trial3","trial4","trial5","trial6","ability"))

# Create a vector of standard deviations
sds = c(7.60, 8.44, 8.95, 9.21, 9.49, 9.62, 5.62)
cov = cor2cov(cor, sds)
# create mean vector
means = c(11.77, 21.39, 27.50, 31.02, 32.58, 34.20, .70)
# Set the seed and generate data. Change the Xs to your birthdate (MMDD) 
set.seed(0922)
data = data.frame(mvrnorm(n = 250, mu = means, Sigma = cov))

#3
cross_lag = '
trial2 ~ trial1
trial3 ~ trial2
trial4 ~ trial3
trial5 ~ trial4
trial6 ~ trial5
'
fit_cross_lag = sem(cross_lag, data)
summary(fit_cross_lag, fit.measures = T)
standardizedsolution(fit_cross_lag)

#5
cross_lag2 = '
trial1 ~ ability
trial2 ~ trial1 + ability
trial3 ~ trial2 + ability
trial4 ~ trial3 + ability
trial5 ~ trial4 + ability
trial6 ~ trial5 + ability
'
fit_cross_lag2 = sem(cross_lag2, data)
summary(fit_cross_lag2, fit.measures = T)
standardizedsolution(fit_cross_lag2)

#On trial 1, ability significantly predicts performance on an air traffic control task such that for 
#every 1 unit increase in cognitive ability, performance on the air traffic control task increases by .72
#On trial 2, ability significantly predicts performance on an air traffic control task such that for 
#every 1 unit increase in cognitive ability, performance on the air traffic control task increases by .24. 
#On trial 5, ability significantly predicts performance on an air traffic control task such that for 
#every 1 unit increase in cognitive ability, performance on the air traffic control task increases by .16.
#For trials 3, 4, and 6, ability did not significantly predict performance on the air traffic control task (p>.05)

#6
growth_model = '
initial =~ 1*trial1 + 1*trial2 + 1*trial3 + 1*trial4 + 1*trial5 + 1*trial6
change =~ 0*trial1 + 1*trial2 + 2*trial3 + 3*trial4 + 4*trial5 + 5*trial6
'

fit_growth_model = growth(growth_model, data = data, missing = 'ml')
summary(fit_growth_model, fit.measures = T, standardized = T)

#7
#The average value for initial performance was 18.50 and the average change across trials was 3.59 

#8 
growth_model2 = '
initial =~ 1*trial1 + 1*trial2 + 1*trial3 + 1*trial4 + 1*trial5 + 1*trial6
change =~ 0*trial1 + 1*trial2 + 2*trial3 + 3*trial4 + 4*trial5 + 5*trial6

trial1 ~~ trial2
trial2 ~~ trial3
trial3 ~~ trial4
trial4 ~~ trial5
trial5 ~~ trial6
'

fit_growth_model2 = growth(growth_model2, data = data)
summary(fit_growth_model2, fit.measures = T, standardized = T)
anova(fit_growth_model, fit_growth_model2)

#We freed the error variances of adjacent trials. By doing so, the degrees of freedom 
#and chi square difference value was significantly reduced (p<.05), which indicates that fit growth model 2 is 
#a better fit than fit growth model 1. Freeing the error variances of adjacent trials accounts for the fact that
#scores close to each other in time tend to correlate more strongly than scores that are further apart in time.

#9
#Model 2 overfits the data, and so I am using the correlation from fit growth model 1. 
#The correlation between the initial and change factors is -.58. However, this correlation is 
#not significant (p=.61). This means that initial scores cannot predict change in scores across trials. 

#10
growth_model3 = '
initial =~ 1*trial1 + 1*trial2 + 1*trial3 + 1*trial4 + 1*trial5 + 1*trial6
change =~ 0*trial1 + 1*trial2 + 2*trial3 + 3*trial4 + 4*trial5 + 5*trial6

trial1 ~~ trial2
trial2 ~~ trial3
trial3 ~~ trial4
trial4 ~~ trial5
trial5 ~~ trial6

abil =~ ability

initial ~ abil
change ~ abil
'

fit_growth_model3 = growth(growth_model3, data = data)
summary(fit_growth_model3, standardized = T)

#Ability significantly predicts initial factors (p<.001). This means that those with higher ability will have higher
#initial scores. Ability does not significantly predict change factors (p=.107). Ability cannot be used to predict
#change factors. 

