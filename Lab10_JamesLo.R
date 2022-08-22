#1
library(lavaan)
library(MASS)


#2
cors = '
1,
.3, 1,
.3, .3, 1,
.1, .1, .1, 1,
.1, .1, .1, .3, 1,
.1, .1, .1, .3, .3, 1,
.1, .1, .1, .1, .1, .1, 1,
.1, .1, .1, .1, .1, .1, .3, 1, .1, .1, .1, .1, .1, .1, .3, .3, 1
'
cov = getCov(cors, lower = T, diagonal = T, names = paste0("v", seq(1,9)))
set.seed(0922) # replace Xs with your birthdate
data = data.frame(mvrnorm(n = 200, mu = rep(0, 9), Sigma = cov))

#3
model1 = '
speed =~ v1 + v2 + v3
memory =~ v4 + v5 + v6
attention =~ v7 + v8 + v9
'

fit_model1 = cfa(model1, data = data) 
summary(fit_model1, fit.measures = T, standardized = T)

#4
#Based on the four different fit statistics, this model fits the data as well as a perfect model would.
#The chi-square value is greater than .05 (p = 0.142), the CFI is above .90 (CFI = .961), the RMSEA
#is less than .08 (RMSEA = .039), and the SRMR is less than .08 (SRMR = .05)

#5
standardizedsolution(fit_model1)

#6
#There is a significant correlation between speed and memory (r = .435, p <.001). This means that the greater your speed, the better your memory. 
#There is a significant correlation between speed and attention (r = .445, p <.001). This means that as speed increases, attention increases. 
#There is a significant correlation between memory and attention (r = .582, p <.001). This means that as memory increases, attention increases. 


#7
cor(data)

#The model estimated correlation for v1 and v2 is .353 (.502*.703 = .353). The observed correlation for 
#v1 and v2 is .314. The model estimated correlation for v1 and v4 is .089 (.502*.406*.435). The observed
#correlation for v1 and v4 is .089. The model estimated correlation for v5 and v7 is .158 (.526*.515*.582).
#The observed correlation for v5 and v7 is .193. 


#8
model2 = '
speedattention =~ v1 + v2 + v3 + v7 + v8 + v9
memory =~ v4 + v5 + v6
'
fit_model2 = cfa(model2, data = data)
summary(fit_model2, fit.measures = T, standardized = T)
standardizedsolution(fit_model2)

#9
anova(fit_model1, fit_model2)
#Model 1 is a better fit

#10
#The chi square test was significant. A significant chi square means one model fits better than the other. We prefer model 1 because 
#it has the fewest degrees of freedom and a lower chi square value than model 2.
#Speed and attention are distinct cognitive constructs. 
