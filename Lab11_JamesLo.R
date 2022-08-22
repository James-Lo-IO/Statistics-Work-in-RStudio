#1
library(lavaan)
data = read_csv('bfi.csv')

#2
model1 = '
conscientiousness =~ bfi3 + bfi8 + bfi13 + bfi18 + bfi23 + bfi28 + bfi33 + bfi38 +bfi43
'

#3
fit_model1 = cfa(model1, data = data)
summary(fit_model1, fit.measures = T, standardized = T)
standardizedsolution(fit_model1)

#The CFI value should ideally be above 0.90, and here it is a bit low (CFI=0.853). 
#The RMSEA value should ideally be less than .08, and here it is higher than that (RMSEA=0.105)
#The SRMR value should ideally be less than .08, and here it meets that criteria (SRMR=0.070)
#The chi square value is significant (p<.001).
#Based on these fit statistics, this model is not a good fit for the data. 

#4 
modificationindices(fit_model1, sort. = T)

#5
model2 = '
conscientiousness =~ bfi3 + bfi8 + bfi13 + bfi18 + bfi23 + bfi28 + bfi33 + bfi38 +bfi43

bfi23 ~~ bfi43
'
fit_model2 = cfa(model2, data = data)
summary(fit_model2, fit.measures = T, standardized = T)
standardizedsolution(fit_model2)

anova(fit_model1, fit_model2)
#In this step, we freed the correlation between the residual variances of bfi23 and bfi43. By doing so,
#the degrees of freedom and chi square difference value was significantly reduced, which indicates that model 2
#is a better fit than model 1. Someone who tends to be lazy (bfi23) may be more 
#mindless, resulting in them being more easily distracted (bfi43). There is reason to believe that bfi23 and bfi43 are particularly correlated
#beyond just being manifestations of conscientiousness.


model3 = '
conscientiousness =~ bfi3 + bfi8 + bfi13 + bfi18 + bfi23 + bfi28 + bfi33 + bfi38 +bfi43

bfi23 ~~ bfi43
bfi3 ~~ bfi13
'
fit_model3 = cfa(model3, data = data)
summary(fit_model3, fit.measures = T, standardized = T)
standardizedsolution(fit_model2)

modificationindices(fit_model3, sort. = T)


anova(fit_model2, fit_model3)

#In this step, we freed the correlation between the residual variances of bfi3 and bfi13. By doing so,
#the degrees of freedom and chi square difference value was significantly reduced, which indicates that model 3
#is a better fit than model 2. Someone who does a thorough job (bfi3) is someone who is reliable (bfi13). 
#To me, being reliable is a part of being thorough. There is reason to believe that bfi3 and bfi13 are particularly correlated
#beyond just being manifestations of conscientiousness. 

#6
model4 = '
agreeableness =~ bfi6 + bfi11+ bfi16 + bfi21+ bfi26 + bfi31+ bfi36
extraversion =~ bfi2 + bfi7+ bfi12 + bfi17+ bfi22 + bfi27+ bfi32 + bfi37 + bfi42
'
fit_model4 = cfa(model4, data = data)
summary(fit_model4, fit.measures = T, standardized = T)
standardizedsolution(fit_model4)

model5 = '
agreeableness =~ bfi6 + bfi11+ bfi16 + bfi21+ bfi26 + bfi31+ bfi36
general =~ bfi6 + bfi11+ bfi16 + bfi21+ bfi26 + bfi31+ bfi36 + bfi2 + bfi7+ bfi12 + bfi17+ bfi22 + bfi27+ bfi32 + bfi37 + bfi42
extraversion =~ bfi2 + bfi7+ bfi12 + bfi17+ bfi22 + bfi27+ bfi32 + bfi37 + bfi42

general ~~ 0*agreeableness
general ~~ 0*extraversion
agreeableness ~~ 0*extraversion
'
fit_model5 = cfa(model5, data = data)
summary(fit_model5, fit.measures = T, standardized = T)
standardizedsolution(fit_model5)

anova(fit_model4, fit_model5)

#Model 5 fits the data better than model 4. Our model fit was improved from model 4 to model 5. Freeing up a parameter reduces our degrees of freedom and chi square value, and there is a 
#significant chi square difference value. 

#7
model6 = '
extraversion =~ bfi1 + bfi6 + bfi16 + bfi21 + bfi26 + bfi31 + bfi36
'
#8



model_6_group <- '
extraversion =~ bfi1 + bfi6 + bfi16 + bfi21 + bfi26 + bfi31 + bfi36
'


fit_model_6_grouped <- cfa(model_6_group, data = data, group = "site")

fit_model_6_weak <- cfa(model_6_group, 
                         data = data, 
                         group = "site",
                         group.equal = "loadings")

summary(fit_model_6_weak, fit.measures = T)
standardizedsolution(fit_model_6_weak)

anova(fit_model_6_grouped, fit_model_6_weak)

fit_strong <- cfa(model_6_group, 
                           data = data, 
                           group = "site",
                           group.equal = c("loadings", "intercepts"))

summary(fit_strong, fit.measures = T)
standardizedsolution(fit_strong)

anova(fit_model_6_grouped, fit_model_6_weak, fit_strong)

#A chi square difference test revealed no significance, indicating that our factor loadings are equal across groups (p=.71). This means that we have evidence for weak invariance.
#Another chi square difference test revealed no significant difference (p=0.45). This means that we have evidence for strong invariance, 
#indicating that our intercepts are equal. 


