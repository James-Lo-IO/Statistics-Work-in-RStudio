#1
library(tidyverse)
library(aod)

#2
data = read_csv('diabetes.csv')

#3
has_diabetes = mean(data$diagnosis)

#4
model1 <- glm(diagnosis ~ age, data = data, family = "binomial")

summary(model1)

#5
intercept = summary(model1)$coefficients[1]
age_effect = summary(model1)$coefficients[2]

log_odds_40 = intercept + age_effect*40
log_odds_65 = intercept + age_effect*65

odds_40 = exp(log_odds_40)
odds_65 = exp(log_odds_65)

# What is the odds ratio?
odds_65/odds_40

#A 65 year old is almost 3 times as likely to have a diagnosis than a 40 year old. 


#6 
predicted_prob1 = predict(model1, newdata = data, type = 'response')
predicted_prob1

data$predict_model1 = predicted_prob1

data = data%>%
  mutate(greater_than_chance = case_when(predict_model1 > mean(data$diagnosis) ~ 1,
                                      predict_model1 < mean(data$diagnosis) ~ 0))

data = data%>%
  mutate(correct_guess = case_when(diagnosis == greater_than_chance ~ 1,
                                         diagnosis != greater_than_chance ~ 0))

guess_is_correct = mean(data$correct_guess)

#7
model2 = glm(diagnosis ~ age + bmi, data, family = 'binomial')
summary(model2)

#8
intercept2 = summary(model2)$coefficients[1]
age_effect2 = summary(model2)$coefficients[2]
bmi_effect2 = summary(model2)$coefficients[3]

log_odds_a = intercept2 + age_effect2*50 + bmi_effect2*25
odds_a = exp(log_odds_a)

log_odds_b = intercept2 + age_effect2*25 + bmi_effect2*35
odds_b = exp(log_odds_b)

odds_a/odds_b
odds_b/odds_a
#The person who is 50 years old with a bmi of 25 is 1.17 times more likely to have a diagnosis
#of diabetes than a person who is 25 years old with a bmi of 35.  

#9
model3 <- glm(diagnosis ~ age + bmi + blood_pressure, data = data, family = "binomial")
summary(model3)



#10 
#Someone with a higher age will have a higher chance of being diagnosed with diabetes.
#For every one unit increase in age, someone is .05 times more likely to get diagnosed with diabetes.
#Someone with a higher BMI will have a higher chance of being diagnosed with diabetes.
#For every one unit increase in BMI, someone is .10 times more likely to be diagnosed with diabetes.
#Blood pressure does not have a significant effect on getting a diagnosis of diabetes.

#11
predicted_prob_model3 = predict(model3, newdata = data, type = 'response')
predicted_prob_model3

data$predict_model3 = predicted_prob_model3


data = data%>%
  mutate(greater_than_chance_model3 = case_when(predict_model3 > mean(data$diagnosis) ~ 1,
                                         predict_model3 < mean(data$diagnosis) ~ 0))

data = data%>%
  mutate(correct_guess_model3 = case_when(diagnosis == greater_than_chance_model3 ~ 1,
                                   diagnosis != greater_than_chance_model3 ~ 0))

guess_is_correct_model3 = mean(data$correct_guess_model3)
guess_is_correct_model3

#12
guess_is_correct_model3/guess_is_correct


#The model from exercise 4 was correct 65% of the time. 
#The model from exercise 9 was correct 68% of the time.
#This means that the model from exercise 9 was 1.04 times more accurate
#than the model from exercise 4. 


