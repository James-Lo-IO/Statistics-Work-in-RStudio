#1
library(tidyverse)
install.packages('moments')
library(moments)

data = read_csv('cancer_reg.csv')

#2
#I predict that median income in a county is a predictor of cancer mortality rates. 
#I hypothesize that those with higher median incomes will experience lower cancer mortality rates.
#I believe that this could be a predictor because those with higher incomes might have less stress, 
#might be able to afford better food, and might have more time for exercise. 

#3
ggplot(data, aes(x = TARGET_deathRate)) +
  geom_histogram(color = 'black', fill = 'grey') +
  theme_classic()
skewness(data$TARGET_deathRate)
kurtosis(data$TARGET_deathRate)
labs('Cancer Deaths Per 100,000 People')
#This histogram is leptokurtic. The data is relatively normally distributed. 


#4
ggplot(data, aes(x = medIncome)) +
  geom_histogram(color = 'black', fill = 'grey') +
  theme_classic()
skewness(data$medIncome)
kurtosis(data$medIncome)
#Median income might be positively skewed because there are more people that make lower median amounts of income than high median amounts of income. 
#There are more average or low earning individuals than high earning individuals. 
#This histogram is leptokurtic. The data is relatively normally distributed. 

#5
ggplot(data = data, aes(x = TARGET_deathRate, y = medIncome)) +
  geom_point(alpha = .3, size = 2) +
  geom_smooth(method = 'lm') +
  theme_classic() +
  labs(x = 'Death Rate', y = 'Median Income')

#6
model = lm(TARGET_deathRate ~ medIncome, data = data)
summary(model)
# I specified a model in which the median income in a county predicted
# cancer mortality rates in that county. The model yielded a significant effect 
# of individuals' median income on cancer mortality rates (b = -0.0009, p < .05). A one dollar increase in the 
# median income in a county was associated with a reduction of 0.0009 deaths per 100,000 people
# in a county. Therefore, median income in the population accounted for a significant portion 
# of variance in cancer death rates.

#7
data = data %>%
  mutate(MedianIncome_c = medIncome - mean(medIncome, na.rm = T))

centered_model = lm(TARGET_deathRate ~ MedianIncome_c, data = data)

summary(centered_model)

# I specified a model in which the median income in a county predicted
# cancer mortality rates in that county. At an average median income, the expected cancer mortality rate is 178.70 deaths per 100,000 people.
#The model yielded a significant effect of individuals' median income on cancer mortality rates (b = -0.0009, p < .05). A one dollar increase in the 
# median income in a county was associated with a reduction of 0.0009 deaths per 100,000 people
# in a county. Therefore, median income in the population accounted for a significant portion 
# of variance in cancer death rates.

#8
data = data %>%
  mutate(MedianIncome_z = scale(medIncome),
         deathRate_z = scale(TARGET_deathRate))

standardized_model = lm(deathRate_z ~ MedianIncome_z, data = data)
summary(standardized_model)

# I specified a model in which the median income of individuals in a county predicted
# cancer mortality rates in that county. I standardized both median income and cancer
# mortality rates before entering them into the model. The model yielded a significant effect 
# of individuals' median income on cancer mortality rates (B = -0.43, p < .05). Therefore, income 
# in the population did account for a significant portion of variance in cancer mortality (R^2 = 0.18).

#9
cor.test(data$MedianIncome_z, data$deathRate_z)

#10
predicted_values = predict(model)
data$predicted = predicted_values
data$residual = residuals(model)

#11
ggplot(data, aes(sample = TARGET_deathRate)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic()

library(performance) 
library(see)
library(patchwork)

check_model(model)

check_autocorrelation(model)
check_heteroscedasticity(model)
check_outliers(model)
check_normality(model)


#Although the the normality of residuals plot looks normal, the check_normality function indicates non-normality of residuals.
#The check_heteroscedasticity function indicates that heteroscedasticity is detected. 

