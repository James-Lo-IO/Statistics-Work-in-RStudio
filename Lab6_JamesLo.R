#1
library(tidyverse)
library(psych)
library(cowplot) 
library(performance)
data = read_csv('cancer_reg.csv')

#2

data = data %>%
  mutate(TARGETdeathRate_z = scale(TARGET_deathRate),
         medIncome_z = scale(medIncome),
         incidenceRate_z = scale(incidenceRate))

data = data %>%
  mutate(TARGET_deathRate_residual = residuals(lm(TARGETdeathRate_z ~ incidenceRate_z)))

cor(data$TARGET_deathRate_z, data$incidenceRate_z)

cor.test(data$TARGET_deathRate_z, data$incidenceRate_z)

m1 = lm(TARGET_deathRate_z ~ medIncome_z + incidenceRate_z, data = data)
summary(m1)
anova(m1)

#This correlation is still significant after partialing out incidence rates.

#3
#I predict that median income in a county is a predictor of cancer mortality rates. 
#I hypothesize that those with higher median incomes will experience lower cancer mortality rates.
#I believe that this could be a predictor because those with higher incomes might have less stress, 
#might be able to afford better food, and might have more time for exercise. 
#I predict that median age is also a predictor of cancer mortality rates. 
#Those who are older tend to have more health problems.
#I also predict that percent married might be a predictor of cancer mortality rates.
#Those that are married might be less stressed since they have 
#a partner that can help them with things in day to day life. Less stress will result in lower 
#cancer mortality rates.

#4
df = data %>%
  select(TARGET_deathRate, medIncome, MedianAge, PercentMarried)%>%
  filter(MedianAge < 100)

#5
# Scatterplot of median income and median age
p1 = ggplot(df, aes(x = medIncome, y = MedianAge)) +
  geom_jitter(alpha = .3) +
  geom_smooth() +
  labs(x = 'Median Income ($)', y = 'Median Age')
p1

# Scatterplot of median income and percent married
p2 = ggplot(df, aes(x = medIncome, y = PercentMarried)) +
  geom_jitter(alpha = .3) +
  geom_smooth() +
  labs(x = 'Median Income ($)', y = 'PercentMarried')
p2

# Scatterplot of median income and target death rate
p3 = ggplot(df, aes(x = medIncome, y = TARGET_deathRate)) +
  geom_jitter(alpha = .3) +
  geom_smooth() +
  labs(x = 'Median Income ($)', y = 'Mortality Rate')
p3

# Scatterplot of median age and target death rate
p4 = ggplot(df, aes(x = MedianAge, y = PercentMarried)) +
  geom_jitter(alpha = .3) +
  geom_smooth() +
  labs(x = 'Median Age', y = 'Percent Married')
p4

# Scatterplot of median age and percent married
p5 = ggplot(df, aes(x = MedianAge, y = TARGET_deathRate)) +
  geom_jitter(alpha = .3) +
  geom_smooth() +
  labs(x = 'Median Age', y = 'Mortality Rate')
p5

# Scatterplot of percent married and mortality rate
p6 = ggplot(df, aes(x = PercentMarried, y = TARGET_deathRate)) +
  geom_jitter(alpha = .3) +
  geom_smooth() +
  labs(x = 'Percent Married', y = 'Mortality Rate')
p6

#Histogram of mortality rate
p7 = ggplot(df, aes(x = TARGET_deathRate)) +
  geom_histogram(color = 'black', fill = 'grey', bins = 15) +
  labs(y = NULL)
p7

#Histogram of percent married
p8 = ggplot(df, aes(x = PercentMarried)) +
  geom_histogram(color = 'black', fill = 'grey', bins = 15) +
  labs(y = NULL)
p8

#Histogram of median income
p9 = ggplot(df, aes(x = medIncome)) +
  geom_histogram(color = 'black', fill = 'grey', bins = 15) +
  labs(y = NULL)
p9

#Histogram of median age
p10 = ggplot(df, aes(x = MedianAge)) +
  geom_histogram(color = 'black', fill = 'grey', bins = 15) +
  labs(y = NULL)
p10

#6
df = df %>%
  mutate(TARGET_deathRate_c = TARGET_deathRate - mean(TARGET_deathRate),
         medIncome_c = medIncome - mean(medIncome),
         MedianAge_c = MedianAge - mean(MedianAge),
         PercentMarried_c = PercentMarried - mean(PercentMarried))

#Step 1
s1 = lm(TARGET_deathRate ~ medIncome_c, data = df)
summary(s1)
anova(s1)
#Median income adds significantly to the explained variance in cancer mortality rates.
#Median income accounts for a significantly unique portions of variance in cancer mortality rates.

#Step 2
s2 = lm(TARGET_deathRate ~ medIncome_c + MedianAge_c, data = df)
summary(s2)
anova(s1, s2)
#Median age adds significantly to the explained variance in cancer mortality rates.
#Both median income and median age account for significant, unique portions of variance in cancer mortality rates


#Step 3
s3 = lm(TARGET_deathRate ~ medIncome_c + MedianAge_c + PercentMarried_c, data = df)
summary(s3)
anova(s2, s3)
#Percent married adds significantly to the explained variance in cancer mortality rates.
#Median income age and percent married account for significant, unique portions of variance,
#but median age does not. 


#8
model = lm(TARGET_deathRate ~ medIncome + MedianAge + PercentMarried, data = df)
summary(model)

check_model(model)

check_collinearity(model)
#According to the check_collinearity() function, there are no issues with multicollinearity
