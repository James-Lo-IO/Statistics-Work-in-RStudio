#1
library(tidyverse)
library(MASS)

#2
n = 100
set.seed(0922)
  df = mvrnorm(n = n, mu = c(100, 20), Sigma = matrix(c(225, 0, 0, 10), ncol = 2))
  prestige = df[,1]
  years = df[,2]
  error = rnorm(n, mean = 0, sd = 15000)
  prestige_c = prestige-mean(prestige)
  years_c = years-mean(years)
  prestige_effect = 500
  years_effect = 3000
  interaction_effect = 50
  
#3
set.seed(0922) #insert your birthday again
salary = 60000 + prestige_effect*prestige_c + years_effect*years_c + interaction_effect*prestige_c*years_c + error

#4
data = data.frame(salary, prestige, prestige_c, years, years_c)

#5
model = lm(salary ~ prestige_c + years_c + prestige_c*years_c, data)
summary(model)

#6
#The predicted intercept shows that at an average amount of prestige of an undergraduate institution
#and an average amount of job experience, a person can expect to earn $61,032.76. For a one unit increase in prestige 
#of undergraduate institution, a person can expect their salary to go up $559.01. For a one unit increase in job salary, 
#a person can expect their salary to go up $3080.97. There is a significant interaction between 
#prestige and years. Prestige moderates the effect of job experience. Prestige has a stronger effect on predicted salary at a high number of years worked.

#7
model$coefficients
intercept = model$coefficients[1]
prestige_main = model$coefficients[2]
years_main = model$coefficients[3]
interaction = model$coefficients[4]

#8
prestige_m = mean(data$prestige_c)
prestige_sd = sd(data$prestige_c)
prestige_low = prestige_m - prestige_sd
prestige_high = prestige_m + prestige_sd

years_m = mean(data$years_c)
years_sd = sd(data$years_c)
years_low = years_m - years_sd
years_high = years_m + years_sd


salary_low_prestige = intercept + prestige_main*prestige_low + years_main*years_c + interaction*prestige_low*years_c
salary_mean_prestige = intercept + prestige_main*prestige_m + years_main*years_c + interaction*prestige_m*years_c
salary_high_prestige = intercept + prestige_main*prestige_high + years_main*years_c + interaction*prestige_high*years_c

salary_high_prestige

#9
predicted_salary = c(salary_low_prestige, salary_mean_prestige, salary_high_prestige)

#10
df2 = data.frame(predicted_salary,
                 prestige_c = c(rep("Low", 100), rep("Average",100), rep("High", 100)), 
                 years_c = c(years_c,years_c,years_c))

#11
ggplot(df2, aes(x = years_c, y = predicted_salary, color = prestige_c, group = prestige_c)) +
  geom_line(size = 2) +
  theme_classic() +
  labs(x = 'Years Worked', y = 'Predicted Salary', color = 'prestige')
ggsave('Figures/gpa_simple_slopes.png', dpi = 'retina', height = 5, width = 7, units = 'in')

#12
#This is a synergistic effect. Prestige has a stronger effect on predicted salary at a high number of years worked.
#Prestige has a weaker effect on predicted salary at a low number of years worked.

