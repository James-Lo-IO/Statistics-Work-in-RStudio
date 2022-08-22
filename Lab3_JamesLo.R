#1
library(tidyverse)
library(car)
library(rstatix)

#2
data = read_csv('working_hours.csv')

#3
df = data %>%
  filter(country == 'Nigeria' | country == 'China' | country == 'India')

#4
plot = ggplot(df, aes(x = country, y = hours, color = country)) +
  geom_boxplot(color = 'black', width = 0.5) +
  geom_jitter(width = .2, height = 0) +
  theme_classic()
ggsave('Figures/boxplot.png', dpi = 'retina', height = 5, width = 5, units = 'in')
plot


#5
df_anova = anova_test(data = df,
                      dv = hours,
                      between = country)
get_anova_table(df_anova)

df_summary = df %>%
  group_by(country) %>%
  summarize(mean = mean(hours),
            se = sd(hours)/sqrt(n()))

#6
#There is a significant difference in the number of hours worked by an individual in a year between China, India, and Nigeria. 
#This means that at least two of the groups differ significantly in the number of hours worked, but pairwise comparisons are needed to determine where the differences lie. 
#On average, people in India worked more than people in China, those in Nigeria worked the least. 

#7
years = data %>%
  filter(year == 1950 | year == 1980 | year == 2010)

#8
years$year = as.factor(years$year)
class(years$year)

#9
years_plot = ggplot(data = years, mapping = aes(x = year, y = hours, color = year)) +
  geom_jitter(alpha = .3) + geom_smooth(method = 'lm')
years_plot

#10
years_repeated_anova = anova_test(data = years, dv = hours, within = year, wid = country)

get_anova_table(years_repeated_anova)

repeated_anova_summary = years %>%
  group_by(year) %>%
  summarize(mean = mean(hours),
            se = sd(hours)/sqrt(n()))

#11
#There is a significant difference in number of hours worked based on year. Pairwise comparisons are needed to see where the differences lie. 
#Those in 1950 worked the most (M = 2158.41) and those in 2010 worked the least (M = 1898.09)

#12
mixed_anova <- anova_test(
  data = years, 
  dv = hours, 
  wid = country,
  between = continent, 
  within = year
)
get_anova_table(mixed_anova)

mixed_anova_summary = years %>%
  group_by(continent) %>%
  summarize(mean = mean(hours),
            se = sd(hours)/sqrt(n()))

#13
#There was not a significant main effect of continent, meaning that the number of hours worked did not differ significantly based on continent.
#There was a significant main effect of year, such that the number of hours worked differed significantly based on the year worked.
#There is also a significant interaction effect between continent and year.
#Follow up tests are needed to determine where the differences lie
#Overall, those in Europe worked the least (M = 1854.57) and those not part of a continent worked the most (M = 2182.03)

#Bonus
bonus_plot = ggplot(data = years, mapping = aes(x = year, y = hours, group = continent, color = continent)) +
  geom_jitter() + geom_smooth(method = 'lm', se = F)
bonus_plot







