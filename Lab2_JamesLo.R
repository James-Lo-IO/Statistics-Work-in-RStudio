#1
library(tidyverse)

#2
data = read_csv('/Users/jameslo/Desktop/Rscripts/life_expectancy.csv')

#3
df = data %>%
  select(country, year, status, life_expectancy)%>%
  filter(year == 2015)

#4
plot = ggplot(data = data, mapping = aes(x = life_expectancy, y = status, color = status)) +
  geom_jitter(alpha = .3) + geom_smooth(method = 'lm')
plot

#5
status_developing = df%>%
  filter(status == 'Developing')

status_developed = df%>% 
  filter(status == 'Developed')

t.test(x = status_developing$life_expectancy, y = status_developed$life_expectancy)

#6
#There is a significant difference in life expectancy between developing countries and developed countries. 

#7
new_df = data%>%
  select(country, year, life_expectancy)%>%
  filter (year == 2000 | year == 2015)

#8
plot_new_df = ggplot(data = new_df, mapping = aes(x = life_expectancy, y = year, color = year)) +
  geom_jitter(alpha = .3) + geom_smooth(method = 'lm')
plot_new_df
# Not sure why the other years are showing up here

#7
data_wide = new_df %>%
  pivot_wider(id = country, names_from = year, values_from = life_expectancy)

#8
colnames(data_wide)[1] = "country"
colnames(data_wide)[2] = "life_exp_2015"
colnames(data_wide)[3] = "life_exp_2000"

#9
t.test(data_wide$life_exp_2000, data_wide$life_exp_2015, paired = T, var.equal = T)

#10
#There is a significant difference between life expectancy in 2000 and life expectancy in 2015

#11
data_wide = data_wide%>%
  mutate(change = life_exp_2015-life_exp_2000)

#12
t.test(data_wide$change)
#same result

