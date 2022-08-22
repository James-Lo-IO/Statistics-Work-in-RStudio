#1
library(tidyverse)
library(psych)
library(cocor)

#2
height = read.csv('height.csv')

#3
ggplot(data = height, aes(x = mother, y = child)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_classic()

#4
(mother_child = cor.test(height$mother, height$child))

#5 
#There is a medium positive correlation between the height of a mother and the height of a child (r(898) = .20)
#Children with taller mothers tend to also be taller. 

#6
height = height %>%
  mutate(mother_bar = mean(mother),
         child_bar = mean(child),
         mother_diff = mother - mother_bar,
         child_diff = child - child_bar,
         cross_products = mother_diff*child_diff,
         z_mother = scale(mother),
         z_child = scale(child),
         z_product = z_mother*z_child)
#7
n = nrow(height)

sum(height$z_product)/(n - 1)

#8 
male_child = height %>%
  filter(gender == "M")

female_child = height %>%
  filter(gender == 'F')

#9
(father_male_child= cor.test(male_child$father, male_child$child))
(mother_female_child = cor.test(female_child$mother, female_child$child))

#10
r1 = cor(male_child$father, male_child$child)
r2 = cor(female_child$mother, female_child$child)

z1 = fisherz(r1)
z2 = fisherz(r2)

#11
se = sqrt((1/(n - 3) + 1/(n - 3)))

#12
z = (z1 - z2)/se

p = pnorm(z, lower.tail = F)*2
p < .05

cocor.indep.groups(r1, r2, 465, 433)

#13
#We estimated the correlation between father and son for the correlation between mother and daughter.
#These are independent correlations, so we tested for a difference between independent correlations.
#Despite a numerically larger correlation for fathers and sons (r(465) = .39) and mothers and daughters (r(433) = .31),
#we cannot conclude that these two correlations are significantly different form each other.
#Both father and mother's height were significantly positively correlated with child's height.
#Sons with tall fathers tended to be tall, and daughters with tall mothers tended to be tall as well. 
