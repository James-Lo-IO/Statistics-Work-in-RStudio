#1
library(tidyverse)
library(lavaan)
salary = read_csv('salary.csv')

#2
model = '
salary ~ b*time + c*publications
time ~ a*publications

ab := a*b
'

fit_model1 = sem(model, data = salary, se = 'bootstrap')
summary(fit_model1)
#Time significantly mediates the relationship between publications and salary. 
#Time has a significant effect on salary. A one unit increase in time results in a salary increase of $1089.01. 
#Publications has a significant effect on time. A one unit increase in publications results in a time increase of .20 years.
#Publications does not have a significant effect on time. 
#Since C is not significant, there is evidence for full mediation. 
#Publications has a significant indirect effect on salary via time. 

#3
salary_model = '
salary ~ l*gender + m*time + n*publications + o*citations
time ~ f*gender
citations ~ j*time + i*gender + k*publications
publications ~ h*time + g*gender

# Gender indirect effects
io := i*o
gn := g*n
gko := g*k*o
fm := f*m
fhn := f*h*n
fjo := f*j*o
fhko := f*h*k*o

# Time indirect effects
jo := j*o
hn := h*n
hko := h*k*o

# Publication indirect effects
ko := k*o
'


fit_model2 = sem(salary_model, data = salary)
summary(fit_model2)
parameterestimates(fit_model2)

#4
total_gender = 942.684+1.794*856.876+1.794*2.114*92.889+1.794*1.034*201.544+1.794*2.114*.19*201.544+2.426*201.544+.657*92.889+.657*.19*201.544

#l+fm+fhn+fjo+fhko+io+gn+gko

total_time = 856.876+1.034*201.544+2.114*.19*201.544+2.114*92.889
  #m+jo+hko+hn

total_publications = 92.889+.19*201.544
  #n+ko

total_citations = 201.544
#o

#5
#Gender does not exert a direct effect on salary after accounting for time since PhD, publications, and citations.
#Gender can't be used to predict salary. 

#6
salary = salary %>%
  mutate(salary_z = scale(salary),
         time_z = scale(time),
         publications_z = scale(publications),
         citations_z = scale(citations))

salary_model_z = '
salary_z ~ l*gender + m*time_z + n*publications_z + o*citations_z
time_z ~ f*gender
citations_z ~ j*time_z + i*gender + k*publications_z
publications_z ~ h*time_z + g*gender

# Gender indirect effects
io := i*o
gn := g*n
gko := g*k*o
fm := f*m
fhn := f*h*n
fjo := f*j*o
fhko := f*h*k*o

# Time indirect effects
jo := j*o
hn := h*n
hko := h*k*o

# Publication indirect effects
ko := k*o
'

fit_model3 = sem(salary_model_z, data = salary)
summary(fit_model3)
parameterestimates(fit_model3)

#7 
#The significance of the parameters do not change, however the estimates do. Since the significance of the 
#parameters do not change,the interpretation of the model stays the same. 
#Gender still does not exert a direct effect on salary after accounting for time since PhD, publications, and citations.


#8
#There is a significant effect of time on salary. With an increase of one standard deviation in time, salary rises by .378 standard deviations. There is also a significant effect of citations on salary.
#With an increase of one standard deviation in citations, salary rises by 0.357 standard deviations. 
#There is a significant effect of time on publications. With an increase of one standard deviation in time, publications rise by .646 standard deviations.  
#There are no significant indirect effects.




