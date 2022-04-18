#1
a = 3
b = 12
c = 15

#2
x = c(a, b, c)

#3
y = c('three', 'twelve', 'fifteen')

#4
z = as.character(y)

#5
y_c = c('three', 'twelve', 'fifteen')

#6
x == y
x == y_c
x == y | x == y_c
x == y & x == y_c

#7
xy = x*y

#8
x_sq = x^2

#9
data = read_csv('gre.csv')

#10
data = data %>%
  mutate(total_score = (quant + verbal + writing))

#11
data = data%>%
  mutate(psych_major = case_when(area_of_study == 'chemistry' | area_of_study =='computer science' | area_of_study == 'business' | area_of_study == 'philosophy'~ 'no', 
                                 area_of_study == 'psychology' ~ 'yes'))
#12
writing_scores = data%>%
  select(writing, area_of_study)

#13
chem_majors = data%>%
  filter(area_of_study == 'chemistry')
  select(writing, verbal, quant)

#14
summary_of_GRE_scores = data%>%
  group_by(area_of_study) %>%
  summarize(mean_quant = mean(quant),
            mean_writing = mean(writing),
            mean_verbal = mean(verbal),
            n=n())
#15
plot = ggplot(data = data, mapping = aes(x = quant, y = verbal, color = area_of_study)) +
  geom_jitter(alpha = .3) + geom_smooth(method = 'lm')
plot


