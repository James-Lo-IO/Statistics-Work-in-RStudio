install.packages("readxl")
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

#Get the names of the sheets in the Excel file
sheets = excel_sheets("/Users/jameslo/Downloads/HR Data.xlsx")

#Read in each individual sheet of the Excel file
for (sheet in sheets) {
  data = read_excel("/Users/jameslo/Downloads/HR Data.xlsx", sheet = sheet)
}

#Access "Past and Current Employees" sheet 
PastandCurrentEmployees = read_excel("/Users/jameslo/Downloads/HR Data.xlsx", sheet = "Past and Current Employees")

#Access "Unit Turnover Rate" sheet
UnitTurnoverRate = read_excel("/Users/jameslo/Downloads/HR Data.xlsx", sheet = "UnitTurnoverRate2022")

#Convert the "Hire_date" and "Termination_date" columns to date format
PastandCurrentEmployees$Hire_date = as.Date(PastandCurrentEmployees$Hire_Date, "%Y-%m-%d")
PastandCurrentEmployees$Termination_date = as.Date(PastandCurrentEmployees$Termination_Date, "%Y-%m-%d")

#Hypothesis 1: Job performance is significantly and negatively related to turnover rate 

#Create a pivot table to calculate unit average performance ratings of 2022
pivot_table = aggregate(Performance_Rating_2022 ~ Unit, PastandCurrentEmployees, mean)
# Rename the columns
colnames(pivot_table) = c("Unit", "Avg_Performance_Rating2022")

#Merge the turnover rate column from UnitTurnoverRate sheet with the Pivot Table
h1_data = merge(pivot_table,UnitTurnoverRate[c("Unit", "2022_Turnover_Rate")], by = 'Unit')

h1_data=h1_data%>%
  rename(Turnover_Rate_2022 = '2022_Turnover_Rate')

#H1 Regression model
model1 = lm(Turnover_Rate_2022 ~ Avg_Performance_Rating2022, data=h1_data)

summary(model1)

#There is a significant relationship between performance ratings in 2022 and job turnover rate in 2022.
#The relationship is negative, meaning that as performance ratings go up, job turnover goes down

#Access the Engagement Survey Results Sheet
EngagementData = read_excel("/Users/jameslo/Downloads/HR Data.xlsx", sheet = "Engagement Survey Results")

filtered_EngagementData = filter(EngagementData, Year ==2022, Category == "Exempt")

#Calculate the unit avg engagement for 2022
filtered_EngagementData$Avg_Eng_2022 = rowMeans(filtered_EngagementData[, c('Job Satisaction', 'Collaboration', 'Communication', 'Support', 'Customer Focus', 'Personal Growth', 'Inclusion', 'Empowerment', 'Accountability')])

#merge the above with turnover
h2_data = merge(h1_data, filtered_EngagementData[c("Unit", "Avg_Eng_2022")], by = "Unit") 

#Hypothesis 2: Employee engagement is significantly and negatively related to turnover rate

#H2 Regression model
model2 = lm(Turnover_Rate_2022 ~ Avg_Eng_2022, data=h2_data)
summary(model2)

#There is a significant relationship between employee engagement and turnover.
#This is a negative relationship, meaning that as employee engagement increases, turnover decreases.

PastandCurrentEmployees=PastandCurrentEmployees%>%
  rename(Employee_Type = 'Category')

filtered_wagedata = filter(PastandCurrentEmployees, Employee_Type == "Exempt")

#Create a pivot table to calculate unit avg wage of 2022
pivot_table2 = aggregate(Wage_Rate_2022 ~ Unit, filtered_wagedata, mean)


# Rename the columns
colnames(pivot_table) = c("Unit", "Avg_Performance_Rating2022")

h3_data = merge(h1_data, pivot_table2[c('Unit', 'Wage_Rate_2022')], by = "Unit")

#Hypothesis 3: The relationship between job performance and turnover is moderated by compensation.


#Regression model 3
model3 = lm(Turnover_Rate_2022 ~ Avg_Performance_Rating2022 + Wage_Rate_2022, data= h3_data)
summary(model3)

#Performance and wage are significantly related to turnover. 
#Performance and wage have a negative relationship to turnover, meaning that as performance
#decreases, turnover increases, and as wage decreases, turnover also increases. 

model4 = lm(Turnover_Rate_2022 ~ Avg_Performance_Rating2022 * Wage_Rate_2022, data= h3_data)
summary(model4)

#There is not a significant interaction effect between wage and performance, meaning 
#This means that the effect of performance on turnover does not depend on wage and vice versa.

#Graph of gender breakdown
gender_count = table(PastandCurrentEmployees$Gender)
gender_percent = gender_count/sum(gender_count)

ggplot(data = data.frame(gender_percent), aes(x = factor(names(gender_percent)), y = gender_percent)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Gender", y = "Percentage of Workforce") +
  theme_minimal()

#Graph of race breakdown
race_count = table(PastandCurrentEmployees$Race)
race_percent = race_count/sum(race_count)

ggplot(data = data.frame(race_percent), aes(x = factor(names(race_percent)), y = race_percent)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Race", y = "Percentage of Workforce") +
  theme_minimal()

ages_df = subset(PastandCurrentEmployees, select = c("ID","Age"))

ages_df$age_band <- cut(ages_df$Age, breaks = c(20, 30, 40, 50, 60, 70), labels = c("20-30", "31-40", "41-50", "51-60", "61-70"))

print(ages_df)

ggplot(na.omit(ages_df), aes(x = age_band)) +
  geom_bar(fill = "lightblue", alpha = 0.5) +
  scale_y_continuous(breaks = seq(0, 35000, by = 5000)) +
  labs(title = "Age Band Frequency",
       x = "Age Bands",
       y = "Frequency")





