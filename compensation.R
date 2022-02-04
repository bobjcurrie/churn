## Daily Rate, Hourly Rate

LabTech1 <- churn %>% filter(JobRole == "Laboratory Technician" & JobLevel == 1)

count(LabTech1)
LabTech1 %>% select(HourlyRate, DailyRate, MonthlyRate, MonthlyIncome) %>% mutate(Div = DailyRate/HourlyRate) %>% head(10)

cor(churn$MonthlyRate, churn$MonthlyIncome)

# plot monthly income
# hist
# bar chart by years exp
# by job title and level