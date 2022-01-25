library(tidyverse)
library(broom)

# Retrieve churn tibble from saved R object
churn <- readRDS("churn.rds")

# EDA aand preliminary analysis

summary(churn$Age)

ggplot(churn, aes(Age)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density()

churn %>%
  count(Gender)

churn %>%
  count(EducationField)

churn %>%
  count(JobRole)

churn %>%
  filter(Attrition == 'Yes') %>%
  count(JobRole) %>%
  arrange(desc(n))

churn %>%
  group_by(JobRole) %>%
  summarize(turnover_level = mean(Attrition == "Yes")) %>%
  arrange(desc(turnover_level))
# also ggplot with geom_col?

churn %>%
  group_by(Department) %>%
  summarize(turnover_level = mean(Attrition == "Yes")) %>%
  arrange(desc(turnover_level))

churn %>%
  group_by(PerformanceRating) %>%
  summarize(turnover_level = mean(Attrition == "Yes")) %>%
  arrange(desc(turnover_level))

ggplot(churn, aes(x=Attrition, y = DistanceFromHome)) +
  geom_boxplot()

# Difference between years at company for attrition?
t.test(YearsAtCompany ~ Attrition, data = churn)

# Same question, for distance from home
t.test(DistanceFromHome ~ Attrition, data = churn)

# Difference between men and women?
chisq.test(churn$Attrition, churn$Gender)

# Marital status?
chisq.test(churn$Attrition, churn$MaritalStatus) %>%
  tidy()

# Overtime?
chisq.test(churn$Attrition, churn$OverTime) %>%
  tidy() %>%
  pull(p.value)

# Let's plot!
churn %>%
  ggplot(aes(x = MaritalStatus, fill = Attrition)) +
  geom_bar()

churn %>%
  group_by(MaritalStatus) %>%
  summarize(turnover_level = mean(Attrition == "Yes")) %>%
  arrange(desc(turnover_level))

churn %>%
  group_by(OverTime) %>%
  summarize(turnover_level = mean(Attrition == "Yes"))


# 100% stacked bar chart
# any differences in department amongst those who stay/leave?
churn %>%
  ggplot(aes(x=Attrition, fill = Department)) +
  geom_bar(position = "fill")

# Marital status?
churn %>%
  ggplot(aes(x=Attrition, fill = MaritalStatus)) +
  geom_bar(position = "fill")

# any difference between sales department and other depts wrt marital status?
churn %>%
  ggplot(aes(x=MaritalStatus, fill=Department)) +
  geom_bar(position = "fill")

# Years at company
churn %>% ggplot(aes(x=Attrition, y=YearsAtCompany,)) +
            geom_boxplot()

# Years at company vs Total working years

# Years at company and years at current role

# Compensation
churn %>% ggplot(aes(x=MonthlyIncome)) +
            geom_histogram()

churn %>% ggplot(aes(x=Department, y=MonthlyIncome)) +
            geom_boxplot()

# Compa-Ratio for each job role and job level
churn_compa_ratio <- churn %>% 
                  group_by(JobRole, JobLevel) %>%
                  mutate(med_pay = median(MonthlyIncome),
                            compa_ratio = MonthlyIncome/med_pay)

churn_compa_ratio %>%
  distinct(JobRole, JobLevel, med_pay) %>%
  arrange(JobRole, JobLevel)

# Add compa_level
churn_final <- churn_compa_ratio %>%
  mutate(compa_level = ifelse(compa_ratio > 1, "Above", "Below"))

# Compare compa_;evel for Active vs Inactive employees
churn_final %>% ggplot(aes(x=Attrition, fill=compa_level)) +
  geom_bar(position = "fill")

# test for signicance
chisq.test(churn_final$Attrition, churn_final$compa_level) %>%
  tidy()

#Years since last promotion, years at company, and Age
churn_final %>%
  group_by(Attrition) %>%
  summarize(AvgYrsSincePromo = mean(YearsSinceLastPromotion),
            AvgYearsAtComp = mean(YearsAtCompany),
            AvgAge = mean(Age))

# Histogram of Years at company facetted by Attrtion
churn_final %>%
  ggplot(aes(x=YearsAtCompany)) +
  geom_histogram() +
  facet_wrap(~ Attrition)

#  Travel?
churn_final %>%
  group_by(BusinessTravel) %>%
  summarize(turnover_level = mean(Attrition == "Yes"))
  
churn_final %>%
  filter(BusinessTravel == "Travel_Frequently", Attrition == "Yes") %>%
  ggplot(aes(x=Department)) +
    geom_bar()
