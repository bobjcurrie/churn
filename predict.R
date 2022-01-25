library(tidyverse)
library(broom)

# Retrieve churn tibble from saved R object
churn <- readRDS("churn.rds")

library(Information)
library(caret)

churn_final <- churn_final %>%
                  mutate(Turnover = ifelse(Attrition == "No", 0, 1)) %>%
                  select(-c("med_pay", "Attrition"))

IV <- create_infotables(data=churn_final, y = "Turnover")

print(IV$Summary)

set.seed(2022)

#store row numbers used to create training dataset
index_train <- createDataPartition(churn_final$Turnover, p=.7, list=FALSE)

train_set <- churn_final[index_train, ]

test_set <- churn_final[-index_train, ]

# Make sure train and test set have same proportion of active employees
train_set %>%
  mutate(status = ifelse(Turnover == 0, "Active", "Inactive")) %>%
  count(status) %>%
  mutate(prop = n / sum(n))
#???

train_set %>%
  count(JobRole)

churn %>%
  count(JobRole)


# Logistic regression
simple_log <- glm(Turnover ~ PercentSalaryHike, data=churn_final)

summary(simple_log)

multi_log = glm(Turnover ~ ., family = "binomial", data=churn_final)

summary(multi_log)

#calculate the correlation coefficient between employee age and monthly income
cor(train_set$Age, train_set$MonthlyIncome)

#test for multi-collinearity
library(car)
vif(multi_log)

new_model <- glm(Turnover ~ . -Department, family="binomial", data=churn_final)
summary(new_model)
vif(new_model)

new_model <- glm(Turnover ~ . -Department -JobLevel -JobRole -YearsAtCompany, family="binomial", data=churn_final)
