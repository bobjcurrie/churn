library(tidyverse)

churn <- read_csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")

glimpse(churn)

churn
#1,470 obs and 35 columns, 9 chr and 26 numeric

summary(churn)
# which character are actually binary, i.e. "Yes"/"No" ?

#Over18, EmployeeCount, EmployeeNumber and StandardHours

churn %>% count(Over18)

summary(churn$EmployeeCount)

summary(churn$EmployeeNumber)

churn %>%
  select(EmployeeNumber, 1:3) %>%
  tail()

summary(churn$StandardHours)

# We see that all employees have 'Over18' as "Y",
# All employee count are 1,
# And EmployeeNumber?
# And StandardHours are 80 for all obs
# All do not add any info

churn <- churn %>%
  select(-starts_with("Employee"), -c("Over18", "StandardHours"))

glimpse(churn)

# Column names using janitor package

# Missing values?

# Column constraints or errors
# Categorical vs numeric

#Factors?
churn$Attrition <- parse_factor(churn$Attrition, levels = c("No", "Yes"))

saveRDS(churn, "churn.rds")


