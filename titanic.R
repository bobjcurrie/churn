library(tidyverse)

titanic <- read_csv("train.csv")

glimpse(titanic)

names <- titanic %>%
            select(Name, Age, Sex, SibSp, Parch, Survived)

head(names); tail(names)

names %>% sample_n(10)

# Names seem to be of the format
# LASTNAME, TITLE. FIRSTNAME SECONDNAME...
# If it's a married woman
# LASTNAME, TITLE. HUSBANDNAME... (WIFEFIRSTNAME SECONDNAME... MAIDENNAME)

# str_extract from stringR package ?

# Children 5 and under
titanic %>%
  filter(Age <= 5) %>%
  count(Survived)
