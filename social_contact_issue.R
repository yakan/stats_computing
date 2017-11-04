data = read.csv("flying.csv")
library(dplyr)

x = complete.cases(data)
sum(!x)

# 1 How many males and females responded to the survey?
male_female = data %>%
  filter(gender %in% c("Male","Female")) %>%
  summarise(n())
male_female

# 2 What is the number of people who said that it is 
# very rude when people recline their seats in the airplane?
veryRude_recline = data %>%
  filter(recline_rude == "Very") %>%
  summarise(n())
veryRude_recline

# 3 How many females are in the age group 18-29?
female_1829 = data %>%
  filter(gender == "Female" & age == "18-29") %>%
  summarise(n())
female_1829

# 4 Divide the dataset into two dataframes: males_data and female_data. 
# Each dataframe should include only the following variables: respondent_id, gender, baby
male_data = data %>%
  select(respondent_id, gender, baby) %>%
  filter(gender == "Male")
head(male_data)

female_data = data %>%
  select(respondent_id, gender, baby) %>%
  filter(gender == "Female")
head(female_data)

# 5 Who is more tolerant?
baby_ok_male = male_data %>%
  filter(baby == "No") %>%
  summarise(n())
percent_baby_ok_male = (baby_ok_male/nrow(male_data))*100
percent_baby_ok_male

baby_ok_female = female_data %>%
  filter(baby == "No") %>%
  summarise(n())
percent_baby_ok_female = (baby_ok_female/nrow(female_data))*100
percent_baby_ok_female

percent_baby_ok_female > percent_baby_ok_male

# 6 Which is the most annoying to fliers?
baby_total = data %>%
  filter(baby == "Very") %>%
  summarise(n())

stranger_total = data %>%
  filter(talk_stranger == "Very") %>%
  summarise(n())

walk_total = data %>%
  filter(wake_up_walk == "Very") %>%
  summarise(n())

bathroom_total = data %>%
  filter(wake_up_bathroom == "Very") %>%
  summarise(n())

baby_total
stranger_total
walk_total
bathroom_total

# 7 gender, AgeGroup, household_income
library(ggplot2)
data = read.csv("flying.csv")

female_income_3044 = data %>%
  select(gender, age, household_income) %>%
  filter(gender == "Female" & age == "30-44") %>%
  na.omit()
female_income_3044 %>%
  ggplot(aes(household_income)) +
  geom_bar()

lev1 = levels(female_income_3044$household_income)[c(1,3,4,2)]
female_income_3044$household_income = factor(female_income_3044$household_income, levels = lev1, labels = c(1,2,3,4))
female_income_3044 %>%
  ggplot(aes(household_income)) +
  geom_bar() +
  ggtitle("Income distribution for females in the range of 30-40") +
  ylab("Count of Flyers") +
  xlab("Income")

male_income_3044 = data %>%
  select(gender, age, household_income) %>%
  filter(gender == "Male" & age == "30-44") %>%
  na.omit()
  
lev2 = levels(male_income_3044$household_income)[c(1,3,4,2)]
male_income_3044$household_income = factor(male_income_3044$household_income, levels = lev2)
male_income_3044 %>%
  ggplot(aes(household_income)) +
  geom_bar() +
  ggtitle("Income distribution for males in the range of 30-40") +
  ylab("Count of Flyers") +
  xlab("Income")
