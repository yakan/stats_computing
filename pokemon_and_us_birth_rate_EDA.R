# The dataset Pokemon.csv 
# contains information about the characters in the popular Pokemon franchise.

library(ggplot2)
library(dplyr)

data = read.csv("dataset/Pokemon.csv")

x = complete.cases(data)
sum(!x)

# 1 What percent of the Pokemon in the dataset are Legendary?
legend = data %>%
  filter(Legendary == TRUE) %>%
  count()

legend_percent = legend/nrow(data) * 100
legend_percent

# 2 Create a graph that shows the average speed of the different types of
pokemons
class(data$Speed)

data %>%
  group_by(Type) %>%
  summarise(avg = mean(Speed)) %>%
  ggplot(aes(reorder(Type,avg),avg)) +
  geom_bar(stat = "identity", fill = "peachpuff", color = "black") +
  coord_flip() +
  labs(x = "Type", y = "Speed", title = "Average Speed vs Type of Pokemon") +
  scale_y_continuous(breaks = seq(0, 125, by = 25))

# 3 Which generation has the highest mean total score, and what is that score?
data %>%
  group_by(Generation) %>%
  summarise(total_score = sum(HP,Attack,Defense, Speed)) %>%
  arrange(-total_score) %>%
  slice(1)
   
# Case 2
# the dataset US_births.csv contains data about births in the United States 
# from 2000 to 2014.

# 4 How many people in the dataset were born on the Fourth of July?
data2 = read.csv("dataset/US_births.csv")

data2 %>%
  filter(month == 7 & date_of_month == 4 ) %>%
  summarise(sum(births))

# 5 Distributions for Number of US Births per Day (Weekday Vs. Weekend)
data2 %>%
  select(day_of_week,births) %>%
  mutate(week = ifelse(day_of_week %in% c("Sat","Sun"), "Weekends", "Weekdays")) %>%
  ggplot(aes(births, fill = week)) +
  geom_density(alpha = .5) +
  labs(title = "Distributions for Number of US Births per Day (Weekday Vs. Weekend)", x = "Number of Births", y = "") +
  theme(legend.title = element_blank())

# 6 Percent of Births vs. Day of Week
y = data2 %>%
  select(day_of_week, births) %>%
  group_by(day_of_week) %>%
  summarise(total = sum(births)) %>%
  mutate(week = ifelse(day_of_week %in% c("Sat","Sun"), "Weekends", "Weekdays"))

lev = levels(y$day_of_week)[c(2,6,7,5,1,3,4)]
y$day_of_week = factor(y$day_of_week, levels = lev)
  
ggplot(y, aes(day_of_week,(total/sum(total)), fill = week)) +
  geom_bar(stat = "identity") +
  labs(title = "Percent of Births vs Day of Week", x = "Days of Week", y = "Percent of Births")

# 7 Number of Births by Month and Year
data2$month = as.factor(data2$month) 

data2 %>%
  group_by(year,month) %>%
  summarise(total = sum(births)) %>%
  ggplot(aes(year,total, fill = month)) +
  geom_area(stat = "identity") +
  scale_y_continuous(labels = paste0(1:6,"M")) +
  scale_fill_discrete("Month",labels = month.name) +
  labs(title = "Number of Births by Month and Year", y = "Number of Births", x = "Year") +
  scale_x_continuous(breaks = seq(2000,2014,1))

  