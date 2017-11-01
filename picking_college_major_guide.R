library(ggplot2)
library(dplyr)
data = read.csv("dataset/college_recent_grads.csv")

# 1 Create a dataframe (call it female_engineering) for all engineering majors 
# who has 50% or more female students. 
# Sort the dataframe in decreasing order based on the % of female students.

female_engineering = data %>%
  filter(sharewomen >= .5) %>%
  arrange(desc(sharewomen))
female_engineering

# 2 What is the total number (sum) of both engineering and business majors?
bus_eng_total = data %>%
  filter(major_category %in% c("Business","Engineering")) %>%
  summarise(n())
bus_eng_total

# 3 Create a new variable unemployment_rate (defined as the the number of people unemployed divided by the total), 
# and add it to the original dataset. Then, return a table (call it top_10)
top_10 = data %>%
  mutate(unemployment_rate = unemployed/total) %>%
  arrange(desc(unemployment_rate)) %>%
  head(n = 10L)
top_10

# 4 Create a dot plot chart to show the highest unemployment rates for the di ernt majors.
# theme(plot.title = element_text(hjust=))
top_10 %>%
  ggplot(aes(reorder(major,unemployment_rate),unemployment_rate*100)) +
  geom_point() +
  coord_flip() +
  ggtitle("highest unemployment rates by major") +
  xlab("Rate (%)") +
  ylab("") +
  theme(plot.title = element_text(hjust = 0.5))

# 5 coord_flip beware!
# coord_flip(), scale_y_continuous(labels = )
top_10 %>%
  ggplot(aes(reorder(major,unemployment_rate),unemployment_rate*100)) +
  geom_point() +
  coord_flip() +
  ggtitle("highest unemployment rates by major") +
  xlab("Rate (%)") +
  ylab("") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(10, 15.5, by = .5),
                     labels = c(10,10.5,11,11.5,12,12.5,13,13.5,14,14.5,15,15.5))

# 6 Create a dataframe (call it majors_total) that shows the total number of students in each of the major categories. 
# Which major category had the highest number of students?
majors_total = data %>%
  group_by(major_category) %>%
  summarise(totalMajor = sum(total)) %>%
  arrange(desc(totalMajor))

majors_total

# 7 a graph of the 5 major categories with the most total students.
# scale_y_continuous(breaks = ), seq()
majors_total[1:5,] %>%
  ggplot(aes(reorder(major_category,totalMajor),totalMajor/1000)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 1250 ,by = 250)) +
  ylab("total number of students (1000's)") +
  xlab("")

# 8 Using the majors_total table you created earlier, create a new variable called total_category such that 
# if total number of students is less than or equal 500,000, then the category is “Low”, otherwise, it is “High”. 
# ifelse()
x = majors_total %>%
  mutate(total_category = ifelse(totalMajor <= 500000, "Low", "High"))
x
# 9 a barchart
# scale_fill_manual(value = )
x %>%
  ggplot(aes(reorder(major_category,totalMajor),totalMajor/1000, fill = total_category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 1250 ,by = 250)) +
  ylab("total number of students (1000's)") +
  xlab("") +
  scale_fill_manual(values = c("red","lightblue"))
