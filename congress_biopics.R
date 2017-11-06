data = read.csv("congress.csv")
library(ggplot2)
library(dplyr)
library(readr)

x = complete.cases(data)
sum(!x)

class(data$party)

# 1
data %>%
  group_by(party) %>%
  filter(party %in% c("D","R")) %>%
  ggplot(aes(age, fill = party)) +
  geom_histogram(color = "black") +
  facet_grid(~party) +
  scale_fill_manual(values = c("blue","red")) +
  guides(fill = FALSE) +
  xlab("Age") +
  ggtitle("Age Distribution for Democrats and Republicans in Congress") +
  theme(plot.title = element_text(hjust = 0.5))

head(data$chamber)
head(data$party)

# 2
data$chamber = factor(data$chamber, labels = c("House of Representative","Senate"))
data %>%
  group_by(chamber) %>%
  ggplot(aes(chamber,age)) +
  geom_boxplot() +
  xlab("Chamber") +
  ylab("Age")

levels(data$party)
class(data$termstart)

# 3
nonDR = data %>%
  select(party, termstart) %>%
  filter(party %in% c("I","ID","AL","L")) %>%
  group_by(termstart) %>%
  summarise(total = n()) %>%
  arrange(desc(total))
nonDR[1:3,]

# 4
class(data$termstart)
senate_age = data %>%
  group_by(termstart) %>%
  filter(chamber == "Senate" & termstart != 1981) %>%
  summarise(age = mean(age)) %>%
  mutate(af_bef = ifelse(termstart <=1980, "Before", "After"))

# change legend title
senate_age %>%
  ggplot(aes(termstart,age, color = af_bef)) +
  geom_point() +
  scale_x_continuous(breaks = seq(1945,2015, by = 10)) +
  scale_y_continuous(breaks = seq(53, 63, by = 1)) +
  guides(color = guide_legend(title = "After 1981?")) +
  xlab("Term Start") +
  ylab("Age") +
  ggtitle("Average age versus term start (Senate)") +
  theme(plot.title = element_text(hjust = .5))

# 5
data2 = read.csv("biopics.csv")
x2 = complete.cases(data2)
sum(!x2)

earnings = data2 %>%
  filter(box_office != "NA") %>%
  summarise(n())
data2 %>% 
  filter(!is.na(box_office)) %>%
  nrow()

# 6
director = data2 %>%
  filter(box_office != "NA") %>%
  distinct(title, .keep_all = TRUE) %>%
  select(title, box_office, director) %>%
  group_by(director) %>%
  summarise(total = sum(box_office)) %>%
  arrange(desc(total))

data2 %>%
  distinct(title, .keep_all = TRUE) %>% #use the hint in the question group_by(director) %>%
  group_by(director) %>%
  summarise(tot = sum(box_office)) %>%
  arrange(-tot) %>%
  slice(1)

# 7
data2 %>%
#  distinct(title, .keep_all = TRUE) %>%
  ggplot(aes(log(box_office))) +
  geom_line(stat = "density") +
  geom_density(fill = "dodgerblue", color = "black") +
  xlim(0,25) +
  xlab("Log Box Office Revenue") +
  ylab("Density") +
  ggtitle("Distribution of Log Bog Office Revenue") +
  theme(plot.title = element_text(hjust = .5))

data2 %>%
  mutate(box_office = log(box_office)) %>%
  ggplot(aes(box_office))+
  geom_density(col = NA, fill = "dodgerblue")+
  geom_line(stat = "density")+
  labs(y = "Density", x = "Log Box Office Revenue", 
    title = "Distribution of Log Box Offic") +
  scale_x_continuous(limits = c(0,25))


# 8
race_sex = data2 %>%
  distinct(title, .keep_all = TRUE) %>%
  filter(subject_race != "NA" & subject_sex != "NA") %>%
  select(subject_race, subject_sex) %>%
  group_by(subject_race, subject_sex) %>%
  count()

race_sex %>%
  ggplot(aes(reorder(subject_race, n), n, fill = subject_sex)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  coord_flip() +
  xlab("Race") +
  ylab("Count") +
  ggtitle("Number of Characters in Biopics by Race and Sex") +
  theme(plot.title = element_text(hjust = .5))

data2 %>%
  filter(!is.na(subject_race)& !is.na(subject_sex)) %>%
  group_by(subject_race,subject_sex) %>%
  count() %>%
  ggplot(aes(reorder(subject_race,n),n,fill=subject_sex)) +
  geom_col(position = "dodge", col = "black") + 
  # similar to geom_bar(stat ="idenitity") 
  coord_flip() +
  labs(x = "Race", y = "Count", title = "Number of Characters in Biopics by Race and Sex")
