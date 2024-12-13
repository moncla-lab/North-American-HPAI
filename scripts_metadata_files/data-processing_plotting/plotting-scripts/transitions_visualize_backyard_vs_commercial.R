library(ggplot2)
library(dplyr)
library(coda)
# viz the number of transitons from wild to backyard birds and commericial birds
# see enumerate transtions python script to calculate transitions from posterior trees

setwd("~/100/")
#####################
df <- read.csv("100/RF-wild-1_1_100.comb.transitionsbyb.txt", sep = "\t", header = TRUE)
df <- df[-1, ]


median(as.mcmc(df$number_transitions))
hpd_interval <- HPDinterval(as.mcmc(df$number_transitions), prob = 0.95)
hpd_interval

# Calculate proportions for each number of transitions
df %>%
  group_by(number_transitions) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) -> df_prop

# Plot 1: Proportion of all records for each value in number_transitions
ggplot(df_prop, aes(x = factor(number_transitions), y = proportion)) +
  geom_bar(stat = "identity") +
  ylim(0,0.2) +
  labs( title = "100 % Wild",
    x = "Transitions from Wild to Backyard Birds",
    y = "Proportion") +
  theme_minimal() +
  theme(text = element_text(size = 30), axis.text.x = element_text(angle = 45)) 


######################

df2 <- read.csv("100/RF-wild-1_1_100.comb.transitionscomm.txt", sep = "\t", header = TRUE)
df2 <- df[-1, ]


median(df2$number_transitions)
hpd_interval <- HPDinterval(as.mcmc(df2$number_transitions), prob = 0.95)
hpd_interval


# Calculate proportions for each number of transitions
df2 %>%
  group_by(number_transitions) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) -> df2_prop

# Plot 2: Proportion of all records for each value in number_transitions
ggplot(df2_prop, aes(x = factor(number_transitions), y = proportion)) +
  geom_bar(stat = "identity") +
  ylim(0,0.2) +
  labs( title = "100 % Wild",
        x = "Transitions from Wild to Commercial",
        y = "Proportion") +
  theme_minimal() +
  theme(text = element_text(size = 30), axis.text.x = element_text(angle = 45)) 




#############
library(ggplot2)
library(dplyr)
library(tidyr)

setwd("~/25/")
df1 <- read.csv("RF-wild2-1_1_25.transitionsbyb.txt", sep = "\t", header = TRUE)
df1 <- df1[-1, ]

mean(df1$number_transitions)
hpd_interval <- HPDinterval(as.mcmc(df1$number_transitions), prob = 0.95)
hpd_interval

# Calculate proportions for df1
df1 %>%
  group_by(number_transitions) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  mutate(source = "Backyard Birds") -> df1_prop

# Load and prepare the second dataframe
df2 <- read.csv("RF-wild2-1_1_25.transitionscomm.txt", sep = "\t", header = TRUE)
df2 <- df2[-1, ]

mean(df2$number_transitions)
hpd_interval <- HPDinterval(as.mcmc(df2$number_transitions), prob = 0.95)
hpd_interval

# Calculate proportions for df2
df2 %>%
  group_by(number_transitions) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  mutate(source = "Commercial") -> df2_prop

# Combine the two dataframes
df_combined <- bind_rows(df1_prop, df2_prop)

# Ensure all combinations of number_transitions and source are present
df_combined <- df_combined %>%
  complete(number_transitions = seq(min(df_combined$number_transitions), max(df_combined$number_transitions)),
           source = c("Backyard Birds", "Commercial"),
           fill = list(proportion = 0))

ggplot(df_combined, aes(x = as.numeric(number_transitions), y = proportion, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlim(20, 60) +
  ylim(0, 0.2) +
  labs(
       x = "Number of Transitions",
       y = "Proportion of posterior trees") +
  theme_minimal() +
  theme(text = element_text(size = 30), legend.position = "bottom") +
  scale_fill_manual(values = c("Commercial" = "#5CA7A4", "Backyard Birds" = "#D1BA56"))

