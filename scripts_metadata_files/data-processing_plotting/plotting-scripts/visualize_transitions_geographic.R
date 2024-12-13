library(ggplot2)
library(dplyr)
library(coda)
# viz the number of transitons from Asia to North America in global HPAI phylogeny 
# see jupyter notebook to get transitions 

setwd("~//")
df <- read.csv("transitionsContinent.txt", sep = "\t", header = TRUE)
df <- df[-1, ]
# account for the introduction from Europe (Consistently 1)
df$number_transitions <- df$number_transitions - 1


mean(df$number_transitions)
hpd_interval <- HPDinterval(as.mcmc(df$number_transitions), prob = 0.95)
hpd_interval

ggplot(df, aes(x = factor(number_transitions))) +
  geom_bar() +
  labs(
       x = "Transitions from Asia to North America",
       y = "Count") +
  theme_minimal()

# Calculate proportions for each number of transitions
df %>%
  group_by(number_transitions) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) -> df_prop

# Plot 2: Proportion of all records for each value in number_transitions
ggplot(df_prop, aes(x = factor(number_transitions), y = proportion)) +
  geom_bar(stat = "identity") +
  ylim(0,0.8) +
  labs(
       x = "Transitions from Asia to North America",
       y = "Proportion") +
  theme_minimal() +
  theme(text = element_text(size = 30)) 
