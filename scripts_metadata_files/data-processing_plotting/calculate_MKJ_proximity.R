library(dplyr)
library(ggplot2)
library(coda) 
library(stringr)

# This script takes the output from the PERL script to summarize jumps in a markov jump analysis and calculates the mean and 95% hpd for different mkj, and for our flyway analysis looks at jumps from east to west and between adjacent and distant flyways
setwd("~")
df <- read.csv("jumptimes_flyways.txt", sep = '\t')



#### 
# mean and 95% HPD calculation with violin plots

# Add a column for the jump pair
df <- df %>%
  mutate(jump_pair = paste(from, to, sep = " -> "))

jumps_per_state_pair <- df %>%
  group_by(state, jump_pair) %>%
  summarise(num_jumps = n(), .groups = "drop")

# Summarize across states for each jump_pair
summary_stats <- jumps_per_state_pair %>%
  group_by(jump_pair) %>%
  summarise(
    mean_jumps = mean(num_jumps),
    lower_hpd = HPDinterval(as.mcmc(num_jumps))[1],
    upper_hpd = HPDinterval(as.mcmc(num_jumps))[2],
    .groups = "drop"
  )

summary_stats

# Split and pad origin and destination
summary_stats <- summary_stats %>%
  mutate(
    origin = word(jump_pair, 1, sep = " -> "),
    destination = word(jump_pair, 2, sep = " -> "),
    # Pad for alignment
    label_aligned = str_pad(origin, width = max(nchar(origin)), side = "right") %>%
      paste("->", str_pad(destination, width = max(nchar(destination)), side = "left"))
  )

# Order by mean_jumps or however you like
summary_stats <- summary_stats %>%
  mutate(label_aligned = factor(label_aligned, levels = label_aligned[order(mean_jumps)]))

# Plot
ggplot(summary_stats, aes(x = mean_jumps, y = label_aligned)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbarh(aes(xmin = lower_hpd, xmax = upper_hpd), height = 0.2, color = "gray40") +
  labs(x = "Mean Number of Jumps", y = "Jump Pair", title = "Migration Jumps with HPD Intervals") +
  theme_minimal(base_family = "mono")  # Use monospaced font for alignment



############
#jumps between adjacent flyways

# jumps from east to west vs west to east
direction_lookup <- tribble(
  ~from,         ~to,           ~direction,
  "atlantic_flyway",    "atlantic_flyway",     "same",
  "atlantic_flyway",    "mississippi_flyway",  "west",
  "atlantic_flyway",    "central_flyway",      "west",
  "atlantic_flyway",    "pacific_flyway",      "west",
  "mississippi_flyway", "atlantic_flyway",     "east",
  "mississippi_flyway", "mississippi_flyway",  "same",
  "mississippi_flyway", "central_flyway",      "west",
  "mississippi_flyway", "pacific_flyway",      "west",
  "central_flyway",     "atlantic_flyway",     "east",
  "central_flyway",     "mississippi_flyway",  "east",
  "central_flyway",     "central_flyway",      "same",
  "central_flyway",     "pacific_flyway",      "west",
  "pacific_flyway",     "atlantic_flyway",     "east",
  "pacific_flyway",     "mississippi_flyway",  "east",
  "pacific_flyway",     "central_flyway",      "east",
  "pacific_flyway",     "pacific_flyway",      "same"
)


df_labeled <- df %>%
  left_join(direction_lookup, by = c("from", "to"))


jumps_by_dir <- df_labeled %>%
  filter(direction %in% c("east", "west")) %>%
  group_by(state, direction) %>%
  summarise(num_jumps = n(), .groups = "drop")


summary_by_dir <- jumps_by_dir %>%
  group_by(direction) %>%
  summarise(
    mean_jumps = mean(num_jumps),
    lower_hpd = HPDinterval(as.mcmc(num_jumps))[1],
    upper_hpd = HPDinterval(as.mcmc(num_jumps))[2],
    .groups = "drop"
  )

summary_by_dir


# visualization of density plots
palette <- c('#7E4794', '#9d2c00', '#0b81a2', '#59a89c')
dir_colors <- c("east" = palette[3], "west" = palette[2])  # Assign suitable contrasting colors


# Make sure `direction` is a factor for consistent plotting
summary_by_dir <- summary_by_dir %>%
  mutate(label = paste0("Mean = ", ceiling(mean_jumps)),
         y_pos = 0.07)

# Plot
ggplot(jumps_by_dir, aes(x = num_jumps, fill = direction, color = direction)) +
  geom_density(alpha = 0.4) +
  geom_vline(data = summary_by_dir, 
             aes(xintercept = mean_jumps, color = direction), 
             linetype = "dashed", size = 1) +
  geom_text(data = summary_by_dir, 
            aes(x = mean_jumps, y = y_pos, label = label, color = direction),
            hjust = -0.1, size = 6) +
  scale_color_manual(values = dir_colors) +
  scale_fill_manual(values = dir_colors) +
  ylim(0,0.085) +
  scale_x_continuous(breaks = seq(0, 300, by = 50),limits = c(0,300)) + 
  labs(x = "Number of Jumps", y = "Density", 
       title = "Posterior Distribution of Markov Jumps by Direction",
       color = "Direction", fill = "Direction") +
  theme_minimal(base_size = 14) +
  theme(text = element_text(size = 18))


################
#### adjacent vs far 

adjacency_lookup <- tribble(
  ~from,               ~to,                  ~proximity,
  "pacific_flyway",    "central_flyway",     "adjacent",
  "central_flyway",    "pacific_flyway",     "adjacent",
  "central_flyway",    "mississippi_flyway", "adjacent",
  "mississippi_flyway","central_flyway",     "adjacent",
  "mississippi_flyway","atlantic_flyway",    "adjacent",
  "atlantic_flyway",   "mississippi_flyway", "adjacent",
  # All same flyway = adjacent
  "pacific_flyway",    "pacific_flyway",     "adjacent",
  "central_flyway",    "central_flyway",     "adjacent",
  "mississippi_flyway","mississippi_flyway", "adjacent",
  "atlantic_flyway",   "atlantic_flyway",    "adjacent",
  # Everything else = distant
  "pacific_flyway",    "mississippi_flyway", "distant",
  "pacific_flyway",    "atlantic_flyway",    "distant",
  "central_flyway",    "atlantic_flyway",    "distant",
  "mississippi_flyway","pacific_flyway",     "distant",
  "atlantic_flyway",   "central_flyway",     "distant",
  "atlantic_flyway",   "pacific_flyway",     "distant"
)

df_adj <- df %>%
  left_join(adjacency_lookup, by = c("from", "to"))

jumps_by_proximity <- df_adj %>%
  filter(proximity %in% c("adjacent", "distant")) %>%
  group_by(state, proximity) %>%
  summarise(num_jumps = n(), .groups = "drop")

summary_proximity <- jumps_by_proximity %>%
  group_by(proximity) %>%
  summarise(
    mean_jumps = mean(num_jumps),
    lower_hpd = HPDinterval(as.mcmc(num_jumps))[1],
    upper_hpd = HPDinterval(as.mcmc(num_jumps))[2],
    .groups = "drop"
  )

summary_proximity

# visualization of density plots
palette <- c('#7E4794', '#9d2c00', '#0b81a2', '#59a89c')
proximity_colors <- c("adjacent" = palette[1], "distant" = palette[4])

# Ensure proximity is a factor
jumps_by_proximity <- jumps_by_proximity %>%
  mutate(proximity = factor(proximity))

# Add rounded-up mean labels
summary_proximity <- summary_proximity %>%
  mutate(label = paste0("Mean = ", ceiling(mean_jumps)),
         y_pos = 0.07)  # Adjust if needed

# Plot
ggplot(jumps_by_proximity, aes(x = num_jumps, fill = proximity, color = proximity)) +
  geom_density(alpha = 0.4) +
  geom_vline(data = summary_proximity, 
             aes(xintercept = mean_jumps, color = proximity), 
             linetype = "dashed", size = 1) +
  geom_text(data = summary_proximity, 
            aes(x = mean_jumps, y = y_pos, label = label, color = proximity),
            hjust = -0.1, size = 6) +
  scale_color_manual(values = proximity_colors) +
  scale_fill_manual(values = proximity_colors) +
  scale_x_continuous(breaks = seq(0, 300, by = 50),limits = c(0,300)) + 
  ylim(0,0.085) +
  labs(x = "Number of Jumps", y = "Density", 
       title = "Distribution of Jumps by Proximity",
       color = "Proximity", fill = "Proximity") +
  theme_minimal(base_size = 14) +
  theme(text = element_text(size = 18))



