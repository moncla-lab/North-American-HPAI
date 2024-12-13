library(ggplot2)
library(dplyr)
library(ggtree)
require(treeio)
library(ggplot2)
library(dplyr)
library(tidytree)
library(reshape2)
library(tidyr)
library(ggnewscale)
library(RColorBrewer)
library(MoMAColors)
library(zoo)
library(lubridate)
library(coda)


# This script contains code to visualize the following: 
# 1. cumulative markov jumps plot over time
# 2. markov rewards proportion plot

# This script includes code to calculate the lag time between different jumps 
# as well as code to calculate the mean, median, and HPD for each markov markov jump transition pair


setwd("~/Documents/HPAI/HPAI_2344b/DTA-BEAST/RF/results/RF-wild2-mkj/results/")
df <- read.csv("jumptimes.txt", sep = '\t')


# get the real date by entering the youngest seqeunce in decimal date here
df$date <-  2022.4931506849316 - df$time


combo_counts <- df %>%
  group_by(state, from, to) %>%
  summarise(count = n())

# Create a new dataframe with the total count for each combination of 'from' and 'to'
total_counts <- combo_counts %>%
  group_by(from, to) %>%
  summarise(total_count = sum(count))

################

# line plots of cumulative markov jumps over time for each posterior sample 
# Create time intervals
df %>%
  filter(time >= 0.002774961 & time <= 0.9898274) %>%
  mutate(interval = cut(time, breaks = seq(0.002774961, 0.9898274, length.out = 1000))) %>%
  group_by(state, from, to, interval) %>%
  summarise(count = n()) %>%
  ungroup() -> df_count

df_count$upper_range <- as.numeric(gsub('.*,(\\d+\\.\\d+).*', '\\1', df_count$interval))
df_count$upper_range <-  2022.4931506849316 - df_count$upper_range


df_count %>%
  ungroup() %>%
  arrange(state, from, to, upper_range) %>%
  group_by(state, from, to) %>%
  mutate(cumulative_count = cumsum(count)) %>%
  ungroup() -> df_count2

df_count2 <- df_count2 %>%
  filter(!(from == "Wild" & upper_range > 2022.38))


mkj <- ggplot(df_count2, aes(x = upper_range, y = cumulative_count, group = interaction(state,interaction(from,to)) , color = interaction(from,to))) +
  geom_line(position = "jitter", alpha = 0.2) +
  labs(
       x = "Date",
       y = "Cumulative Markov Jumps") +
  scale_x_continuous(limits = c(2022.0,2022.51), 
                     breaks= c( 2022.01, 2022.0833,2022.1639,2022.2466, 2022.3288,2022.4146,2022.4967),  
                     labels = c("Jan\n2022","Feb\n2022","Mar\n2022","Apr\n2022","May\n2022","Jun\n2022","Jul\n2022"),
                     expand = c(0, 0)) + 
  scale_y_continuous(breaks= c(0,20,40,60)) +
  theme_minimal() +
  theme(text = element_text(size = 24), panel.grid.minor = element_line(colour=NA,size=NA),
        panel.grid.major = element_line(colour=NA,size=NA),
        axis.line.x=element_line(colour="black"),
        axis.line.y=element_line(colour="black"),
        axis.text = element_text(colour = "black"), axis.ticks = element_line(size = 1),
        legend.title = element_blank(), legend.text = element_text(size = 12), legend.position = "bottom") +
  scale_color_manual(values = c("grey50","#FFC300","#082a54","#59a89c","#e02b35","#a559aa")) +
  guides(colour = guide_legend(override.aes = list(size=3)))
        #+ scale_color_manual(values = c("#008080","#581845","#900C3F","#C70039","#FF5733","#FFC300"))

mkj

# Calculate lag between wild to backyard bird and wild to commercial
filtered_df_count2 <- df_count2[df_count2$from %in% c('Wild'), ]
filtered_df_count2 <- filtered_df_count2 %>%
  filter(!(from == "Wild" & upper_range > 2022.36)) %>%
  filter(!(upper_range < 2022.0))

filtered_df_count2$interaction <- paste(filtered_df_count2$from, filtered_df_count2$to, sep = "-")
intervals <- seq(min(filtered_df_count2$upper_range), max(filtered_df_count2$upper_range), by = 0.00273975) # by day
filtered_df_count2$intervals <- cut(filtered_df_count2$upper_range, breaks = intervals, include.lowest = TRUE)

filtered_df_count2$interval_upper <- as.numeric(gsub('.*,(\\d+\\.\\d+).*', '\\1', filtered_df_count2$intervals))


# Next, group by states, interaction, and interval, then calculate the average of cumulative_count
filtered_df_count2_cumsum <- filtered_df_count2 %>%
  group_by(state, interaction, interval_upper) %>%
  summarise(average_cumulative_count = mean(cumulative_count, na.rm = TRUE))

filtered_df_count2_cumsum$interval_upper <- as.character(filtered_df_count2_cumsum$interval_upper)
average_filtered_df_count2_cumsum <- filtered_df_count2_cumsum %>%
  group_by(interaction, interval_upper) %>%
  summarize(average_cumulative_count = mean(average_cumulative_count))

average_filtered_df_count2_cumsum <- na.omit(average_filtered_df_count2_cumsum)
average_filtered_df_count2_cumsum

backyard <- average_filtered_df_count2_cumsum[average_filtered_df_count2_cumsum$interaction == "Wild-Backyard_bird", ]
domestic <- average_filtered_df_count2_cumsum[average_filtered_df_count2_cumsum$interaction == "Wild-Domestic", ]



# Find where values are equal
backyard$interval_upper2 <- as.Date(date_decimal(as.numeric(backyard$interval_upper)))
domestic$interval_upper2 <- as.Date(date_decimal(as.numeric(domestic$interval_upper)))
merged <- merge(backyard, domestic, by = "interval_upper2")


correlation <- ccf(merged$average_cumulative_count.x, merged$average_cumulative_count.y, lag.max=10)
plot(correlation, main = "Cross-correlation Plot", xlab = "Lag", ylab = "Correlation")
print(correlation)

merged$average_cumulative_count.x <- round(merged$average_cumulative_count.x)
merged$average_cumulative_count.y <- round(merged$average_cumulative_count.y)
lag_time <- numeric()

# Loop through the rows to calculate the lag time between value1 and value2
for (i in 1:(nrow(merged) - 1)) {
  # Find the index where value2 is equal to or greater than value1
  idx <- which(merged$average_cumulative_count.y[i + 1:nrow(merged)] >= merged$average_cumulative_count.x[i])[1]
  # Calculate the time difference between the corresponding rows
  if (!is.na(idx)) {
    lag_time[i] <- difftime(merged$interval_upper2[i + idx], merged$interval_upper2[i], units = "days")
  } else {
    lag_time[i] <- NA  # If value2 never exceeds value1, set NA
  }
}

# Compute average lag time
average_lag_time <- mean(lag_time, na.rm = TRUE)
average_lag_time



##############
# Markov Rewards visualization
# use the MCC tree with the markov rewards annotated to the branches

beast_tree <- read.beast("RF-wild-1_1_100_MKJ.domwildbyb.history.MCC.tree")
data<-as_tibble(beast_tree)
## filter data
df<- data %>% select(ends_with('reward'),height)

# input date of earliest seq
df <- df %>% mutate(year = 2022.4931506849316 - as.numeric(height))
## get the propotrion of rewards
df = as.data.frame(sapply(df, as.numeric))


test<-aggregate(cbind(Backyard_bird_reward2=df$Backyard_bird_reward,
                      Domestic_reward2=df$Domestic_reward,
                      Wild_reward2=df$Wild_reward)
                ,by=list(year2=df$year), FUN=sum)
test<-test%>% mutate(sum = rowSums(.[1:3]))
test<-test %>% mutate(Backyard_bird_reward_p = Backyard_bird_reward2/sum)
test<-test %>% mutate(Domestic_reward_p = Domestic_reward2/sum)
test<-test %>% mutate(Wild_reward_p = Wild_reward2/sum)



test<- test%>% filter(test$sum!=0)
test2 <- test %>% mutate_all(funs(replace_na(.,0)))
test2 <-test2%>% select(ends_with('reward_p'),year2)
test3<- test2 %>% gather(location,value,Backyard_bird_reward_p,Domestic_reward_p,Wild_reward_p)
write.csv(test3,"test3.csv")
test4 <- read.csv("test3.csv")
test4 <-  as.data.frame(test4)

interval_width <- 0.019178 # one week 
test4$interval <- cut(test4$year2, breaks = seq(min(test4$year2), max(test4$year2) + interval_width, by = interval_width))

# Aggregate values within intervals for each location
result <- aggregate(value ~ location + interval, data = test4, FUN = mean)
result$interval_upper <- as.numeric(gsub('.*,(\\d+\\.\\d+).*', '\\1', result$interval))
result$interval_upper <- as.numeric(result$interval_upper)

result

colors <- c("#D1BA56","#5CA7A4","#2664A5")


# stacked area chart
wildrf_rewards<-
  ggplot(result, aes(x=interval_upper, y=value, fill=location)) + geom_area()+
  xlab("Year")+
  ylab("Trunk Reward Propotion")+
  labs(fill = "Location", title = "Trunk Reward proportion") +
  scale_x_continuous(limits = c(2022.0,2022.51), 
                     breaks= c( 2022.01, 2022.0833,2022.1639,2022.2466, 2022.3288,2022.4146,2022.4967),  
                     labels = c("Jan\n2022","Feb\n2022","Mar\n2022","Apr\n2022","May\n2022","Jun\n2022","Jul\n2022"),
                     expand = c(0, 0)) +
  theme_minimal() +
  theme(legend.position = "bottom", text = element_text(size = 15), panel.grid.minor = element_line(colour=NA,size=NA),
        panel.grid.major = element_line(colour=NA,size=NA),
        axis.line.x=element_line(colour="black"),
        axis.line.y=element_line(colour="black"),
        axis.text = element_text(colour = "black"),
        legend.title = element_blank(), axis.ticks = element_line()) +
  scale_fill_manual(values = colors) 

wildrf_rewards



library(cowplot)
cowplot::plot_grid(wildrf_rewards,mkj, align = "v", ncol = 1, axis = "b", labels = "AUTO")

#### 
# calculate the mean, median and 9% HPD for the markov jumps between groups using 
# the jump history file which is summarized from the complete jump history log file 
# using the collect_times perl script provided on the BEAST community website 

setwd("~")
df <- read.csv("jumphistory.txt", sep = '\t')

# get the real date by entering the youngest sequence in decimal date here
df$date <-  2022.4931506849316 - df$time

df$from_to <- paste(df$from, df$to, sep = "-")
df_state_cumul <- df %>%
  filter(date > 2022) %>% # Filter rows where time > 2022
  group_by(state, from_to) %>% # Group by state and from_to
  summarise(cumulative_count = n(), .groups = "drop") # Count occurrences

print(df_state_cumul)

hpd_results <- df_state_cumul %>%
  group_by(from_to) %>% # Group by from_to
  summarise(
    median_count = median(cumulative_count), 
    mean_count = mean(cumulative_count),
    lower_hpd = HPDinterval(as.mcmc(cumulative_count))[1], # Lower HPD bound
    upper_hpd = HPDinterval(as.mcmc(cumulative_count))[2]  # Upper HPD bound
  )

print(hpd_results)

