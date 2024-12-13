library(ggplot2)
# example plotting script for results of PACT (persistence)
# from the results of running pact command: summary persistence

setwd("")
df <- read.csv("persistance_result", sep = '\t')
df$statistic <- factor(df$statistic, levels = rev(sort(unique(df$statistic))))


# Host orders
ggplot(df, aes(x = mean, y = statistic, color = statistic)) +
  geom_point(size = 5) + 
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.1) + 
  labs(title = "Persistence", x = "Persistence (years)", y = "Order") +
  theme_minimal() + 
  theme(text = element_text(size = 18)) +
  scale_color_manual(values = c("accipitriformes" = "#228B22", "anseriformes" = "#274257", "charadriiformes" = "#4E83AE", "galliformes" = "#CEB540", "nonhuman-mammal" = "#B33226","passeriformes" = "#556B2F", "strigiformes" = "#FF8C66"))

