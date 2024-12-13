library(ggplot2)
library(dplyr)
setwd("~/")


# input: results of BSSVS, dataframe with the combined results of discrete trait diffusion rates, summarized using RateSummary_ActualRates.py, and the Bayes factor support calcualted using SPREAD3
# create matrix plot with asterisks for bayes factor support
# Function to add asterisk if value > 1000 (make the 

add_asterisk <- function(value) {
  if (is.na(value)) {
    return("")
  } else if (value >= 3 & value <= 10) {
    return("")
  } else if (value > 10 & value <= 30) {
    return("*")
  } else if (value > 30 & value <= 100) {
    return("**")
  } else if (value > 100) {
    return("***")
  } else {
    return("")
  }
}

bf_range_upper <- function(value) {
  if (is.na(value)) {
    return("0")
  } else if (value >= 3 & value <= 10) {
    return("3")
  } else if (value > 10 & value <= 30) {
    return("10")
  } else if (value > 30 & value <= 100) {
    return("30")
  } else if (value > 100) {
    return("100")
  } else {
    return("0")
  }
}

bf_range_cont <- function(value) {
  if (is.na(value)) {
    return("0")
  } else if (value >= 3 & value <= 10) {
    return("3")
  } else if (value > 10 & value <= 30) {
    return("10")
  } else {
    return("0")
  }
}


df_flyway <- read.csv("flyway/BF-flyway.txt", sep = '\t')
df_flyway <- df_flyway %>%
  mutate(mean = ifelse(is.na(BAYES_FACTOR), NA, mean))

df_flyway$mean[df_flyway$POSTERIOR.PROBABILITY < 0.5] <- NA
df_flyway$FROM <- gsub("_flyway","",df_flyway$FROM)
df_flyway$TO <- gsub("_flyway","",df_flyway$TO)

p1 <- ggplot(df_flyway, aes(x =TO, y=FROM, fill= mean)) + 
  geom_tile()+
  theme_classic()+
  xlab("Sink") +
  ylab("Source")+
  labs(title= "Mean transition rate - Flyway",fill = "Mean transition rate
(transitions from A to B /year)" ) +
  scale_fill_gradient(low = ("lightyellow"), high = ("darkgreen"), na.value = "white") + 
  geom_text(aes(label = sapply(BAYES_FACTOR, add_asterisk)), size = 12) +
  theme(axis.title.y = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 15, face = "bold"), legend.position = "bottom")


p1

#####################
# BAR PLOTS
## started with smaller for flyway
df_flyway <- read.csv(".F-flyway.txt", sep = '\t')

df_flyway$FROM <- gsub("_flyway","",df_flyway$FROM)
df_flyway$TO <- gsub("_flyway","",df_flyway$TO)
df_flyway$from_to_combined <- paste(df_flyway$FROM, "  ", df_flyway$TO)
df_flyway <- df_flyway[order(df_flyway$mean),]
df_flyway$aster <- as.numeric(sapply(df_flyway$BAYES_FACTOR, bf_range_upper))
df_flyway$from_to_combined <- gsub("_flyway","",df_flyway$from_to_combined)
  
# Create a vertical bar chart
ggplot(df_flyway, aes(x = mean, y = factor(df_flyway$from_to_combined, levels = unique(df_flyway$from_to_combined)), fill = aster)) +
  geom_bar(stat = "identity", color = "grey") +
  scale_fill_gradient(low = "white", high = "forestgreen",breaks = c(0, 3, 10, 30, 100, Inf), labels = c("< 3", "3 - 10", "10 - 30", "30 - 100", ">100", ">100")) +
  theme_minimal() +
  labs(x = "mean transition rate (transitions from A to B /year)",
       y = " Source -> Sink",
       fill = "Bayes Factor",
       title = "BSSVS transition rates - Flyway - Equal Order") +
  theme(axis.title.y = element_blank(),axis.text.x = element_text(size = 14),
        legend.position = "bottom") +
  guides(fill = guide_legend(title = "Bayes Factor", reverse = TRUE))   # Facet grid for FROM_TO_combination

from_colorold <- c("atlantic" = '#7E4794', "central" = '#9d2c00', "mississippi" = '#0b81a2', "pacific" = '#59a89c')
from_colors <- c("atlantic" = '#118AB2', "central" = '#FFD166', "mississippi" = '#06D6A0', "pacific" = '#EF476F')

# Create a vertical bar chart
ggplot(df_flyway, aes(x = mean, y = factor(from_to_combined, levels = unique(from_to_combined)), fill = FROM, alpha = aster)) +
  geom_bar(stat = "identity", color = "grey") +
  scale_fill_manual(values = from_colors) +  # Use manual color scale
  scale_alpha_continuous(range = c(0, 1), guide = "legend") +  # Set alpha range from 0 to 1
  theme_minimal() +
  labs(x = "mean transition rate (transitions from A to B /year)",
       y = " Source -> Sink",
       title = "USFWS Migratory Flyways") +
  theme(axis.title.y = element_blank(),legend.position = "none", text = element_text(size = 18), panel.grid.minor = element_line(colour=NA,size=NA),
        panel.grid.major = element_line(colour=NA,size=NA),
        axis.line.x=element_line(colour="black"),
        axis.line.y=element_line(colour="black"),
        axis.text = element_text(colour = "black")) 


ggsave("flyway_stacked.pdf", width = 8, height = 10, units = "in")
##########
#v2 poster
df_flyway$from_to_combined <- gsub("mississippi","MIS",df_flyway$from_to_combined)
df_flyway$from_to_combined <- gsub("atlantic","ATL",df_flyway$from_to_combined)
df_flyway$from_to_combined <- gsub("pacific","PAC",df_flyway$from_to_combined)
df_flyway$from_to_combined <- gsub("central","CEN",df_flyway$from_to_combined)
df_flyway$from_to_combined <- gsub("->"," ",df_flyway$from_to_combined)
   

ggplot(df_flyway, aes(x = mean, y = factor(df_flyway$from_to_combined, levels = unique(df_flyway$from_to_combined)), fill = aster)) +
  geom_bar(stat = "identity", color = "grey") +
  scale_fill_gradient(low = "white", high = "forestgreen",breaks = c(0, 3, 10, 30, 100, Inf), labels = c("< 3", "3 - 10", "10 - 30", "30 - 100", ">100", ">100")) +
  theme_minimal() +
  labs(x = "mean transition rate (transitions from A to B /year)",
       y = " Source -> Sink",
       fill = "Bayes Factor",
       title = "BSSVS transition rates - Flyway - Equal Order") +
  theme(axis.title.y = element_blank(),axis.text.x = element_text(size = 14),
        legend.position = "bottom") +
  guides(fill = guide_legend(title = "Bayes Factor", reverse = TRUE))   # Facet grid for FROM_TO_combination
#########


# Host order 
df_order <- read.csv("order/bf-order.txt", sep = '\t')

df_order$from_to_combined <- paste(df_order$FROM, "  ", df_order$TO)
df_order <- df_order[order(df_order$mean),]
df_order$aster <- as.numeric(sapply(df_order$BAYES_FACTOR, bf_range_upper))
df_order$from_to_combined <- gsub("_order","",df_order$from_to_combined)


# Create a vertical bar chart
ggplot(df_order, aes(x = mean, y = factor(df_order$from_to_combined, levels = unique(df_order$from_to_combined)), fill = aster)) +
  geom_bar(stat = "identity", color = "grey") +
  scale_fill_gradient(low = "white", high = "forestgreen",breaks = c(0, 3, 10, 30, 100, Inf), labels = c("< 3", "3 - 10", "10 - 30", "30 - 100", ">100", ">100")) +
  theme_minimal() +
  labs(x = "mean transition rate (transitions from A to B /year)",
       y = " Source -> Sink",
       fill = "Bayes Factor",
       title = "BSSVS transition rates - order - Equal Order") +
  theme(axis.title.y = element_blank(),legend.position = "none", text = element_text(size = 18), panel.grid.minor = element_line(colour=NA,size=NA),
        panel.grid.major = element_line(colour=NA,size=NA),
        axis.line.x=element_line(colour="black"),
        axis.line.y=element_line(colour="black"),
        axis.text = element_text(colour = "black"),
        legend.title = element_blank()) +
  guides(fill = guide_legend(title = "Bayes Factor", reverse = TRUE))   # Facet grid for FROM_TO_combination



from_colors <- c("galliformes"="#CEB540" ,
                 "anseriformes"="#274257" ,
                 "charadriiformes"="#4E83AE" ,
                 "accipitriformes"="#228B22" ,
                 "nonhuman-mammal"="#B33226" ,
                 "passeriformes"="#556B2F" ,
                 "strigiformes"="#FF8C66")

# Create a vertical bar chart
ggplot(df_order, aes(x = mean, y = factor(from_to_combined, levels = unique(from_to_combined)), fill = FROM, alpha = aster)) +
  geom_bar(stat = "identity", color = "grey") +
  scale_fill_manual(values = from_colors) +  # Use manual color scale
  scale_alpha_continuous(range = c(0, 1), guide = FALSE) +  # Set alpha range from 0 to 1
  theme_minimal() +
  labs(x = "mean transition rate\n(transitions from A to B /year)",
       y = " Source -> Sink",
       fill = "Bayes Factor",
       title = "Taxonomic Order") + xlim(0,4) +
  theme(axis.title.y = element_blank(),legend.position = "none", text = element_text(size = 18), panel.grid.minor = element_line(colour=NA,size=NA),
        panel.grid.major = element_line(colour=NA,size=NA),
        axis.line.x=element_line(colour="black"),
        axis.line.y=element_line(colour="black"),
        axis.text = element_text(colour = "black"),
        legend.title = element_blank()) +
  guides(fill = guide_legend(title = "Bayes Factor", reverse = TRUE))   # Facet grid for FROM_TO_combination

ggsave("orders_stacked2.pdf", width = 8, height = 10, units = "in")

#########
# v2

df_order$from_to_combined <- gsub("anseriformes","ans",df_order$from_to_combined)
df_order$from_to_combined <- gsub("galliformes","gal",df_order$from_to_combined)
df_order$from_to_combined <- gsub("charadriiformes","chr",df_order$from_to_combined)
df_order$from_to_combined <- gsub("passeriformes","pas",df_order$from_to_combined)
df_order$from_to_combined <- gsub("nonhuman-mammal","mam",df_order$from_to_combined)
df_order$from_to_combined <- gsub("accipitriformes","acp",df_order$from_to_combined)
df_order$from_to_combined <- gsub("strigiformes","str",df_order$from_to_combined)
df_order$from_to_combined <- gsub("->","  ",df_order$from_to_combined)

ggplot(df_order, aes(x = mean, y = factor(df_order$from_to_combined, levels = unique(df_order$from_to_combined)), fill = aster)) +
  geom_bar(stat = "identity", color = "grey") +
  scale_fill_gradient(low = "white", high = "forestgreen",breaks = c(0, 3, 10, 30, 100, Inf), labels = c("< 3", "3 - 10", "10 - 30", "30 - 100", ">100", ">100")) +
  theme_minimal() +
  labs(x = "mean transition rate (transitions from A to B /year)",
       y = " Source -> Sink",
       fill = "Bayes Factor",
       title = "BSSVS transition rates - order - Equal Order") +
  theme(axis.title.y = element_blank(),axis.text.x = element_text(size = 14),
        legend.position = "bottom") +
  guides(fill = guide_legend(title = "Bayes Factor", reverse = TRUE))   # Facet grid for FROM_TO_combination
############

from_colors <- c("atlantic" = '#7E4794', "central" = '#9d2c00', "mississippi" = '#0b81a2', "pacific" = '#59a89c')



# Create a vertical bar chart
ggplot(df_flyway, aes(x = mean, y = factor(from_to_combined, levels = unique(from_to_combined)), fill = FROM, alpha = aster)) +
  geom_bar(stat = "identity", color = "grey") +
  scale_fill_manual(values = from_colors) +  # Use manual color scale
  scale_alpha_continuous(limits = c(0,100), breaks = c(0,3,10,30,100)) +  # Set alpha range from 0 to 1
  theme_minimal() +
  labs(x = "mean transition rate (transitions from A to B /year)",
       y = " Source -> Sink",
       fill = "Bayes Factor",
       title = "USFWS Flyway") +
  theme(axis.title.y = element_blank(),legend.position = "bottom", text = element_text(size = 18), panel.grid.minor = element_line(colour=NA,size=NA),
        panel.grid.major = element_line(colour=NA,size=NA),
        axis.line.x=element_line(colour="black"),
        axis.line.y=element_line(colour="black"),
        axis.text = element_text(colour = "black"),
        legend.title = element_blank())

ggsave("migrations_stacked.pdf", width = 8, height = 10, units = "in")



####
# migration 
df_migration <- read.csv("../../order_eq/comb/migration/bf-migration.txt", sep = '\t')

df_migration$from_to_combined <- paste(df_migration$FROM, "->", df_migration$TO)
df_migration <- df_migration[order(df_migration$mean),]
df_migration$aster <- as.numeric(sapply(df_migration$BAYES_FACTOR, bf_range_upper))
df_migration$from_to_combined <- gsub("_migration","",df_migration$from_to_combined)
df_migration$from_to_combined <- gsub("three","Migratory",df_migration$from_to_combined)
df_migration$from_to_combined <- gsub("two","Part. Migratory",df_migration$from_to_combined)
df_migration$from_to_combined <- gsub("one","Sedentary",df_migration$from_to_combined)
df_migration$from_to_combined <- gsub("nonhuman-mammal","Mammal",df_migration$from_to_combined)
df_migration$from_to_combined <- gsub("domestic","Domestic",df_migration$from_to_combined)
df_migration$from_to_combined <- gsub("->","  ",df_migration$from_to_combined)


# Create a vertical bar chart
ggplot(df_migration, aes(x = mean, y = factor(df_migration$from_to_combined, levels = unique(df_migration$from_to_combined)), fill = aster)) +
  geom_bar(stat = "identity", color = "grey50") +
  scale_fill_gradient(low = "white", high = "grey30",breaks = c(0, 3, 30, 100, Inf)) +
  theme_minimal() +
  labs(x = "mean transition rate (transitions from A to B /year)",
       y = " Source -> Sink",
       fill = "Bayes Factor",
       title = "Migration Behavior") +
  theme(axis.title.y = element_blank(),legend.position = "bottom", text = element_text(size = 18), panel.grid.minor = element_line(colour=NA,size=NA),
        panel.grid.major = element_line(colour=NA,size=NA),
        axis.line.x=element_line(colour="black"),
        axis.line.y=element_line(colour="black"),
        axis.text = element_text(colour = "black"))  # Facet grid for FROM_TO_combination

ggsave("migration_stacked.pdf", width = 10, height = 8, units = "in")


