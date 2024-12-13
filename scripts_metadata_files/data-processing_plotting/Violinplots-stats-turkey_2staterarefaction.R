library(ggplot2)
library(dplyr)
library(tidyr)
library(MoMAColors)
library(reshape2)
library(stringr)

########
# Read in logfiles and get the tmcra and rates and append with the run name (Change for whatever you want to vis)
# merge the dataframes horizontally after selection

setwd("~/Documents/HPAI/HPAI_2344b/DTA-BEAST/RF/results/RF-dta_2stateturkey/bybincturkey/")
#BYBIncluded

BYBI1_1 <- read.csv("1_1/RF_Wbybturk_1_1.comb.rates.log", sep = '\t')
BYBI1_1 <- BYBI1_1 %>% select(domwild.rates.domestic.wild,domwild.rates.wild.domestic) %>% rename_with(~ paste0(., "BYB_turk_1_1"), everything()) %>%   mutate(Index = row_number())

BYBI1_1_5 <- read.csv("1_1_5/RF_Wbybturk_1_1_5.comb.rates.log", sep = '\t')
BYBI1_1_5 <- BYBI1_1_5 %>% select(domwild.rates.domestic.wild,domwild.rates.wild.domestic) %>% rename_with(~ paste0(., "BYB_turk_1_1_5"), everything()) %>%   mutate(Index = row_number())

BYBI1_2 <- read.csv("1_2/RF_Wbybturk_1_2.comb.rates.log", sep = '\t')
BYBI1_2<- BYBI1_2 %>% select(domwild.rates.domestic.wild,domwild.rates.wild.domestic) %>% rename_with(~ paste0(., "BYB_turk_1_2"), everything()) %>%   mutate(Index = row_number())


BYBImerged <- BYBI1_1 %>%
  left_join(BYBI1_1_5, by = "Index") %>%
  left_join(BYBI1_2, by = "Index") 



#rates
dom_to_wild_cols <-BYBImerged %>% select(contains("domestic.wild"))
wild_to_dom_cols <-BYBImerged %>% select(contains("wild.domestic"))


rates <- BYBImerged %>% select(contains("rates."))

  
################
# Violin plots with mean and 2 std deviations (95%) point range


#############

#############
# Rates domestic to wild

dom_to_wild_cols2 <- dom_to_wild_cols %>%
  gather(column_name, value)

dom_to_wild_cols2$fill_color <- ifelse(grepl("BYBE", dom_to_wild_cols2$column_name), "BYBE", "BYBI")
dom_to_wild_cols2$column_name <- str_remove_all(dom_to_wild_cols2$column_name, "domwild\\.rates\\.Domestic\\.Wild")

# Create the violin plot
ggplot(dom_to_wild_cols2, aes(x = column_name, y = value)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.75), outlier.shape = NA) +
  labs(title = "Transition rate Domestic to Wild for H5Nx Rarefaction analysis",
       x = "Analysis",
       y = "Transition Rate") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 14)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#############
# Rates wild to domestic

wild_to_dom_cols2 <- wild_to_dom_cols %>%
  gather(column_name, value)

wild_to_dom_cols2$fill_color <- ifelse(grepl("BYBE", wild_to_dom_cols2$column_name), "BYBE", "BYBI")
wild_to_dom_cols2$column_name <- str_remove_all(wild_to_dom_cols2$column_name, "domwild\\.rates\\.Wild\\.Domestic")

# Create the violin plot
ggplot(wild_to_dom_cols2, aes(x = column_name, y = value)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.75), outlier.shape = NA) +
  labs(title = "Transition rate Wild to Domestic for H5Nx Rarefaction analysis",
       x = "Analysis",
       y = "Transition Rate") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 14)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#################
# all rates bybE

allbybErates2 <- allbybErates %>%
  gather(column_name, value)

allbybErates2$fill_color <- ifelse(grepl("Domestic.Wild", allbybErates2$column_name), "Domestic.Wild", "Wild.Domestic")
allbybErates2$column_name <- str_remove_all(allbybErates2$column_name, "domwild\\.rates\\.")
allbybErates2$column_name <- str_remove_all(allbybErates2$column_name, "Domestic.Wild")
allbybErates2$column_name <- str_remove_all(allbybErates2$column_name, "Wild.Domestic")
allbybErates2$column_name <- str_remove_all(allbybErates2$column_name, "BYBE_")

allbybErates2 <- allbybErates2 %>%
  mutate(column_name = case_when(
    fill_color == "Domestic.Wild" ~ paste(column_name, "D.W", sep = "_"),
    fill_color == "Wild.Domestic" ~ paste(column_name, "W.D", sep = "_")
  ))




allbybErates2 <- na.omit(allbybErates2)
allbybErates2$column_name <- factor(allbybErates2$column_name, levels = c("1_1_D.W", "1_1_W.D", "1_1_5_D.W",
                                                                          "1_1_5_W.D","1_2_D.W","1_2_W.D",
                                                                          "1_2_5_D.W", "1_2_5_W.D","1_3_D.W","1_3_W.D"))

# Create the violin plot
ggplot(allbybErates2, aes(x = column_name, y = value, fill = fill_color)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.75), outlier.shape = NA) +
  labs(title = "Transition rates for H5Nx Rarefaction analysis (BYB excluded)",
       x = "Analysis",
       y = "Transition Rate") +
  scale_fill_manual(values = moma.colors("Koons")) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 14)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# all rates BYBI
allbybIrates2 <- BYBImerged %>%
  gather(column_name, value)

allbybIrates2 <- allbybIrates2[allbybIrates2$column_name != "Index", ]

allbybIrates2$fill_color <- ifelse(grepl("domestic.wild", allbybIrates2$column_name), "domestic.wild", "wild.domestic")
allbybIrates2$column_name <- str_remove_all(allbybIrates2$column_name, "domwild\\.rates\\.")
allbybIrates2$column_name <- str_remove_all(allbybIrates2$column_name, "domestic.wild")
allbybIrates2$column_name <- str_remove_all(allbybIrates2$column_name, "wild.domestic")
allbybIrates2$column_name <- str_remove_all(allbybIrates2$column_name, "BYB_turk_")

allbybIrates2 <- allbybIrates2 %>%
  mutate(column_name = case_when(
    fill_color == "domestic.wild" ~ paste(column_name, "D.W", sep = "_"),
    fill_color == "wild.domestic" ~ paste(column_name, "W.D", sep = "_")
  ))


 

allbybIrates2 <- na.omit(allbybIrates2)
allbybIrates2$column_name <- factor(allbybIrates2$column_name, levels = c("1_1_D.W", "1_1_W.D", "1_1_5_D.W",
                                                                          "1_1_5_W.D","1_2_D.W"))


allbybIrates2$column_name[is.na(allbybIrates2$column_name)] <- "1_2_W.D"



# Create the violin plot
ggplot(allbybIrates2, aes(x = column_name, y = value, fill = fill_color)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.75), outlier.shape = NA) +
  labs(title = "Domestic/Wild Rarefaction",
       x = "Analysis",
       y = "Transition Rate") +
  scale_fill_manual(values = c("#A52D2F","#A56626")) +
  theme_minimal() +
  theme(legend.position = "bottom", text = element_text(size = 18), panel.grid.minor = element_line(colour=NA,size=NA),
        panel.grid.major = element_line(colour=NA,size=NA),
        axis.line.x=element_line(colour="black"),
        axis.line.y=element_line(colour="black"),
        axis.text = element_text(colour = "black")) 


