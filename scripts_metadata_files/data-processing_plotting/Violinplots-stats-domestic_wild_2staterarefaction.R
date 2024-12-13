library(ggplot2)
library(dplyr)
library(tidyr)
library(MoMAColors)
library(reshape2)
library(stringr)

setwd("~/")
# LD 2023-11
# Violin plots and summary stats from log files BEAST for 2 state domestic wild analysis

########
# Read in logfiles and get the tmcra and rates and append with the run name (Change for whatever you want to vis)
# merge the dataframes horizontally after selection

#BYBExcluded

BYBE1_1 <- read.csv("bybexcluded/1_1/RF_bybE_1_1.comb.log", sep = '\t')
BYBE1_1 <- BYBE1_1 %>% select(age.root.,domwild.rates.Domestic.Wild,domwild.rates.Wild.Domestic) %>% rename_with(~ paste0(., "BYBE_1_1"), everything()) %>%   mutate(Index = row_number())

BYBE1_1_5 <- read.csv("bybexcluded/1_1_5/RF_bybE_1_1_5.comb.log", sep = '\t')
BYBE1_1_5 <- BYBE1_1_5 %>% select(age.root.,domwild.rates.Domestic.Wild,domwild.rates.Wild.Domestic) %>% rename_with(~ paste0(., "BYBE_1_1_5"), everything()) %>%   mutate(Index = row_number())

BYBE1_2 <- read.csv("bybexcluded/1_2/RF_bybE_1_2.comb.log", sep = '\t')
BYBE1_2 <- BYBE1_2 %>% select(age.root.,domwild.rates.Domestic.Wild,domwild.rates.Wild.Domestic) %>% rename_with(~ paste0(., "BYBE_1_2"), everything()) %>%   mutate(Index = row_number())

BYBE1_2_5 <- read.csv("bybexcluded/1_2_5/RF_bybE_1_2_5.comb.log", sep = '\t')
BYBE1_2_5 <- BYBE1_2_5 %>% select(age.root.,domwild.rates.Domestic.Wild,domwild.rates.Wild.Domestic) %>% rename_with(~ paste0(., "BYBE_1_2_5"), everything()) %>%   mutate(Index = row_number())

BYBE1_3 <- read.csv("bybexcluded/1_3/RF_bybE_1_3.comb.log", sep = '\t')
BYBE1_3 <- BYBE1_3 %>% select(age.root.,domwild.rates.Domestic.Wild,domwild.rates.Wild.Domestic) %>% rename_with(~ paste0(., "BYBE_1_3"), everything()) %>%   mutate(Index = row_number())

# merge (put the largest log first)
BYBEmerged <- BYBE1_3 %>%
  left_join(BYBE1_1_5, by = "Index") %>%
  left_join(BYBE1_2, by = "Index") %>%
  left_join(BYBE1_2_5, by = "Index") %>%
  left_join(BYBE1_1, by = "Index")

#BYBIncluded

BYBI1_1 <- read.csv("bybincluded/1_1/RF_Wbyb_1_1.comb.log", sep = '\t')
BYBI1_1 <- BYBI1_1 %>% select(age.root.,domwild.rates.Domestic.Wild,domwild.rates.Wild.Domestic) %>% rename_with(~ paste0(., "BYBI_1_1"), everything()) %>%   mutate(Index = row_number())

BYBI1_1_5 <- read.csv("bybincluded/1_1_5/RF_Wbyb_1_1_5.comb.log", sep = '\t')
BYBI1_1_5 <- BYBI1_1_5 %>% select(age.root.,domwild.rates.Domestic.Wild,domwild.rates.Wild.Domestic) %>% rename_with(~ paste0(., "BYBI_1_1_5"), everything()) %>%   mutate(Index = row_number())

BYBI1_2 <- read.csv("bybincluded/1_2/RF_Wbyb_1_2.comb.log", sep = '\t')
BYBI1_2 <- BYBI1_2 %>% select(age.root.,domwild.rates.Domestic.Wild,domwild.rates.Wild.Domestic) %>% rename_with(~ paste0(., "BYBI_1_2"), everything()) %>%   mutate(Index = row_number())

BYBI1_2_5 <- read.csv("bybincluded/1_2_5/RF_Wbyb_1_2_5.comb.log", sep = '\t')
BYBI1_2_5 <- BYBI1_2_5 %>% select(age.root.,domwild.rates.Domestic.Wild,domwild.rates.Wild.Domestic) %>% rename_with(~ paste0(., "BYBI_1_2_5"), everything()) %>%   mutate(Index = row_number())

BYBI1_3 <- read.csv("bybincluded/1_3/RF_Wbyb_1_3.comb.log", sep = '\t')
BYBI1_3 <- BYBI1_3 %>% select(age.root.,domwild.rates.Domestic.Wild,domwild.rates.Wild.Domestic) %>% rename_with(~ paste0(., "BYBI_1_3"), everything()) %>%   mutate(Index = row_number())


BYBImerged <- BYBI1_2 %>%
  left_join(BYBI1_1_5, by = "Index") %>%
  left_join(BYBI1_3, by = "Index") %>%
  left_join(BYBI1_2_5, by = "Index") %>%
  left_join(BYBI1_1, by = "Index")



all_merged <- BYBEmerged %>% left_join(BYBImerged, by="Index")


#####################
#subset data based on parameter 
#tmcra
age_root_cols <-all_merged %>% select(contains("age.root"))

#rates
dom_to_wild_cols <-all_merged %>% select(contains("Domestic.Wild"))
wild_to_dom_cols <-all_merged %>% select(contains("Wild.Domestic"))


rates <- all_merged %>% select(contains("rates."))
allbybErates <- rates %>% select(contains("BYBE"))
allbybIrates <- rates %>% select(contains("BYBI"))
  
################
# Violin plots with mean and 2 std deviations (95%) point range


#############
#TMCRA
# age root make color cats (based on BYBE or BYBI) 

age_root_cols2 <- age_root_cols %>%
  gather(column_name, value)

age_root_cols2$fill_color <- ifelse(grepl("BYBE", age_root_cols2$column_name), "BYBE", "BYBI")
age_root_cols2$column_name <- str_remove_all(age_root_cols2$column_name, "age\\.root\\.")

# Create the violin plot
ggplot(age_root_cols2, aes(x = column_name, y = value, fill = fill_color)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.75), outlier.shape = NA) +
  labs(title = "TMCRA estimates for H5Nx Rarefaction analysis",
       x = "Analysis",
       y = "TMRCA") +
  scale_fill_manual(values = moma.colors("Palermo")) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 14)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#############
# Rates domestic to wild

dom_to_wild_cols2 <- dom_to_wild_cols %>%
  gather(column_name, value)

dom_to_wild_cols2$fill_color <- ifelse(grepl("BYBE", dom_to_wild_cols2$column_name), "BYBE", "BYBI")
dom_to_wild_cols2$column_name <- str_remove_all(dom_to_wild_cols2$column_name, "domwild\\.rates\\.Domestic\\.Wild")

# Create the violin plot
ggplot(dom_to_wild_cols2, aes(x = column_name, y = value, fill = fill_color)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.75), outlier.shape = NA) +
  labs(title = "Transition rate Domestic to Wild for H5Nx Rarefaction analysis",
       x = "Analysis",
       y = "Transition Rate") +
  scale_fill_manual(values = moma.colors("Palermo")) +
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
ggplot(wild_to_dom_cols2, aes(x = column_name, y = value, fill = fill_color)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.75), outlier.shape = NA) +
  labs(title = "Transition rate Wild to Domestic for H5Nx Rarefaction analysis",
       x = "Analysis",
       y = "Transition Rate") +
  scale_fill_manual(values = moma.colors("Palermo")) +
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
allbybIrates2 <- allbybIrates %>%
  gather(column_name, value)

allbybIrates2$fill_color <- ifelse(grepl("Domestic.Wild", allbybIrates2$column_name), "Domestic.Wild", "Wild.Domestic")
allbybIrates2$column_name <- str_remove_all(allbybIrates2$column_name, "domwild\\.rates\\.")
allbybIrates2$column_name <- str_remove_all(allbybIrates2$column_name, "Domestic.Wild")
allbybIrates2$column_name <- str_remove_all(allbybIrates2$column_name, "Wild.Domestic")
allbybIrates2$column_name <- str_remove_all(allbybIrates2$column_name, "BYBI_")

allbybIrates2 <- allbybIrates2 %>%
  mutate(column_name = case_when(
    fill_color == "Domestic.Wild" ~ paste(column_name, "D.W", sep = "_"),
    fill_color == "Wild.Domestic" ~ paste(column_name, "W.D", sep = "_")
  ))


 

allbybIrates2 <- na.omit(allbybIrates2)
allbybIrates2$column_name <- factor(allbybIrates2$column_name, levels = c("1_1_D.W", "1_1_W.D", "1_1_5_D.W",
                                                                          "1_1_5_W.D","1_2_D.W","1_2_W.D",
                                                                          "1_2_5_D.W", "1_2_5_W.D","1_3_D.W","1_3_W.D"))

# Create the violin plot
ggplot(allbybIrates2, aes(x = column_name, y = value, fill = fill_color)) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.75), outlier.shape = NA) +
  labs(title = "Domestic/Wild Rarefaction",
       x = "Analysis",
       y = "Transition Rate") +
  scale_fill_manual(values = c("#5CA7A4","#2664A5")) +
  theme_minimal() +
  theme(legend.position = "bottom", text = element_text(size = 18), panel.grid.minor = element_line(colour=NA,size=NA),
        panel.grid.major = element_line(colour=NA,size=NA),
        axis.line.x=element_line(colour="black"),
        axis.line.y=element_line(colour="black"),
        axis.text = element_text(colour = "black")) 


