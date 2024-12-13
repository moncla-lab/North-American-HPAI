library(ggplot2)
library(dplyr)
library(tidyr)


setwd("~/HPAI_USDA_casedata")
# download datasets from USDA APHIS: (https://www.aphis.usda.gov/aphis/ourfocus/animalhealth/animal-disease-information/avian/avian-influenza/2022-hpai)
# for commericial flocks dataframe remove first row with "Control Area released" column headers 
wildbirdss <- read.csv("hpai-wild-birds.csv", header = TRUE)
speciesfixs <- read.csv("metadata/species.csv")
speciesfixs <- speciesfixs %>% distinct(common_name_correction, .keep_all = TRUE)


wildbirdss$Bird.Species <- gsub(" ", "_", tolower(wildbirdss$Bird.Species))
wildbirdss$Bird.Species <- gsub("'", "", wildbirdss$Bird.Species)  # Remove single quotes
wildbirdss$Bird.Species <- gsub("_\\(unidentified\\)", "", wildbirdss$Bird.Species)  # Remove the string "_(unidentified)"

wildbirdsss <- merge(wildbirdss,speciesfixs, by.x ="Bird.Species", by.y ="common_name_correction", all.x = TRUE)

# get order condensed
species <-  read.csv("metadata/order_condensed.csv")

wildbirds <- merge(wildbirdsss,species, by ="order", all.x = TRUE)
wildbirds$count <- 1
wildbirds$Sampling.Method <-  tolower(wildbirds$Sampling.Method)
wildbirds$Sampling.Method <- gsub("mortility","mortality", x = wildbirds$Sampling.Method)
wildbirds$WOAH.Classification <- tolower(wildbirds$WOAH.Classification)

# Calculate the count of each category within each group
counts <- aggregate(count ~ Sampling.Method + order, data = wildbirds, FUN = sum)
df_agency <- data.frame(table(wildbirds$Submitting.Agency))
write.csv(df_agency, "df_agencysubmitt.csv")


ggplot(wildbirds, aes(x = Sampling.Method, fill = order)) +
  geom_bar(position = "stack") +
  xlab("State") +
  ylab("Count") +
  ggtitle("Detections of HPAI 2.3.4.4b in Wild birds by Sampling Method") + 
  theme_minimal() +
  theme(text = element_text(size = 14))


ggplot(wildbirds, aes(x = WOAH.Classification, fill = order)) +
  geom_bar(position = "stack") +
  xlab("State") +
  ylab("Count") +
  ggtitle("Detections of HPAI 2.3.4.4b in Wild birds by WOAH.Classification") + 
  theme_minimal() +
  theme(text = element_text(size = 14))


#read in agencies with condensed versions
df_agency2 <- read.csv("metadata/df_agencysubmittnew.csv")
wildbirds <-  merge(wildbirds, df_agency2, by.x ="Submitting.Agency", by.y = "Var1" )


df_agency3 <- df_agency2 %>%
  group_by(condensed) %>%
  summarise(frequency = sum(Freq))


ggplot(df_agency3, aes(x = condensed, y = frequency)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = frequency), vjust = -0.5) +
  xlab("State") +
  ylab("Count") +
  ggtitle("Detections of HPAI 2.3.4.4b in Wild birds by Submitting Agency") + 
  theme_minimal() +
  theme(text = element_text(size = 14), axis.text.x = element_text(angle = 90))

df_agency4 <- df_agency2 %>%
  group_by(condensed2) %>%
  summarise(frequency = sum(Freq))


ggplot(df_agency4, aes(x = condensed2, y = frequency)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = frequency), vjust = -0.5) +
  xlab("State") +
  ylab("Count") +
  ggtitle("Detections of HPAI 2.3.4.4b in Wild birds by Submitting Agency (condensed)") + 
  theme_minimal() +
  theme(text = element_text(size = 14), axis.text.x = element_text(angle = 90))



ordercond <- read.csv("metadata/order_condensed.csv")
wildbirds <- merge(wildbirds, ordercond, by ="order")



df_complete <- wildbirds %>%
  count(condensed2, order_condensed.y) %>%
  complete(condensed2, order_condensed.y, fill = list(n = 0))

ggplot(df_complete, aes(x = condensed2, y = n, fill = order_condensed.y)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "Submitting Agency", y = "Order", title = "Order counts by Submitting Agency") +
  theme_minimal()


### filter by date frange from the anaylsis 

wildbirds_analysisdates <- wildbirds %>%
  filter(as.Date(Collection.Date) < as.Date("2023-08-01"))

wildbirds_analysisdates_comp <- wildbirds_analysisdates %>%
  count(condensed2, order_condensed.y) %>%
  complete(condensed2, order_condensed.y, fill = list(n = 0))

ggplot(wildbirds_analysisdates_comp, aes(x = condensed2, y = n, fill = order_condensed.y)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "Submitting Agency", y = "Order", title = "Order counts by Submitting Agency") +
  theme_minimal()


######
ggplot(wildbirds, aes(x = Sampling.Method, fill = WOAH.Classification)) +
  geom_bar(position = "stack") +
  xlab("State") +
  ylab("Count") +
  ggtitle("Detections of HPAI 2.3.4.4b in Wild birds by Sampling Method") + 
  theme_minimal() +
  theme(text = element_text(size = 14))


ggplot(wildbirds, aes(x = condensed2, fill = WOAH.Classification)) +
  geom_bar(position = "stack") +
  xlab("State") +
  ylab("Count") +
  ggtitle("Detections of HPAI 2.3.4.4b in Wild birds by Sampling Method") + 
  theme_minimal() +
  theme(text = element_text(size = 14))
