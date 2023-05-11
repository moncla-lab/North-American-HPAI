library(dplyr)
library(lubridate)
library(scales)
library(viridis)




setwd("~/Documents/HPAI_2344b")

# download datasets from USDA APHIS: (https://www.aphis.usda.gov/aphis/ourfocus/animalhealth/animal-disease-information/avian/avian-influenza/2022-hpai)
# for commericial flocks dataframe remove first row with "Control Area released" column headers 
wildbirds <- read.csv("HPAI_USDA_data/hpai-wild-birds-ver2_2.3.4.4b.csv", header = TRUE)
dombirds <- read.csv("HPAI_USDA_data/commeriialflcoks.tsv", header = TRUE, sep = '\t')
mammals <- read.csv("HPAI_USDA_data/hpai-mammals.csv", header = TRUE)

#######
# Wildbirds vis

#counts by date
wildbirdcount <- wildbirds %>%
  group_by(Date.Detected) %>%
  summarize(count = n())

wildbirdcount$Date.Detected <- mdy(wildbirdcount$Date.Detected)
wildbirds$Bird.Species <- tolower(wildbirds$Bird.Species)

ggplot(wildbirdcount, aes(y=count, x=Date.Detected)) + 
  geom_line() +
  labs(title = "Wild bird detections HPAI 2.3.4.4b - North America", x = "Detection Date", caption = "Source: USDA - APHIS") +
  theme_minimal()

# counts by state
ggplot(wildbirds, aes(State)) +
  geom_bar() +
  xlab("State") +
  ylab("Count") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5,size = 2) +
  ggtitle("Detections of HPAI 2.3.4.4b in Wild birds by U.S. State") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# by species
ggplot(wildbirds, aes(Bird.Species)) +
  geom_bar() +
  xlab("Species") +
  ylab("Count") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5,size = 2) +
  ggtitle("Detections of HPAI 2.3.4.4b in Wild birds by Species") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# filter bird species
# duck, gull, owl, goose, eagle, hawk, swan, teal, tern, crow

wildbirds$condSpecies <- ifelse(grepl("duck", wildbirds$Bird.Species), "duck",
                       ifelse(grepl("gull", wildbirds$Bird.Species), "gull",
                              ifelse(grepl("owl", wildbirds$Bird.Species), "owl",
                                     ifelse(grepl("goose", wildbirds$Bird.Species), "goose",
                                            ifelse(grepl("eagle", wildbirds$Bird.Species), "eagle",
                                                   ifelse(grepl("hawk", wildbirds$Bird.Species), "hawk",
                                                          ifelse(grepl("swan", wildbirds$Bird.Species), "swan",
                                                                 ifelse(grepl("teal", wildbirds$Bird.Species), "teal",
                                                                        ifelse(grepl("tern", wildbirds$Bird.Species), "tern",
                                                                               ifelse(grepl("crow", wildbirds$Bird.Species), "crow",
                                                          wildbirds$Bird.Species))))))))))

ggplot(wildbirds, aes(condSpecies)) +
  geom_bar() +
  xlab("Species") +
  ylab("Count") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5,size = 2) +
  ggtitle("Detections of HPAI 2.3.4.4b in Wild birds by Species (Cond.)") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# domestic/commerical flock data
# Note: data was reoraganized to provide number of birds effected based on date of confirmation. Contral area release number is not taken into account and temporally is lagged for many sites 
dombirds[is.na(dombirds)] <- ""
dombirds_cond <- dombirds %>%
  unite("Infected_Birds", starts_with(colnames(dombirds)[6]):ends_with(colnames(dombirds)[205]), sep = "")

dombirds_cond$Confirmeddate <- dmy(dombirds_cond$Confirmed)
dombirds_cond$Infected_Birds <- as.numeric(gsub(",","",dombirds_cond$Infected_Birds))

ggplot(dombirds_cond, aes(x=Confirmeddate, y=Infected_Birds)) + 
  geom_line() +
  scale_y_continuous(labels = label_comma(), breaks =c(500000,1000000,1500000,2000000,2500000,3000000,3500000,4000000,4500000,5000000,5500000,6000000)) + 
  labs(title = "Domestic bird detections HPAI 2.3.4.4b - North America", x = "Confirmation date", y = "Infected Birds", caption = "Source: USDA - APHIS") +
  theme_minimal() 

ggplot(dombirds_cond, aes(Production)) +
  geom_bar() +
  xlab("Production") +
  ylab("Count") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5,size = 2) +
  ggtitle("Detections of HPAI 2.3.4.4b in Domestic birds by Production") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 



# mammal data 
# used date collected as that was earlier than the detection information
mammalscount <- mammals %>%
  group_by(Date.Collected) %>%
  summarize(count = n())

mammalscount$Date.Collected <- ymd(mammalscount$Date.Collected)

ggplot(mammalscount, aes(y=count, x=Date.Collected)) + 
  geom_line() +
  labs(title = "Mammal detections HPAI 2.3.4.4b - North America", x = "Date Collected", caption = "Source: USDA - APHIS") +
  theme_minimal()

# counts of species
ggplot(mammals, aes(Species)) +
  geom_bar() +
  xlab("Species") +
  ylab("Count") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5,size = 2) +
  ggtitle("Detections of HPAI 2.3.4.4b in mammals by species") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# counts by state

ggplot(mammals, aes(State, fill = Species)) +
  geom_bar() +
  xlab("State") +
  ylab("Count") +
  ggtitle("Detections of HPAI 2.3.4.4b in mammals by US State and Species") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_viridis_d()



######
# plot of cases all together

ggplot(wildbirdcount, aes(x = Date.Detected, y = count)) +
  geom_line() +
  geom_line(data = dombirds_cond, aes(x = Confirmeddate , y = Infected_Birds), color = "red") +
  geom_line(data = mammalscount, aes(x = Date.Collected, y = count), color = "blue") +
  labs(title = "Case counts HPAI in North America",
       x = "Date",
       y = "Count",
       color = "Animal") +
  scale_y_continuous(labels = label_comma(), breaks =c(500000,1000000,1500000,2000000,2500000,3000000,3500000,4000000,4500000,5000000,5500000,6000000)) + 
  theme_minimal()


ggplot(wildbirdcount, aes(x = Date.Detected, y = count, color = "red")) +
  geom_line() +
  geom_line(data = mammalscount, aes(x = Date.Collected, y = count), color = "blue") +
  labs(title = "Case counts HPAI in North America - Wild bird/ Mammal",
       x = "Date",
       y = "Count") +
  theme_minimal()