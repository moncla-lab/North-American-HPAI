library(dplyr)
library(lubridate)
library(scales)
library(viridis)
library(zoo)
library(MoMAColors)
library(cowplot)
library(psych)
library(ggpubr)
library(forecast)
library(ggpubr)
library(purrr)

# This code contains several visualizations of HPAI data with some code for correlation calculations as well as calucation of transitions between groups from line list data.

setwd("~/")
# download datasets from USDA APHIS: (https://www.aphis.usda.gov/aphis/ourfocus/animalhealth/animal-disease-information/avian/avian-influenza/2022-hpai)
# for commericial flocks dataframe remove first row with "Control Area released" column headers 
# data updated for wild birds 2024-09-26 to use  colledted date column 

wildbirds <- read.csv("hpai-wild-birds.csv", header = TRUE)
dombirds <- read.csv("domestic-table.csv", header = TRUE)
mammals <- read.csv("hpai-mammals.csv", header = TRUE)


##########################
# Wildbirds vis

#counts by date
wildbirdcount <- wildbirds %>%
  group_by(Collection.Date) %>%
  summarize(count = n())

wildbirdcount$Collection.Date <- ymd(wildbirdcount$Collection.Date)
wildbirds$Bird.Species <- tolower(wildbirds$Bird.Species)

ggplot(wildbirdcount, aes(y=count, x=Collection.Date)) + 
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

##########################
# domestic/commerical flock data
# Note: data was reoraganized to provide number of birds effected based on date of confirmation. Contral area release number is not taken into account and temporally is lagged for many sites 
dombirds$Confirmeddate <- mdy(dombirds$Confirmed)
dombirds$Infected_Birds <- as.numeric(gsub(",","",dombirds$Birds.Affected))

dombirds <- dombirds %>%
  mutate(condensed = ifelse(grepl("Turkey", Production), "Turkey",
                             ifelse(grepl("Table", Production), "Chicken",
                                    ifelse(grepl("Broiler", Production), "Chicken",
                                        ifelse(grepl("Duck", Production), "Duck",
                                           Production)))))

dombirds <- dombirds %>%
  mutate(WOAH = ifelse(grepl("WOAH Non-Poultry", Production), "WOAH Non-Poultry",
                            ifelse(grepl("Live Bird Market", Production), "Live Bird Market",
                                   "WOAH Poultry")))

dombirds <- dombirds %>%
  mutate(WOAH2 = ifelse(grepl("WOAH Non-Poultry", Production), "WOAH Non-Poultry",
                              "WOAH Poultry"))


# get the proprotions of each production type
prop.table(table(dombirds$condensed))

ggplot(dombirds, aes(x=Confirmeddate, y=Infected_Birds)) + 
  geom_line() +
  scale_y_continuous(labels = label_comma(), breaks =c(500000,1000000,1500000,2000000,2500000,3000000,3500000,4000000,4500000,5000000,5500000,6000000)) + 
  labs(title = "Domestic birds Affected - HPAI 2.3.4.4b - North America", x = "Confirmation date", y = "Birds Affected", caption = "Source: USDA - APHIS") +
  theme_minimal() 



ggplot(dombirds, aes(Production)) +
  geom_bar() +
  xlab("Production") +
  ylab("Count") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5,size = 2) +
  ggtitle("Detections of HPAI 2.3.4.4b in Domestic birds by Production") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


#### Detections ###

domescount <- dombirds %>%
  group_by(Confirmeddate) %>%
  summarize(count = n())

domescount2 <- dombirds %>%
  group_by(Confirmeddate, condensed) %>%
  summarize(count = n())

domescount3 <- dombirds %>%
  group_by(Confirmeddate, WOAH) %>%
  summarize(count = n())

# counrts by month 
domescount2 <- domescount2 %>%
  mutate(month_year = format(Confirmeddate, "%Y-%m"))
domescount2_mn <- domescount2 %>%
  group_by(month_year, condensed) %>%
  summarise(total_count = sum(count))


domescount3 <- domescount3 %>%
  mutate(month_year = format(Confirmeddate, "%Y-%m"))
domescount3_mn <- domescount3 %>%
  group_by(month_year, WOAH) %>%
  summarise(total_count = sum(count))
# detections plots 
# all dates

ggplot(domescount2, aes(x=Confirmeddate, y=count, group = condensed, color = condensed)) + 
  geom_line() +
  labs(title = "Domestic birds Detections - HPAI 2.3.4.4b - North America", x = "Confirmation date", y = "Count", caption = "Source: USDA - APHIS") +
  theme_minimal() + scale_color_moma_d("Connors")

ggplot(domescount2_mn, aes(x=month_year, y=total_count,  group = condensed, color = condensed )) + 
  geom_line() +
  labs(title = "Domestic birds Detections - HPAI 2.3.4.4b - North America", x = "Confirmation date", y = "Count", caption = "Source: USDA - APHIS") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) +
  scale_color_moma_d("Connors")

ggplot(domescount2_mn, aes(x = month_year, y = total_count, fill = condensed)) +
  geom_bar(stat = "identity") +
  labs(x = "Month-Year", y = "Total Count", fill = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_moma_d("Connors")


ggplot(domescount3, aes(x=Confirmeddate, y=count, group = WOAH, color = WOAH)) + 
  geom_line() +
  labs(title = "Domestic birds Detections - HPAI 2.3.4.4b - North America", x = "Confirmation date", y = "Count", caption = "Source: USDA - APHIS") +
  theme_minimal() + 
  theme(text = element_text(size = 18)) +
  scale_color_moma_d("Connors")


ggplot(domescount3_mn, aes(x=month_year, y=total_count, group = WOAH, color = WOAH )) + 
  geom_line() +
  labs(title = "Domestic birds Detections - HPAI 2.3.4.4b - North America", x = "Confirmation date", y = "Count", caption = "Source: USDA - APHIS") +
  theme_minimal() + 
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90)) +
  scale_color_moma_d("Connors")



domescount3$Confirmeddate <- as.Date(domescount3$Confirmeddate)

# Step 2: Create a complete sequence of dates from the min to max Confirmeddate
all_dates <- seq(min(domescount3$Confirmeddate), max(domescount3$Confirmeddate), by = "day")

poultry_cases <- domescount3 %>% 
  filter(WOAH == "WOAH Poultry") %>%
  group_by(Confirmeddate) %>%
  summarize(cases = sum(count))

non_poultry_cases <- domescount3 %>% 
  filter(WOAH == "WOAH Non-Poultry") %>%
  group_by(Confirmeddate) %>%
  summarize(cases = sum(count))

# Step 4: Merge with the complete sequence of dates
poultry_cases_full <- data.frame(Confirmeddate = all_dates) %>%
  left_join(poultry_cases, by = "Confirmeddate") %>%
  replace(is.na(.), 0)

non_poultry_cases_full <- data.frame(Confirmeddate = all_dates) %>%
  left_join(non_poultry_cases, by = "Confirmeddate") %>%
  replace(is.na(.), 0)

# Step 5: Merge the two time series
time_series <- merge(poultry_cases_full, non_poultry_cases_full, by = "Confirmeddate", all = TRUE)
names(time_series) <- c("Date", "Poultry_Cases", "Non_Poultry_Cases")

ggplot(time_series, aes(x = Date)) +
  geom_line(aes(y = Poultry_Cases, color = "Poultry Cases")) +
  geom_line(aes(y = Non_Poultry_Cases, color = "Non-Poultry Cases")) +
  labs(title = "WOAH Poultry vs Non-Poultry Cases Over Time",
       x = "Date", y = "Number of Cases") +
  scale_color_manual(values = c("Poultry Cases" = "blue", "Non-Poultry Cases" = "red"))

# Step 7: Calculate cross-correlation
ccf_result <- ccf(time_series$Poultry_Cases, time_series$Non_Poultry_Cases, lag.max = 10, plot = TRUE)

# Interpretation of the cross-correlation plot
ccf_result

correlation <- cor(time_series$Poultry_Cases, time_series$Non_Poultry_Cases)

correlation


##########################
# mammal data 
# used date collected as that was earlier than the detection information
mammalscount <- mammals %>%
  group_by(date_collected) %>%
  summarize(count = n())

mammalscount$date_collected <- mdy(mammalscount$date_collected)

ggplot(mammalscount, aes(y=count, x=date_collected)) + 
  geom_line() +
  labs(title = "Mammal detections HPAI 2.3.4.4b - North America", x = "Date Collected", caption = "Source: USDA - APHIS") +
  theme_minimal()


mammals$species <- tolower(mammals$species)
# get the proprotions of each production type
prop.table(table(mammals$species))

# counts of species
ggplot(mammals, aes(species)) +
  geom_bar() +
  xlab("Species") +
  ylab("Count") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5,size = 2) +
  ggtitle("Detections of HPAI 2.3.4.4b in mammals by species") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# counts by state

ggplot(mammals, aes(state, fill = species)) +
  geom_bar() +
  xlab("State") +
  ylab("Count") +
  ggtitle("Detections of HPAI 2.3.4.4b in mammals by US State and Species") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_viridis_d()



##########################
# plot of cases all together

ggplot(wildbirdcount, aes(x = Collection.Date, y = count, color="Wild bird")) +
  geom_line() +
  geom_line(data = domescount, aes(x = Confirmeddate , y = count, color="Domestic bird")) +
  geom_line(data = mammalscount, aes(x = date_collected, y = count, color="Mammal")) +
  labs(title = "HPAI detections by host in North America",
       x = "Date",
       y = "Count", caption = "Source: USDA - APHIS") +
  theme_minimal() + scale_color_manual(name = "Host", values = c("Wild bird" ="darkgreen", "Domestic bird" = "darkred", "Mammal" = "darkblue"))


##########################
# proportion plots
# make proportion counts plot of the data

domescount <- domescount %>%
  mutate(yearmonth = as.yearmon(Confirmeddate))

colnames(domescount)[1] <- "Date"

total_counts_domes <- aggregate(count ~ yearmonth, data = domescount, sum)
proportions_domes <- transform(total_counts_domes,
                         proportion = count / ave(count, FUN = sum))


mammalscount <- mammalscount %>%
  mutate(yearmonth = as.yearmon(date_collected))

colnames(mammalscount)[1] <- "Date"
mammalscount$Date <- mdy(mammalscount$Date)

total_counts_mam <- aggregate(count ~ yearmonth, data = mammalscount, sum)
proportions_mam <- transform(total_counts_mam,
                               proportion = count / ave(count, FUN = sum))

wildbirdcount <- wildbirdcount %>%
  mutate(yearmonth = as.yearmon(Collection.Date))

colnames(wildbirdcount)[1] <- "Date"
 
total_counts_wild <- aggregate(count ~ yearmonth, data = wildbirdcount, sum)
proportions_wild <- transform(total_counts_wild,
                             proportion = count / ave(count, FUN = sum))



#### merged data together 
combined_df <- dplyr::bind_rows(list(Domesticbird=domescount, Wildbird=wildbirdcount, Mammal=mammalscount), .id = 'source')

total_counts <- aggregate(count ~ yearmonth + source, data = combined_df, sum)

proportions <- transform(total_counts,
                         proportion = count / ave(count, FUN = sum))


## plot of proportion against ALL detections
ggplot(proportions, aes(x = yearmonth, y = proportion, color = source)) +
  geom_line() +
  labs(title = "HPAI proportion of total detections by host", subtitle = "North America", x = "Date", y = "Proportion", color = "Source") +
  theme_minimal()


## plot of proportion against respective host detections


ggplot(proportions_wild, aes(x = yearmonth, y = proportion, color="Wild bird")) +
  geom_line() +
  geom_line(data = proportions_domes, aes(x = yearmonth, y = proportion, color="Domestic bird")) +
  geom_line(data = proportions_mam, aes(x = yearmonth, y = proportion, color="Mammal")) +
  labs(title = "HPAI proportion of total detections by host",
       subtitle = "North America (Prop. with respect to host count)",
       x = "Date",
       y = "Proportion") +
  theme_minimal() + scale_color_manual(name = "Host", values = c("Wild bird" ="darkgreen", "Domestic bird" = "darkred", "Mammal" = "darkblue"))



##########################
# Estimate how many times backyard poultry showed up before domestic and vice versa for each state

dombirds$YearMonth <- format(dombirds$Confirmeddate, "%Y-%m")

# Sort the data by State and YearMonth
dombirds <- dombirds %>% arrange(State, YearMonth)
# Calculate transitions between production types for each month

transition_counts_monthly <- dombirds %>%
  group_by(State, YearMonth) %>%
  mutate(prev_type = lag(condensed)) %>%
  filter(!is.na(prev_type)) %>%
  count(prev_type, condensed) %>%
  ungroup() %>%
  mutate(transition = paste(prev_type, "to", condensed))

# Sum transitions across all months
total_transitions <- transition_counts_monthly %>%
  group_by(State, transition) %>%
  summarize(total_count = sum(n)) %>%
  ungroup() %>%
  group_by(transition) %>%
  summarize(total_count = sum(total_count)) %>%
  arrange(desc(total_count))

# Create the plot
ggplot(total_transitions, aes(x = transition, y = total_count)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Transitions Between Production Types (Across Months)",
       x = "Transition",
       y = "Total Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size = 12))
# For a given state see how many times that the backyard detection happen before wild bird detections as well as domestic before wild
# merge with wild and mammal detections

######################################
# visualize skyGrid reconstructions and calculate correlations with detections
# skygrid reconstructions

order_eq1_sky <- read.csv("skygrids/order_equal_northamerica_h5nx_skygrid.tsv", sep = '\t')
order_eq2_sky <- read.csv("skygrids/order2skygrid.tsv", sep = '\t')
order_eq3_sky <- read.csv("skygrids/order3skygrid.tsv", sep = '\t')
prop1_sky <- read.csv("skygrids/prop1_order.skygrid.tsv", sep = '\t')
prop2_sky <- read.csv("skygrids/prop2_order.skygrid.tsv", sep = '\t')
prop3_sky <- read.csv("skygrids/prop3_order.skygrid.tsv", sep = '\t')

combined_sky_data <- rbind(
  transform(order_eq1_sky, dataset = "order_eq1_sky"),
  transform(order_eq2_sky, dataset = "order_eq2_sky"),
  transform(order_eq3_sky, dataset = "order_eq3_sky"),
  transform(prop1_sky, dataset = "prop1_sky"),
  transform(prop2_sky, dataset = "prop2_sky"),
  transform(prop3_sky, dataset = "prop3_sky")
)


combined_sky_data$date <- as.Date(combined_sky_data$date)


skygrid_plot <- ggplot(combined_sky_data, aes(x = date, y = mean, color = dataset)) +
  geom_line(linewidth = 1.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = dataset), alpha = 0.3) +
  labs(title = "SkyGrid effective population size esitmates HPAI H5Nx",
       x = "Date",
       y = "log(Ne)") +
  scale_y_log10() +
  scale_x_date(limits = c(as.Date("2021-11-01"), as.Date("2023-08-11"))) +
  theme_minimal() +
  theme(text = element_text(size = 18)) +
  scale_fill_moma_d("ustwo") +
  scale_color_moma_d("ustwo")


skygrid_plot


#### comabined skygrids by month, with detections

combined_sky_data

combined_sky_data <- combined_sky_data %>%
  mutate(year_month = format(date, "%Y-%m"))

monthly_totals_sky <- combined_sky_data %>%
  group_by(dataset, year_month) %>%
  summarize(total_mean = sum(mean),
            total_upper = sum(upper),
            total_lower = sum(lower))



ggplot(monthly_totals_sky, aes(x = year_month, y = total_mean, color = dataset, group = dataset)) +
  geom_line(size = 1.5) +
  labs(title = "SkyGrid effective population size esitmates HPAI H5Nx",
       x = "Date",
       y = "log(Ne)") +
  scale_y_log10() +
  theme_minimal() +
  theme(text = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_moma_d("VanGogh")
## calculate correlations with detections
# calculate the total detection across all hosts 

combined_detections <- bind_rows(wildbirdcount, mammalscount, domescount)
combined_detections$Date <- as.Date(combined_detections$Date)
combined_detections$year_month <- format(combined_detections$Date, "%Y-%m")

combined_detections_total <- combined_detections %>%
  group_by(year_month) %>%
  summarise(total_count = sum(count))

combined_detections_total_week <- combined_detections %>%
  mutate(week_start = round_date(Date, unit = "week")) %>%
  group_by(week_start) %>%
  summarise(total_count = sum(count, na.rm = TRUE))


order_eq1_sky$date <- as.Date(order_eq1_sky$date)
order_eq1_sky_week <- order_eq1_sky %>%
  mutate(week_start = round_date(date, unit = "week")) %>%
  group_by(week_start) %>%
  summarise(total_mean_ordereq1 = sum(mean, na.rm = TRUE),
            total_upper_ordereq1 = sum(upper, na.rm = TRUE),
            total_lower_ordereq1 = sum(lower, na.rm = TRUE))

order_eq2_sky$date <- as.Date(order_eq2_sky$date)
order_eq2_sky_week <- order_eq2_sky %>%
  mutate(week_start = round_date(date, unit = "week")) %>%
  group_by(week_start) %>%
  summarise(total_mean_ordereq2 = sum(mean, na.rm = TRUE),
            total_upper_ordereq2 = sum(upper, na.rm = TRUE),
            total_lower_ordereq2 = sum(lower, na.rm = TRUE))

order_eq3_sky$date <- as.Date(order_eq3_sky$date)
order_eq3_sky_week <- order_eq3_sky %>%
  mutate(week_start = round_date(date, unit = "week")) %>%
  group_by(week_start) %>%
  summarise(total_mean_ordereq3 = sum(mean, na.rm = TRUE),
            total_upper_ordereq3 = sum(upper, na.rm = TRUE),
            total_lower_ordereq3 = sum(lower, na.rm = TRUE))


prop1_sky$date <- as.Date(prop1_sky$date)
prop1_sky_week <- prop1_sky %>%
  mutate(week_start = round_date(date, unit = "week")) %>%
  group_by(week_start) %>%
  summarise(total_mean_prop1 = sum(mean, na.rm = TRUE),
            total_upper_prop1 = sum(upper, na.rm = TRUE),
            total_lower_prop1 = sum(lower, na.rm = TRUE))

prop2_sky$date <- as.Date(prop2_sky$date)
prop2_sky_week <- prop2_sky %>%
  mutate(week_start = round_date(date, unit = "week")) %>%
  group_by(week_start) %>%
  summarise(total_mean_prop2 = sum(mean, na.rm = TRUE),
            total_upper_prop2 = sum(upper, na.rm = TRUE),
            total_lower_prop2 = sum(lower, na.rm = TRUE))

prop3_sky$date <- as.Date(prop3_sky$date)
prop3_sky_week <- prop3_sky %>%
  mutate(week_start = round_date(date, unit = "week")) %>%
  group_by(week_start) %>%
  summarise(total_mean_prop3 = sum(mean, na.rm = TRUE),
            total_upper_prop3 = sum(upper, na.rm = TRUE),
            total_lower_prop3 = sum(lower, na.rm = TRUE))



merged_detect_skys_week_or1 <- merge(combined_detections_total_week, order_eq1_sky_week, by = "week_start")
merged_detect_skys_week_or2 <- merge(combined_detections_total_week, order_eq2_sky_week, by = "week_start")
merged_detect_skys_week_or3 <- merge(combined_detections_total_week, order_eq3_sky_week, by = "week_start")

merged_detect_skys_week_prop1 <- merge(combined_detections_total_week, prop1_sky_week, by = "week_start")
merged_detect_skys_week_prop2 <- merge(combined_detections_total_week, prop2_sky_week, by = "week_start")
merged_detect_skys_week_prop3 <- merge(combined_detections_total_week, prop3_sky_week, by = "week_start")


########
# correlations and visaulization of correlation


or1corr <- ggplot(merged_detect_skys_week_or1, aes(x = total_count, y = log(total_mean_ordereq1))) +
  geom_point(stat = "identity", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black" ) + # Add linear regression line
  stat_cor(method = "spearman", label.x = 200, label.y = -2) + 
  labs(title = "Equal order 1",
       x = "HPAI detections",
       y = "log(Ne)") +
  theme_minimal() + 
  ylim(-3,6) + 
  theme(legend.position = "bottom", text = element_text(size = 14), panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), axis.ticks = element_line())

or1corr

or2corr <- ggplot(merged_detect_skys_week_or2, aes(x = total_count, y = log(total_mean_ordereq2))) +
  geom_point(stat = "identity", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black" ) + # Add linear regression line
  stat_cor(method = "spearman", label.x = 200, label.y = -2) + 
  labs(title = "Equal order 2",
       x = "HPAI detections",
       y = "log(Ne)") +
  theme_minimal() + 
  ylim(-3,6) + 
  theme(legend.position = "bottom", text = element_text(size = 14), panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), axis.ticks = element_line())

or2corr

or3corr <- ggplot(merged_detect_skys_week_or3, aes(x = total_count, y = log(total_mean_ordereq3))) +
  geom_point(stat = "identity", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black" ) + # Add linear regression line
  stat_cor(method = "spearman", label.x = 200, label.y = -2) + 
  labs(title = "Equal order 3",
       x = "HPAI detections",
       y = "log(Ne)") +
  ylim(-3,6) + 
  theme_minimal() + 
  theme(legend.position = "bottom", text = element_text(size = 14), panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), axis.ticks = element_line())

or3corr


prop1corr <- ggplot(merged_detect_skys_week_prop1, aes(x = total_count, y = log(total_mean_prop1))) +
  geom_point(stat = "identity", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black" ) + # Add linear regression line
  stat_cor(method = "spearman", label.x = 200, label.y = -2) + 
  labs(title = "Proportional 1",
       x = "HPAI detections",
       y = "log(Ne)") +
  ylim(-3,6) + 
  theme_minimal() + 
  theme(legend.position = "bottom", text = element_text(size = 14), panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), axis.ticks = element_line())

prop1corr


prop2corr <- ggplot(merged_detect_skys_week_prop2, aes(x = total_count, y = log(total_mean_prop2))) +
  geom_point(stat = "identity", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black" ) + # Add linear regression line
  stat_cor(method = "spearman", label.x = 200, label.y = -2) + 
  labs(title = "Proportional 2",
       x = "HPAI detections",
       y = "log(Ne)") +
  ylim(-3,6) + 
  theme_minimal() + 
  theme(legend.position = "bottom", text = element_text(size = 14), panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), axis.ticks = element_line())

prop2corr


prop3corr <- ggplot(merged_detect_skys_week_prop3, aes(x = total_count, y = log(total_mean_prop3))) +
  geom_point(stat = "identity", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black" ) + # Add linear regression line
  stat_cor(method = "spearman", label.x = 200, label.y = -2) + 
  labs(title = "Proportional 3",
       x = "HPAI detections",
       y = "log(Ne)") +
  ylim(-3,6) + 
  theme_minimal() + 
  theme(legend.position = "bottom", text = element_text(size = 14), panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), axis.ticks = element_line())

prop3corr


plot_grid(or1corr,or2corr,or3corr,prop1corr,prop2corr,prop3corr)

#############
# check temporal lags 


correlation_or1 <- ccf(merged_detect_skys_week_or1$total_count, log(merged_detect_skys_week_or1$total_mean_ordereq1))
ccfor1 <- ggCcf(merged_detect_skys_week_or1$total_count,log(merged_detect_skys_week_or1$total_mean_ordereq1), main = "Equal order 1\n log(Ne) vs Detections", xlab = "Lag", ylab = "Correlation") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        axis.line.x=element_line(colour="black"),
        axis.line.y=element_line(colour="black"))
ccfor1

print(correlation_or1)

correlation_or2 <- ccf(merged_detect_skys_week_or2$total_count, log(merged_detect_skys_week_or2$total_mean_ordereq2))
ccfor2 <- ggCcf(merged_detect_skys_week_or2$total_count,log(merged_detect_skys_week_or2$total_mean_ordereq2), main = "Equal order 2\n log(Ne) vs Detections", xlab = "Lag", ylab = "Correlation") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        axis.line.x=element_line(colour="black"),
        axis.line.y=element_line(colour="black"))
ccfor2

print(correlation_or2)

correlation_or3 <- ccf(merged_detect_skys_week_or3$total_count, log(merged_detect_skys_week_or3$total_mean_ordereq3))
ccfor3 <- ggCcf(merged_detect_skys_week_or3$total_count,log(merged_detect_skys_week_or3$total_mean_ordereq3), main = "Equal order 3\n log(Ne) vs Detections", xlab = "Lag", ylab = "Correlation") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        axis.line.x=element_line(colour="black"),
        axis.line.y=element_line(colour="black"))
ccfor3

print(correlation_or3)


correlation_prop1 <- ccf(merged_detect_skys_week_prop1$total_count, log(merged_detect_skys_week_prop1$total_mean_prop1))
ccfprop1 <- ggCcf(merged_detect_skys_week_prop1$total_count,log(merged_detect_skys_week_prop1$total_mean_prop1), main = "Proportional 1\n log(Ne) vs Detections", xlab = "Lag", ylab = "Correlation") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        axis.line.x=element_line(colour="black"),
        axis.line.y=element_line(colour="black"))
ccfprop1

print(correlation_prop1)



correlation_prop2 <- ccf(merged_detect_skys_week_prop2$total_count, log(merged_detect_skys_week_prop2$total_mean_prop2))
ccfprop2 <- ggCcf(merged_detect_skys_week_prop2$total_count,log(merged_detect_skys_week_prop2$total_mean_prop2), main = "Proportional 2\n log(Ne) vs Detections", xlab = "Lag", ylab = "Correlation") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        axis.line.x=element_line(colour="black"),
        axis.line.y=element_line(colour="black"))
ccfprop2

print(correlation_prop2)

correlation_prop3 <- ccf(merged_detect_skys_week_prop3$total_count, log(merged_detect_skys_week_prop3$total_mean_prop3))
ccfprop3 <- ggCcf(merged_detect_skys_week_prop3$total_count,log(merged_detect_skys_week_prop3$total_mean_prop3), main = "Proportional 1\n log(Ne) vs Detections", xlab = "Lag", ylab = "Correlation") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        axis.line.x=element_line(colour="black"),
        axis.line.y=element_line(colour="black"))
ccfprop3

print(correlation_prop3)




plot_grid(ccfor1,ccfor2,ccfor3,ccfprop1,ccfprop2,ccfprop3)


###########
# Unified fig with detections, numseqs, and effecgive pop sizes (Figure 1 ms)

# detections 
wildbirdcount <- wildbirds %>%
  group_by(Collection.Date) %>%
  summarize(count = n())
wildbirdcount$Collection.Date <- ymd(wildbirdcount$Collection.Date)
colnames(wildbirdcount)[colnames(wildbirdcount) == "Collection.Date"] <- "Date"

dombirds$Confirmeddate <- mdy(dombirds$Confirmed)
domescount <- dombirds %>%
  group_by(Confirmeddate) %>%
  summarize(count = n())
colnames(domescount)[colnames(domescount) == "Confirmeddate"] <- "Date"


mammalscount <- mammals %>%
  group_by(date_collected) %>%
  summarize(count = n())
mammalscount$date_collected <- mdy(mammalscount$date_collected)
colnames(mammalscount)[colnames(mammalscount) == "date_collected"] <- "Date"

line_date <- as.Date("2023-08-11")

p1a <- ggplot(wildbirdcount, aes(x = Date, y = count, fill="Wild bird")) +
  geom_bar(stat = "identity", width = 2) +
  geom_bar(data = domescount, aes(x = Date , y = count, fill="Domestic bird"), stat = "identity",  width = 2) +
  geom_bar(data = mammalscount, aes(x = Date, y = count, fill="Mammal"), stat = "identity", width = 2) +
  geom_vline(xintercept = line_date, linetype = "dashed", color = "black") +
  labs(title = "HPAI detections by host in North America",
       x = "Date",
       y = "HPAI detections", caption = "Source: USDA - APHIS") +
  scale_x_date(labels = date_format("%b\n%Y"), limits = c(as.Date("2022-01-01"), as.Date("2024-10-01")),
               breaks = seq(as.Date("2022-01-01"), as.Date("2024-10-01"),
                            by = "2 months"), expand = c(0,0)) +
  scale_y_continuous(expand = c(0, 0),limits = c(0,140), breaks = c(20,40,60,80,100,120,140)) +
  theme_minimal() + 
  theme(legend.position = "bottom", text = element_text(size = 20), panel.grid.minor = element_line(colour=NA,size=NA),
       panel.grid.major = element_line(colour=NA,size=NA),
       axis.line.x=element_line(colour="black"),
       axis.line.y=element_line(colour="black"),
       axis.ticks.length = unit(0.2, "cm"),
       axis.title.x = element_blank(),
       axis.ticks = element_line(linewidth = 1),
       axis.text = element_text(colour = "black"),
       legend.title = element_blank()) +
  scale_fill_manual(name = "Host", values = c("Wild bird" ="#2664A5", "Domestic bird" = "#5CA7A4", "Mammal" = "#B2313D")) 

p1a



# plots of detections with the skygrid 
order_eq1_sky$date <- as.Date(order_eq1_sky$date)
order_eq2_sky$date <- as.Date(order_eq2_sky$date)


#############
# Final figure.1 fig 
line_date <- as.Date("2023-08-11")

px4 <- ggplot(order_eq1_sky, aes(x = date, y = log(mean)+5), fill="grey50") +
  geom_line(linetype = "dashed") +
  geom_ribbon(aes(ymin = log(lower)+5, ymax = log(upper)+5), alpha = 0.1) +
  geom_bar(data = wildbirdcount, aes(x = Date , y = count/25, fill="Wild bird"), stat = "identity",  width = 2) +
  geom_bar(data = domescount, aes(x = Date , y = count/25, fill="Domestic bird"), stat = "identity",  width = 2) +
  geom_bar(data = mammalscount, aes(x = Date, y = count/25, fill="Mammal"), stat = "identity", width = 2) +
  geom_vline(xintercept = line_date, linetype = "dashed", color = "black") +
  labs(title = "HPAI detections by host in North America",
       x = "Date",
       y = "Detections", caption = "Source: USDA - APHIS") +
  scale_x_date(labels = date_format("%b\n%Y"), limits = c(as.Date("2021-11-02"), as.Date("2024-10-01")),
               breaks = seq(as.Date("2022-01-01"), as.Date("2024-10-01"),
                            by = "2 months")) +
  scale_y_continuous(labels = seq(0,400,50), breaks = seq(0,400,50) / 25, limits = c(0,400) / 25, 
    sec.axis = sec_axis( transform = ~.,name="log(Ne)", labels = c(-5,0,5,10,15), breaks = seq(-5,15,5)+5)) +
  theme_minimal() + 
  theme(legend.position = "bottom", text = element_text(size = 20),panel.grid.minor = element_line(colour=NA,size=NA),
        panel.grid.major = element_line(colour=NA,size=NA),
        axis.line.x=element_line(colour="black"),
        axis.line.y=element_line(colour="black"),
        axis.text = element_text(colour = "black"),
        legend.title = element_blank(), axis.title.x = element_blank(), axis.ticks = element_line()) +
  scale_fill_manual(name = "Host", values = c("Wild bird" ="#2664A5", "Domestic bird" = "#5CA7A4", "Mammal" = "#B2313D", "grey50" = "grey50")) +
  scale_linetype_manual(values = "dashed") +
  scale_color_manual(values = "gray50") +
  guides(linetype = guide_legend(override.aes = list(color = "black")))


px4






skygrideq1 <- ggplot(order_eq1_sky, aes(x = date, y = log(mean)), fill="grey50") +
  geom_line(linetype = "dashed") +
  geom_ribbon(aes(ymin = log(lower), ymax = log(upper)), alpha = 0.1) +
  labs(title = "SkyGrid Ne",
       x = "Date",
       y = "log(Ne)") +
  scale_x_date(labels = date_format("%b\n%Y"), limits = c(as.Date("2022-01-01"), as.Date("2024-10-01")),
               breaks = seq(as.Date("2022-01-01"), as.Date("2024-10-01"),
                            by = "2 months"), expand = c(0,0)) +
  scale_y_continuous(breaks = c(-5,-2.5,0,2.5,5,7.5,10)) +
  theme_minimal() + 
  theme(legend.position = "bottom", text = element_text(size = 20),panel.grid.minor = element_line(colour=NA,size=NA),
        panel.grid.major = element_line(colour=NA,size=NA),
        axis.line.x=element_line(colour="black"),
        axis.line.y=element_line(colour="black"),
        axis.text = element_text(colour = "black"),
        legend.title = element_blank(), axis.title.x = element_blank(), axis.ticks = element_line()) +
  scale_linetype_manual(values = "dashed") +
  scale_color_manual(values = "gray50") +
  guides(linetype = guide_legend(override.aes = list(color = "black")))


skygrideq1


plot_grid(p1a,skygrideq1,nrow = 2)
