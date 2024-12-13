library(ggplot2)
library(tidyverse)
library(dplyr)

setwd("~/")

# read in the detection data and dataframe of species and thier corresponding host order 
detections <- read.csv("hpai-wild-birds.csv")
species_defs <- read.csv("speciescond.csv")
order_condensed <- read.csv("order_condensed.csv")
domestics <- read.csv("domestic-table.csv")
mammals <- read.csv("hpai-mammals.csv")

detections_sepecies_merged_df <-  merge(detections, species_defs, by.x = "bird_species_fix", by.y = "common_name_correction", all.x = TRUE)
detections_sepecies_merged_df2 <- merge(detections_sepecies_merged_df, order_condensed, by.x = "order", by.y = "order", all.x = TRUE)
detections_sepecies_merged_df3 <- detections_sepecies_merged_df2 %>%
  select(Collection.Date, order_condensed)

domestics$order_condensed <- "galliformes"
domestics <- domestics %>%
  rename(Collection.Date = Confirmed) %>%
  select(Collection.Date, order_condensed)

mammals$order_condensed <- "non-human_mammals"
mammals <- mammals %>%
  rename(Collection.Date = Date.Collected) %>%
  select(Collection.Date, order_condensed)


detections_sepecies_merged_df4 <- rbind(detections_sepecies_merged_df3, domestics, mammals)

order_counts <- table(detections_sepecies_merged_df4$order_condensed)
order_proportions <- prop.table(order_counts)
print(order_proportions*655)


#### read in seqeuence data metadata file (GISAID metadata)

seqs <- read.csv("../")

# use proportions calculated above
sample_counts <- c(
  accipitriformes = 133,
  anseriformes = 342,
  passeriformes = 12,
  `nonhuman-mammal` = 16,
  galliformes = 83,
  charadriiformes = 40,
  strigiformes = 29
)

# Function to sample rows for each value
sample_rows <- function(data, value, n) {
  data %>%
    filter(order_condensed == value) %>%
    sample_n(n)
}

# Apply sampling for each value in order_condensed based on the given counts
sampled_df <- bind_rows(lapply(names(sample_counts), function(val) {
  sample_rows(seqs, val, sample_counts[val])
}))

sampled_df <- sampled_df %>%
  mutate(combined_column = paste(Seqname, order_condensed, sep = "|")) %>%
  select(-seq, everything(), seq)

# View the sampled dataframe
print(sampled_df)
write.csv(sampled_df,".proportional_sample.csv")
