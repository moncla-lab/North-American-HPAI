# Script to standardize metadta for species, and geography for H5Nx in North America
# Lambodhar Damdoaran
# 2023-11

setwd("~/")
library(ggplot2)
library(dplyr)
library(stringr)


# read in metadata file and files for mergeing order, avonet, and geography info
h5_data_dedup <- read.csv("gisaidmetadata.tsv", sep = '\t')
species_order <- read.csv("metadata/species.csv")
order_cond <- read.csv("metadata/order_condensed.csv")
flyway_info <-  read.csv("metadata/flyway_regions.csv")
species_to_avonet <- read.csv("metadata/species_avonet.txt", sep = '\t')
avonet <- read.csv("metadata/AVONET3species.tsv", sep = '\t')


# make sure that names are lowercase and that any spaces and ' replaced with _ a
h5_data_dedup <- as.data.frame(lapply(h5_data_dedup, function(x) str_replace_all(x, "'", "")))
h5_data_dedup <- as.data.frame(lapply(h5_data_dedup, function(x) str_replace_all(x, " ", "_")))
h5_data_dedup <- as.data.frame(lapply(h5_data_dedup, tolower))
species_order <- as.data.frame(lapply(species_order, tolower))
flyway_info <- as.data.frame(lapply(flyway_info, function(x) str_replace_all(x, "[' ]", "_")))
flyway_info <- as.data.frame(lapply(flyway_info, tolower))


species_order <- species_order %>% distinct(common_name_correction, .keep_all = TRUE)

# merge dataframes
h5_data_dedup_sp <- left_join(h5_data_dedup, species_order, by = c("Species" = "common_name_correction"), relationship ="many-to-many")
h5_data_dedup_sp_oc <- left_join(h5_data_dedup_sp, order_cond, by = "order", relationship ="many-to-many")
h5_data_dedup_sp_oc <- left_join(h5_data_dedup_sp_oc, flyway_info, by = c("State_Province" = "location"), relationship ="many-to-many")
h5_data_dedup_sp_oc <- left_join(h5_data_dedup_sp_oc, species_to_avonet, by = "Species", relationship ="many-to-many")
h5_data_dedup_sp_oc_av <- left_join(h5_data_dedup_sp_oc, avonet, by = "taxonomic_genus_species", relationship ="many-to-many")




# Double checking no duplicated
h5_data_dedup_sp <- h5_data_dedup_sp %>%
  distinct(Isolate_Name, .keep_all = TRUE)

h5_data_dedup_sp_oc <- h5_data_dedup_sp_oc %>%
  distinct(Isolate_Name, .keep_all = TRUE)

h5_data_dedup_sp_oc_av <- h5_data_dedup_sp_oc_av %>%
  distinct(Isolate_Name, .keep_all = TRUE)


# for anything galliformes make the lifesttyle information domestic unless otherwise specified as wild in Domestic status
# make sure that domesticated anseriformes (ducks and goose) are included domestic


last_columns <- tail(names(h5_data_dedup_sp_oc_av), 5)  # Select the last 5 columns (the avonet data)

# change galliformes to domestic for avo cats, except for when denoted wild (for wild turkeys)
h5_data_dedup_sp_oc_av <- h5_data_dedup_sp_oc_av %>%
  mutate(across(all_of(last_columns), 
                ~ ifelse(order_condensed == "galliformes" & Domestic_Status != "wild", "domestic", .)))

# change anseriformes catagories for domesticated goose and ducks 
h5_data_dedup_sp_oc_av <- h5_data_dedup_sp_oc_av %>%
  mutate(across(all_of(last_columns), 
                ~ ifelse(order_condensed == "anseriformes" & (Domestic_Status %in% c("domestic", "backyard_bird")), "domestic", .)))


write.csv(h5_data_dedup_sp_oc_av, file = "H5Nx-2021-2023_seq-avonet.csv")
##########
# plots
# count visualize order number
order_counts <- table(h5_data_dedup_sp$order)
order_counts_df <- as.data.frame(order_counts)
names(order_counts_df) <- c("Value", "Frequency")

order_counts_df <- order_counts_df %>%
  filter(Value != "avian")

ggplot(order_counts_df, aes(x = Value, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = Frequency), vjust = -0.5) +
  labs(title = "Occurrences of Host Orders", x = "Order", y = "Frequency") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),text = element_text(size = 18)) 

# count and visualize order condensed

ordercond_counts <- table(h5_data_dedup_sp_oc$order_condensed)
ordercond_counts_df <- as.data.frame(ordercond_counts)
names(ordercond_counts_df) <- c("Value", "Frequency")


ggplot(ordercond_counts_df, aes(x = Value, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = Frequency), vjust = -0.5) +
  labs(title = "Occurrences of Host Orders (Condensed)", x = "Order", y = "Frequency") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),text = element_text(size = 18)) 

### FIX font sizes

#######

# merege with list of seqeunces for empirical tree
emptree_taxonlist <-  read.csv("scripts_metadata_files/taxa_empirical_treeset.txt", sep = '\t')

h5_emptree <- left_join(emptree_taxonlist, h5_data_dedup_sp_oc_av, by = c("Taxa" = "Seqname"), relationship ="many-to-many")

h5_emptree$flyway_domwild <- paste(h5_emptree$Domestic_Status, h5_emptree$flyway, sep = "_")
table(h5_emptree$flyway_domwild)

write.csv(h5_emptree, file = "h5_emptree-metaparseclean.csv")





