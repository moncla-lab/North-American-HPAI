# process duplictes in Genbank for HPAI
# Some sequences collected in 2022 and shared on GISAID  were re-uplouded with differernt IDs 
# example:
# A/eagle/Georgia/USDA-XXXXX-XXX/2023, A/eagle/Georgia/22-XXXXX-XXX/2023, and A/eagle/Georgia/USDA-XXXXX-XXX-original/2023
# all represent the same sequence
# This script breaks down strain name and deduplicates on the IDs taking the first occurence of the isolate
# after deuplication 407 sequences were removed

library(tidyverse)
# set working directory and read in gisaid data
setwd("~/Desktop")
all_northamerican <- read.csv("gisaidmeta.tsv", sep = '\t')


all_northamerican <- all_northamerican %>%
  mutate(usda_22_id = str_split(Isolate_Name, "/", simplify = TRUE)[, 4])

# sort by date
all_northamerican <- all_northamerican %>%
  arrange(Collection_Date)

# standardize ids (remove -original) and replace USDA with 22 (this is most likely the year of the isolate, some IDs have 23-, none of the USDA- isolates are form 2023)
all_northamerican <- all_northamerican %>%
  mutate(usda_22_id = gsub("-original", "", usda_22_id),  # Remove "-original"
         usda_22_id = gsub("USDA", "22", usda_22_id))      # Replace "USDA" with "22"

# duplicates to a new dataframe

duplicates_dataframe <- all_northamerican %>%
  group_by(!!sym("usda_22_id")) %>%
  filter(n() > 1) %>%
  ungroup()

duplicates_dataframe_ids <- duplicates_dataframe %>%
  group_by(usda_22_id) %>%
  summarise(strains = toString(Isolate_Name))

write.csv(duplicates_dataframe_ids, "duplicates_dataframe_ids.csv")

# drop duplicates retaining the first occurence based on the usda_22_id column
all_northamerican <- all_northamerican %>%
  distinct(usda_22_id, .keep_all = TRUE)


# If merged HA sequences (convert fasta to tab file and merge on isolate name)
# drop identical seequences if they occur on the same day 
# this results in 675  sequences being dropped 

all_northamerican <- all_northamerican %>%
  distinct(Collection_Date, HA_seq, .keep_all = TRUE)

# resulting dataset is 1818 sequences. (as of 2023-11-01)
