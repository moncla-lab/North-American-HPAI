library(dplyr)
library(stringr)

# script to annotate metadta as domestic/wild

# Load metadata CSV (will have to open the XLXS from GISAID and save the first table as a C)
metadata <- read.csv("gisaidmeta.csv", stringsAsFactors = FALSE)

# convert to lowercase
metadata <- metadata %>%
  mutate(across(where(is.character), ~ tolower(.)))

# create a new column for inferred domesticity
metadata <- metadata %>%
  mutate(
    Domesticity = case_when(
      # from 'Domestic_Status' field if available and not NA
      !is.na(Domestic_Status) & str_detect(Domestic_Status, "domestic") ~ "domestic",
      !is.na(Domestic_Status) & str_detect(Domestic_Status, "wild") ~ "wild",
      
      # from 'Note' field if contains relevant keywords
      is.na(Domestic_Status) & !is.na(Note) & str_detect(Note, "domestic") ~ "domestic",
      is.na(Domestic_Status) & !is.na(Note) & str_detect(Note, "wild") ~ "wild",
      
      # if 'Isolate_Name'. strain name field "domestic"
      str_detect(Isolate_Name, "domestic") ~ "domestic",
      
      # unknown
      TRUE ~ "U"
    )
  )

write.csv(metadata, "newmetadata_withdom.csv", row.names = FALSE)