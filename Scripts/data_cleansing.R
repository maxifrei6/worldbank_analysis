### This program cleans the original data sets and saves modified data sets for future utilization. ###

## OUTLINE
# Data preparation
  # Load data
  # Initial NA removal
  # Data overview

# Data modification
  # Renaming and selecting columns
  # Removing metadata rows at end of dataset

# TODO (???)

# Data Merging and Cleaning
  # Merging datasets

# TODO

# Final steps 
  # Save cleaned datasets for future use

library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(checkmate)
source("Scripts/functions.R")

# Read the csv files from Data/Raw folder
data_geo <- read.csv("Data/Raw/Worldbank2.csv", header = TRUE, sep = ",", na.strings = "")
data_co2 <- read.csv("Data/Raw/Co2_emi_WB.csv", header = TRUE, sep = ";", na.strings = "")

# Remove only the first instance of a dot if followed by another dot in the number (co2 emissions format x.xxx.xxx)
data_co2 <- data_co2 %>%
  mutate(across(everything(), ~ str_replace(., "(?<=\\d)\\.(?=\\d+\\.\\d*)", "")))

str(data_geo)
str(data_co2)
head(data_co2)

# Read the xlsx file from Data/Raw folder
data_sociometrics <- read_excel("Data/Raw/Worldbank1.xlsx", sheet = 1, col_names = TRUE, na = "..")

# TODO: create dictionary/list from sheet 2 to match abbreviated metrics with description

# Delete columns or rows if they are all NAs.
# Apply the remove_NAs function to all interested datasets
data_geo <- remove_NA(data_geo)
data_sociometrics <- remove_NA(data_sociometrics)
data_co2 <- remove_NA(data_co2)

# Remove duplicate rows if any
data_geo <- distinct(data_geo)
data_sociometrics <- distinct(data_sociometrics)
data_co2 <- distinct(data_co2)

# Get an overview of the datasets
str(data_geo)
summary(data_geo)
str(data_sociometrics)
summary(data_sociometrics)
str(data_co2)
summary(data_co2)

# Modification to datasets, simplifying their joining
# Colnames for years aligning in both datasets
colnames(data_geo) <- gsub(pattern = "X([[:digit:]]{4})..YR[[:digit:]]{4}.",
                          replacement = "\\1", colnames(data_geo))
colnames(data_co2) <- gsub(pattern = "X([[:digit:]]{4})..YR[[:digit:]]{4}.",
                          replacement = "\\1", colnames(data_co2))
colnames(data_sociometrics) <- gsub(pattern = "([[:digit:]]{4}) \\[YR\\1\\]",
                               replacement = "\\1", colnames(data_sociometrics))

# Remaining colnames in data_geo and data_co2 aligning with data_sociometrics,
# i.e. replacing dot between words with space
data_geo <- data_geo %>%
  mutate(across(where(is.character))) %>%
  rename_with(~ gsub(pattern = "\\.", replacement = " ", .))
data_co2 <- data_co2 %>%
  mutate(across(where(is.character))) %>%
  rename_with(~ gsub(pattern = "\\.", replacement = " ", .))

# Removing metadata rows
metadata_source <- "Data from database: World Development Indicators" 
metadata_time <- "Last Updated: 10/24/2024"
data_geo <- data_geo[data_geo$`Country Name` != metadata_source &
                     data_geo$`Country Name` != metadata_time, ]
data_co2 <- data_co2[data_co2$`Country Name` != metadata_source &
                     data_co2$`Country Name` != metadata_time, ]
data_sociometrics <- data_sociometrics[data_sociometrics$`Country Name` != metadata_source &
                               data_sociometrics$`Country Name` != metadata_time, ]



# TODO (???)
str(data_geo)
str(data_sociometrics)
str(data_co2)

# Delete column "average" and "Series Name" to avoid data type issues
data_geo <- data_geo %>%
  select(c(-average, -`Series Name`))
data_sociometrics <- data_sociometrics %>%
  select(c(-average, -`Series Name`))
data_co2 <- data_co2 %>%
  select(c(-average, -`Series Name`))

# Ensure all year columns  are numeric to avoid merge errors
data_geo <- data_geo %>%
  mutate(across(matches("^[0-9]{4}$"), as.numeric))
data_sociometrics <- data_sociometrics %>%
  mutate(across(matches("^[0-9]{4}$"), as.numeric))
data_co2 <- data_co2 %>%
  mutate(across(matches("^[0-9]{4}$"), as.numeric))

# Merge both datasets into one full dataset containing all information
data_merged <- full_join(data_geo, data_sociometrics, by = colnames(data_geo)) %>%
  full_join(data_co2, by = colnames(data_geo))

# Sort the joined dataset by Country Name
data_merged <- sort_by(data_merged, data_merged$`Country Name`)


# TODO


# Columns with Years in column "Year"
data_merged <- data_merged %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "Year",
    values_to = "Value",
    values_drop_na = FALSE
  )

# Variables in "Series Code" as columns
data_merged <- data_merged %>%
  pivot_wider(
    id_cols = c(`Country Name`, `Country Code`, Year),
    names_from = `Series Code`,
    values_from = Value
  )

# Save the merged dataset for future use in the processed data directory
saveRDS(data_merged, file = "Data/Processed/data_merged.RDS")
