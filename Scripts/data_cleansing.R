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
geodata <- read.csv("Data/Raw/Worldbank2.csv", header = TRUE, sep = ",", na.strings = "")
co2data <- read.csv("Data/Raw/Co2_emi_WB.csv", header = TRUE, sep = ";", na.strings = "")

# Remove only the first instance of a dot if followed by another dot in the number (co2 emissions format x.xxx.xxx)
co2data <- co2data %>%
  mutate(across(everything(), ~ str_replace(., "(?<=\\d)\\.(?=\\d+\\.\\d*)", "")))

str(geodata)
str(co2data)
head(co2data)

# Read the xlsx file from Data/Raw folder
sociometrics <- read_excel("Data/Raw/Worldbank1.xlsx", sheet = 1, col_names = TRUE, na = "..")

# TODO: create dictionary/list from sheet 2 to match abbreviated metrics with description

# Delete columns or rows if they are all NAs.
# Apply the remove_NAs function to all interested datasets
geodata <- remove_NA(geodata)
sociometrics <- remove_NA(sociometrics)
co2data <- remove_NA(co2data)

# Remove duplicate rows if any
geodata <- distinct(geodata)
sociometrics <- distinct(sociometrics)
co2data <- distinct(co2data)

# Get an overview of the datasets
str(geodata)
summary(geodata)
str(sociometrics)
summary(sociometrics)
str(co2data)
summary(co2data)

# Modification to datasets, simplifying their joining
# Colnames for years aligning in both datasets
colnames(geodata) <- gsub(pattern = "X([[:digit:]]{4})..YR[[:digit:]]{4}.",
                          replacement = "\\1", colnames(geodata))
colnames(co2data) <- gsub(pattern = "X([[:digit:]]{4})..YR[[:digit:]]{4}.",
                          replacement = "\\1", colnames(co2data))
colnames(sociometrics) <- gsub(pattern = "([[:digit:]]{4}) \\[YR\\1\\]",
                               replacement = "\\1", colnames(sociometrics))

# Remaining colnames in geodata and co2data aligning with sociometrics, i.e. replacing dot between words with space
geodata <- geodata %>%
  mutate(across(where(is.character))) %>%
  rename_with(~ gsub(pattern = "\\.", replacement = " ", .))
co2data <- co2data %>%
  mutate(across(where(is.character))) %>%
  rename_with(~ gsub(pattern = "\\.", replacement = " ", .))

# Removing metadata rows
metadata_source <- "Data from database: World Development Indicators" 
metadata_time <- "Last Updated: 10/24/2024"
geodata <- geodata[geodata$`Country Name` != metadata_source &
                     geodata$`Country Name` != metadata_time, ]
co2data <- co2data[co2data$`Country Name` != metadata_source &
                     co2data$`Country Name` != metadata_time, ]
sociometrics <- sociometrics[sociometrics$`Country Name` != metadata_source &
                               sociometrics$`Country Name` != metadata_time, ]



# TODO (???)
str(geodata)
str(sociometrics)
str(co2data)

# Delete column "average" and "Series Name" to avoid data type issues
geodata <- geodata %>%
  select(c(-average, -`Series Name`))
sociometrics <- sociometrics %>%
  select(c(-average, -`Series Name`))
co2data <- co2data %>%
  select(c(-average, -`Series Name`))

# Ensure all year columns  are numeric to avoid merge errors
geodata <- geodata %>%
  mutate(across(matches("^[0-9]{4}$"), as.numeric))
sociometrics <- sociometrics %>%
  mutate(across(matches("^[0-9]{4}$"), as.numeric))
co2data <- co2data %>%
  mutate(across(matches("^[0-9]{4}$"), as.numeric))

# Merge both datasets into one full dataset containing all information
full_data <- full_join(geodata, sociometrics, by = colnames(geodata)) %>%
  full_join(co2data, by = colnames(geodata))

# Sort the joined dataset by Country Name
full_data <- sort_by(full_data, full_data$`Country Name`)


# TODO


# Columns with Years in column "Year"
full_data <- full_data %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "Year",
    values_to = "Value",
    values_drop_na = FALSE
  )

# Variables in "Series Code" as columns
full_data <- full_data %>%
  pivot_wider(
    id_cols = c(`Country Name`, `Country Code`, Year),
    names_from = `Series Code`,
    values_from = Value
  )

# Save the merged dataset for future use in the processed data directory
saveRDS(full_data, file = "Data/Processed/full_data.RDS")