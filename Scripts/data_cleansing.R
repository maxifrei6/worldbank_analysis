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

# Read the csv file from Data/Raw folder
geodata <- read.csv("Data/Raw/Worldbank2.csv", header = TRUE, sep = ",", na.strings = "")

# Read the xlsx file from Data/Row folder
sociometrics <- read_excel("Data/Raw/Worldbank1.xlsx", sheet = 1, col_names = TRUE, na = "..")

# Delete columns or rows if they are all NAs.
# Apply the remove_NAs function to all interested datasets
geodata <- remove_NA(geodata)
sociometrics <- remove_NA(sociometrics)

# Get an overview of the datasets
str(geodata)
summary(geodata)
str(sociometrics)
summary(sociometrics)

# Modification to datasets, simplifying their joining
# Colnames for years aligning in both datasets
colnames(geodata) <- gsub(pattern = "X([[:digit:]]{4})..YR[[:digit:]]{4}.",
                          replacement = "\\1", colnames(geodata))
colnames(sociometrics) <- gsub(pattern = "([[:digit:]]{4}) \\[YR\\1\\]",
                               replacement = "\\1", colnames(sociometrics))
# Remaining colnames in geodata aligning with sociometrics, i.e. replacing dot between words with space
geodata <- geodata %>%
  mutate(across(where(is.character))) %>%
  rename_with(~ gsub(pattern = "\\.", replacement = " ", .))

# Removing metadata rows
metadata_source <- "Data from database: World Development Indicators" 
metadata_time <- "Last Updated: 10/24/2024"
geodata <- geodata[geodata$`Country Name` != metadata_source &
                     geodata$`Country Name` != metadata_time, ]
sociometrics <- sociometrics[sociometrics$`Country Name` != metadata_source &
                               sociometrics$`Country Name` != metadata_time, ]



# TODO (???)



# Merge both datasets into one full dataset containing all information
full_data <- full_join(geodata, sociometrics, by = colnames(geodata))

# Sort the joined dataset by Country Name
full_data <- sort_by(full_data, full_data$`Country Name`)

# Change the row numbering according to new sorting order 
# TODO