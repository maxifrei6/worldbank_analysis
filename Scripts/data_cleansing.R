### This program cleans the original data sets and saves modified data sets for future
### utilization.

## OUTLINE
# Data preparation
  # Load data
  # Adjustments to correct data inconsistencies for CO2 data set
  # Initial NA removal
  # Data overview

# Data modification and merging
  # Renaming and selecting columns
  # Removing metadata rows at end of data set
  # Merging data sets

# Final steps 
  # Save cleaned data sets for future use

library(here)
library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(checkmate)
source(here("Scripts", "functions.R"))

# Read the CSV files using 'here' for relative paths.
data_geo <- read.csv(here("Data", "Raw", "Worldbank2.csv"),
                     header = TRUE, sep = ",", na.strings = "")
data_co2 <- read.csv(here("Data", "Raw", "Co2_emi_WB.csv"),
                     header = TRUE, sep = ";", na.strings = "")

# Remove all dots in the CO2 data entries.
data_co2 <- data_co2 %>%
  mutate(across(
    .cols = matches("^X(2000|20[0-1][0-9]|2020|2021)..YR(2000|20[0-1][0-9]|2020|2021)\\.$"),
    .fns = ~ gsub(pattern = "\\.", replacement = "", .)))

# If the data entries start with zero, the remaining digits behind the zero are decimal places.
# Therefore, put a dot behind the zero if it is a leading zero.
data_co2 <- data_co2 %>%
  mutate(across(
    .cols = matches("^X(2000|20[0-1][0-9]|2020|2021)..YR(2000|20[0-1][0-9]|2020|2021)\\.$"),
    .fns = ~ gsub(pattern = "^0", replacement = "0.", .)))

# For all other data entries, insert a dot after fourth digit from the right if number has more
# than four digits or after third digit from the right if number has at most four digits, thereby
# converting it to decimal number.
data_co2 <- data_co2 %>%
  mutate(across(
    .cols = matches("^X(2000|20[0-1][0-9]|2020|2021)..YR(2000|20[0-1][0-9]|2020|2021)\\.$"),
    .fns = ~ {
      # Apply transformation only to entries that do not start with zero.
      ifelse(substr(., 1, 1) != "0",
             ifelse(nchar(.) > 4,
                    # Insert the dot after the 4th digit and convert to numeric.
                    as.numeric(substr(., 1, nchar(.) - 4)) +
                      as.numeric(substr(., nchar(.) - 3, nchar(.))) / 10000,
                    # Insert the dot after the 3rd digit and convert to numeric.
                    as.numeric(substr(., 1, nchar(.) - 3)) +
                      as.numeric(substr(., nchar(.) - 2, nchar(.))) / 1000),
             # Keep entries starting with zero unchanged.
             as.numeric(.))
      }))

str(data_geo)
str(data_co2)
head(data_co2)

# Read the .xlsx file using 'here'.
data_sociometrics <-
  read_excel(here("Data", "Raw", "Worldbank1.xlsx"), sheet = 1, col_names = TRUE, na = "..")

# TODO: CREATE DICTIONARY/LIST FROM SHEET 2 TO MATCH ABBREVIATED METRICS WITH DESCRIPTION

# Delete columns or rows if they consist of only NAs. Apply 'remove_NA' to all interested data sets
# using 'list2env' to automatically update the inputted variables without manual assignment.
list2env(remove_NA(data_geo, data_co2, data_sociometrics), envir = .GlobalEnv)

# Get an overview of the data sets.
str(data_geo)
summary(data_geo)
str(data_sociometrics)
summary(data_sociometrics)
str(data_co2)
summary(data_co2)

# Modification to data sets, simplifying their joining.
# Column names for years aligning in both data sets.
colnames(data_geo) <- gsub(pattern = "X([[:digit:]]{4})..YR[[:digit:]]{4}.",
                          replacement = "\\1", colnames(data_geo))
colnames(data_co2) <- gsub(pattern = "X([[:digit:]]{4})..YR[[:digit:]]{4}.",
                          replacement = "\\1", colnames(data_co2))
colnames(data_sociometrics) <- gsub(pattern = "([[:digit:]]{4}) \\[YR\\1\\]",
                               replacement = "\\1", colnames(data_sociometrics))

# Remaining column names in data_geo and data_co2 aligning with data_sociometrics, i.e. replacing
# dot between words with space. Apply 'align_colnames' to all interested data sets using 'list2env'
# to automatically update the inputted variables without manual assignment.
list2env(align_colnames(data_geo, data_co2, data_sociometrics), envir = .GlobalEnv)

# Removing metadata rows by applying 'remove_metadata' to all interested data sets using 'list2env'
# to automatically update the inputted variables without manual assignment.
list2env(remove_metadata(metadata_source = "Data from database: World Development Indicators",
                         metadata_time = "Last Updated: 10/24/2024",
                         data_geo, data_co2, data_sociometrics), envir = .GlobalEnv)

# Get an overview of the adjusted data sets.
str(data_geo)
str(data_sociometrics)
str(data_co2)

# Define the translation mapping for the 'Country Name' column to German for plot visualization
# purposes at later point.
translation_mapping <- c("Bangladesh" = "Bangladesch",
                         "Bolivia" = "Bolivien",
                         "Brazil" = "Brasilien",
                         "Cambodia" = "Kambodscha",
                         "Chad" = "Tschad",
                         "Czechia" = "Tschechien",
                         "Finland" = "Finnland",
                         "India" = "Indien",
                         "Kazakhstan" = "Kasachstan",
                         "New Zealand" = "Neuseeland",
                         "Qatar" = "Katar",
                         "Russian Federation" = "Russland",
                         "Tanzania" = "Tansania",
                         "United Kingdom" = "Vereinigtes Königreich",
                         "United States" = "USA",
                         "Viet Nam" = "Vietnam")

# Change the 'Country Name' column by applying 'translate' to all interested data sets using
# 'list2env' to automatically update the inputted variables without manual assignment.
list2env(translate(translation_mapping, "Country Name", data_geo, data_co2, data_sociometrics),
         envir = .GlobalEnv)

# Join the data sets together.
data_pregrouped <- full_join(data_geo, data_sociometrics, by = colnames(data_geo))

# Sort the rows alphabetically according to the countries' names again.
data_pregrouped <- data_pregrouped %>% arrange(`Country Name`)

# Create the RDS before deleting averages and save the data set using 'here'.
saveRDS(data_pregrouped, file = here("Data", "Processed", "data_pregrouped.RDS"))

# Delete column 'average' and 'Series Name' to avoid data type issues and ensure all year columns
# are numeric to avoid merge errors by applying 'prepare_join' to all interested data sets using
# 'list2env' to automatically update the inputted variables without manual assignment.
list2env(prepare_join(data_geo, data_co2, data_sociometrics), envir = .GlobalEnv)

# Merge both data sets into one full data set containing all information.
data_merged <- full_join(data_geo, data_sociometrics, by = colnames(data_geo)) %>%
  full_join(data_co2, by = colnames(data_geo))

# Sort the joined data set by 'Country Name'.
data_merged <- sort_by(data_merged, data_merged$`Country Name`)

# Columns with Years in column 'Year'.
data_merged <- data_merged %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "Year",
    values_to = "Value",
    values_drop_na = FALSE
  )

# Variables in 'Series Code' as columns.
data_merged <- data_merged %>%
  pivot_wider(
    id_cols = c(`Country Name`, `Country Code`, Year),
    names_from = `Series Code`,
    values_from = Value
  )

# Save the merged data set using 'here' for the file path.
saveRDS(data_merged, file = here("Data", "Processed", "data_merged.RDS"))
