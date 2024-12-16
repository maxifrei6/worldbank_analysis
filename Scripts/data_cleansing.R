### This program cleans the original data sets and saves modified data sets for future utilization. ###

## OUTLINE
# Data preparation
  # Load data
  # Adjustments to correct data inconsistencies for CO2-dataset
  # Initial NA removal
  # Data overview

# Data modification and merging
  # Renaming and selecting columns
  # Removing metadata rows at end of dataset
  # Merging datasets

# Final steps 
  # Save cleaned datasets for future use

library(here)
library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(checkmate)
source(here("Scripts", "functions.R"))

# Read the CSV files using 'here' for relative paths
data_geo <- read.csv(here("Data", "Raw", "Worldbank2.csv"), header = TRUE, sep = ",", na.strings = "")
data_co2 <- read.csv(here("Data", "Raw", "Co2_emi_WB.csv"), header = TRUE, sep = ";", na.strings = "")


###################################################################################################################
#################### NEW WAY TO READ IN AND FORMAT THE CO2 EMISSIONS FROM 2024-12-09 ONWARDS: #####################
# Remove all dots in the CO2 data entries
data_co2 <- data_co2 %>%
  mutate(across(.cols = matches("^X(2000|20[0-1][0-9]|2020|2021)..YR(2000|20[0-1][0-9]|2020|2021)\\.$"),
                .fns = ~ gsub(pattern = "\\.", replacement = "", .)))

# If the data entries start with zero, the remaining digits behind the zero are decimal places. Therefore, put a
# dot behind the zero if it is a leading zero.
data_co2 <- data_co2 %>%
  mutate(across(.cols = matches("^X(2000|20[0-1][0-9]|2020|2021)..YR(2000|20[0-1][0-9]|2020|2021)\\.$"),
                .fns = ~ gsub(pattern = "^0", replacement = "0.", .)))

# For all other data entries, insert a dot after fourth digit from the right if number has more than four digits
# or after third digit from the right if number has at most four digits, thereby converting it to decimal number.
data_co2 <- data_co2 %>%
  mutate(across(.cols = matches("^X(2000|20[0-1][0-9]|2020|2021)..YR(2000|20[0-1][0-9]|2020|2021)\\.$"),
                .fns = ~ {
                  # Apply transformation only to entries that do not start with zero
                  ifelse(substr(., 1, 1) != "0",
                    ifelse(nchar(.) > 4,
                      # Insert the dot after the 4th digit and convert to numeric
                      as.numeric(substr(., 1, nchar(.) - 4)) + as.numeric(substr(., nchar(.) - 3, nchar(.))) / 10000,
                      # Insert the dot after the 3rd digit and convert to numeric
                      as.numeric(substr(., 1, nchar(.) - 3)) + as.numeric(substr(., nchar(.) - 2, nchar(.))) / 1000),
                    # Keep entries starting with zero unchanged
                    as.numeric(.))
                }))


# TODO - IDEA: GO THROUGH EACH ROW (=EACH COUNTRY) AND CHECK THE RELATIVE CHANGE FROM YEAR TO YEAR. 
# IF THE CHANGE IS LARGER THAN (E.G.) -50%, THEN YOU ADD A TRAILING ZERO TO THE AS.CHARACTER()-ENTRY.
# AFTERWARDS YOU REMOVE THE CURRENT DOT AND SET A NEW DOT AT THE CORRECTED SPOT FOR 4 DECIMAL PLACES.
###################################################################################################################

str(data_geo)
str(data_co2)
head(data_co2)

# Read the xlsx file using `here`
data_sociometrics <- read_excel(here("Data", "Raw", "Worldbank1.xlsx"), sheet = 1, col_names = TRUE, na = "..")

# TODO: create dictionary/list from sheet 2 to match abbreviated metrics with description

# Delete columns or rows if they are all NAs.
# Apply the remove_NAs function to all interested datasets
data_geo <- remove_NA(data_geo)
data_sociometrics <- remove_NA(data_sociometrics)
data_co2 <- remove_NA(data_co2)

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

# Get an overview of the adjusted datasets
str(data_geo)
str(data_sociometrics)
str(data_co2)

# Create and save RDS before deleting averages
data_pregrouped <- full_join(data_geo, data_sociometrics, by = colnames(data_geo))

# Save the dataset using `here`
saveRDS(data_pregrouped, file = here("Data", "Processed", "data_pregrouped.RDS"))

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

# Save the merged dataset using `here` for the file path
saveRDS(data_merged, file = here("Data", "Processed", "data_merged.RDS"))
