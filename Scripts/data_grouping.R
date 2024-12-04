library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(checkmate)
source("Scripts/functions.R")

## 1. Grouping by population criteria
# Load and filter the data
data_grouped <- readRDS("Data/Processed/data_pregrouped.RDS") %>%
  select(`Country Name`,
         `Country Code`,
         `Series Name`,
         `Series Code`,
         `average`)

data_grouped_pop <- data_grouped %>%
  filter(`Series Code` == "SP.POP.TOTL")

# Calculate the 1/3 and 2/3 quantiles of the 'average' column
quantiles <- quantile(data_grouped_pop$average,
                      probs = c(1 / 3, 2 / 3),
                      na.rm = TRUE)

# Add new column 'population_size' as an ordered factor
data_grouped_pop <- data_grouped_pop %>%
  mutate(
    population_size = case_when(
      average <= quantiles[1] ~ "Low",
      average <= quantiles[2] ~ "Medium",
      TRUE ~ "High"
    ),
    # Convert to ordered factor with levels High > Medium > Low
    population_size = factor(
      population_size,
      levels = c("High", "Medium", "Low"),
      ordered = TRUE
    )
  )

# same for pop_density
data_grouped_pop_density <- data_grouped %>%
  filter(`Series Code` == "EN.POP.DNST")

# Calculate the 1/3 and 2/3 quantiles of the 'average' column
quantiles <- quantile(data_grouped_pop_density$average,
                      probs = c(1 / 3, 2 / 3),
                      na.rm = TRUE)

print(quantiles)

# Add new column 'population_density' as an ordered factor
data_grouped_pop_density <- data_grouped_pop_density %>%
  mutate(
    population_density = case_when(
      average <= quantiles[1] ~ "Low",
      average <= quantiles[2] ~ "Medium",
      TRUE ~ "High"
    ),
    # Convert to ordered factor with levels High > Medium > Low
    population_density = factor(
      population_density,
      levels = c("High", "Medium", "Low"),
      ordered = TRUE
    )
  )

# same for income
data_grouped_income_levels <- data_grouped %>%
  filter(`Series Code` == "NY.ADJ.NNTY.PC.CD")

# Calculate the 1/3 and 2/3 quantiles of the 'average' column
quantiles <- quantile(data_grouped_income_levels$average,
                      probs = c(1 / 3, 2 / 3),
                      na.rm = TRUE)

print(quantiles)

# Add new column 'income_level' as an ordered factor
data_grouped_income_levels <- data_grouped_income_levels %>%
  mutate(
    income_level = case_when(
      average <= quantiles[1] ~ "Low",
      average <= quantiles[2] ~ "Medium",
      TRUE ~ "High"
    ),
    # Convert to ordered factor with levels High > Medium > Low
    income_level = factor(
      income_level,
      levels = c("High", "Medium", "Low"),
      ordered = TRUE
    )
  )

# select relevant columns
data_grouped_pop <- data_grouped_pop %>%
  select(`Country Name`, `Country Code`, `population_size`)
data_grouped_pop_density <- data_grouped_pop_density %>%
  select(`Country Name`, `Country Code`, `population_density`)
data_grouped_income_levels <- data_grouped_income_levels %>%
  select(`Country Name`, `Country Code`, `income_level`)

# merge all groups
data_grouped <- left_join(data_grouped_pop,
                          data_grouped_pop_density,
                          by = c("Country Name", "Country Code")) %>%
  left_join(data_grouped_income_levels,
            by = c("Country Name", "Country Code"))

# save as RDS
saveRDS(data_grouped, file = "Data/Processed/data_grouped.RDS")


## 2. Relevant for HIV evaluation
# 2.1 Grouping by alcohol consumption
# grouping over quantiles to reach evenly distribution
# Problem: Country has a large variance of alcohol consumption values
# -> grouping over average could be a problem!
data_grouped_hiv <- readRDS(
  "C:/Users/Leonie/Documents/Studium/3. Semester/Statsoft/worldbank_analysis/Data/Processed/data_pregrouped.RDS"
) %>%
  select(`Country Name`,
         `Country Code`,
         `Series Name`,
         `Series Code`,
         `average`)

data_grouped_alc <- data_grouped_hiv %>%
  filter(`Series Code` == c("SH.ALC.PCAP.LI"))

# remove rows with NA's in average column
data_grouped_alc <- data_grouped_alc %>%
  filter(is.na(average) == FALSE)

# Calculate the 0.2, 0.4, 0.6, 0.8 quantiles of the 'average' column
quantiles <- quantile(data_grouped_alc$average,
                      probs = c(1 / 5, 2 / 5, 3 / 5, 4 / 5),
                      na.rm = TRUE)

# Add new column 'alc_consumption' as an ordered factor
data_grouped_alc <- data_grouped_alc %>%
  mutate(
    alc_consumption = case_when(
      average <= quantiles[1] ~ "Very Low",
      average <= quantiles[2] ~ "Low",
      average <= quantiles[3] ~ "Medium",
      average <= quantiles[4] ~ "High",
      TRUE ~ "Very High"
    ),
    # Convert to ordered factor with levels Very High > High > Medium > Low > Very Low
    alc_consumption = factor(
      alc_consumption,
      levels = c("Very High", "High", "Medium", "Low", "Very Low"),
      ordered = TRUE
    )
  )

# 2.2 Grouping by basic education of the labor force
# grouping over quantiles to reach evenly distribution
# Problem: if country has a large variance of basic education values
# -> grouping over average could be a problem!
data_grouped_edu <- data_grouped_hiv %>%
  filter(`Series Code` == c("SL.TLF.BASC.ZS"))

# remove rows with NA's in average column
data_grouped_edu <- data_grouped_edu %>%
  filter(is.na(average) == FALSE)

# Calculate the 0.2, 0.4, 0.6, 0.8 quantiles of the 'average' column
quantiles <- quantile(data_grouped_edu$average,
                      probs = c(1 / 5, 2 / 5, 3 / 5, 4 / 5),
                      na.rm = TRUE)

# Add new column 'education' as an ordered factor
data_grouped_edu <- data_grouped_edu %>%
  mutate(
    edu_basic = case_when(
      average <= quantiles[1] ~ "Very Low",
      average <= quantiles[2] ~ "Low",
      average <= quantiles[3] ~ "Medium",
      average <= quantiles[4] ~ "High",
      TRUE ~ "Very High"
    ),
    # Convert to ordered factor with levels Very High > High > Medium > Low > Very Low
    edu_basic = factor(
      edu_basic,
      levels = c("Very High", "High", "Medium", "Low", "Very Low"),
      ordered = TRUE
    )
  )

# select relevant columns
data_grouped_alc <- data_grouped_alc %>%
  select(`Country Name`, `Country Code`, `alc_consumption`)
data_grouped_edu <- data_grouped_edu %>%
  select(`Country Name`, `Country Code`, `edu_basic`)

# merge all groups
data_grouped <- full_join(data_grouped_alc,
                          data_grouped_edu,
                          by = c("Country Name", "Country Code")) %>%
  full_join(data_grouped, by = c("Country Name", "Country Code"))

# save as RDS
saveRDS(data_grouped, file = "Data/Processed/data_grouped.RDS")

## 3. Relevant for tobacco evaluation (leonie)
# Grouping by prevalence of current tobacco use (% of adults)
# grouping over quantiles to reach evenly distribution
# Problem: Country has a large variance of alcohol consumption values over the years
# -> grouping over average could be a problem!
data_grouped_tob <- readRDS(
  "C:/Users/Leonie/Documents/Studium/3. Semester/Statsoft/worldbank_analysis/Data/Processed/data_pregrouped.RDS"
) %>%
  select(`Country Name`,
         `Country Code`,
         `Series Name`,
         `Series Code`,
         `average`)

data_grouped_tob <- data_grouped_tob %>%
  filter(`Series Code` == c("SH.PRV.SMOK"))

# remove rows with NA's in average column
data_grouped_tob <- data_grouped_tob %>%
  filter(is.na(average) == FALSE)

# Calculate the 0.2, 0.4, 0.6, 0.8 quantiles of the 'average' column
quantiles <- quantile(data_grouped_tob$average,
                      probs = c(1 / 5, 2 / 5, 3 / 5, 4 / 5),
                      na.rm = TRUE)

# Add new column 'tobacco_usage' as an ordered factor
data_grouped_tobacco_usage <- data_grouped_tob %>%
  mutate(
    tobacco_usage = case_when(
      average <= quantiles[1] ~ "Very Low",
      average <= quantiles[2] ~ "Low",
      average <= quantiles[3] ~ "Medium",
      average <= quantiles[4] ~ "High",
      TRUE ~ "Very High"
    ),
    # Convert to ordered factor with levels Very High > High > Medium > Low > Very Low
    tobacco_usage = factor(
      tobacco_usage,
      levels = c("Very High", "High", "Medium", "Low", "Very Low"),
      ordered = TRUE
    )
  )

# select relevant columns
data_grouped_tobacco_usage <- data_grouped_tobacco_usage %>%
  select(`Country Name`, `Country Code`, `tobacco_usage`)

# merge all groups
data_grouped <- full_join(data_grouped_tobacco_usage, data_grouped,
                          by = c("Country Name", "Country Code"))

# save as RDS
saveRDS(data_grouped, file = "Data/Processed/data_grouped.RDS")




##############################################################################################################
# Grouping for the question regarding agriculture - by surface area:
# Start with the grouped dataset
data_grouped_agr <- readRDS("Data/Processed/data_pregrouped.RDS")

data_grouped_agr <- group_data(df = data_grouped_agr,
                               grouping_by = "AG.SRF.TOTL.K2",
                               column_new = "surfaceArea",
                               quantile_labels = c("Very small", "Small", "Moderate", "Large", "Very Large"))

# Merge all groups
# TODO: all data groupings merge in one session with electricity, education, hiv, tobacco and agriculture.

# Save as RDS
# TODO: save one data_grouped file at the end after all single groups have been merged.
##############################################################################################################