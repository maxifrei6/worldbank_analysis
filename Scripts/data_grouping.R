library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(checkmate)
source("Scripts/functions.R")

# Load and filter the data
data_grouped <- readRDS("Data/Processed/data_pregrouped.RDS") %>%
  select(`Country Name`, `Country Code`, `Series Name`, `Series Code`, `average`)

data_grouped_pop <- data_grouped %>%
  filter(`Series Code` == "SP.POP.TOTL")

# Calculate the 1/3 and 2/3 quantiles of the 'average' column
quantiles <- quantile(data_grouped_pop$average, probs = c(1/3, 2/3), na.rm = TRUE)

# Add new column 'population_size' as an ordered factor
data_grouped_pop <- data_grouped_pop %>%
  mutate(
    population_size = case_when(
      average <= quantiles[1] ~ "Low",
      average <= quantiles[2] ~ "Medium",
      TRUE ~ "High"
    ),
    # Convert to ordered factor with levels High > Medium > Low
    population_size = factor(population_size, levels = c("High", "Medium", "Low"), ordered = TRUE)
  )

# same for pop_density
data_grouped_pop_density <- data_grouped %>%
  filter(`Series Code` == "EN.POP.DNST")

# Calculate the 1/3 and 2/3 quantiles of the 'average' column
quantiles <- quantile(data_grouped_pop_density$average, probs = c(1/3, 2/3), na.rm = TRUE)

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
    population_density = factor(population_density, levels = c("High", "Medium", "Low"), ordered = TRUE)
  )

# same for income
data_grouped_income_levels<- data_grouped %>%
  filter(`Series Code` == "NY.ADJ.NNTY.PC.CD")

# Calculate the 1/3 and 2/3 quantiles of the 'average' column
quantiles <- quantile(data_grouped_income_levels$average, probs = c(1/3, 2/3), na.rm = TRUE)

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
    income_level = factor(income_level, levels = c("High", "Medium", "Low"), ordered = TRUE)
  )

# select relevant columns
data_grouped_pop <- data_grouped_pop %>%
  select(`Country Name`, `Country Code`,`population_size`)
data_grouped_pop_density <- data_grouped_pop_density %>%
  select(`Country Name`, `Country Code`,`population_density`)
data_grouped_income_levels <- data_grouped_income_levels %>%
  select(`Country Name`, `Country Code`,`income_level`)

# merge all groups
data_grouped <- left_join(data_grouped_pop, data_grouped_pop_density, by = c("Country Name", "Country Code")) %>%
  left_join(data_grouped_income_levels, by = c("Country Name", "Country Code"))

# save as RDS
saveRDS(data_grouped, file = "Data/Processed/data_grouped.RDS")
