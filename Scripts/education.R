# Load necessary libraries
library(dplyr)
library(ggplot2)

# Placeholder for extensive computations to facilitate markdown analysis file

data_merged <- readRDS("/Users/maximilian/Desktop/wb_local/Data/Processed/data_merged.RDS")
data_grouped <- readRDS("/Users/maximilian/Desktop/wb_local/Data/Processed/data_grouped.RDS")
data_education <- left_join(data_merged, data_grouped, by = c("Country Name", "Country Code"))

# Add new column 'decade' as an ordered factor
data_education <- data_education %>%
  mutate(
    decade = case_when(
      Year <= 2009 ~ "2000s", # 2000 - 2009
      Year <= 2019 ~ "2010s", # 2010 - 2019
      TRUE ~ "2020s"          # 2020 and later
    ),
    # Convert 'decade' to an ordered factor with levels 2020s > 2010s > 2000s
    decade = factor(
      decade,
      levels = c("2000s", "2010s", "2020s"),
      ordered = TRUE
    )
  )
