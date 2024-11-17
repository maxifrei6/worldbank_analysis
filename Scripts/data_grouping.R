library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(checkmate)
source("Scripts/functions.R")

# Step 1: Calculate the mean population and population density for each country
country_means <- full_data %>%
  group_by(`Country Name`) %>%
  summarize(
    Mean_Population = mean(SP.POP.TOTL, na.rm = TRUE),
    Mean_Density = mean(EN.POP.DNST, na.rm = TRUE)
  )

# Step 2: Define thresholds for three buckets for Population and Density
# You can adjust these values based on your data's distribution
population_thresholds <- c(1e7, 5e7)  # Example thresholds for population
density_thresholds <- c(50, 200)      # Example thresholds for density

# Step 3: Categorize each country based on these mean values
country_means <- country_means %>%
  mutate(
    Population_Category = case_when(
      Mean_Population < population_thresholds[1] ~ "Low",
      Mean_Population >= population_thresholds[1] & Mean_Population < population_thresholds[2] ~ "Medium",
      TRUE ~ "High"
    ),
    
    Density_Category = case_when(
      Mean_Density < density_thresholds[1] ~ "Low",
      Mean_Density >= density_thresholds[1] & Mean_Density < density_thresholds[2] ~ "Medium",
      TRUE ~ "High"
    )
  )

# View the result
head(country_means)

# Step 4: Join the categorized data back to the original dataset
extended_data <- full_data %>%
  left_join(country_means %>% select(`Country Name`, Population_Category, Density_Category), 
            by = "Country Name")

# View the first few rows of the extended data
head(extended_data)