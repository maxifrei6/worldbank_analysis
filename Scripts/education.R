# Load necessary libraries
library(dplyr)
library(ggplot2)

# Assuming `extended_data` is the extended dataset with categories

# Check if countries with higher government debt (% of GDP) have a lower percentage of labor force with basic education
# Also check if countries with higher basic education levels maintain lower pupil-teacher ratios

# Filter relevant columns and remove rows with NA values in the key columns, include the categories
education_debt_data <- extended_data %>%
  select(`Country Name`, Year, GC.DOD.TOTL.GD.ZS, SL.TLF.BASC.ZS, Population_Category, Density_Category) %>%
  filter(!is.na(GC.DOD.TOTL.GD.ZS) & !is.na(SL.TLF.BASC.ZS))

# Plot 1: Relationship between Government Debt (% of GDP) and Labor Force with Basic Education with Population_Category Facets
ggplot(education_debt_data, aes(x = GC.DOD.TOTL.GD.ZS, y = SL.TLF.BASC.ZS)) +
  geom_point(alpha = 0.6) +
  labs(title = "Government Debt vs. Labor Force with Basic Education",
       x = "Government Debt (% of GDP)",
       y = "Labor Force with Basic Education (%)") +
  facet_wrap(~ Population_Category) +
  theme_minimal()

# Plot 2: Relationship between Government Debt (% of GDP) and Labor Force with Basic Education with Density_Category Facets
ggplot(education_debt_data, aes(x = GC.DOD.TOTL.GD.ZS, y = SL.TLF.BASC.ZS)) +
  geom_point(alpha = 0.6) +
  labs(title = "Government Debt vs. Labor Force with Basic Education",
       x = "Government Debt (% of GDP)",
       y = "Labor Force with Basic Education (%)") +
  facet_wrap(~ Density_Category) +
  theme_minimal()

# Additional analysis: Relationship between Basic Education and Tertiary Pupil-Teacher Ratio with Population_Category and Density_Category

# Filter relevant columns and remove rows with NA values in the key columns, include the categories
education_quality_data <- extended_data %>%
  select(`Country Name`, Year, SL.TLF.BASC.ZS, SE.TER.ENRL.TC.ZS, Population_Category, Density_Category) %>%
  filter(!is.na(SL.TLF.BASC.ZS) & !is.na(SE.TER.ENRL.TC.ZS))

# Plot 3: Relationship between Basic Education and Tertiary Pupil-Teacher Ratio with Population_Category Facets
ggplot(education_quality_data, aes(x = SL.TLF.BASC.ZS, y = SE.TER.ENRL.TC.ZS)) +
  geom_point(alpha = 0.6) +
  labs(title = "Basic Education vs. Tertiary Pupil-Teacher Ratio",
       x = "Labor Force with Basic Education (%)",
       y = "Tertiary Pupil-Teacher Ratio") +
  facet_wrap(~ Population_Category) +
  theme_minimal()

# Plot 4: Relationship between Basic Education and Tertiary Pupil-Teacher Ratio with Density_Category Facets
ggplot(education_quality_data, aes(x = SL.TLF.BASC.ZS, y = SE.TER.ENRL.TC.ZS)) +
  geom_point(alpha = 0.6) +
  labs(title = "Basic Education vs. Tertiary Pupil-Teacher Ratio",
       x = "Labor Force with Basic Education (%)",
       y = "Tertiary Pupil-Teacher Ratio") +
  facet_wrap(~ Density_Category) +
  theme_minimal()