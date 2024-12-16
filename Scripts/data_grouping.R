library(here)
library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(checkmate)
source(here("Scripts", "functions.R"))


##############################################################################################################
# Load the preprocessed dataset using `here`
data_processed <- readRDS(here("Data", "Processed", "data_pregrouped.RDS"))


# 1. Grouping for the question regarding agriculture - by surface area:

data_grouped_agr <- group_data(df = data_processed,
                               grouping_by = "AG.SRF.TOTL.K2",
                               column_new = "surfaceArea",
                               quantile_labels = c("Very Small", "Small", "Moderate", "Large", "Very Large"))

# 2. Relevant for HIV evaluation
# 2.1 Grouping by alcohol consumption 
data_grouped_alc <- group_data(df = data_processed, grouping_by = "SH.ALC.PCAP.LI", column_new = "alc",
                               quantile_labels = c("Very Low", "Low", "Medium", "High", "Very High"))

# 2.2 Grouping by basic education of the labor force
data_grouped_edu <- group_data(df = data_processed, grouping_by = "SL.TLF.BASC.ZS",
                               column_new = "education", quantile_labels = 
                                 c("Very Low", "Low", "Medium", "High", "Very High"))

# 3. Relevant for tobacco evaluation - grouping by tobacco usage
data_grouped_tob <- group_data(df = data_processed, grouping_by = "SH.PRV.SMOK",
                               column_new = "tob_usage", quantile_labels = 
                                 c("Very Low", "Low", "Medium", "High", "Very High"))

# 4. Grouping by population size
data_grouped_pop <- group_data(df = data_processed, grouping_by = "SP.POP.TOTL",
                               column_new = "population_size", quantile_labels = 
                                 c("Low", "Medium", "High"))

# 5. Grouping by population density
data_grouped_pop_density <- group_data(df = data_processed, grouping_by = "EN.POP.DNST",
                               column_new = "population_density", quantile_labels = 
                                 c("Low", "Medium", "High"))

# 5. Grouping by income levels
data_grouped_income_levels <- group_data(df = data_processed, grouping_by = "NY.ADJ.NNTY.PC.CD",
                               column_new = "income_level", quantile_labels = 
                                 c("Low", "Medium", "High"))

# Merge all groups
# TODO: all data groupings merge in one session with electricity, education, hiv, tobacco and agriculture. 


# merge all groups
data_grouped <- left_join(data_grouped_alc,
                          data_grouped_edu,
                          by = c("Country Name", "Country Code")) %>%
  left_join(data_grouped_tob, by = c("Country Name", "Country Code")) %>% 
  left_join(data_grouped_agr, by = c("Country Name", "Country Code")) %>% 
  left_join(data_grouped_pop, by = c("Country Name", "Country Code")) %>%
  left_join(data_grouped_pop_density, by = c("Country Name", "Country Code")) %>% 
  left_join(data_grouped_income_levels, by = c("Country Name", "Country Code"))

# Save the grouped data using `here`
saveRDS(data_grouped, file = here("Data", "Processed", "data_grouped.RDS"))
##############################################################################################################