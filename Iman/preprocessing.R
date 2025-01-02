library(dplyr)
library(readr)
library(readxl)


###############################################################################
worldbank1 <- read_excel("Data/Raw/Worldbank1.xlsx")

names(worldbank1) <- gsub("^(20\\d{2})\\s.*$", "\\1", names(worldbank1))

worldbank1_elect_clean <- worldbank1 %>%
  filter(`Series Code` %in% c("EG.ELC.ACCS.ZS", "NY.ADJ.NNTY.PC.CD")) %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(
    year = as.numeric(str_extract(year, "\\d{4}")),
    value = na_if(value, ".."),
    value = as.numeric(value)
  ) %>%
  pivot_wider(
    id_cols     = c("Country Name", "Country Code", "year"),
    names_from  = `Series Name`,     # or `names_from = "Series Name"`
    values_from = "value"
  )
################################################################################

worldbank2 <- read_csv("Data/Raw/Worldbank2.csv")

names(worldbank2) <- gsub("^(20\\d{2})\\s.*$", "\\1", names(worldbank2))

worldbank2_elect_clean <- worldbank2 %>%
  filter(`Series Code` %in% c("SP.POP.TOTL", "EN.POP.DNST", "AG.SRF.TOTL.K2")) %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(
    year  = as.numeric(str_extract(year, "\\d{4}")),
    # Convert `value` to character first so `na_if()` can handle ".." properly
    value = as.character(value),
    value = na_if(value, ".."),
    # Then convert back to numeric once ".." is replaced with NA
    value = as.numeric(value)
  ) %>%
  pivot_wider(
    id_cols     = c("Country Name", "Country Code", "year"),
    names_from  = `Series Name`,     # or `names_from = "Series Name"`
    values_from = "value"
  )
################################################################################
co2_emissions <- read_delim("Data/Raw/Co2_emi_WB.csv", delim = ";")

names(co2_emissions) <- gsub("^(20\\d{2})\\s.*$", "\\1", names(co2_emissions))


co2_emissions_elect_clean <- co2_emissions %>%
  filter(`Series Code` %in% c("EN.GHG.CO2.MT.CE.AR5")) %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "year",
    values_to = "value"
  ) %>%
  mutate(
    year  = as.numeric(str_extract(year, "\\d{4}")),
    # Convert `value` to character first so `na_if()` can handle ".." properly
    value = as.character(value),
    value = na_if(value, ".."),
    # Then convert back to numeric once ".." is replaced with NA
    value = as.numeric(value)
  ) %>%
  pivot_wider(
    id_cols     = c("Country Name", "Country Code", "year"),
    names_from  = `Series Name`,    
    values_from = "value"
  )
################################################################################

merged_data_elect <- worldbank1_elect_clean %>%
  inner_join(worldbank2_elect_clean, by = c("Country Name", "Country Code", "year")) %>%
  inner_join(co2_emissions_elect_clean, by = c("Country Name", "Country Code", "year"))


names(merged_data_elect) <- c(
  "Country_Name",
  "Country_Code",
  "Year",
  "Access_to_Electricity",
  "Adjusted_Net_National_Income_Per_Capita",
  "Population",
  "Population_density",
  "Surface_area",
  "CO2_Emissions"
)

################################################################################
write_csv(merged_data_elect, "iman/data/merged_data_elect.csv")