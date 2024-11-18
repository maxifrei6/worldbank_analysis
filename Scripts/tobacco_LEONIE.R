# Tobacco_LEONIE

library(dplyr)
library(ggplot2)
# How does GDP per capita relate to the prevalence of current tobacco use 
# (% of adults)?
#     1)  in each Country: every year a dot (problem: NA`s)
#     2)  whole data: mean of each country one dot

# Variables:  GDP per capita, PPP (constant 2021 international $)
#             NY.GDP.PCAP.PP.KD

#             Prevalence of current tobacco use (% of adults)
#             SH.PRV.SMOK

# dotplot:  x-Achse:  GDP per capita (BIP pro Kopf)
#           y-Achse:  Prevalence of current tobacco use (% of adults)
#           colour:   Country
#                     -> grouped? -> mean?

# Check how many NA`s are in the data to be used
# No NA`s in GDP per capita
sum(is.na(full_data$NY.GDP.PCAP.PP.KD))
# 382 NA`s in Prevalence of current tobacco use
sum(is.na(full_data$SH.PRV.SMOK))

# Set up for dataframe
tobacco_df <- full_data %>%
  select(`Country Name`,
         `Country Code`,
         Year,
         NY.GDP.PCAP.PP.KD,
         SH.PRV.SMOK)
View(tobacco_df)

# Example with Afghanistan option 2)
mean_gdp_afg <- mean(unlist(tobacco_df[1:22, "NY.GDP.PCAP.PP.KD"]))
mean_tob_afg <- mean(unlist(tobacco_df[1:22, "SH.PRV.SMOK"]), na.rm = TRUE)
