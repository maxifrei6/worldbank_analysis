# Tobacco_LEONIE

library(dplyr)
library(ggplot2)
# How does GDP per capita relate to the prevalence of current tobacco use 
# (% of adults)?
#   Options of completing this task:    
#     1)  in each Country: every year a dot (problem: NA's)
#     2)  whole data: mean of each country one dot
#     3)  last data used for evaluation -> Year 2021
#     4)  facetting by year

# Variables:  GDP per capita, PPP (constant 2021 international $)
#             NY.GDP.PCAP.PP.KD

#             Prevalence of current tobacco use (% of adults)
#             SH.PRV.SMOK

# dotplot:  x-Achse:  GDP per capita (BIP pro Kopf)
#           y-Achse:  Prevalence of current tobacco use (% of adults)
#           colour:   Country
#                     -> grouped? -> mean?

# Check how many NA's are in the data to be used
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
str(tobacco_df$NY.GDP.PCAP.PP.KD)
str(tobacco_df$SH.PRV.SMOK)

# Example with Afghanistan option 2)
mean_gdp_afg <- mean(unlist(tobacco_df[1:22, "NY.GDP.PCAP.PP.KD"]))
mean_tob_afg <- mean(unlist(tobacco_df[1:22, "NY.GDP.PCAP.PP.KD"]), na.rm = TRUE)

# Means over Years grouped by country
means_per_country <- tobacco_df %>%
  group_by(`Country Name`) %>%
  summarise(
    mean_gdp = mean(NY.GDP.PCAP.PP.KD),
    mean_tobacco = mean(NY.GDP.PCAP.PP.KD, na.rm = TRUE)
  )

# Checking for NA's in table with means (-> empty columns of specific countries?)
sum(is.na(means_per_country$mean_tobacco))

# first simple plot for overview
ggplot(means_per_country,
       aes(x = mean_gdp, y = mean_tobacco, colour = `Country Name`)) +
  geom_dotplot() +
  labs(title = "Correlation of GDP per Capita and tobacco prevalence",
       x = "GDP per Capita in $",
       y = "Prevalence of current tobacco use in % of adults",
       colour = "Country")

# y-axis has to be converted!


# Have to compromise countries! Make subgroups


# Chatgpt code for plotting one variable grouped by country:
# df %>%
# group_by(country) %>%
#   summarise(mean_value = mean(variable, na.rm = TRUE)) %>%
#   ggplot(aes(x = country, y = mean_value)) +
#   geom_bar(stat = "identity") +
#   theme_minimal() +
#   labs(title = "Mittelwert der Variable je Land", x = "Land", y = "Mittelwert")