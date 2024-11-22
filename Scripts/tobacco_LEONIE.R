# Tobacco_LEONIE

library(dplyr)
library(ggplot2)
## How does GDP per capita relate to the prevalence of current tobacco use 
# (% of adults)?
#   Options of completing this task:    
#     1)  in each Country: every year a dot (problem: NA's)
#     2)  whole data: mean of each country one dot
#     3)  last data used for evaluation -> Year 2021
#     4)  facetting by year
#     5)  Bar chart comparing GDP to tabacco usage
#     6)  Facetting by country, x-axis = Year, y-axis1 = GDP, y-axis2 = tobacco usage
#     7)  Facetting by country, x-axis = Year, y-axis = tobacco usage

# Variables:  GDP per capita, PPP (constant 2021 international $)
#             NY.GDP.PCAP.PP.KD

#             Prevalence of current tobacco use (% of adults)
#             SH.PRV.SMOK

# dotplot:  x-Achse:  GDP per capita (BIP pro Kopf)
#           y-Achse:  Prevalence of current tobacco use (% of adults)
#           colour:   Country
#                     -> grouped? -> mean?

## Set up for dataframe
tobacco_df <- full_data %>%
  select(`Country Name`,
         `Country Code`,
         Year,
         NY.GDP.PCAP.PP.KD,
         SH.PRV.SMOK)
View(tobacco_df)
str(tobacco_df$NY.GDP.PCAP.PP.KD)
str(tobacco_df$SH.PRV.SMOK)

## Check how many NA's are in the data to be used
# No NA`s in GDP per capita
sum(is.na(full_data$NY.GDP.PCAP.PP.KD))
# 382 NA`s in Prevalence of current tobacco use
sum(is.na(full_data$SH.PRV.SMOK))

# Check how many NA's in tabacco each Year
years_data_tobacco <- tobacco_df %>% 
  group_by(Year) %>% 
  reframe(num_na = sum(is.na(SH.PRV.SMOK))) %>% 
  filter(num_na < 25)
# Knowing there are only 25 countries to investigate, we only have data of the 
# Years 2000, 2005, 2010, 1015, 1018, 2019 and 2020 on percentage of tabacco 
# usage in the adult population of all 24 countries (excluding Aruba).

# (first have to solve grouping of countries generally before performing this task:)
# Exclude Aruba from the tabacco_df

## data in year:
tobacco_year_df <- function(year){
  tobacco_df %>% 
    filter(Year %in% year)
}
# Check number of NA's in tabacco usage variable of Year k
sum(is.na(tobacco_year_df$SH.PRV.SMOK))

## Example with Afghanistan option 2) (just testing)
mean(unlist(tobacco_df[1:22, "NY.GDP.PCAP.PP.KD"]))
mean(unlist(tobacco_df[1:22, "NY.GDP.PCAP.PP.KD"]), na.rm = TRUE)

## Means over Years grouped by country
means_per_country <- tobacco_df %>%
  group_by(`Country Name`) %>%
  summarise(
    mean_gdp = mean(NY.GDP.PCAP.PP.KD),
    mean_tobacco = mean(SH.PRV.SMOK, na.rm = TRUE)
  )
# Checking for NA's in table with means (-> empty columns of specific countries?)
sum(is.na(means_per_country$mean_tobacco))
# -> Aruba

## 2) whole data: mean of each country one dot
# Dotplot: mean over time, grouped by country, correlation between tobacco usage and GDP
ggplot(means_per_country,
       aes(x = mean_gdp, y = mean_tobacco, colour = `Country Name`)) +
  geom_point(size = 2) +
  labs(title = "Correlation of GDP per Capita and tobacco prevalence",
       x = "GDP per Capita in $",
       y = "Prevalence of tobacco use in % of adults",
       colour = "Country")

## 5) Bar chart comparing GDP to tabacco usage
# Get an overview of tobacco variable
# Code for plotting Prevalence tobacco usage grouped by country
means_per_country %>%
  ggplot(aes(x = `Country Name`, y = mean_tobacco)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Prävalenz des Tabakkonsums nach Land", x = "Land", 
       y = "Prävalenz des Tabakkonsums in % der Erwachsenen")
# Code for plotting GDP per Capita grouped by country
tobacco_df %>%
  group_by(`Country Name`) %>%
  summarise(mean_gdp = mean(NY.GDP.PCAP.PP.KD)) %>%
  ggplot(aes(x = `Country Name`, y = mean_gdp)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13)) +
  labs(title = "BIP pro Kopf nach Land", x = "Land", 
       y = "BIP pro Kopf")
# put last two together:
## genius plot try (Skizze am Ipad!) not working :(
# Aktuellste Daten im Jahr 2020
tobacco_2020_df <- tobacco_df %>% 
  filter(Year %in% 2020)
# negative axis of tobacco = SH.PRV.SMOK.NEG
tobacco_2020_df$SH.PRV.SMOK.NEG <- -tobacco_2020_df$SH.PRV.SMOK
ggplot(data = tobacco_2020_df) +
  geom_bar(aes(x = `Country Name`, y = NY.GDP.PCAP.PP.KD), stat = "identity") +
  geom_bar(aes(x = `Country Name`, y = SH.PRV.SMOK.NEG), stat = "identity") +
  labs(
    title = "Tabakkonsum im Vergleich zu BIP",
    x = "Land",
    y = NULL
  ) +
  scale_y_continuous(
    name = "BIP pro Kopf in $",
    sec.axis = sec_axis(~ -., name = "Tabakkonsum in % der Erwachsenen")
  )

# 3)
# Scatterplot der Daten von 2020 (aktuellste) -> Querschnittsdaten
ggplot(tobacco_2020_df, aes(x = NY.GDP.PCAP.PP.KD, y = SH.PRV.SMOK, color = `Country Name`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "BIP pro Kopf in $", y = "Tabakkonsum in % der Erwachsenen", 
       title = "Korrelation zwischen BIP und Tabakkonsum") +
  theme_minimal()

# 7) Scatterplot facettiert, x = Jahr, Entwicklung des Tabakkonsums (macht nicht was ich will)
tobacco_noaruba_df <- tobacco_df[tobacco_df$`Country Name` != "Aruba", ]
ggplot(tobacco_df, aes(x = Year, y = SH.PRV.SMOK)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ `Country Name`) +
  labs(x = "BIP pro Kopf in $", y = "Tabakkonsum in % der Erwachsenen", 
       title = "Korrelation pro Land über die Zeit") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal()

# 6b) extra idee: heatmap der correlation
heatmap_data <- data %>%
  group_by(Country, Year) %>%
  summarise(correlation = cor(GDP_per_capita, Tobacco_use, use = "complete.obs")) %>%
  na.omit()

ggplot(heatmap_data, aes(x = Year, y = Country, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(x = "Jahr", y = "Land", fill = "Korrelation", title = "Korrelation über Länder und Jahre") +
  theme_minimal()
# Have to compromise countries! Make subgroups


# Sprache in Graphen auf deutsch anpassen