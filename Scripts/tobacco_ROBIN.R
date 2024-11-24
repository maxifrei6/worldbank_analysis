# Load libraries
# TODO

# Load the "_____" dataset
# TODO
data_full <- readRDS("Data/Processed/data_full.RDS")

# Questions:
# 1. How does GDP per capita relate to the prevalence of current tobacco use (% of adults)?

# Variables:  GDP per capita, PPP (constant 2021 international $)
#             NY.GDP.PCAP.PP.KD

#             Prevalence of current tobacco use (% of adults)
#             SH.PRV.SMOK

# Answer / Visualization of 1.)
# 1.1) Scatter plot of relationship of interested variables facetted years
# 1.2) 

# Filter relevant columns Country Name, Country Code, Year, NY.GDP.PCAP.PP.KD and SH.PRV.SMOK 
tobacco <- full_data %>%
  select(`Country Name`, `Country Code`, Year, NY.GDP.PCAP.PP.KD, SH.PRV.SMOK)

# Check whether any missing values in interested data subset
all(!is.na(tobacco))
sum(is.na(tobacco$NY.GDP.PCAP.PP.KD))
sum(is.na(tobacco$SH.PRV.SMOK))

# Check whether any countries with completely missing data.
tobacco %>% group_by(`Country Name`) %>% count(SH.PRV.SMOK) %>% filter(is.na(SH.PRV.SMOK)) %>% print(n = 25)

# As data to "Aruba" only contains NAs, remove Aruba from dataset
tobacco <- tobacco[tobacco$`Country Name` != "Aruba", ]

# Check which years are given for each of the other countries
tobacco %>% group_by(`Country Name`) %>% group_by(!is.na(SH.PRV.SMOK)) %>% count(Year) %>% print(n = 22)

# Convert column `Year` to data type Integer
tobacco$Year <- as.integer(tobacco$Year)



###################################################################################################################
###################################################################################################################
###################################################################################################################
# What makes the most sense for NAs? Decide on treatment of missing values, as we only have data on each country 
# for years 2000, 2005, 2010, 2018, 2019, 2020:
# Preliminary treatment: remove all missing years for each country
tobacco <- tobacco[tobacco$Year %in% c(2000, 2005, 2010, 2018, 2019, 2020), ]
tobacco %>% group_by(`Country Name`) %>% group_by(!is.na(SH.PRV.SMOK)) %>% count(Year) %>% print(n = 22)

### Idea 1.1: Scatter plot of relationship of interested variables facetted years
ggplot(tobacco, aes(color = `Country Name`)) +
  geom_point(aes(x = NY.GDP.PCAP.PP.KD, y = SH.PRV.SMOK, group = Year)) +  
  labs(title = "Zusammenhang zwischen der Prävalenz des aktuellen Tabakkonsums und BIP pro Kopf",
       x = "BIP pro Kopf", y = "Prävalenz des aktuellen Tabakkonsums [%]", color = "Land") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(hjust = 0.5)) +
  guides(color = guide_legend(ncol = 8)) +
  facet_wrap(~ Year)