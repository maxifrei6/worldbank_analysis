# Load libraries
# TODO

# Load the "_____" dataset
# TODO
data_full <- readRDS("Data/Processed/data_full.RDS")

# Questions:
# 1. Is there a relationship between the percentage of agricultural land and
#    CO2 emissions per capita across countries?
# 2. Does the size of the surface area of the country play a role?

# Variables:  Agricultural land (% of land area)
#             AG.LND.AGRI.ZS

#             Carbon dioxide (CO2) emissions (total) excluding LULUCF (Mt CO2e)
#             EN.GHG.CO2.MT.CE.AR5

#             Surface area (sq. area)
#             AG.SRF.TOTL.K2

# Answer / Visualization of 1.)
# 1.1) Scatter plot with agricultural land [%] as color and free y-axis
# 1.2) Scatter plot with country as color and logarithmic CO2 for y-axis
# 1.3) Heatmap (only CO2 emissions though) in table design
# 1.4) Heatmap (only CO2 emissions though) in world map design
# 1.5) Logarithmic CO2 emissions by country, average agricultural land [%] for color and facet order
# 1.6) Normalized and logarithmic CO2 emissions by country, average agricultural land [%] for color and facet order
# 1.7) Line plots with normalized data over time for each country
# 1.8) Boxplots (only one variable though)

# Answer / Visualization of 2.)
# 2.1) 
# 2.2) 

# Filter relevant columns Country Name, Country Code, Year, AG.LND.AGRI.ZS, AG.SRF.TOTL.K2 and EN.GHG.CO2.MT.CE.AR5	 
agriculture <- full_data %>%
  select(`Country Name`, `Country Code`, Year, AG.LND.AGRI.ZS, AG.SRF.TOTL.K2, EN.GHG.CO2.MT.CE.AR5)

# Check whether any missing values in interested data subset
all(!is.na(agriculture))

# Convert column `Year` to data type Integer
agriculture$Year <- as.integer(agriculture$Year)

# Normalize variables to same scale
agriculture_normd <- agriculture %>%
  group_by(`Country Name`) %>%
  mutate(
    co2_min = min(EN.GHG.CO2.MT.CE.AR5),
    co2_max = max(EN.GHG.CO2.MT.CE.AR5),
    co2_normd = ((EN.GHG.CO2.MT.CE.AR5 - co2_min) / (co2_max - co2_min))
  ) %>%
  ungroup()

# Pivot normalized agriculture dataset to longer format for better visualization of two variables (Idea 1.7)
agriculture_normd_long <- agriculture_normd %>%
  pivot_longer(cols = c(co2_normd, AG.LND.AGRI.ZS), names_to = "variable", values_to = "value") %>%
  mutate(value = case_when(
    variable == "co2_normd" ~ value * 100,  # used for [%] comparison
    variable == "AG.LND.AGRI.ZS" ~ value))

# Compute the average change in agricultural land [%] by country
country_order_by_agriland <- agriculture %>%
  group_by(`Country Name`) %>%
  summarise(AG.LND.AGRI.ZS_avg = mean(AG.LND.AGRI.ZS)) %>%
  arrange(AG.LND.AGRI.ZS_avg) %>%
  pull(`Country Name`)



### Idea 1.1: Scatter plot with agricultural land [%] as color
ggplot(agriculture, aes(colour = AG.LND.AGRI.ZS)) +
  geom_point(aes(x = Year, y = EN.GHG.CO2.MT.CE.AR5,)) +
  labs(title = "Relationship between agricultural land and CO2 emissions per capita",
       x = "Year", y = "y") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ `Country Name`, ncol = 5, scales = "free_y")



### Idea 1.2: Scatter Plot with Country as Color
ggplot(agriculture, aes(x = AG.LND.AGRI.ZS, y = log10(EN.GHG.CO2.MT.CE.AR5 +1))) +
  geom_point(aes(colour = `Country Name`), alpha = .5) +
  labs(title = "Relationship between agricultural land and CO2 emissions per capita",
       x = "Agricultural land [%]", fill = "Country") +
  xlim(0, 100) +
  # scale_y_log10(name = "CO2 Emissionen pro Kopf",
                # breaks = log10(c(0, 100, 1000, 10000, 100000) + 1),
                # labels = c("0", "100", "1.000", "10.000", "100.000")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "grey"),
        legend.margin = margin(10, 10, 10, 10),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(ncol = 10)) +
  facet_wrap(~ `Year`, ncol = 5)



### Idea 3: Heatmap (only CO2 emissions though) in table design
ggplot(agriculture, aes(x = Year, y = `Country Name`, fill = log10(EN.GHG.CO2.MT.CE.AR5 + 1))) +
  geom_tile(col = "white") +  # Create heatmap tiles
  scale_fill_gradient(low = "white", high = "black", name = "Logarithmic CO2 Emissions\nper capita", 
                      breaks = log10(c(0, 100, 1000, 10000, 100000) + 1),
                      labels = c("0", "100", "1000", "10000", "100000")) +  # Gradient color scale
  ggtitle("Heatmap of CO2 Emissions by Country and Year") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(x = "Year", y = "Country")



### Idea 1.4: Heatmap (only CO2 emissions though) in world map design
library(rnaturalearth)
library(sf)
worldmap <- ne_countries(scale = "medium", returnclass = "sf")
agriculture_worldmap <- worldmap %>%
  left_join(agriculture, by = c("iso_a3" = "Country Code"))
# Filter out the NA countries to only show the countries of the interested dataset
agriculture_worldmap <- agriculture_worldmap %>% filter(!is.na(Year))
# Build five/six year timespans for each country
agriculture_worldmap <- agriculture_worldmap %>%
  mutate(timespan = case_when(
    Year >= 2000 & Year <= 2005 ~ "2000-2005",
    Year >= 2006 & Year <= 2010 ~ "2006-2010",
    Year >= 2011 & Year <= 2015 ~ "2011-2015",
    Year >= 2016 & Year <= 2021 ~ "2016-2021"
  ))
# Plot the worldmap 
ggplot(agriculture_worldmap) +
  geom_sf(aes(fill = log10(EN.GHG.CO2.MT.CE.AR5 + 1))) +
  scale_fill_gradient(low = "white", high = "blue", name = "Logarithmierte CO2 Emissionen pro Kopf", 
                      breaks = log10(c(0, 100, 1000, 10000, 100000) + 1),
                      labels = c("0", "100", "1000", "10000", "100000")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.title.position = "left") +
  labs(title = "CO2 Emissionen pro Kopf nach Land",
       fill = "CO2 Emissionen pro Kopf") +
  facet_wrap(~ `timespan`, ncol = 2)



### Idea 1.5: Logarithmic CO2 emissions by country, average agricultural land [%] for color and facet order
ggplot(agriculture, aes(colour = AG.LND.AGRI.ZS)) +
  geom_point(aes(x = Year, y = log10(EN.GHG.CO2.MT.CE.AR5 + 1))) +
  scale_color_continuous(name = "Landwirtschaftliche Nutzfläche [%]",
                         type = "viridis",
                         breaks = c(0, 20, 40, 60, 80, 100),
                         labels = c("0", "20", "40", "60", "80", "100")) +
  labs(title = "Zusammenhang zwischen landwirtschaftlicher Nutzfläche und CO2 Emissionen pro Kopf",
       x = "Jahr", y = "CO2 Emissionen pro Kopf") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ factor(agriculture$`Country Name`, levels = country_order_by_agriland), ncol = 5)



### Idea 1.6: Normalized and logarithmic CO2 emissions by country, avg. agricultural land [%] for color and facet order
ggplot(agriculture_normd, aes(colour = AG.LND.AGRI.ZS)) +
  geom_point(aes(x = Year, y = co2_normd)) +  #
  scale_color_continuous(name = "Landwirtschaftliche Nutzfläche [%]",
                         type = "viridis",
                         breaks = c(0, 20, 40, 60, 80, 100),
                         labels = c("0", "20", "40", "60", "80", "100")) +
  labs(title = "Zusammenhang zwischen landwirtschaftlicher Nutzfläche und CO2 Emissionen pro Kopf",
       x = "Jahr", y = "Normalisierte CO2 Emissionen pro Kopf") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ factor(agriculture_normd$`Country Name`, levels = country_order_by_agriland), ncol = 5)



### Idea 1.7: Line plots with normalized data over time for each country
ggplot(agriculture_normd_long,
       aes(x = Year, y = value, color = variable, group = interaction(`Country Name`, variable))) +
  geom_point() +  # or geom_line? (And is geom_line overdramatizing the (wrong?) outliers in CO2 emissions?)
  labs(title = "Zusammenhang zwischen CO2 Emissionen pro Kopf und landwirtschaftlicher Nutzfläche",
       x = "Jahr", y = "Anteil", color = "") +
  scale_color_manual(values = c("AG.LND.AGRI.ZS" = "orange", "co2_normd" = "blue"),
                     labels = c("Landwirtschaftliche Nutzfläche [%]",
                                "Logarithmierte, normalisierte CO2 Emissionen pro Kopf")) +
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020),  # Text labels for x-axis
                     minor_breaks = seq(2000, 2021, by = 1)) +  # Add ticks
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(hjust = 0.5)) +
  facet_wrap(~ `Country Name`)



### Idea 1.8: Boxplots 