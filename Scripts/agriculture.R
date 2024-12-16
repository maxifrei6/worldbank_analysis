# Load libraries
# TODO

library(here)

# Load the processed "data_merged" dataset using `here`
data_agriculture <- readRDS(here("Data", "Processed", "data_merged.RDS"))


# Questions:
# 1. Is there a relationship between the percentage of agricultural land and
#    CO2 emissions per capita across countries?
# 2. Does the size of the surface area of the country play a role?

# Variables:  Agricultural land (% of land area)
#             AG.LND.AGRI.ZS

#             Carbon dioxide (CO2) emissions (total) excluding LULUCF (Mt CO2e)
#             EN.GHG.CO2.MT.CE.AR5

#             Surface area (sq. km)
#             AG.SRF.TOTL.K2

# Answer / Visualization of 1.)
# 1.1) Scatter plot with agricultural land [%] as color and free y-axis
# 1.2) Scatter plot with country as color and logarithmic CO2 for y-axis
# 1.3) A) Heatmap only CO2 emissions / B) Lineplot only agriculture land [%] / C) Lineplot only CO2 emissions
# 1.4) Heatmap (only CO2 emissions though) in world map design
# 1.5) Logarithmic CO2 emissions by country, average agricultural land [%] for color and facet order
# 1.6) Normalized and logarithmic CO2 emissions by country, average agricultural land [%] for color and facet order
# 1.7) Line plots with normalized data over time for each country
# 1.8) Boxplots (only one variable though)
# 1.9) Scatterplot with smoothed line for comparison of agricultural land [%] and CO2 emissions

# Answer / Visualization of 2.)
# 2.1) 
# 2.2) 

# Filter relevant columns Country Name, Country Code, Year, AG.LND.AGRI.ZS, AG.SRF.TOTL.K2 and EN.GHG.CO2.MT.CE.AR5
data_agriculture <- data_agriculture %>%
  select(`Country Name`, `Country Code`, Year, AG.LND.AGRI.ZS, AG.SRF.TOTL.K2, EN.GHG.CO2.MT.CE.AR5)

# Check whether any missing values in interested data subset
all(!is.na(data_agriculture))

# Convert column `Year` to data type Integer
data_agriculture$Year <- as.integer(data_agriculture$Year)

# Normalize variables to same scale
data_agriculture_normd <- data_agriculture %>%
  group_by(`Country Name`) %>%
  mutate(
    co2_min = min(EN.GHG.CO2.MT.CE.AR5),
    co2_max = max(EN.GHG.CO2.MT.CE.AR5),
    co2_normd = ((EN.GHG.CO2.MT.CE.AR5 - co2_min) / (co2_max - co2_min))
  ) %>%
  ungroup()

# Pivot normalized data_agriculture dataset to longer format for better visualization of two variables (Idea 1.7)
data_agriculture_normd_long <- data_agriculture_normd %>%
  pivot_longer(cols = c(co2_normd, AG.LND.AGRI.ZS), names_to = "variable", values_to = "value") %>%
  mutate(value = case_when(
    variable == "co2_normd" ~ value * 100,  # used for [%] comparison
    variable == "AG.LND.AGRI.ZS" ~ value))

# Save the ascending order of average agricultural land [%] by country
country_order_by_agrLand <- data_agriculture %>%
  group_by(`Country Name`) %>%
  summarise(avg_AG.LND.AGRI.ZS = mean(AG.LND.AGRI.ZS)) %>%
  arrange(avg_AG.LND.AGRI.ZS) %>%
  pull(`Country Name`)

# Get an overview over the CO2 emissions data
summary(data_agriculture$EN.GHG.CO2.MT.CE.AR5)

# Save the ascending order of within-country variance in CO2 emissions
country_order_by_CO2variance <- data_agriculture %>%
  group_by(`Country Name`) %>%
  summarise(avg_EN.GHG.CO2.MT.CE.AR5 = mean(EN.GHG.CO2.MT.CE.AR5)) %>%
  arrange(avg_EN.GHG.CO2.MT.CE.AR5) %>%
  pull(`Country Name`)

# Compute the between-country variance in CO2 emissions
data_agriculture_variance_between_CO2 <- var(data_agriculture %>%
                                               group_by(`Country Name`) %>%
                                               summarise(Average_CO2 = mean(EN.GHG.CO2.MT.CE.AR5)) %>%
                                               select(Average_CO2))

# Create boxplots for within-country variances (vertical reference line showing the between-country variance)
ggplot(data_agriculture,
       aes(x = EN.GHG.CO2.MT.CE.AR5, y = factor(`Country Name`, levels = country_order_by_CO2variance))) +
  geom_boxplot() +
  # geom_vline(xintercept = data_agriculture_variance_between_CO2,
  #            color = "black", linetype = "dashed", size = 1.2) +  # Between-country variance
  labs(title = "Streuungszerlegung der CO2 Emissionen pro Kopf bezüglich Ländern",
       x = "CO2 Emissionen pro Kopf [mt]", y = "Land") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Get an overview over the agriculture data
summary(data_agriculture$AG.LND.AGRI.ZS)

# Recall the already existing ascending order by agricultural land [%]
country_order_by_agrLand

# Compute the between-country variance in CO2 emissions
data_agriculture_variance_between_agrLand <- var(data_agriculture %>%
                                                   group_by(`Country Name`) %>%
                                                   summarise(Average_agrLand = mean(AG.LND.AGRI.ZS)) %>%
                                                   select(Average_agrLand))

# Create boxplots for within-country variances (vertical reference line showing the between-country variance)
ggplot(data_agriculture,
       aes(x = AG.LND.AGRI.ZS, y = factor(`Country Name`, levels = country_order_by_agrLand))) +
  geom_boxplot() +
  # geom_vline(xintercept = data_agriculture_variance_between_agrLand,
  #           color = "black", linetype = "dashed", size = 1.2) +  # Between-country variance
  labs(title = "Streuungszerlegung der landwirtschaftlichen Nutzfläche bezüglich Ländern",
       x = "Landwirtschaftliche Nutzfläche [%]", y = "Land") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



### Idea 1.1: Scatter plot with agricultural land [%] as color
ggplot(data_agriculture, aes(colour = AG.LND.AGRI.ZS)) +
  geom_point(aes(x = Year, y = EN.GHG.CO2.MT.CE.AR5,)) +
  labs(title = "Relationship between agricultural land and CO2 emissions per capita",
       x = "Year", y = "y") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ `Country Name`, ncol = 5, scales = "free_y")



### Idea 1.2: Scatter Plot with Country as Color
ggplot(data_agriculture, aes(x = AG.LND.AGRI.ZS, y = log10(EN.GHG.CO2.MT.CE.AR5 +1))) +
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



### Idea 1.3.A: Heatmap (only CO2 emissions though) in table design
ggplot(data_agriculture, aes(x = Year, y = `Country Name`, fill = log10(EN.GHG.CO2.MT.CE.AR5 + 1))) +
  geom_tile(col = "white") +  # Create heatmap tiles
  scale_fill_gradient(low = brewer.pal(9, "Blues")[1],
                      high = brewer.pal(9, "Blues")[9],
                      name = "Logarithmic CO2 Emissions\nper capita",
                      breaks = log10(c(0, 100, 1000, 10000, 100000) + 1),
                      labels = c("0", "100", "1000", "10000", "100000")) +  # Gradient color scale
  ggtitle("Heatmap of CO2 Emissions by Country and Year") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(x = "Year", y = "Country")

### Idea 1.3.B: Lineplot (only agriculture land [%] though) in table design
ggplot(data_agriculture, aes(x = Year, y = AG.LND.AGRI.ZS)) +
  geom_line() +  
  ggtitle("Landwirtschaftliche Nutzfläche") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        legend.title.position = "left",
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(x = "Jahr", y = "Anteil [%]") +
  facet_wrap(~ factor(data_agriculture$`Country Name`, levels = country_order_by_agrLand), ncol = 5)

### Idea 1.3.C: Lineplot (only CO2 emissions though) in table design
ggplot(data_agriculture, aes(x = Year, y = EN.GHG.CO2.MT.CE.AR5)) +
  geom_line() +  
  ggtitle("CO2 Emissionen pro Kopf") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        legend.title.position = "left",
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Jahr", y = "Logarithmierte CO2 Emissionen pro Kopf") +
  facet_wrap(~ `Country Name`, scales = "free_y")



### Idea 1.4: Heatmap (only CO2 emissions though) in world map design
library(rnaturalearth)
library(sf)
worldmap <- ne_countries(scale = "medium", returnclass = "sf")
data_agriculture_worldmap <- worldmap %>%
  left_join(data_agriculture, by = c("iso_a3" = "Country Code"))
# Filter out the NA countries to only show the countries of the interested dataset
data_agriculture_worldmap <- data_agriculture_worldmap %>% filter(!is.na(Year))
# Build five/six year timespans for each country
data_agriculture_worldmap <- data_agriculture_worldmap %>%
  mutate(timespan = case_when(
    Year >= 2000 & Year <= 2005 ~ "2000-2005",
    Year >= 2006 & Year <= 2010 ~ "2006-2010",
    Year >= 2011 & Year <= 2015 ~ "2011-2015",
    Year >= 2016 & Year <= 2021 ~ "2016-2021"
  ))
# Plot the worldmap 
ggplot(data_agriculture_worldmap) +
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
ggplot(data_agriculture, aes(colour = AG.LND.AGRI.ZS)) +
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
  facet_wrap(~ factor(data_agriculture$`Country Name`, levels = country_order_by_agrLand), ncol = 5)



### Idea 1.6: Normalized and logarithmic CO2 emissions by country, avg. agricultural land [%] for color and facet order
ggplot(data_agriculture_normd, aes(colour = AG.LND.AGRI.ZS)) +
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
  facet_wrap(~ factor(data_agriculture_normd$`Country Name`, levels = country_order_by_agrLand), ncol = 5)



### Idea 1.7: Line plots with normalized data over time for each country
ggplot(data_agriculture_normd_long,
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



### Idea 1.8: Boxplots agricultural land [%]
ggplot(data_agriculture,
       aes(x = factor(`Country Name`, levels = country_order_by_agrLand),
           y = AG.LND.AGRI.ZS)) +
  geom_boxplot() +  
  ggtitle("Landwirtschaftliche Nutzfläche") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        legend.title.position = "left",
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(x = "Jahr", y = "Anteil [%]")



### Idea 1.9: Comparison of agricultural land [%] and CO2 emissions
ggplot(data_agriculture, aes(x = log(EN.GHG.CO2.MT.CE.AR5 + 1), y = AG.LND.AGRI.ZS)) +
  geom_point(alpha = .5, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Zusammenhang zwischen CO2 Emissionen pro Kopf und Anteil landwirtschaftlicher Nutzfläche",
       x = "Logarithmierte CO2 Emissionen pro Kopf", y = "Landwirtschaftliche Nutzfläche [%]") +
  # scale_x_log10(name = "CO2 Emissionen pro Kopf",
  #               breaks = log10(c(0, 10, 100, 1000, 10000, 100000) + 1),
  #               labels = c("0", "10", "100", "1.000", "10.000", "100.000")) +
  # scale_x_log10(breaks = trans_breaks("log10", function(x) 10 ^ x),
  #               labels = trans_format("log10", function(x) as.character(10 ^ x))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


##############################################################################################################

