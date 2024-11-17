# Load libraries
# TODO

# Load the "_____" dataset
# TODO
data_full <- readRDS("Data/Processed/data_full.RDS")

# Questions:
# 1. Is there a relationship between the percentage of agricultural land and
#    CO2 emissions per capita across countries?
# 2. Does the size of the surface area of the country play a role?

# Filter relevant columns Country Name, Year, AG.LND.AGRI.ZS, AG.SRF.TOTL.K2 and EN.GHG.CO2.MT.CE.AR5	 
agriculture <- full_data %>%
  select(`Country Name`, Year, AG.LND.AGRI.ZS, AG.SRF.TOTL.K2, EN.GHG.CO2.MT.CE.AR5)

# Check whether any missing values in interested data subset
any(!is.na(agriculture))

# Plot the information

# Idea 1: Scatter Plot with Agricultural land [%] as Color
ggplot(agriculture, aes(colour = AG.LND.AGRI.ZS)) +
  geom_point(aes(x = Year, y = EN.GHG.CO2.MT.CE.AR5,)) +
  labs(title = "Relationship between agricultural land and CO2 emissions per capita",
       x = "Year", y = "y") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ `Country Name`, ncol = 5, scales = "free_y")

# Idea 2: Scatter Plot with Country as Color
ggplot(agriculture, aes(x = AG.LND.AGRI.ZS, y = EN.GHG.CO2.MT.CE.AR5, fill = `Country Name`)) +
  geom_point(alpha = .5) +
  labs(title = "Relationship between agricultural land and CO2 emissions per capita",
       x = "Agricultural land [%]", y = "CO2 emissions per capita", fill = "Country") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ `Year`, ncol = 1)


# Idea 3: Heatmap
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
