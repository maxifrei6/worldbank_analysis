# Load libraries
# TODO

# Load the "_____" dataset
# TODO
data_tobacco <- data_merged
# readRDS("Data/Processed/data_merged.RDS")

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
data_tobacco <- data_tobacco %>%
  select(`Country Name`, `Country Code`, Year, NY.GDP.PCAP.PP.KD, SH.PRV.SMOK)

# Check whether any missing values in interested data subset
all(!is.na(data_tobacco))
sum(is.na(data_tobacco$NY.GDP.PCAP.PP.KD))
sum(is.na(data_tobacco$SH.PRV.SMOK))

# Check whether any countries with completely missing data.
data_tobacco %>% group_by(`Country Name`) %>% count(SH.PRV.SMOK) %>% filter(is.na(SH.PRV.SMOK)) %>% print(n = 25)

# As data to "Aruba" only contains NAs, remove Aruba from dataset
data_tobacco <- data_tobacco[data_tobacco$`Country Name` != "Aruba", ]

# Check which years are given for each of the other countries
data_tobacco %>% group_by(`Country Name`) %>% group_by(!is.na(SH.PRV.SMOK)) %>% count(Year) %>% print(n = 22)

# Convert column `Year` to data type Integer
data_tobacco$Year <- as.integer(data_tobacco$Year)



###################################################################################################################
###################################################################################################################
###################################################################################################################
# What makes the most sense for NAs? Decide on treatment of missing values, as we only have data on each country 
# for years 2000, 2005, 2010, 2018, 2019, 2020:
# Preliminary treatment: remove all missing years for each country
data_tobacco <- data_tobacco[data_tobacco$Year %in% c(2000, 2005, 2010, 2018, 2019, 2020), ]
data_tobacco %>% group_by(`Country Name`) %>% group_by(!is.na(SH.PRV.SMOK)) %>% count(Year) %>% print(n = 22)

### Idea 1.1: Scatter plot of relationship of interested variables facetted years
plot_tobacco1 <- ggplot(data_tobacco) +
  geom_point(aes(x = SH.PRV.SMOK, y = NY.GDP.PCAP.PP.KD, group = Year)) +  
  labs(title = "Zusammenhang zwischen der Prävalenz des aktuellen Tabakkonsums und BIP pro Kopf",
       x = "Prävalenz des aktuellen Tabakkonsums [%]", y = "BIP pro Kopf [$]", color = "Land") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(hjust = 0.5)) +
  facet_wrap(~ Year)

# Calculate mean and standard deviation for each year
data_tobacco_normdist <- data_tobacco %>%
  group_by(Year) %>%
  summarize(SH.PRV.SMOK_mean = mean(SH.PRV.SMOK),
            SH.PRV.SMOK_sd = sd(SH.PRV.SMOK))

# Generate evenly-spaced values of x and compute density for each year
data_tobacco_normdist <- data_tobacco_normdist %>%
  rowwise() %>%
  mutate(
    # Create 100 evenly-spaced points inbetween the lowest and highest observed datapoint 
    x_vals = list(seq(min(data_tobacco$SH.PRV.SMOK), max(data_tobacco$SH.PRV.SMOK), length.out = 100)),
    # Compute the corresponding PDF values
    y_vals = list(dnorm(x_vals, mean = SH.PRV.SMOK_mean, sd = SH.PRV.SMOK_sd))) %>%
  unnest(c(x_vals, y_vals))  # Convert the lists into columns

# Plot the initial relationship again, this time with overlayed nromal distribution curves
plot_tobacco1 +
  geom_hline(aes(yintercept = 0, group = Year), color = "black", linetype = "dotted")+
  geom_smooth(
    data = data_tobacco,
    aes(x = SH.PRV.SMOK, y = NY.GDP.PCAP.PP.KD, group = Year),
    method = MASS::rlm,  # TODO: Make choice out of loess, lm, glm, gam, rlm, nls AND IF RLM USED --> PACKAGE MASS!!!
    color = "yellow",
    size = 1,
    se = FALSE         # Turn off the confidence interval if FALSE
  ) +
  geom_smooth(
    data = data_tobacco,
    aes(x = SH.PRV.SMOK, y = NY.GDP.PCAP.PP.KD, group = Year),
    method = "lm",  # TODO: Make choice out of loess, lm, glm, gam, rlm, nls
    color = "green",
    size = 1,
    se = FALSE         # Turn off the confidence interval if FALSE
  ) +
  geom_smooth(
    data = data_tobacco,
    aes(x = SH.PRV.SMOK, y = NY.GDP.PCAP.PP.KD, group = Year),
    method = "loess",  # TODO: Make choice out of loess, lm, glm, gam, rlm, nls
    color = "violet",
    size = 1,
    se = FALSE         # Turn off the confidence interval if FALSE
  ) +
  geom_line(data = data_tobacco_normdist,
            aes(x = x_vals,
                y = y_vals * 1000000,  # Scale density for visibility
                group = Year),
            color = brewer.pal(3, "Set2")[[1]],
            size = 1,
            inherit.aes = FALSE) +
  geom_vline(data = data_tobacco_normdist, aes(xintercept = SH.PRV.SMOK_mean, group = Year),
             color = "black", linetype = "dashed") +
  geom_density(data = data_tobacco, aes(x = SH.PRV.SMOK,
                                        y = after_stat(density) * 1000000,  # Scale density for visibility
                                        group = Year),
               color = brewer.pal(3, "Set2")[[2]], size = 1, alpha = 0.5, bw = 5)

### Idea 2: 
