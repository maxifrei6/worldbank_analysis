# Load libraries
# TODO

# Load the "_____" dataset
# TODO
data_tobacco <- readRDS("Data/Processed/data_merged.RDS")

# Questions:
# 1. How does GDP per capita relate to the prevalence of current tobacco use (% of adults)?

# Variables:  GDP per capita, PPP (constant 2021 international $)
#             NY.GDP.PCAP.PP.KD

#             Prevalence of current tobacco use (% of adults)
#             SH.PRV.SMOK

# Answer / Visualization of 1.)
# 1.1) Scatter plot of relationship of interested variables
# 1.2) Scatter plot of relationship of interested variables; except outlier Qatar
# 1.3) Scatter plot of relationship of interested variables faceted years
# 1.4) Scatter plot of relationship of interested variables faceted years with density curves

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

# As we only have data on each country for the years 2000, 2005, 2010, 2018, 2019, 2020,
# we remove all missing years for each country
data_tobacco <- data_tobacco[data_tobacco$Year %in% c(2000, 2005, 2010, 2018, 2019, 2020), ]
data_tobacco %>% group_by(`Country Name`) %>% group_by(!is.na(SH.PRV.SMOK)) %>% count(Year) %>% print(n = 22)

### Idea 1.1: Scatter plot of relationship of interested variables
ggplot(data_tobacco, aes(x = SH.PRV.SMOK, y = NY.GDP.PCAP.PP.KD)) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(x = SH.PRV.SMOK, y = NY.GDP.PCAP.PP.KD, color = "Robust-Linear"),
              method = MASS::rlm,  # TODO: IF RLM USED --> PACKAGE MASS!
              linewidth = 1, se = FALSE) +
  geom_smooth(aes(x = SH.PRV.SMOK, y = NY.GDP.PCAP.PP.KD, color = "Linear"),
              method = "lm", linewidth = 1, se = FALSE) +
  ggtitle("Zusammenhang zwischen der Prävalenz des aktuellen Tabakkonsums und BIP pro Kopf") +
  scale_x_continuous(name = "Prävalenz des aktuellen Tabakkonsums [%]", breaks = c(0, 10, 20, 30, 40, 50, 60),
                     minor_breaks = waiver(), limits = c(0, 60)) +
  scale_y_continuous(name = "BIP pro Kopf [$]", breaks = c(0, 25000, 50000, 75000, 100000, 125000),
                     minor_breaks = waiver(), limits = c(0, 125000)) +
  scale_color_brewer(name = "Regression", palette = "Set2") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

# Investigate the y-axis outlier - are they all from the same country?
data_tobacco %>% filter(NY.GDP.PCAP.PP.KD > 80000)
# Indeed, all the data points originate out of Qatar

### Idea 1.2: Scatter plot of relationship of interested variables; except outlier Qatar
ggplot(data_tobacco[data_tobacco$`Country Name` != "Qatar", ], aes(x = SH.PRV.SMOK, y = NY.GDP.PCAP.PP.KD)) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(x = SH.PRV.SMOK, y = NY.GDP.PCAP.PP.KD, color = "Robust-Linear"),
              method = MASS::rlm,  # TODO: IF RLM USED --> PACKAGE MASS!
              linewidth = 1, se = FALSE) +
  geom_smooth(aes(x = SH.PRV.SMOK, y = NY.GDP.PCAP.PP.KD, color = "Linear"),
              method = "lm", linewidth = 1, se = FALSE) +
  ggtitle("Zusammenhang zwischen der Prävalenz des aktuellen Tabakkonsums und BIP pro Kopf") +
  scale_x_continuous(name = "Prävalenz des aktuellen Tabakkonsums [%]", breaks = c(0, 10, 20, 30, 40, 50, 60),
                     minor_breaks = waiver(), limits = c(0, 60)) +
  scale_y_continuous(name = "BIP pro Kopf [$]", breaks = c(0, 25000, 50000, 75000, 100000, 125000),
                     minor_breaks = waiver(), limits = c(0, 125000)) +
  scale_color_brewer(name = "Regression", palette = "Set2") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

### Idea 1.3: Scatter plot of relationship of interested variables faceted years
ggplot(data_tobacco, aes(x = SH.PRV.SMOK, y = NY.GDP.PCAP.PP.KD)) +
  geom_point() +
  geom_smooth(data = data_tobacco,
              aes(x = SH.PRV.SMOK, y = NY.GDP.PCAP.PP.KD, group = Year, color = "Robust-Linear"),
              method = MASS::rlm, linewidth = 1, se = FALSE) +  # IF RLM USED --> PACKAGE MASS!!!
  geom_smooth(data = data_tobacco,
              aes(x = SH.PRV.SMOK, y = NY.GDP.PCAP.PP.KD, group = Year, color = "Linear"),
              method = "lm", linewidth = 1, se = FALSE) +  # TODO: CHOOSE METHOD
  geom_smooth(data = data_tobacco,
              aes(x = SH.PRV.SMOK, y = NY.GDP.PCAP.PP.KD, group = Year, color = "Lokal-Polynomiell"),
              method = "loess", linewidth = 1, se = FALSE) +  # TODO: CHOOSE METHOD
  labs(title = "Zusammenhang zwischen der Prävalenz des aktuellen Tabakkonsums und BIP pro Kopf",
       x = "Prävalenz des aktuellen Tabakkonsums [%]", y = "BIP pro Kopf [$]") +
  scale_color_manual(name = "Regression",
                     values = c("Robust-Linear" = brewer.pal(3, "Set2")[[2]],
                                "Linear" = brewer.pal(3, "Set2")[[1]],
                                "Lokal-Polynomiell" = brewer.pal(3, "Set2")[[3]])) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(hjust = 0.5)) +
  facet_wrap(~ Year)

# Calculate mean and standard deviation for each year
data_tobacco_normdensity <- data_tobacco %>%
  group_by(Year) %>%
  summarize(SH.PRV.SMOK_mean = mean(SH.PRV.SMOK),
            SH.PRV.SMOK_sd = sd(SH.PRV.SMOK))

# Generate evenly-spaced values of x and compute density for each year
data_tobacco_normdensity <- data_tobacco_normdensity %>%
  rowwise() %>%
  mutate(
    # Create 100 evenly-spaced points inbetween the lowest and highest observed datapoint 
    norm_x_val = list(seq(0, 100, length.out = 100)),
    # Compute the corresponding PDF values
    norm_y_val = list(dnorm(norm_x_val, mean = SH.PRV.SMOK_mean, sd = SH.PRV.SMOK_sd))) %>%
  unnest(c(norm_x_val, norm_y_val))  # Convert the lists into columns

# Remove rows with values of the normal distribution calculation that are larger than 60% (as 60% > max(SH.PRV.SMOK))
data_tobacco_normdensity <- data_tobacco_normdensity %>%
  filter(norm_x_val <= 60)

### Idea 1.4: Scatter plot of relationship of interested variables faceted years with density curves
# Plot the upper row of the data points for the assembled tobacco plotting
plot_tobacco1_data_upper <- ggplot(data_tobacco %>% filter(Year %in% c(2000, 2005, 2010))) +
  geom_point(aes(x = SH.PRV.SMOK, y = NY.GDP.PCAP.PP.KD, group = Year), alpha = 0.5) +
  geom_smooth(data = data_tobacco %>% filter(Year %in% c(2000, 2005, 2010)),
              aes(x = SH.PRV.SMOK, y = NY.GDP.PCAP.PP.KD, group = Year),
              method = MASS::rlm, linewidth = 1, se = FALSE, color = "black") +
  ggtitle("Zusammenhang zwischen der Prävalenz des aktuellen Tabakkonsums und BIP pro Kopf") +
  scale_x_continuous(name = "Prävalenz des aktuellen Tabakkonsums [%]", breaks = c(0, 10, 20, 30, 40, 50, 60),
                     minor_breaks = waiver(), limits = c(0, 60)) +
  scale_y_continuous(name = "BIP pro Kopf [$]", breaks = c(0, 25000, 50000, 75000, 100000, 125000),
                     minor_breaks = waiver(), limits = c(0, 125000)) +
  theme_bw() +
  theme(plot.margin = margin(0, 0, 0, 0),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  facet_wrap(~ Year)

# Plot the bottom row of the data points for the assembled tobacco plotting
plot_tobacco1_data_bottom <- ggplot(data_tobacco %>% filter(Year %in% c(2018, 2019, 2020))) +
  geom_point(aes(x = SH.PRV.SMOK, y = NY.GDP.PCAP.PP.KD, group = Year), alpha = 0.5) +
  geom_smooth(data = data_tobacco %>% filter(Year %in% c(2018, 2019, 2020)),
              aes(x = SH.PRV.SMOK, y = NY.GDP.PCAP.PP.KD, group = Year),
              method = MASS::rlm, linewidth = 1, se = FALSE, color = "black") +
  scale_x_continuous(name = "Prävalenz des aktuellen Tabakkonsums [%]", breaks = c(0, 10, 20, 30, 40, 50, 60),
                     minor_breaks = waiver(), limits = c(0, 60)) +
  scale_y_continuous(name = "BIP pro Kopf [$]", breaks = c(0, 25000, 50000, 75000, 100000, 125000),
                     minor_breaks = waiver(), limits = c(0, 125000)) +
  theme_bw() +
  theme(plot.margin = margin(0, 0, 0, 0),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  facet_wrap(~ Year)

# Add overlayed normal distribution curves to the assembled plot in the data displays
# Plot the upper row of the density curves for the assembled tobacco plotting
plot_tobacco1_density_upper <- ggplot() +
  geom_line(data = data_tobacco_normdensity %>% filter(Year %in% c(2000, 2005, 2010)),  # TODO: data
            aes(x = norm_x_val, y = norm_y_val, group = Year),
            color = brewer.pal(4, "Set2")[[2]], linewidth = 1, inherit.aes = FALSE) +
  geom_density(data = data_tobacco %>% filter(Year %in% c(2000, 2005, 2010)),
               aes(x = SH.PRV.SMOK, y = after_stat(density), group = Year),
               color = brewer.pal(4, "Set2")[[1]], linewidth = 1, alpha = 0.5, bw = 5) +
  geom_vline(data = data_tobacco_normdensity %>% filter(Year %in% c(2000, 2005, 2010)),  # TODO: data
             aes(xintercept = SH.PRV.SMOK_mean, group = Year),
             color = "black", linetype = "dashed") +
  scale_x_continuous(name = "Prävalenz des aktuellen Tabakkonsums [%]", breaks = c(0, 10, 20, 30, 40, 50, 60),
                     minor_breaks = waiver(), limits = c(0, 60)) +
  scale_y_continuous(name = "Dichte", breaks = c(0, 0.02, 0.04), minor_breaks = waiver()) +
  theme_bw() +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10))) +
  facet_wrap(~ Year)

# Plot the bottom row of the density curves for the assembled tobacco plotting
plot_tobacco1_density_bottom <- ggplot() +
  geom_line(data = data_tobacco_normdensity %>% filter(Year %in% c(2018, 2019, 2020)),  # TODO: data
            aes(x = norm_x_val, y = norm_y_val, group = Year, color = "Normalverteilung"),
            linewidth = 1, inherit.aes = FALSE) +
  geom_density(data = data_tobacco %>% filter(Year %in% c(2018, 2019, 2020)),
               aes(x = SH.PRV.SMOK, y = after_stat(density), group = Year),
               color = brewer.pal(3, "Set2")[[1]], linewidth = 1, bw = 5, show.legend = FALSE) +
  # Add an invisible line by framing dummy data to show "KDE" with line instead of hollow square for shape in legend
  geom_line(data = data.frame(x = c(0,0), y = c(0,0)),
            aes(x = x, y = y, color = "Kerndichteschätzer"), linewidth = 1) +
  geom_vline(data = data_tobacco_normdensity %>% filter(Year %in% c(2018, 2019, 2020)),  # TODO: data
             aes(xintercept = SH.PRV.SMOK_mean, group = Year),
             color = "black", linetype = "dashed") +
  scale_x_continuous(name = "Prävalenz des aktuellen Tabakkonsums [%]", breaks = c(0, 10, 20, 30, 40, 50, 60),
                     minor_breaks = waiver(), limits = c(0, 60)) +
  scale_y_continuous(name = "Dichte", breaks = c(0, 0.02, 0.04), minor_breaks = waiver()) +
  scale_color_brewer(name = "", palette = "Set2") +
  theme_bw() +
  theme(strip.text = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "bottom",
        legend.text = element_text(size = 10)) +
  facet_wrap(~ Year)

# Assemble the plot
(plot_tobacco1_data_upper /
    plot_tobacco1_density_upper /
    plot_tobacco1_data_bottom /
    plot_tobacco1_density_bottom) +
  plot_layout(heights = c(2, 0.5, 2, 0.5))
