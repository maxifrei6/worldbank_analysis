library(here)
library(scales)
library(dplyr)
library(tidyr)
library(ggradar)
library(ggplot2)
library(patchwork)

# Load the processed "data_merged" dataset using `here`
data_indicators <- readRDS(here("Data", "Processed", "data_merged.RDS"))

# Calculate the mean for every indicator for each country and rescale the mean on a zero
# to 100 % scale relative between all countries' values
data_indicators <- data_indicators %>%
  group_by(`Country Name`) %>%
  summarize(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), 0, rescale(.))))

# Pivot the data to long format with the different indicators in one column
data_indicators <- data_indicators %>%
  pivot_longer(cols = -`Country Name`, names_to = "Indicator", values_to = "Value")

# Prepare the data for radar chart plotting by spreading countries over columns
data_indicators <- data_indicators %>%
  pivot_wider(names_from = `Country Name`, values_from = Value)

# Divide the indicators into two (topically-consistent) groups of eight indicators each,
# leaving out the following indicators as they are not used in the data analysis:
# --- Current health expenditure per capita (current US$)
#     SH.XPD.CHEX.PC.CD
# --- Life expectancy at birth, female (years)
#     SP.DYN.LE00.FE.IN
# --- Life expectancy at birth, male (years)
#     SP.DYN.LE00.MA.IN
# --- Prevalence of moderate or severe food insecurity in the population (%)
#     SN.ITK.MSFI.ZS

indicators_group1 <- c("SP.POP.TOTL",           # Population, total
                       "EN.POP.DNST",           # Population density (people per sq. km of
                                                # land area)
                       "AG.SRF.TOTL.K2",        # Surface area (sq. km)
                       "AG.LND.AGRI.ZS",        # Agricultural land (% of land area)
                       "NY.ADJ.NNTY.PC.CD",     # Adjusted net national income per capita
                                                # (current US$)
                       "GC.DOD.TOTL.GD.ZS",     # Central government debt, total (% of
                                                # GDP)
                       "NY.GDP.PCAP.PP.KD")     # GDP per capita, PPP (constant 2021
                                                # international $)
indicators_group2 <- c("EN.GHG.CO2.MT.CE.AR5",  # Carbon dioxide (CO2) emissions (total)
                                                # excluding LULUCF (Mt CO2e)
                       "EG.ELC.ACCS.ZS",        # Access to electricity (% of population)
                       "SH.DYN.AIDS.ZS",        # Prevalence of HIV, total (% of
                                                # population ages 15-49)
                       "SE.TER.ENRL.TC.ZS",     # Pupil-teacher ratio, tertiary
                       "SH.PRV.SMOK",           # Prevalence of current tobacco use (% of
                                                # adults)
                       "SL.TLF.BASC.ZS",        # Labor force with basic education
                                                # (% of total working-age population with
                                                # basic education)
                       "SH.ALC.PCAP.LI")        # Total alcohol consumption per capita
                                                # (liters of pure alcohol, projected
                                                # estimates, 15+ years of age)

data_indicators_group1 <- data_indicators %>% filter(Indicator %in% indicators_group1)
data_indicators_group2 <- data_indicators %>% filter(Indicator %in% indicators_group2)

# Plot the radar chart
plot_indicators_group1 <- ggradar(data_indicators_group1,
                                  axis.label.size = 3,
                                  grid.label.size = 4,
                                  gridline.min.colour = "black",
                                  gridline.mid.colour = "black",
                                  gridline.max.colour = "black",
                                  group.line.width = 1,
                                  group.point.size = 2)+
  theme_minimal() +
  scale_color_brewer(name = "Indikatoren\n(auf 0 bis 100% der beobachteten Länder)",
                     palette = "Set2",
                     labels = c("NY.GDP.PCAP.PP.KD"= "BIP pro Kopf",
                                "SP.POP.TOTL" = "Einwohnerzahl",
                                "EN.POP.DNST" = "Einwohnerdichte", 
                                "AG.SRF.TOTL.K2" = "Landfläche",
                                "AG.LND.AGRI.ZS" =
                                  "Anteil landwirtschaftlicher\nNutzfläche",
                                "NY.ADJ.NNTY.PC.CD" =
                                  "Nettonationaleinkommen\npro Kopf (bereinigt)",
                                "GC.DOD.TOTL.GD.ZS" = "Schulden des Zentralstaats")) +
  theme(axis.text = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        legend.title.position = "top",
        panel.grid = element_blank()) +
  guides(color = guide_legend(nrow = 4, byrow = TRUE))

plot_indicators_group2 <- ggradar(data_indicators_group2,
                                  axis.label.size = 3,
                                  grid.label.size = 4,
                                  gridline.min.colour = "black",
                                  gridline.mid.colour = "black",
                                  gridline.max.colour = "black",
                                  group.line.width = 1,
                                  group.point.size = 2)+
  theme_minimal() +
  scale_color_brewer(name = "Indikatoren\n(auf 0 bis 100% der beobachteten Länder)",
                     palette = "Set2",
                     labels = c("SH.ALC.PCAP.LI" = "Prävalenz des Alkoholkonsum",
                                "SL.TLF.BASC.ZS" = "Erwerbstätige mit Grundbildung",
                                "EN.GHG.CO2.MT.CE.AR5" = "CO2 Emissionen pro Kopf",
                                "SH.PRV.SMOK" = "Prävalenz des Tabakkonsums",
                                "SH.DYN.AIDS.ZS" = "Prävalenz von HIV",
                                "SE.TER.ENRL.TC.ZS" = "Schüler-Lehrer-Relation",
                                "EG.ELC.ACCS.ZS" = "Zugang zu Elektrizität")) +
  theme(axis.text = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        legend.title.position = "top",
        panel.grid = element_blank()) +
  guides(color = guide_legend(nrow = 4, byrow = TRUE))

plot_indicators_group1 + plot_indicators_group2
