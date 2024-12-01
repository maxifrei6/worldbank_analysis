# HIV

library(dplyr)
library(ggplot2)

## HIV Variable:  Prevalence of HIV, total (% of population ages 15-49)
#                 SH.DYN.AIDS.ZS

#                 Total alcohol consumption per capita
#                 (liters of pure alcohol, projected estimates, 15+ years of age)
#                 SH.ALC.PCAP.LI

#                 Labor force with basic education
#                 (% of total working-age population with basic education)
#                 SL.TLF.BASC.ZS

## Questions:
# 1) How does the HIV prevalence in the population aged 15-49 relate to the total
#     alcohol consumption per capita?
# 2) Do countries with higher percentage of labor force with basic education
#     have lower HIV prevalence rates in the 15-49 population?

## Answer / Visualization of 1)
# 1.1) last available data with alcohol counsumption on x-axis and HIV prevalence on y-axis
#       countries/continents coloured
# 1.2)
# 1.3)
# 1.4)
# 1.5) facetting by country, alcoholconsumption vs HIV
# 1.6) facetting by country, time vs HIV
# 1.7)
# 1.8)

## Answer / Visualization of 2)
# 2.1) grouping of countries by level of education (2 to 4 groups)
#       HIV prevalence can be displayed over time by lineplot
# 2.2) education level coded over continous colour scale scale_color_gradient(low = "blue", high = "red")
#       dotplot of last available data
#       countrie coded on x-scale?
# 2.3) boxplot with grouped countries by education and HIV prevalence values
# 2.4) facetting by Year, countries over colour, corr of education and HIV displayed
# 2.5) facetting by country, education vs HIV
# 2.6) y-axis: HIV Prevalence per Labour Force with basic information (HIV/EDU)
# 2.a) grouping countries by development level of Worldbank
# 2.b) grouping countries by continent

## Adjust dataframe
hiv_df <- data_merged %>%
  select(`Country Name`,
         `Country Code`,
         Year,
         SH.DYN.AIDS.ZS,
         SH.ALC.PCAP.LI,
         SL.TLF.BASC.ZS)
View(hiv_df)
str(hiv_df$SH.DYN.AIDS.ZS)
str(hiv_df$SH.ALC.PCAP.LI)
str(hiv_df$SL.TLF.BASC.ZS)

## Check how many NA's are in the data to be used
# NA`s in Prevalence of HIV
sum(is.na(hiv_df$SH.DYN.AIDS.ZS))
# NA's per year
hiv_years_data <- hiv_df %>%
  group_by(Year) %>%
  reframe(num_na_hiv = sum(is.na(SH.DYN.AIDS.ZS))) %>%
  filter(num_na_hiv < 25)
# NA`s in alcohol consumption
sum(is.na(hiv_df$SH.ALC.PCAP.LI))
# NA's per year
alc_years_data <- hiv_df %>%
  group_by(Year) %>%
  reframe(num_na_alc = sum(is.na(SH.ALC.PCAP.LI))) %>%
  filter(num_na_alc < 25)
# NA`s in Labor force with basic education
sum(is.na(hiv_df$SL.TLF.BASC.ZS))
# NA's per Year
edu_years_data <- hiv_df %>%
  group_by(Year) %>%
  reframe(num_na_edu = sum(is.na(SL.TLF.BASC.ZS))) %>%
  filter(num_na_edu < 25)
# joined Information df about NA's
hiv_years_data <- hiv_df %>%
  group_by(Year) %>%
  reframe(
    num_na_hiv = sum(is.na(SH.DYN.AIDS.ZS)),
    num_na_alc = sum(is.na(SH.ALC.PCAP.LI)),
    num_na_edu = sum(is.na(SL.TLF.BASC.ZS))
  ) %>%
  filter(num_na_hiv < 25 & num_na_alc < 25 & num_na_edu < 25)
View(hiv_years_data)
which(is.na(hiv_df$SH.ALC.PCAP.LI))
which(is.na(hiv_df$SH.DYN.AIDS.ZS))
# possibly no data about following countries basic education:
j <- 1
for (i in seq_len(25)) {
  if (j %in% which(is.na(hiv_df$SL.TLF.BASC.ZS))) {
    print(hiv_df[j, "Country Name"])
  }
  j <- j + 22
}
# no data about Arubas alcohol consumption, hiv rate
# no data about Chinas hiv rate
# no data about Finlands hiv rate
# no data about Kazakhstans hiv rate
# no data about Nigerias hiv rate
# no data about Russian Federations hiv rate
# no data about United Kingdoms hiv rate
# no data about United States hiv rate

## data in year:
hiv_year_df <- function(year) {
  hiv_df %>%
    filter(Year %in% year)
}

## 1.1)
# Dataframe adjusted (no data for Aruba)
hiv_alc_df <- hiv_df %>%
  filter(`Country Name` != "Aruba") %>%
  select(-SL.TLF.BASC.ZS)
hiv_alc_2020_df <- hiv_alc_df %>%
  filter(Year == 2020)

# plot results over countries
ggplot(hiv_alc_2020_df,
       aes(SH.ALC.PCAP.LI, SH.DYN.AIDS.ZS, colour = `Country Name`)) +
  geom_point() +
  labs(
    x = "Alkoholkonsum in Litern pro Kopf",
    y = "HIV Prävalenz in %",
    title = "Alkoholkonsum vs. HIV Prävalenz",
    colour = "Land"
  )
# plot results over continents
hiv_alc_2020_continent_df <- hiv_alc_2020_df %>%
  group_by(continent) %>%
  summarise(
    SH.ALC.PCAP.LI = mean(SH.ALC.PCAP.LI, na.rm = TRUE),
    SH.DYN.AIDS.ZS = mean(SH.DYN.AIDS.ZS, na.rm = TRUE)
  )
ggplot(hiv_alc_2020_continent_df,
       aes(SH.ALC.PCAP.LI, SH.DYN.AIDS.ZS, colour = continent)) +
  geom_point() +
  labs(
    x = "Alkoholkonsum in Litern pro Kopf",
    y = "HIV Prävalenz in %",
    title = "Alkoholkonsum vs. HIV Prävalenz",
    colour = "Kontinent"
  )
# as comparison: here countries coded over their continent, to take a look at variance, sample size, ...
ggplot(hiv_alc_2020_df,
       aes(SH.ALC.PCAP.LI, SH.DYN.AIDS.ZS, colour = continent)) +
  geom_point(size = 3, alpha = 0.5) +
  labs(
    x = "Alkoholkonsum in Litern pro Kopf",
    y = "HIV Prävalenz in %",
    title = "Alkoholkonsum vs. HIV Prävalenz",
    colour = "Kontinent"
  )

# plot results over development status
hiv_alc_2020_dev_df <- hiv_alc_2020_df %>%
  group_by(dev_status) %>%
  summarise(
    SH.DYN.AIDS.ZS = mean(SH.DYN.AIDS.ZS, na.rm = TRUE),
    SH.ALC.PCAP.LI = mean(SH.ALC.PCAP.LI, na.rm = TRUE)
  )
ggplot(hiv_alc_2020_dev_df,
       aes(SH.ALC.PCAP.LI, SH.DYN.AIDS.ZS, colour = dev_status)) +
  geom_point() +
  labs(
    x = "Alkoholkonsum pro Kopf in Litern",
    y = "HIV Prävalenz",
    colour = "Entwicklungstand",
    title = "Alkoholkonsum vs HIV Prävalenz im Jahr 2020"
  )
# for comparison all countries but only coded by development state (variance, etc ...)
ggplot(hiv_alc_2020_df,
       aes(SH.ALC.PCAP.LI, SH.DYN.AIDS.ZS, colour = dev_status)) +
  geom_point() +
  labs(
    x = "Alkoholkonsum pro Kopf in Litern",
    y = "HIV Prävalenz",
    colour = "Entwicklungsstand des Landes",
    title = "Alkoholkonsum vs HIV Prävalenz im Jahr 2020"
  )

## 1.5)
ggplot(hiv_alc_df, aes(SH.ALC.PCAP.LI, SH.DYN.AIDS.ZS, color = Year)) +
  geom_point(size = 1) +
  facet_wrap( ~ `Country Name`) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black",
    size = 0.5
  ) +
  labs(y = "HIV Prävalenz in %", x = "totaler Alkoholkonsum pro Kopf", color = "Jahr")

# 1.6)
ggplot(hiv_alc_df, aes(Year, SH.DYN.AIDS.ZS, color = SH.ALC.PCAP.LI)) +
  geom_point(size = 1) +
  facet_wrap( ~ `Country Name`) +
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    size = 4
  )) +
  labs(
    x = "Jahr",
    y = "HIV Prävalenz in %",
    color = "totaler Alkoholkonsum pro Kopf",
    title = "HIV Prälenz über Zeit sowie Alkoholkonsum"
  )

## 2.4)
ggplot(hiv_df,
       aes(SL.TLF.BASC.ZS, SH.DYN.AIDS.ZS, color = `Country Name`)) +
  geom_point(size = 1) +
  facet_wrap(~ Year) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black",
    size = 0.5
  ) +
  labs(x = "HIV Prävalenz", y = "Bildungsstand der Erwerbstätigen", color = "Land")
# maybe add rate of change of linear regression line

## 2.5)
ggplot(hiv_df, aes(SL.TLF.BASC.ZS, SH.DYN.AIDS.ZS, color = Year)) +
  geom_point(size = 1) +
  facet_wrap( ~ `Country Name`) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black",
    size = 0.5
  ) +
  labs(y = "HIV Prävalenz in %", x = "% Erwerbstätige mit Grundbildung", color = "Jahr")

## 2.6)
#   if high value: high HIV and low edu
#   low value: low HIV or high HIV but high edu, high edu or high HIV and high Edu
summary(hiv_df$SH.DYN.AIDS.ZS)
summary(hiv_df$SL.TLF.BASC.ZS)
ggplot(hiv_df, aes(Year, SH.DYN.AIDS.ZS / SL.TLF.BASC.ZS)) +
  geom_point(size = 1) +
  facet_wrap( ~ `Country Name`) +
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    size = 5
  )) +
  labs(x = "Jahr", y = "HIV Prävalenz / Erwerbstätige mit Bildung")

## 2.a)
# Categorize all 25 countries: (AI, https://deepai.org/chat/free-chatgpt)
# (Entwicklungs-, Schwellen- und hoch entwickelte Länder)
development_ct <- c(
  "Afghanistan",
  "Bangladesh",
  "Bolivia",
  "Cambodia",
  "Chad",
  "Tanzania",
  "Nigeria",
  "Viet Nam",
  "Ghana",
  "India",
  "Mali",
  "Pakistan",
  "Peru"
)

emerging_ct <- c("Brazil",
                 "China",
                 "Thailand",
                 "Russian Federation",
                 "India",
                 "Kazakhstan",
                 "Nigeria")

highdeveloped_ct <- c(
  "United States",
  "United Kingdom",
  "New Zealand",
  "Finland",
  "Czechia",
  "Aruba",
  "Qatar"
)
# extra column for categorization
hiv_df$dev_status <- ifelse(
  hiv_df$`Country Name` %in% development_ct,
  "development countries",
  ifelse(
    hiv_df$`Country Name` %in% emerging_ct,
    "emerging countries",
    ifelse(
      hiv_df$`Country Name` %in% highdeveloped_ct,
      "highly developed countries",
      "Unbekannt"
    )
  )
)
# Because the grouping by development status is of relative fresh data, we only
# take a look at year 2021; we can delete alc column (doesn't have any values)
hiv_development_df <- hiv_df %>%
  filter(Year == 2021) %>%
  select(-SH.ALC.PCAP.LI)
# Dataframe of grouped countries and their value
hiv_development_df <- hiv_development_df %>%
  group_by(dev_status) %>%
  summarise(
    SH.DYN.AIDS.ZS = mean(SH.DYN.AIDS.ZS, na.rm = TRUE),
    SL.TLF.BASC.ZS = mean(SL.TLF.BASC.ZS, na.rm = TRUE)
  )
# keep year-information (funktioniert nicht)
hiv_df %>%
  select(-SH.ALC.PCAP.LI) %>%
  group_by(Year) %>%
  group_by(dev_status) %>%
  summarise(
    SH.DYN.AIDS.ZS = mean(SH.DYN.AIDS.ZS, na.rm = TRUE),
    SL.TLF.BASC.ZS = mean(SL.TLF.BASC.ZS, na.rm = TRUE)
  )

## Visualising results of results
ggplot(hiv_development_df,
       aes(SL.TLF.BASC.ZS, SH.DYN.AIDS.ZS, colour = dev_status)) +
  geom_point() +
  labs(
    title = "Korrelation zwischen HIV Auftreten und Bildungsstand",
    y = "HIV Prävalenz",
    x = "Bildungsstand der Erwerbstätigen",
    colour = "Entwicklungsstand"
  )
# stimmen die Werte wirklich?

## 2b)
continent_mapping <- data.frame(
  Country = c(
    'Afghanistan',
    'Aruba',
    'Bangladesh',
    'Bolivia',
    'Brazil',
    'Cambodia',
    'Chad',
    'Russian Federation',
    'Tanzania',
    'Thailand',
    'United States',
    'United Kingdom',
    'Nigeria',
    'Viet Nam',
    'China',
    'Czechia',
    'Finland',
    'Ghana',
    'India',
    'Kazakhstan',
    'Mali',
    'New Zealand',
    'Pakistan',
    'Peru',
    'Qatar'
  ),
  continent = c(
    'Asien',
    'Nordamerika',
    'Asien',
    'Südamerika',
    'Südamerika',
    'Asien',
    'Afrika',
    'Europa',
    'Afrika',
    'Asien',
    'Nordamerika',
    'Europa',
    'Afrika',
    'Asien',
    'Asien',
    'Europa',
    'Europa',
    'Afrika',
    'Asien',
    'Asien',
    'Afrika',
    'Ozeanien',
    'Asien',
    'Südamerika',
    'Asien'
  )
)
# dataframe with continent variable
hiv_df <- hiv_df %>%
  left_join(continent_mapping, by = c("Country Name" = "Country"))
# grouped dataframe with values by mean over all years
hiv_continent_df <- hiv_df %>%
  group_by(continent) %>%
  summarise(
    SH.DYN.AIDS.ZS = mean(SH.DYN.AIDS.ZS, na.rm = TRUE),
    SL.TLF.BASC.ZS = mean(SL.TLF.BASC.ZS, na.rm = TRUE)
  )
# only year 2021
hiv_continent_2021_df <- hiv_df %>%
  filter(Year == 2021) %>%
  select(-SH.ALC.PCAP.LI) %>%
  group_by(continent) %>%
  summarise(
    SH.DYN.AIDS.ZS = mean(SH.DYN.AIDS.ZS, na.rm = TRUE),
    SL.TLF.BASC.ZS = mean(SL.TLF.BASC.ZS, na.rm = TRUE)
  )
# plot results
ggplot(hiv_continent_2021_df,
       aes(SL.TLF.BASC.ZS, SH.DYN.AIDS.ZS, colour = continent)) +
  geom_point() +
  labs(
    x = "% Erwerbstätige mit Grundbildung",
    y = "HIV Prävalenz in %",
    title = "Grundbildung vs. HIV Prävalenz",
    colour = "Kontinent"
  )
