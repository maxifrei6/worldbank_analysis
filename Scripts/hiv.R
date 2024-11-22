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
# 1.2)  
# 1.3)  
# 1.4)  
# 1.5)  
# 1.6)  
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
# 2.5) facetting by Country, year over colour, corr of education and HIV displayed
# 2.6) y-axis: HIV Prevalence per Labour Force with basic information (HIV/EDU) 
#       
# 2.7)  
# 2.8)  

## Adjust dataframe
hiv_df <- full_data %>%
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
for (i in seq_len(25)){
  if (j %in% which(is.na(hiv_df$SL.TLF.BASC.ZS))){
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
hiv_year_df <- function(year){
  hiv_df %>% 
    filter(Year %in% year)
}

## 2.4) 
ggplot(hiv_df,
       aes(SL.TLF.BASC.ZS, SH.DYN.AIDS.ZS, color = `Country Name`)) +
  geom_point(size = 1) +
  facet_wrap(~ Year) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.5) +
  labs(x = "HIV Prävalenz", y = "Bildungsstand der Erwerbstätigen", color = "Land")
# maybe add rate of change of linear regression line

## 2.5)
ggplot(hiv_df,
       aes(SL.TLF.BASC.ZS, SH.DYN.AIDS.ZS, color = Year)) +
  geom_point(size = 1) +
  facet_wrap(~ `Country Name`) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.5) +
  labs(x = "HIV Prävalenz", y = "Bildungsstand der Erwerbstätigen", color = "Jahr")

## 2.6)
#   if high value: high HIV and low edu
#   low value: low HIV or high HIV but high edu, high edu or high HIV and high Edu
summary(hiv_df$SH.DYN.AIDS.ZS)
summary(hiv_df$SL.TLF.BASC.ZS)
ggplot(hiv_df,
       aes(Year, SH.DYN.AIDS.ZS/SL.TLF.BASC.ZS)) +
  geom_point(size = 1) +
  facet_wrap(~ `Country Name`) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5)) +
  labs(x = "Jahr", y = "HIV Prävalenz / Erwerbstätige mit Bildung")
