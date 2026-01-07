#libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
#----------Distribution of PM2.5 PM10 no2(all cities,all years)--------
data <- read_csv("intro_ds.csv") %>%
  mutate(
    date=dmy(date),
    year=year(date),
    no2=pmax(0,no2),
    pm10=pmax(0,pm10),
    pm25=pmax(0,pm25))
# Long format
data_long<-data %>%
  pivot_longer(
    cols =c(pm25,pm10,no2),
    names_to="pollutant",
    values_to="value") %>%mutate(
    pollutant=factor(
      pollutant,
      levels=c("pm25","pm10","no2"),
      labels=c("PM2.5","PM10","NO2")))
#colour blind palettes
colour<- c("PM2.5"="#0072B2", #Brilliant Blue  
         "PM10"="#E69F00",  # saturated Orange 
         "NO2"="#009E73")   #Bluish Green
ggplot(data_long, aes(x=value, fill=pollutant)) +
  geom_histogram(bins=30, colour="white") +
  facet_wrap(~ pollutant, scales="free_x") +
  scale_fill_manual(values=colour,guide="none") +
  labs(
    title="Distributions of PM2.5, PM10, and NO2 (all cities, all years)",
    x="Concentration (µg/m³)",y="Count",
    caption="Figure 1. Faceted histograms showing the distributions of PM2.5, PM10, and NO2 concentrations across all cities and years." ) +
  theme_minimal()+theme(
    plot.caption=element_text(hjust = 0.5))


#--------------------------PM2.5 by period and city-----------------------------------

# Create period variable (Baseline / Bonfire / Recovery)
data<-data %>%
  mutate(
    bonfire_days=as.numeric(date - as.Date(paste0(year, "-11-01"))),
    period=case_when(
      bonfire_days<4  ~ "Baseline",
      bonfire_days==4 ~ "Bonfire",
      bonfire_days>4  ~ "Recovery"))
#Colour blind safe palette for cities
colour2<-c(
  "Leeds"="#E69F00",     #Hot Butter
  "Manchester"="#56B4E9",# sky blue
  "Nottingham"="#009E73",#bluish-green
  "Sheffield"="#F0E442",#warm yellow
  "York"="#0072B2")#Brilliant Blue
#Boxplot chart
ggplot(data, aes(x=period, y=pm25, fill=city)) +
  geom_boxplot(alpha=0.8, outlier.alpha=0.4) +
  scale_fill_manual(values=colour2) +
  labs(
    title="PM2.5 by period and city",
    x="",
    y="PM2.5 (µg/m³)",
    fill="City",
    caption="Figure 2. Distribution of PM2.5 concentrations across baseline, bonfire, and recovery periods for each city.") +
  theme_minimal()+ theme(
    plot.caption=element_text(hjust=0.5))


#-------------------------------PM2.5 vs wind factor---------------------------
pm25_data <- data %>% filter(!is.na(pm25))
ggplot(pm25_data, aes(x=wind_factor, y = pm25)) +
  geom_point(alpha=0.3, colour ="#0072B2") +  #Brilliant Blue
  geom_smooth(method ="lm", se =FALSE, colour="#D55E00") +  #Burnt Orange
  labs(
    title="PM2.5 vs wind factor",
    x="Wind factor",
    y="PM2.5 (µg/m³)",
    caption="Figure 3. Relationship between PM2.5 concentration and wind factor across all observations.") +
  theme_minimal()+ theme(
    plot.caption=element_text(hjust=0.5))


#--------------------Time-series PM2.5 around Bonfire Night---------------------
colour3 <- c(
  "Leeds"="#E69F00",#Hot Butter
  "Manchester"="#56B4E9",#clear blue
  "Nottingham"="#009E73",#Bright Cyan
  "Sheffield"="#D55E00",  # Burnt Orange
  "York"="#0072B2")#bright blue
pm25_daily= data %>%
  group_by(city,year,date) %>%
  summarise(pm25_mean=mean(pm25,na.rm=TRUE), .groups="drop") %>%
  mutate(day_index=as.numeric(date - as.Date(paste0(year,"-11-01"))))
ggplot(pm25_daily, aes(x=day_index, y=pm25_mean, colour=city)) +
  geom_line(linewidth=0.7) +
  geom_vline(xintercept=4, linetype="dashed", colour="black") +#for vertical line
  scale_colour_manual(values=colour3) +
  scale_x_continuous(
    breaks=0:9,
    labels =c("1 Nov","2","3","4","5 (Bonfire)","6","7","8","9","10")) +
  labs(
    title = "Daily mean PM2.5 around Bonfire Night",
    x = "Day in November",
    y = "PM2.5 (µg/m³)",
    colour = "City",
    caption = "Figure 4. Daily mean PM2.5 concentrations around Bonfire Night (1–10 November) across cities.") +
  theme_minimal()+ theme(
    plot.caption = element_text(hjust= 0.5))


#-----------------------PM2.5 vs PM10 by period and city------------------------
data=data %>%
  filter(month(date) == 11,day(date)>=1,day(date)<=10) %>%
  mutate(
    bonfire_days=day(date) - 1,
    period=case_when(
      bonfire_days<4~"Baseline",
      bonfire_days==4~"Bonfire",
      bonfire_days>4~"Recovery"),
    period=factor(period,levels=c("Baseline","Bonfire","Recovery")))
ggplot(data,aes(x= pm25, y =pm10, colour=period)) +
  geom_point(alpha=0.3) +
  scale_colour_manual(
    values = c(
      "Baseline" = "#1AFF1A",#vibrant green
      "Bonfire"  = "#E66100",#bold orange
      "Recovery" = "#D35FB7")) +#magenta 
  facet_wrap(~city) +
  labs(
    title="Relationship between PM2.5 and PM10 by city and period",
    x= "PM2.5 (µg/m³)",
    y ="PM10 (µg/m³)",
    colour="Period",
    caption="Figure 5. Relationship between PM2.5 and PM10 concentrations by period across cities.") +
  theme_minimal()+
  theme(
    plot.caption =element_text(hjust=0.5))

#----------------COVID vs non COVID----------------------
data <- read_csv("intro_ds.csv") %>%
  mutate(
    date=dmy(date),
    year=year(date),
    no2=pmax(0,no2),
    pm10=pmax(0,pm10),
    pm25=pmax(0,pm25),
    bonfire= as.numeric(date -as.Date(paste0(year, "-11-01"))),
    period=case_when(
      bonfire<4~"Baseline",
      bonfire==4~"Bonfire",
      bonfire>4~ "Recovery"),
    covid_period=if_else(year %in% c(2020, 2021),
                           "COVID years", "Non‑COVID years"))
pm25_covid_city <- data %>%
  group_by(city, covid_period, period) %>%
  summarise(
    mean_pm25 = mean(pm25, na.rm = TRUE),
    .groups = "drop")
# Faceted plot: one panel per city, lines for covid vs non‑covid
ggplot(pm25_covid_city,
       aes(x=period,y=mean_pm25,
           colour=covid_period,group=covid_period)) +
  geom_line(linewidth=0.9) +
  geom_point(size=2) +
  facet_wrap(~ city) +
  labs(title ="Mean PM2.5 by period in COVID vs non COVID years, by city",
       x="Period",
       y="Mean PM2.5 (µg/m³)",
       colour="Year type",
       caption="Figure 6. Mean PM2.5 concentrations (µg/m³) by period (baseline, Bonfire, recovery) during COVID and non-COVID years, faceted by city.") +
  theme_minimal()+theme(
    plot.caption=element_text(hjust= 0.5))


#--------------------------------PCA of pollution+wind--------------------------------
pca <- prcomp(agg %>% dplyr::select(mean_pm25,
                                    mean_pm10, 
                                    mean_no2, 
                                    mean_wind),
              scale = TRUE)
#extracting the PC1 and PC2 into a plotting table
agg.pca <- data.frame(
  city = agg$city,
  period = agg$period,
  covid_period = agg$covid_period %>%
    str_replace("Non‑COVID years", "Non-COVID years") %>%  
    factor(levels = c("Non-COVID years", "COVID years")),
  PC1 = pca$x[,1],
  PC2 = pca$x[,2])
# scatterplot of PC1 vs PC2
ggplot(agg.pca, aes(PC1, PC2)) +
  geom_point(aes(colour = covid_period), size = 2.5, alpha = 0.7) +
  facet_wrap(~ period) +
  scale_colour_manual(
    values = c("Non-COVID years" = "#FFC20A","COVID years" = "#DC3220")) +
  labs(
    title = "PCA of pollution + wind (city–year–period means)",
    x = "PC1", y = "PC2", colour = "Year type",
    caption = "Figure 7. PCA of pollution and wind variables (PC1 vs PC2), faceted by baseline, bonfire, and recovery periods.")+
  theme_minimal()+
  theme(plot.caption=element_text(hjust = 0.5))


#--------------------------------------------------------------------------------





