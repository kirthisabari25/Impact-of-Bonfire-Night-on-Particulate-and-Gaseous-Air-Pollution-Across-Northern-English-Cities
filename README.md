# Impact-of-Bonfire-Night-on-Particulate-and-Gaseous-Air-Pollution-Across-Northern-English-Cities
Exploratory data visualisation of air pollution around Bonfire Night across UK cities using R and ggplot2.
Bonfire Night Air Pollution Analysis

**Overview**

This repository contains an R-based exploratory data visualisation of air pollution around Bonfire Night (5 November) across five UK cities: Leeds, Manchester, Sheffield, Nottingham, and York. The analysis focuses on PM2.5, PM10, and NO₂, examining short-term pollution spikes, meteorological influence, COVID vs non-COVID differences, and multivariate patterns using PCA.

**Data**

Hourly air quality and wind data are used.
The dataset (intro_ds.csv) is not included; place it in the project root to reproduce results.

**Visualisations**

Pollutant distributions (PM2.5, PM10, NO₂)

PM2.5 by baseline, bonfire, and recovery periods

PM2.5 vs wind factor

Daily PM2.5 trends (1–10 November)

PM2.5 vs PM10 by city and period

COVID vs non-COVID comparison

PCA of pollution and wind variables

All plots are created using ggplot2 with colour-blind–safe palettes.

**How to Run**

install.packages(c("tidyverse", "lubridate", "ggplot2"))

source("analysis.R")
