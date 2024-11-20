Synthetic Control Analysis
================
Asia Kaiser
2024-10-17

# Load Packages

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(tidysynth)
library(purrr)
library(ggpubr)
```

# Load in Data

``` r
rm(list = ls())
city.data <- read.csv("data/cities.scm.input.csv")
```

DELETE THIS CHUNK AFTER RERUNNING ‘GBIF DATA EXTRACTION SCRIPT’

``` r
#Subset for cities with closer number of observations during treatment year
cities.pool <- city.data %>%
  filter(year == 2021) %>%
  filter(n.obs45km > 75 & n.obs45km < 500)

#Extract the city column
cities.pool <-cities.pool$City

#Keeping subset of cities in final dataframe
#Removing Boston, Atlanta and Baltimore for experiencing this treatment. Removing Houston for abnormal abundance spike in 2016.
city.data <- city.data %>%
  filter(City != "Boston"& City != "Atlanta" & City != "Baltimore" & City != "Houston")%>%
  filter(City %in% cities.pool)
```

# Synthetic Control Analysis: Abundance

## Creating the Synthetic Control

- Creating the control objects
- Selecting the predictors
- Generating weights

``` r
# Create synthetic control object
gbifcity_out <- city.data %>%
  # Initial synthetic control object
  synthetic_control(outcome = n.obs45km, # outcome
                    unit = City, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "Philadelphia", # unit where the intervention occurred
                    i_time = 2021, # time period when the intervention occurred
                    generate_placebos = TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  
  #Generate aggregate predictors to fit weights to each unit
  generate_predictor(time_window = 2015:2023,
                     Temperature = mean(Temp,rm.na= TRUE),
                     Precipitation = mean(Prec, rm.na= TRUE),
                     Population = mean(Population, rm.na= TRUE),
                     ) %>%
  
  generate_predictor(time_window = 2021,
                     "Tree canopy cover" = tcc45km,                     ,
                     "Impervious surface" = imperv45km,
                     "Area (km2)" = area_km2
                     ) %>%

  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 2015:2021, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  
  # Generate the synthetic control
  generate_control()
```
