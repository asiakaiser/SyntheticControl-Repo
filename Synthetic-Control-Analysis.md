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

## Visualizing the data

Creating Personal Theme

``` r
Asia_Theme <- theme(
  plot.title = element_text(size = 20),
  plot.caption = element_text(size = 16),
  axis.title = element_text(size = 20),
  axis.text = element_text(size = 20),
  axis.text.x = element_text(size = 20),
  legend.text = element_text(size= 20))
```

**Trends**: plot of trends of observed (treated) unit compared to
synthetic control over time, with dashed vertical line representing the
treatment event.

``` r
trendplot <- gbifcity_out %>% plot_trends()+
  Asia_Theme +
  labs(title = "Synthetic Control Method", caption = NULL,
        x = "Year",
        y = "Bee observations")
trendplot
```

![](Synthetic-Control-Analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

**Differences**: plot of difference in outcome variable between the
observed (treated) unit and the estimated outcome of the synthetic
control.

``` r
diffplot <- gbifcity_out %>% plot_differences()+
  Asia_Theme +
  labs(title = NULL, subtitle = NULL,
        x = "Year",
        y = "(Estimated - Observed) Bee observations")
```

**Weights**: Barplots showing the relative contributions (weights) of
each control unit and each predictor/variable unit to the synthetic
control.

``` r
weightplot <- gbifcity_out %>% plot_weights()+
  Asia_Theme +
  labs(title = NULL, subtitle = NULL,
       y = "Weight")
weightplot
```

![](Synthetic-Control-Analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

**Balance Table**: A table with values of the predictors/variables so
that you can see how similar the synthetic control is to observed
variables of the treated unit.

``` r
balancetab <- gbifcity_out %>% grab_balance_table()
```

## Inference

**In-space placebos**: The same as difference plot above, this time
comparing each control unit to the synthetic control. This is to see if
the difference between the treated unit and the synthetic control
actually stands out as significantly greater.

``` r
placebos <- gbifcity_out %>% plot_placebos()+
  Asia_Theme +
  labs(title = NULL, subtitle = NULL,
        x = "Year",
        y = "(Estimated - Observed) Bee observations")
placebos
```

![](Synthetic-Control-Analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
?plot_placebos
```

**Ratio of pre and post intervention period mean squared predictive
error (mspe)**

``` r
mpseratio <- gbifcity_out %>% plot_mspe_ratio()+
  Asia_Theme +
  labs(title = NULL, subtitle = NULL)
mpseratio
```

![](Synthetic-Control-Analysis_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### Significance table

**Constructing Fisher’s Exact P-value**: This table takes the mspe ratio
and lists units in descending order. P-values are constructed by
dividing a units rank by the total. *You need at least 20 cases in the
unit pool to achieve a p-value below .05*

``` r
significance.tab <- gbifcity_out %>% grab_significance()
significance.tab <- as.data.frame(significance.tab)

#This code provides the causal estimate value (difference in outcome between the synthetic control and the observed treated unit)
gbifcity_out %>% grab_synthetic_control()%>%
  filter(time_unit == 2022)%>%
  mutate(causal_estimate= synth_y - real_y)%>%
  mutate(percent_estimate= (synth_y - real_y)/synth_y)%>%
  head
```

    ## # A tibble: 1 × 5
    ##   time_unit real_y synth_y causal_estimate percent_estimate
    ##       <int>  <int>   <dbl>           <dbl>            <dbl>
    ## 1      2022    238    278.            40.3            0.145

### Confidence Intervals

**Extracting Confidence Intervals**: This package does not directly
provide confidence intervals as an output. However, they can be
generated by reassigning the treatment to other units and computing the
treatment effect for these “placebo” groups. This helps you to generate
a distribution of treatment effects under the null hypothesis of no
treatment effect.

``` r
# Extract all synthetic control results (including placebo units)
all_synth_results <- gbifcity_out %>%
  grab_synthetic_control(placebo = TRUE)

# Extract placebo gaps (exclude the treated unit)
placebo_gaps <- all_synth_results %>%
  filter(.placebo != 0) %>% 
    mutate(gap = synth_y - real_y)%>%
  select(time_unit, gap, .id)

# Extract the actual treated unit's gaps
treated_gaps <- all_synth_results %>%
  filter(.placebo == 0) %>%
    mutate(gap = synth_y - real_y)%>%
  select(time_unit, gap)

# Calculate empirical 95% confidence intervals for placebo gaps
# For each time point, we compute the 2.5th and 97.5th percentiles of the placebo gaps
gap_quantiles <- placebo_gaps %>%
  group_by(time_unit) %>%
  summarize(lower_ci = quantile(gap, probs = 0.025),   # 2.5th percentile
            upper_ci = quantile(gap, probs = 0.975))   # 97.5th percentile

# Combine the treated unit's gaps with the confidence intervals
treated_gaps_with_ci <- treated_gaps %>%
  left_join(gap_quantiles, by = "time_unit")

# Print the resulting dataframe with treated gaps and confidence intervals
print(treated_gaps_with_ci)
```

    ## # A tibble: 14 × 4
    ##    time_unit     gap lower_ci upper_ci
    ##        <int>   <dbl>    <dbl>    <dbl>
    ##  1      2010  0.474     -1.78    0.773
    ##  2      2011  0.796     -1.39    1.07 
    ##  3      2012  0.619     -6.35    4.96 
    ##  4      2013  4.47     -13.0     5.50 
    ##  5      2014  2.05     -16.5     9.86 
    ##  6      2015  6.17     -19.7    15.4  
    ##  7      2016  7.09     -75.9    34.5  
    ##  8      2017  4.86     -75.4    39.7  
    ##  9      2018  0.0466   -97.3    71.9  
    ## 10      2019 -2.42    -165.    126.   
    ## 11      2020  0.406   -228.    175.   
    ## 12      2021 -1.20    -258.    176.   
    ## 13      2022 40.3     -225.    133.   
    ## 14      2023 39.8     -310.    193.

# Other Approaches

## Difference in Difference w/ most similar city

``` r
#Creating the relevant dummy variables
city.data.did <- city.data %>%
  mutate(Time = ifelse(year >= 2021, 1, 0)) %>%
  mutate(Treated = if_else(City == "Philadelphia",1,0))%>%
    filter(City == "Philadelphia" | City == "Chicago")

#Running the Difference in Difference Regression
did.model <- lm(n.obs45km ~ Treated*Time, data = city.data.did)
summary(did.model)
```

    ## 
    ## Call:
    ## lm(formula = n.obs45km ~ Treated * Time, data = city.data.did)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -93.33 -32.91 -26.12  14.64 162.64 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    39.364     18.667   2.109   0.0456 *  
    ## Treated        -6.455     26.400  -0.244   0.8089    
    ## Time          321.970     40.326   7.984 3.27e-08 ***
    ## Treated:Time  -99.545     57.030  -1.745   0.0937 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 61.91 on 24 degrees of freedom
    ## Multiple R-squared:  0.7993, Adjusted R-squared:  0.7742 
    ## F-statistic: 31.86 on 3 and 24 DF,  p-value: 1.554e-08

``` r
confint(did.model)
```

    ##                     2.5 %    97.5 %
    ## (Intercept)     0.8359689  77.89130
    ## Treated       -60.9408953  48.03180
    ## Time          238.7404228 405.19897
    ## Treated:Time -217.2494229  18.15851

### Simple Average Plot

- Creating a data frame with observation values from all donor cities
  equally weighted in average.

``` r
#Observed outcomes
philly.data <- city.data %>%
  filter(City == "Philadelphia") %>%
  pull(n.obs45km)

#Synthetic outcomes

Synthetic <- gbifcity_out %>% grab_synthetic_control()

#Dataframe with observed, simple average, and synthetic control
city.data.avg <- city.data %>%
  filter(City != "Philadelphia") %>%
  group_by(year) %>%
  summarise(Simple_Average = mean(n.obs45km))%>%
  mutate(Observed = philly.data)%>%
  mutate(Synthetic = Synthetic$synth_y)%>%
  pivot_longer(cols = c(Simple_Average, Observed, Synthetic), 
               names_to = "Unit", 
               values_to = "Observations")
```

*Simple average trend plot*

``` r
palette.colors(palette = "Okabe-Ito")
```

    ## [1] "#000000" "#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00"
    ## [8] "#CC79A7" "#999999"

``` r
did.trendplot <- ggplot(city.data.did, aes(x = year, y = n.obs45km, color = City)) +
  geom_line(linewidth = 1, aes(linetype = City))+
  geom_point()+
  scale_linetype_manual(values=c(1,3))+
  scale_color_manual(values=c('#D55E00','#56B4E9'))+
  geom_vline(xintercept = 2021, linetype="dashed")+
  labs(title = "Difference in Difference",
       x = "Year",
       y = "Bee observations") +
  theme_minimal()+
  theme(legend.position="bottom",legend.title = element_blank())+
  Asia_Theme

did.trendplot
```

![](Synthetic-Control-Analysis_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## Interrupted Time Series Regression

<https://rpubs.com/chrissyhroberts/1006858>

``` r
#Creating the relevant dummy variables in dataframe
city.data.its <- city.data %>%
  filter(City== "Philadelphia" & year >= 2017)%>%
  mutate(Treated = if_else(year >= 2021, 1, 0))%>%
  mutate(Time_since= if_else(year < 2021, 0, (year-2020)))
city.data.its
```

    ##           City State_Abbrev        State area_km2      Lat      Long  ID
    ## 1 Philadelphia           PA Pennsylvania      348 39.95258 -75.16522 249
    ## 2 Philadelphia           PA Pennsylvania      348 39.95258 -75.16522 249
    ## 3 Philadelphia           PA Pennsylvania      348 39.95258 -75.16522 249
    ## 4 Philadelphia           PA Pennsylvania      348 39.95258 -75.16522 249
    ## 5 Philadelphia           PA Pennsylvania      348 39.95258 -75.16522 249
    ## 6 Philadelphia           PA Pennsylvania      348 39.95258 -75.16522 249
    ## 7 Philadelphia           PA Pennsylvania      348 39.95258 -75.16522 249
    ##       Temp Prec   tcc45km imperv45km year Population n.obs45km  rich45km
    ## 1 12.74951 1141 252934612  127985949 2017    1580601        16  5.234375
    ## 2 12.74951 1141 252934612  127985949 2018    1583592        45 35.800000
    ## 3 12.74951 1141 252934612  127985949 2019    1584064        98 22.938776
    ## 4 12.74951 1141 252934612  127985949 2020    1600684       189 35.947090
    ## 5 12.74951 1141 252934612  127985949 2021    1589623       229 55.393013
    ## 6 12.74951 1141 252934612  127985949 2022    1566836       238 69.973214
    ## 7 12.74951 1141 252934612  127985949 2023    1550542       299 37.536975
    ##   shann45km Treated Time_since
    ## 1  4.494448       0          0
    ## 2 15.464085       0          0
    ## 3 10.665252       0          0
    ## 4 11.225539       0          0
    ## 5 12.968323       1          1
    ## 6 14.903670       1          2
    ## 7 12.043038       1          3

``` r
#Running the interrupted time series regression
its.model <- lm(n.obs45km ~ year + Treated + Time_since, data = city.data.its)
summary(its.model)
```

    ## 
    ## Call:
    ## lm(formula = n.obs45km ~ year + Treated + Time_since, data = city.data.its)
    ## 
    ## Residuals:
    ##       1       2       3       4       5       6       7 
    ##  14.800 -13.400 -17.600  16.200   8.667 -17.333   8.667 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -1.154e+05  1.965e+04  -5.871  0.00985 **
    ## year         5.720e+01  9.735e+00   5.876  0.00983 **
    ## Treated      1.253e+01  3.791e+01   0.331  0.76269   
    ## Time_since  -2.220e+01  1.821e+01  -1.219  0.30996   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 21.77 on 3 degrees of freedom
    ## Multiple R-squared:  0.9793, Adjusted R-squared:  0.9587 
    ## F-statistic: 47.41 on 3 and 3 DF,  p-value: 0.00501

``` r
confint(its.model)
```

    ##                     2.5 %       97.5 %
    ## (Intercept) -177904.68256 -52837.71744
    ## year             26.21983     88.18017
    ## Treated        -108.11709    133.18376
    ## Time_since      -80.15859     35.75859

# Save Figures

``` r
trends.figure <- ggarrange(trendplot,did.trendplot,
                    labels = c("A", "B"))

ggsave("figures/Trend plot.png", plot = trendplot)
```

    ## Saving 7 x 5 in image

``` r
ggsave("figures/Difference plot.png", plot = diffplot)
```

    ## Saving 7 x 5 in image

``` r
ggsave("figures/Weight plot.png", plot = weightplot)
```

    ## Saving 7 x 5 in image

``` r
ggsave("figures/Placebos plot.png", plot = placebos)
```

    ## Saving 7 x 5 in image

``` r
ggsave("figures/MPSE ratio plot.png", plot = mpseratio)
```

    ## Saving 7 x 5 in image

``` r
ggsave("figures/Abundance estimates trend plot.png", plot = did.trendplot)
```

    ## Saving 7 x 5 in image

``` r
ggsave("figures/Method comparison plot.png", plot = trends.figure, height = 6, width = 12)
```

# Citations

``` r
citation()
```

    ## To cite R in publications use:
    ## 
    ##   R Core Team (2024). _R: A Language and Environment for Statistical
    ##   Computing_. R Foundation for Statistical Computing, Vienna, Austria.
    ##   <https://www.R-project.org/>.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {R: A Language and Environment for Statistical Computing},
    ##     author = {{R Core Team}},
    ##     organization = {R Foundation for Statistical Computing},
    ##     address = {Vienna, Austria},
    ##     year = {2024},
    ##     url = {https://www.R-project.org/},
    ##   }
    ## 
    ## We have invested a lot of time and effort in creating R, please cite it
    ## when using it for data analysis. See also 'citation("pkgname")' for
    ## citing R packages.

``` r
devtools::session_info()
```

    ## ─ Session info ───────────────────────────────────────────────────────────────
    ##  setting  value
    ##  version  R version 4.4.0 (2024-04-24)
    ##  os       macOS 15.0.1
    ##  system   x86_64, darwin20
    ##  ui       X11
    ##  language (EN)
    ##  collate  en_US.UTF-8
    ##  ctype    en_US.UTF-8
    ##  tz       America/Denver
    ##  date     2024-11-20
    ##  pandoc   3.1.11 @ /Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/x86_64/ (via rmarkdown)
    ## 
    ## ─ Packages ───────────────────────────────────────────────────────────────────
    ##  package     * version    date (UTC) lib source
    ##  abind         1.4-8      2024-09-12 [1] CRAN (R 4.4.1)
    ##  backports     1.5.0      2024-05-23 [1] CRAN (R 4.4.0)
    ##  broom         1.0.6      2024-05-17 [1] CRAN (R 4.4.0)
    ##  cachem        1.1.0      2024-05-16 [1] CRAN (R 4.4.0)
    ##  car           3.1-2      2023-03-30 [1] CRAN (R 4.4.0)
    ##  carData       3.0-5      2022-01-06 [1] CRAN (R 4.4.0)
    ##  cli           3.6.3      2024-06-21 [1] CRAN (R 4.4.0)
    ##  colorspace    2.1-0      2023-01-23 [1] CRAN (R 4.4.0)
    ##  cowplot       1.1.3      2024-01-22 [1] CRAN (R 4.4.0)
    ##  devtools      2.4.5      2022-10-11 [1] CRAN (R 4.4.0)
    ##  digest        0.6.36     2024-06-23 [1] CRAN (R 4.4.0)
    ##  dplyr       * 1.1.4      2023-11-17 [1] CRAN (R 4.4.0)
    ##  ellipsis      0.3.2      2021-04-29 [1] CRAN (R 4.4.0)
    ##  evaluate      0.24.0     2024-06-10 [1] CRAN (R 4.4.0)
    ##  fansi         1.0.6      2023-12-08 [1] CRAN (R 4.4.0)
    ##  farver        2.1.2      2024-05-13 [1] CRAN (R 4.4.0)
    ##  fastmap       1.2.0      2024-05-15 [1] CRAN (R 4.4.0)
    ##  forcats     * 1.0.0      2023-01-29 [1] CRAN (R 4.4.0)
    ##  fs            1.6.4      2024-04-25 [1] CRAN (R 4.4.0)
    ##  generics      0.1.3      2022-07-05 [1] CRAN (R 4.4.0)
    ##  ggplot2     * 3.5.1      2024-04-23 [1] CRAN (R 4.4.0)
    ##  ggpubr      * 0.6.0      2023-02-10 [1] CRAN (R 4.4.0)
    ##  ggsignif      0.6.4      2022-10-13 [1] CRAN (R 4.4.0)
    ##  glue          1.8.0      2024-09-30 [1] CRAN (R 4.4.1)
    ##  gtable        0.3.5      2024-04-22 [1] CRAN (R 4.4.0)
    ##  highr         0.11       2024-05-26 [1] CRAN (R 4.4.0)
    ##  hms           1.1.3      2023-03-21 [1] CRAN (R 4.4.0)
    ##  htmltools     0.5.8.1    2024-04-04 [1] CRAN (R 4.4.0)
    ##  htmlwidgets   1.6.4      2023-12-06 [1] CRAN (R 4.4.0)
    ##  httpuv        1.6.15     2024-03-26 [1] CRAN (R 4.4.0)
    ##  kernlab       0.9-32     2023-01-31 [1] CRAN (R 4.4.0)
    ##  knitr         1.48       2024-07-07 [1] CRAN (R 4.4.0)
    ##  labeling      0.4.3      2023-08-29 [1] CRAN (R 4.4.0)
    ##  later         1.3.2      2023-12-06 [1] CRAN (R 4.4.0)
    ##  lifecycle     1.0.4      2023-11-07 [1] CRAN (R 4.4.0)
    ##  lubridate   * 1.9.3      2023-09-27 [1] CRAN (R 4.4.0)
    ##  magrittr      2.0.3      2022-03-30 [1] CRAN (R 4.4.0)
    ##  memoise       2.0.1      2021-11-26 [1] CRAN (R 4.4.0)
    ##  mime          0.12       2021-09-28 [1] CRAN (R 4.4.0)
    ##  miniUI        0.1.1.1    2018-05-18 [1] CRAN (R 4.4.0)
    ##  munsell       0.5.1      2024-04-01 [1] CRAN (R 4.4.0)
    ##  nloptr        2.0.3      2022-05-26 [1] CRAN (R 4.4.0)
    ##  numDeriv      2016.8-1.1 2019-06-06 [1] CRAN (R 4.4.0)
    ##  optimx        2023-10.21 2023-10-24 [1] CRAN (R 4.4.0)
    ##  pillar        1.9.0      2023-03-22 [1] CRAN (R 4.4.0)
    ##  pkgbuild      1.4.4      2024-03-17 [1] CRAN (R 4.4.0)
    ##  pkgconfig     2.0.3      2019-09-22 [1] CRAN (R 4.4.0)
    ##  pkgload       1.3.4      2024-01-16 [1] CRAN (R 4.4.0)
    ##  pracma        2.4.4      2023-11-10 [1] CRAN (R 4.4.0)
    ##  profvis       0.3.8      2023-05-02 [1] CRAN (R 4.4.0)
    ##  promises      1.3.0      2024-04-05 [1] CRAN (R 4.4.0)
    ##  purrr       * 1.0.2      2023-08-10 [1] CRAN (R 4.4.0)
    ##  R6            2.5.1      2021-08-19 [1] CRAN (R 4.4.0)
    ##  ragg          1.3.2      2024-05-15 [1] CRAN (R 4.4.0)
    ##  Rcpp          1.0.12     2024-01-09 [1] CRAN (R 4.4.0)
    ##  readr       * 2.1.5      2024-01-10 [1] CRAN (R 4.4.0)
    ##  remotes       2.5.0      2024-03-17 [1] CRAN (R 4.4.0)
    ##  rlang         1.1.4      2024-06-04 [1] CRAN (R 4.4.0)
    ##  rmarkdown     2.27       2024-05-17 [1] CRAN (R 4.4.0)
    ##  rstatix       0.7.2      2023-02-01 [1] CRAN (R 4.4.0)
    ##  rstudioapi    0.16.0     2024-03-24 [1] CRAN (R 4.4.0)
    ##  scales        1.3.0      2023-11-28 [1] CRAN (R 4.4.0)
    ##  sessioninfo   1.2.2      2021-12-06 [1] CRAN (R 4.4.0)
    ##  shiny         1.8.1.1    2024-04-02 [1] CRAN (R 4.4.0)
    ##  stringi       1.8.4      2024-05-06 [1] CRAN (R 4.4.0)
    ##  stringr     * 1.5.1      2023-11-14 [1] CRAN (R 4.4.0)
    ##  systemfonts   1.1.0      2024-05-15 [1] CRAN (R 4.4.0)
    ##  textshaping   0.4.0      2024-05-24 [1] CRAN (R 4.4.0)
    ##  tibble      * 3.2.1      2023-03-20 [1] CRAN (R 4.4.0)
    ##  tidyr       * 1.3.1      2024-01-24 [1] CRAN (R 4.4.0)
    ##  tidyselect    1.2.1      2024-03-11 [1] CRAN (R 4.4.0)
    ##  tidysynth   * 0.2.0      2023-05-21 [1] CRAN (R 4.4.0)
    ##  tidyverse   * 2.0.0      2023-02-22 [1] CRAN (R 4.4.0)
    ##  timechange    0.3.0      2024-01-18 [1] CRAN (R 4.4.0)
    ##  tzdb          0.4.0      2023-05-12 [1] CRAN (R 4.4.0)
    ##  urlchecker    1.0.1      2021-11-30 [1] CRAN (R 4.4.0)
    ##  usethis       2.2.3      2024-02-19 [1] CRAN (R 4.4.0)
    ##  utf8          1.2.4      2023-10-22 [1] CRAN (R 4.4.0)
    ##  vctrs         0.6.5      2023-12-01 [1] CRAN (R 4.4.0)
    ##  withr         3.0.0      2024-01-16 [1] CRAN (R 4.4.0)
    ##  xfun          0.45       2024-06-16 [1] CRAN (R 4.4.0)
    ##  xtable        1.8-4      2019-04-21 [1] CRAN (R 4.4.0)
    ##  yaml          2.3.9      2024-07-05 [1] CRAN (R 4.4.0)
    ## 
    ##  [1] /Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/library
    ## 
    ## ──────────────────────────────────────────────────────────────────────────────

``` r
c("tidyverse", "tidysynth") %>%
  map(citation) %>%
  print(style = "text")
```

    ## [[1]]
    ## Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R,
    ## Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller E,
    ## Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi
    ## K, Vaughan D, Wilke C, Woo K, Yutani H (2019). "Welcome to the
    ## tidyverse." _Journal of Open Source Software_, *4*(43), 1686.
    ## doi:10.21105/joss.01686 <https://doi.org/10.21105/joss.01686>.
    ## 
    ## [[2]]
    ## Dunford E (2023). _tidysynth: A Tidy Implementation of the Synthetic
    ## Control Method_. R package version 0.2.0,
    ## <https://CRAN.R-project.org/package=tidysynth>.
