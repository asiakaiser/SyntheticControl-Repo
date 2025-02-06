library(scpi)
library(data.table)
library(tidyverse)

rm(list = ls())
city.data <- read.csv("city_dat.csv")

#creating variables
donor.cities <- unique(city.data$City)
donor.cities <- donor.cities[!(donor.cities == "Philadelphia")]
city.data <- city.data %>%
    rename("nobs45km" = "n.obs45km")
feature.vars <- c("Temp","Prec","imperv45km","Population","nobs45km","tcc45km")


#scdata object
dat <- scdata(df = city.data,
              id.var = "City",
              time.var = "year",
              outcome.var = "nobs45km",
              period.pre = (2015:2021),
              period.post = (2022),
              unit.tr = "Philadelphia",
              unit.co = donor.cities,
              features = feature.vars,
              cov.adj = list('Temp' = c("constant"),
                             'Prec' = c("constant"),
                             'imperv45km' = c("constant"),
                             'Population' = c("trend"),
                             'nobs45km' = c("trend"),
                             'tcc45km' = c("constant")))
summary(dat)

#scest object
est.si  <- scest(data = dat)

#seems to be no significant correlation between predictor variables
city.data.cors <- city.data |>
    select("Temp","Prec","tcc45km","imperv45km","Population","nobs45km")
cor(city.data.cors)

#Estimating uncertainty
sims <- 1000
u.order <- 1
u.lags <- 0
u.sigma <- "HC1"
u.missp <- TRUE
e.order <- 1
e.lags <- 0
e.method <- "gaussian"
lgapp <- "linear"
cores <- 1
set.seed(0)

res.pi <- scpi(data = dat, sims = sims, e.method = e.method, e.order = e.order, e.lags = e.lags, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, u.missp = u.missp, cores = cores, w.constr = list(name = "simplex"))

summary(res.pi)

res.pi$inference.results

scplot(
    res.pi,
    fig.path = NULL,
    fig.name = NULL,
    fig.format = "png",
    scplot 31
    e.out = TRUE,
    joint = FALSE,
    col.treated = "black",
    col.synth = "mediumblue",
    label.xy = NULL,
    plot.range = NULL,
    x.ticks = NULL,
    event.label = NULL,
    plot.specs = NULL,
    save.d)

scplot(res.pi,
       e.out = TRUE)
#knitr::stitch('scpi.r')
