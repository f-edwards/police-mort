#########################
# Fatal Encounters Data
# division main file, .r
# ... revision edits 
# last edit 3/12 (by: FE)
########################
## 1: Set-up
# ... attach packages; set_wd; set_seed; etc. 
rm(list=ls())
gc()
set.seed(1)
library(tidyverse)
library(rstanarm)
library(parallel)
library(data.table)
options(mc.cores = parallel::detectCores())
theme_set(theme_minimal())
setwd("~/Projects/police-mort")
select  = dplyr::select
### read and transform data
source("read.r")

#########################################################

## 2: Modeling
# ... run [death count ~ f(urban-rural code + division)] models for each group
# .... notes: 
# ..... intercept priors based on Kreager et al 2005 15-35 Black and Whitemale mortality rates, scaled for included period
# ..... additional priors weakly informative 
# ..... 2000 iterations, 4 chains, alpha_delta set high for small steps

# .... for blacks 
blk.stan.0 = stan_glmer(d.black ~ (1|ur.code) + (1|division),
						prior_intercept=normal((log(2234/365*0.94/100000)), 2.5), 
						prior = normal(0, 2.5), 
						prior_covariance = decov(1, 1, 1, 1), 
						data = tmp2, offset=I(log(black.men+1)), 
						family="neg_binomial_2", iter=2000, chains=4, 
						adapt_delta = 0.99)

# ... for whites
wht.stan.0 = stan_glmer(d.white ~ (1|ur.code) + (1|division),
                      prior_intercept=normal((log(2234/365*0.37/100000)), 2.5), 
                      prior = normal(0, 2.5),
                      prior_covariance = decov(1, 1, 1, 1),
                      data = tmp2, offset=log(white.men), 
                      family="neg_binomial_2", iter=2000, chains=4,
                      adapt_delta = 0.99)

# ... for latinos
lat.stan.0 = stan_glmer(d.latino ~ (1|ur.code) + (1|division),
                      prior_intercept=normal(log(2234/365*0.37/100000), 2.5), 
                      prior = normal(0, 2.5), 
                      prior_covariance = decov(1, 1, 1, 1),
                      data = tmp2, offset=I(log(latino.men+1)), 
                      family="neg_binomial_2", iter=2000, chains=4,
                      adapt_delta = 0.99)

# ... for total, uses prior for white mortality
tot.stan.0 = stan_glmer(d.total ~ (1|ur.code) + (1|division),
                        prior_intercept=normal(log(2234/365*0.37/100000), 2.5), 
                        prior = normal(0, 2.5), 
                        prior_covariance = decov(1, 1, 1, 1),
                        data = tmp2, offset=I(log(tot.men)), 
                        family="neg_binomial_2", iter=2000, chains=4,
                        adapt_delta = 0.99)

save.image('division_ur_models.RData')