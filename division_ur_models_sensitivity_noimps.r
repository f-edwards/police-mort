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
source("read_sensitivity_noimps.r")

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

##############################
### copied from division_ur_visuals.r


#### function to clean ur.codes for plotting:
clean_ur<-function(x){
  x<-as.character(x)
  levels<-str_to_title(substr(sort(unique(x)), 4, nchar(sort(unique(x)))))
  output<-ordered(str_to_title(substr(x, 4, nchar(x))),
                   levels = levels)
  return(output)
}

## 3: Visuals
# ... re-load models

blk.stan <- blk.stan.0
wht.stan <- wht.stan.0
lat.stan <- lat.stan.0
#tot.stan<-tot.stan.0
tmp2 <- tmp2 %>% as.data.frame()

# ... set up scenarios for prediction
# .... (9 divisions x 6 ur.codes)
ur.code  = unique(tmp2$ur.code)
division = unique(tmp2$division)
ur.division = expand.grid(division, ur.code);
names(ur.division) = c("division", "ur.code")
n = length(division)*length(ur.code)

# .... use 100,000,000 for offset to get enough trials
# .... then rescale to rate per 100,000
newdata <- tmp2[1:n,]
newdata$fips <- 0; newdata$tot.men<-newdata$black.men<-newdata$white.men<-newdata$latino.men<-100000000;
newdata$ur.code<-ur.division$ur.code; newdata$division<-ur.division$division

# ... estimate predictive posterior for each group
# .... for blacks
post.blk = posterior_predict(blk.stan, newdata = newdata)/1000
post.blk.sims <- cbind(ur.division, t(post.blk)) %>%
  gather(., key = sim, value = sim.rate, -ur.code, -division)

# .... for whites
post.wht = posterior_predict(wht.stan, newdata = newdata)/1000
post.wht.sims <- cbind(ur.division, t(post.wht)) %>%
  gather(., key = sim, value = sim.rate, -ur.code, -division)

# ... latino
post.lat = posterior_predict(lat.stan, newdata = newdata)/1000
post.lat.sims <- cbind(ur.division, t(post.lat)) %>%
  gather(., key = sim, value = sim.rate, -ur.code, -division)

# ... calculate mean and 95% uncertainty intervals for each group
post.blk.int = bind_cols(ur.division, as.data.frame(
  t(apply(post.blk, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
post.blk.int = post.blk.int %>% mutate(race = 'Black')

post.wht.int = bind_cols(ur.division, as.data.frame(
  t(apply(post.wht, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
post.wht.int = post.wht.int %>% mutate(race = 'White')

post.lat.int = bind_cols(ur.division, as.data.frame(
  t(apply(post.lat, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
post.lat.int = post.lat.int %>% mutate(race = 'Latino')

# ... bind everything together & get ready for plotting
post.estimates = bind_rows(post.blk.int, post.wht.int, post.lat.int);
colnames(post.estimates)[3:5] = c('lwr', 'estimate', 'upr')

#... appendix,  table 3
apTab3<-post.estimates%>%
  mutate(lwr=lwr*(365/2234),
         estimate=estimate*(365/2234),
         upr=upr*(365/2234))%>%
  mutate(ur.code = clean_ur(ur.code))

apTabReformat<-apTab3%>%
  mutate(estimate=paste(
    round(estimate, 1),
    " (",
    round(lwr, 1),
    ", ",
    round(upr, 1),
    ")",
    sep=""))%>%
  select(- lwr, -upr)%>%
  spread(race, estimate)%>%
  arrange(division, ur.code)%>%
  mutate(division=as.character(division),
         ur.code=as.character(ur.code))
###### write the table to html
library(xtable)
print.xtable(
  xtable(apTabReformat,
         caption = NULL,
         digits = 2,
         auto = TRUE), 
  type = "html",
  file = "./visuals/appendix_sensitivity_noimps.html",
  include.rownames = FALSE)

save.image("division_ur_models_sensitivity_noimps.RData")