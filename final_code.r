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

save.image('final_models_scaled_prior.RData')

################################################################

## 3: Visuals
# ... re-load models
# load('final_models.RData')
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


###############################################################
# ... Figures 1 + 2: difference between observed, median estimated rates
# .... 1: calculate expected deaths from posterior, count observed deaths/pop
# .... 2: lump divisions, UR using expected counts
###############################################################

# .... make observed deaths, pop DF
post.observed.blk<-tmp2%>%
  group_by(ur.code, division)%>%
  summarise(observed.deaths=sum(d.black),
            observed.pop=sum(black.men))%>%
  ungroup()

post.observed.lat<-tmp2%>%
  group_by(ur.code, division)%>%
  summarise(observed.deaths=sum(d.latino),
            observed.pop=sum(latino.men))%>%
  ungroup()

post.observed.wht<-tmp2%>%
  group_by(ur.code, division)%>%
  summarise(observed.deaths=sum(d.white),
            observed.pop=sum(white.men))%>%
  ungroup()

#... pull full posterior sim results,
# .... & convert into expected deaths to aggregate to ur ...
blk.ur.post.mort<-post.observed.blk%>%
  left_join(post.blk.sims)%>%
  ungroup()%>%
  select(-division)%>%
  group_by(ur.code, sim)%>%
  summarise(sim.mort.rt=sum((sim.rate/100000) * observed.pop)/sum(observed.pop) * 100000,
            obs.mort.rt=sum(observed.deaths)/sum(observed.pop)*100000)%>%
  mutate(Race="Black")

lat.ur.post.mort<-post.observed.lat%>%
  left_join(post.lat.sims)%>%
  ungroup()%>%
  select(-division)%>%
  group_by(ur.code, sim)%>%
  summarise(sim.mort.rt=sum((sim.rate/100000) * observed.pop)/sum(observed.pop) * 100000,
            obs.mort.rt=sum(observed.deaths)/sum(observed.pop)*100000)%>%
  mutate(Race="Latino")

wht.ur.post.mort<-post.observed.wht%>%
  left_join(post.wht.sims)%>%
  ungroup()%>%
  select(-division)%>%
  group_by(ur.code, sim)%>%
  summarise(sim.mort.rt=sum((sim.rate/100000) * observed.pop)/sum(observed.pop) * 100000,
            obs.mort.rt=sum(observed.deaths)/sum(observed.pop)*100000)%>%
  mutate(Race="White")

ur.post<-bind_rows(blk.ur.post.mort,
                   lat.ur.post.mort,
                   wht.ur.post.mort)

ur.post<-ur.post%>%
  ungroup()%>%
  mutate_at(vars(sim.mort.rt, obs.mort.rt), funs(./(2234/365)))

# .... and the same thing for divisions
blk.div.post.mort<-post.observed.blk%>%
  left_join(post.blk.sims)%>%
  ungroup()%>%
  select(-ur.code)%>%
  group_by(division, sim)%>%
  summarise(sim.mort.rt=sum((sim.rate/100000) * observed.pop)/sum(observed.pop) * 100000,
            obs.mort.rt=sum(observed.deaths)/sum(observed.pop)*100000)%>%
  mutate(Race="Black")

lat.div.post.mort<-post.observed.lat%>%
  left_join(post.lat.sims)%>%
  ungroup()%>%
  select(-ur.code)%>%
  group_by(division, sim)%>%
  summarise(sim.mort.rt=sum((sim.rate/100000) * observed.pop)/sum(observed.pop) * 100000,
            obs.mort.rt=sum(observed.deaths)/sum(observed.pop)*100000)%>%
  mutate(Race="Latino")

wht.div.post.mort<-post.observed.wht%>%
  left_join(post.wht.sims)%>%
  ungroup()%>%
  select(-ur.code)%>%
  group_by(division, sim)%>%
  summarise(sim.mort.rt=sum((sim.rate/100000) * observed.pop)/sum(observed.pop) * 100000,
            obs.mort.rt=sum(observed.deaths)/sum(observed.pop)*100000)%>%
  mutate(Race="White")

div.post <- bind_rows(blk.div.post.mort,
					  lat.div.post.mort,
					  wht.div.post.mort)

div.post <- div.post %>%
  	mutate_at(vars(sim.mort.rt, obs.mort.rt), funs(./(2234/365)))

# ... plot it!
# .... ur codes
ggplot(ur.post, aes(sim.mort.rt, fill=Race))+
  geom_density(alpha=0.7, bw = .2, color = 'grey100')+
  geom_point(aes(x=obs.mort.rt, y=0, shape=Race), size=2)+
  facet_wrap(~ur.code)+
  xlab("Police-involved deaths per 100,000 per year")+
  scale_fill_brewer(palette = 'Set2') +
  ylab("")+
  coord_cartesian(xlim=c(0,7))+
  theme(legend.position="bottom")+
  ggsave("./visuals/UR_Density_men.tiff",width=6.5, height=6.5, units="in")

# .... and divisions
ggplot(div.post, aes(sim.mort.rt, fill=Race))+
  geom_density(alpha=0.7, bw = .15, color = 'grey100')+
  geom_point(aes(x=obs.mort.rt, y=0, shape=Race), size=2)+
  facet_wrap(~division)+
  xlab("Police-involved deaths per 100,000 per year")+
  ylab("")+
  coord_cartesian(xlim=c(0,7))+
  scale_fill_brewer(palette = 'Set2') +
  theme(legend.position="bottom") +
  ggsave("./visuals/Div_Density_men.tiff", width=6.5, height=6.5, units="in")


#########################################################
# ... Figure 3: Rate Ratios
# .... calculate posterior rate ratios for (blk v wht) v. (lat v wht)
bw.post = bind_cols(ur.division, as.data.frame(t(apply(
	post.blk/post.wht, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
bw.post$rate = "Black/White"

lw.post = bind_cols(ur.division, as.data.frame(t(apply(
	post.lat/post.wht, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
lw.post$rate = "Latino/White"

# ... get data ready for plotting
plot.dat = bind_rows(bw.post, lw.post)

plot.dat$ur.code<-factor(plot.dat$ur.code,
						 levels=sort(as.character(unique(plot.dat$ur.code))))
plot.dat$division<-factor(plot.dat$division,
						 levels=sort(as.character(unique(plot.dat$division))))

plot.dat<-plot.dat[order(plot.dat$ur.code, plot.dat$division), ]
names(plot.dat)[3:6]<-c("lwr", "Median", "upr", 'comparison')

# ... add indicator for whether interval includes 1
plot.dat$CI_includes_1 = ifelse(data.table::between(1, plot.dat$lwr,
										   plot.dat$upr) == TRUE, 'yes', 'no')
plot.dat2 =
	plot.dat %>%
	mutate(ci_includes_1 = ifelse(CI_includes_1 == 'no', 1.05, 1))%>%
    rename(`Interval\nIncludes 1`=CI_includes_1)%>%
    rename(Comparison=comparison)

# ... plot it out
ggplot(plot.dat2,
       aes(x = ur.code, y = Median, group = Comparison, fill = Comparison, color =  Comparison)) +
    facet_wrap(~division) +
    geom_point(aes(size = `Interval\nIncludes 1`), alpha = .9, pch = 21, color = 'grey1') +
    scale_fill_brewer(palette = 'Set2') +
    #theme_bw() +
    ylab("Mortality rate ratio") +
    xlab(" ")+
    geom_hline(yintercept = 1, lty = 2) +
    #theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 9)) +
    #geom_line() +
    scale_size_discrete(range = c(5,2)) +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(as.factor(plot.dat2$ur.code))))+
    theme(legend.position="bottom") +
  ggsave("./visuals/fig2_men.tiff", width=6.5, height=6.5, units="in")

#########################################################

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
         upr=upr*(365/2234))

apTabReformat<-apTab3%>%
  mutate(estimate=paste(
    round(estimate, 1),
    " (",
    round(lwr, 1),
    ", ",
    round(upr, 1),
    ")",
    sep=""
  ))%>%
  select(- lwr, -upr)%>%
  spread(race, estimate)%>%
  mutate(division=as.character(division),
         ur.code=as.character(ur.code))%>%
  arrange(division, ur.code)
###### write the table to html
library(xtable)
print.xtable(
  xtable(apTab3,
         caption = NULL,
         digits = 2,
         auto = TRUE), 
  type = "html",
  file = "./visuals/appendix_table_3.html",
  include.rownames = FALSE)