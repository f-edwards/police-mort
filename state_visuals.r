#########################
# Fatal Encounters Data
# states visuals, .r
# last edit 3/5 (by: ME)
#########################

## Set-up
# 1: load packages; setwd etc. 
rm(list=ls())
set.seed(1)

library(ggplot2)
library(readr)
library(date)
library(lme4)
library(MASS)
library(rstanarm)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(gridExtra)
library(data.table)
select = dplyr::select
setwd("~/Projects/police_mort")
load("models_men_3_cat_states_fixed.RData")
theme_set(theme_minimal())

blk.stan<-blk.stan.0
wht.stan<-wht.stan.0
lat.stan<-lat.stan.0
tot.stan<-tot.stan.0
tmp2 <- tmp2 %>% as.data.frame()

ur.code2 = unique(tmp2$ur.code2)
state = unique(tmp2$state)
ur.state = expand.grid(state, ur.code2)
names(ur.state) = c("state", "ur.code2")
n = length(state)*length(ur.code2)

# ... use 100,000,000 for offset to get enough trials
# ... then rescale to rate per 100,000 
newdata <- tmp2[1:n,]
newdata$fips <- 0; 
newdata$tot.men<-newdata$black.men<-newdata$white.men<-newdata$latino.men<-100000000; 
newdata$ur.code2<-ur.state$ur.code2; newdata$state<-ur.state$state

# 2: estimate predictive posterior for each group 
# ... {blk: black; wht: white; lat: latinx}

# ... all
post.tot = posterior_predict(tot.stan, newdata = newdata)/1000

# ... blacks
post.blk = posterior_predict(blk.stan, newdata = newdata)/1000
post.blk.sims<-cbind(ur.state, t(post.blk))

post.blk.sims<-gather(post.blk.sims, 
                      key = sim, 
                      value = sim.rate, 
                      -ur.code2, -state)

# ... whites
post.wht = posterior_predict(wht.stan, newdata = newdata)/1000
post.wht.sims<-cbind(ur.state, t(post.wht))

post.wht.sims<-gather(post.wht.sims, 
                      key = sim, 
                      value = sim.rate, 
                      -ur.code2, -state)

# ... latinos
post.lat = posterior_predict(wht.stan, newdata = newdata)/1000
post.lat.sims<-cbind(ur.state, t(post.lat))

post.lat.sims<-gather(post.lat.sims, 
                      key = sim, 
                      value = sim.rate, 
                      -ur.code2, -state)

# 3: calculate mean and 95% uncertainty intervals for each group 
post.blk.int = bind_cols(ur.state, as.data.frame(
	t(apply(post.blk, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
post.blk.int = post.blk.int %>% mutate(race = 'Black')

post.tot.int = bind_cols(ur.state, as.data.frame(
  t(apply(post.tot, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
post.tot.int = post.tot.int %>% mutate(race = 'Total')

post.wht.int = bind_cols(ur.state, as.data.frame(
	t(apply(post.wht, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
post.wht.int = post.wht.int %>% mutate(race = 'White')

post.lat.int = bind_cols(ur.state, as.data.frame(
	t(apply(post.lat, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
post.lat.int = post.lat.int %>% mutate(race = 'Latino')

# 4: bind everything together & get ready for plotting
post.estimates = bind_rows(post.blk.int, post.wht.int, post.lat.int);
colnames(post.estimates)[3:5] = c('lwr', 'estimate', 'upr')


# ... take a look
# ... (vary unqie(...) number for different states 
state.plot1 = 
ggplot(post.estimates %>% filter(state == unique(tmp2$state)[1]), 
			aes(y = estimate, x = ur.code2, ymax = upr, ymin = lwr, 
				color = race, group = race)) +
	geom_point(position = position_dodge(width =.5), size =3) +
	geom_errorbar(width = .01, position = position_dodge(width =.5)) +
	facet_wrap(~state) +
	scale_color_brewer(palette = 'Set2') +
	coord_flip()


# ... rate ratios
bw.post = bind_cols(ur.state, as.data.frame(t(apply(
	post.blk/post.wht, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
bw.post$rate = "Black/White"

lw.post = bind_cols(ur.state, as.data.frame(t(apply(
	post.lat/post.wht, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
lw.post$rate = "Latinx/White"

plot.dat = bind_rows(bw.post, lw.post) 
names(plot.dat) = c('state', 'ur.code2', 'lwr', 'Median', 'upr', 'comparison')

plot.dat$CI_includes_1 = ifelse(between(1, plot.dat$lwr, 
										   plot.dat$upr) == TRUE, 'Yes', 'No')

plot.dat2 = 
	plot.dat %>% 
	mutate(ci_includes_1 = ifelse(CI_includes_1 == 'no', 1.05, 1))%>%
    rename(`Interval\nIncludes 1`=CI_includes_1)%>%
    rename(Comparison=comparison)


# ratio plots HERE
# ... vary this for different cuts of data 
hold = tmp2 %>% filter(Division == unique(tmp2$Division)[1]) %>% .$state %>% unique()

ggplot(filter(plot.dat2, state %in% hold),
  aes(x = ur.code2, y = Median, group = Comparison, color = Comparison)) +
  facet_wrap(~state) +
  geom_point(aes(size = `Interval\nIncludes 1`), alpha = .75) +
  scale_size_discrete(range = c(5,2)) +
  coord_flip() +
  theme(legend.position="bottom") +
  scale_color_brewer(palette = 'Set2') +
  geom_hline(yintercept = 1, lty = 2) 
