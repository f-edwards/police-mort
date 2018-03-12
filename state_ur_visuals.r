library(tidyverse)
library(rstanarm)
library(parallel)
library(data.table)


## visuals 
#load('final_states_mods.RData')
blk.stan<-blk.stan.0
wht.stan<-wht.stan.0
lat.stan<-lat.stan.0
tmp2 <- tmp2 %>% as.data.frame()

## ...set up scenarios for prediction 
ur.code  = unique(tmp2$ur.code)
state = unique(tmp2$state)
ur.state = expand.grid(state, ur.code); 
names(ur.state) = c("state", "ur.code")
n = length(state)*length(ur.code)

## ... use 100,000,000 for offset to get enough trials
## ... then rescale to rate per 100,000 
newdata <- tmp2[1:n,]
newdata$fips <- 0; newdata$tot.men<-newdata$black.men<-newdata$white.men<-newdata$latino.men<-100000000; 
newdata$ur.code<-ur.state$ur.code; newdata$state<-ur.state$state

# ... estimate predictive posterior for each group 
# .... for blacks
post.blk = posterior_predict(blk.stan, newdata = newdata)/1000
post.blk.sims <- cbind(ur.state, t(post.blk)) %>%
  gather(., key = sim, value = sim.rate, -ur.code, -state)

# .... for whites
post.wht = posterior_predict(wht.stan, newdata = newdata)/1000
post.wht.sims <- cbind(ur.state, t(post.wht)) %>%
  gather(., key = sim, value = sim.rate, -ur.code, -state)

# ... latino
post.lat = posterior_predict(lat.stan, newdata = newdata)/1000
post.lat.sims <- cbind(ur.state, t(post.lat)) %>%
  gather(., key = sim, value = sim.rate, -ur.code, -state)

# ... calculate mean and 95% uncertainty intervals for each group 
post.blk.int = bind_cols(ur.state, as.data.frame(
  t(apply(post.blk, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
post.blk.int = post.blk.int %>% mutate(race = 'Black')

post.wht.int = bind_cols(ur.state, as.data.frame(
  t(apply(post.wht, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
post.wht.int = post.wht.int %>% mutate(race = 'White')

post.lat.int = bind_cols(ur.state, as.data.frame(
  t(apply(post.lat, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
post.lat.int = post.lat.int %>% mutate(race = 'Latino')

# ... bind everything together & get ready for plotting
post.estimates = bind_rows(post.blk.int, post.wht.int, post.lat.int);
colnames(post.estimates)[3:5] = c('lwr', 'estimate', 'upr')

p.dat1 = post.estimates %>%
  rename(rate = estimate) %>% 
  mutate(type = 'estimated', 
         ur.code  = as.character(ur.code), 
         state = as.character(state)) %>%
  select(ur.code, state, rate, race, type) 

# ... for plots:
# ... add in divisons
hld = tmp2 %>% 
  select(state, division) %>%
  distinct()

p.dat2 = left_join(p.dat1, hld, 'state')

# ... also add in marker for if (state, ur.code) combo exist in data
a = tmp2 %>% 
  select(state, ur.code) %>%
  distinct() %>%
  mutate(state_code = paste(state, ur.code, sep = '-')) %>%
  select(state_code) 

b = p.dat2 %>%
  mutate(state_code = paste(state, ur.code, sep = '-')) %>%
  tbl_df()

p.dat3 = b %>% 
  mutate(real = ifelse((b$state_code %in% a$state_code) == TRUE, 1, 0)) %>%
  mutate(rate = rate*(365/2234))

# ... plot of rates 
#### Reduce to three cats, add a total

ggplot(p.dat3 %>% filter(ur.code == '1: large central metro', real == 1), 
       aes(x = reorder(state, rate), y = rate, fill = race, group = race)) + #, #ymin = `2.5%`, ymax = `97.5%`)) + 
  geom_point(color = 'grey10', pch = 21, size = 2.5, alpha = .75) + 
  coord_flip() + 
  scale_fill_brewer(palette = 'Set2') + 
  facet_wrap(~division, scale = 'free_y') +
  ylab('state') +
  xlab('rate, per 100,000 population') 

ggplot(p.dat3 %>% filter(ur.code == '3: medium metro', real == 1), 
       aes(x = reorder(state, rate), y = rate, fill = race, group = race)) + #, #ymin = `2.5%`, ymax = `97.5%`)) + 
  geom_point(color = 'grey10', pch = 21, size = 2.5, alpha = .75) + 
  coord_flip() + 
  scale_fill_brewer(palette = 'Set2') + 
  facet_wrap(~division, scale = 'free_y') +
  ylab('state') +
  xlab('rate, per 100,000 population') 

ggplot(p.dat3 %>% filter(ur.code == '6: noncore', real == 1), 
       aes(x = reorder(state, rate), y = rate, fill = race, group = race)) + #, #ymin = `2.5%`, ymax = `97.5%`)) + 
  geom_point(color = 'grey10', pch = 21, size = 2.5, alpha = .75) + 
  coord_flip() + 
  scale_fill_brewer(palette = 'Set2') + 
  facet_wrap(~division, scale = 'free_y') +
  ylab('state') +
  xlab('rate, per 100,000 population') 




## ... plot of rate ratios
#bw.post = bind_cols(ur.state, as.data.frame(t(apply(
#  post.blk/post.wht, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
#bw.post$rate = "Black/White"#

#lw.post = bind_cols(ur.state, as.data.frame(t(apply(
#  post.lat/post.wht, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
#lw.post$rate = "Latino/White"#

#plot.dat = bind_rows(bw.post, lw.post) %>%
#          left_join(hld, 'state')#

#plot.dat<-plot.dat[order(plot.dat$ur.code, plot.dat$ur.code), ]
#names(plot.dat)[3:6]<-c("lwr", "Median", "upr", 'comparison')#

## ... add indicator for whether interval includes 1
#plot.dat$CI_includes_1 = ifelse(data.table::between(1, plot.dat$lwr, 
#                       plot.dat$upr) == TRUE, 'yes', 'no')#

#plot.dat2 = 
#  plot.dat %>% 
#  mutate(ci_includes_1 = ifelse(CI_includes_1 == 'no', 1.05, 1))%>%
#    rename(`Interval\nIncludes 1`=CI_includes_1)%>%
#    rename(Comparison=comparison)#

## ... plot it out
#ggplot(plot.dat2,
#       aes(x = state, y = Median, group = Comparison, fill = Comparison, color =  Comparison, shape = ur.code)) +
#    facet_wrap(~division, scales = 'free_y') + 
#    geom_point(alpha = .9) + 
#    scale_color_brewer(palette = 'Set2') +
#    #theme_bw() +
#    ylab("Mortality rate ratio") + 
#    xlab(" ")+
#    geom_hline(yintercept = 1, lty = 2) +
#    #theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 9)) +
#    #geom_line() +
#    coord_flip() +
#    #scale_x_discrete(limits = rev(levels(as.factor(plot.dat2$ur.code))))+
#    theme(legend.position="bottom") 