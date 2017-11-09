##########################
# Fatal Encounters Project
# visuals, .R
##########################

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

load("models_sensitivity.RData")

## Figure 1: violin plots of observed vs. estimated region variation
# 1: set up scenarios for prediction 
ur.code  = unique(tmp2$ur.code)
division = unique(tmp2$division)
ur.division = expand.grid(division, ur.code); 
names(ur.division) = c("division", "ur.code")
n = length(division)*length(ur.code)

# ... use 100,000,000 for offset to get enough trials
# ... then rescale to rate per 100,000 
newdata <- tmp2[1:n,]
newdata$fips <- 0; newdata$tot.pop<-newdata$black<-newdata$white<-newdata$latino<-100000000; 
newdata$ur.code<-ur.division$ur.code; newdata$division<-ur.division$division

for(i in 1:3){
  
  ### set each run
  if(i==1){
    blk.stan<-blk.stan.0
    wht.stan<-wht.stan.0
    lat.stan<-lat.stan.0
    tot.stan<-tot.stan.0
  }
  
  if(i==2){
    blk.stan<-blk.stan.1
    wht.stan<-wht.stan.1
    lat.stan<-lat.stan.1
    tot.stan<-tot.stan.1
  }
  
  if(i==3){
    blk.stan<-blk.stan.2
    wht.stan<-wht.stan.2
    lat.stan<-lat.stan.2
    tot.stan<-tot.stan.2
  }
  
  # 2: estimate predictive posterior for each group 
  # ... {blk: black; wht: white; lat: latinx}
  post.blk = posterior_predict(blk.stan, newdata = newdata)/1000
  post.wht = posterior_predict(wht.stan, newdata = newdata)/1000
  post.lat = posterior_predict(lat.stan, newdata = newdata)/1000
  
  # 3: calculate mean and 95% uncertainty intervals for each group 
  post.blk.int = bind_cols(ur.division, as.data.frame(
  	t(apply(post.blk, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
  post.blk.int = post.blk.int %>% mutate(race = 'Black')
  
  post.wht.int = bind_cols(ur.division, as.data.frame(
  	t(apply(post.wht, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
  post.wht.int = post.wht.int %>% mutate(race = 'White')
  
  post.lat.int = bind_cols(ur.division, as.data.frame(
  	t(apply(post.lat, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
  post.lat.int = post.lat.int %>% mutate(race = 'Latino')
  
  # 4: bind everything together & get ready for plotting
  post.estimates = bind_rows(post.blk.int, post.wht.int, post.lat.int);
  colnames(post.estimates)[3:5] = c('lwr', 'estimate', 'upr')
  
  p.dat1 = post.estimates %>%
  		 rename(rate = estimate) %>% 
  		 mutate(type = 'estimated', 
  		 		ur.code  = as.character(ur.code), 
  		 		division = as.character(division)) %>%
  		 select(ur.code, division, rate, race, type) 
  
  # 5: get observed, average rates by race + county type + division
  p.dat2 = tmp2 %>%
  	  	 group_by(ur.code, division) %>%
  	  	 summarise(y.black.mean  =   sum(d.black)/sum(black)*100000,
  	          	   y.latino.mean = sum(d.latino)/sum(latino)*100000,
  	  			   y.white.mean  =   sum(d.white)/sum(white)*100000) %>%
  	  	 gather(rate, value, y.black.mean:y.white.mean) %>%
  	   	 mutate(race = ifelse(rate == 'y.black.mean',  'Black',
  		 			   ifelse(rate == 'y.white.mean',  'White',
  		 			   ifelse(rate == 'y.latino.mean', 'Latino', 'nah'))),
  		 	  	type = 'observed') %>%
  	   	select(ur.code, division, value, race, type) %>%
  	   	rename(rate = value)
  
  # 6: bind together estimated and observed rates and scale 
  p.dat = bind_rows(p.dat1, p.dat2) %>%
  		mutate_at(vars(rate), funs(./(1588/366)))
  
  # 7: plot 
  # ... set up plotting space for tiff image
  tiff(paste("fig1_hq", i,  ".tif", sep=""), 
       units="in", width=12, height=7.5, res=300)
  
  ggplot(data = p.dat, aes(x = ur.code, y = rate)) +
  	geom_violin(aes(x = ur.code, y = rate, fill = type), 
  				position = position_dodge(width = .9), 
  				alpha = .15,
  				scale = "width",
  				width = 0.1) +
  	geom_point(aes(y = rate, group = type, color = division), 
  				   position = position_jitterdodge(dodge.width   = .9, 
  				   								   jitter.width  = .0,
  				   								   jitter.height = .0),
  				   size  = 2.5, 
  				   alpha = .8) +
  	facet_wrap(~race, scales = 'free_x') +
    	scale_fill_brewer(palette  = 'Dark2') +
    	scale_color_brewer(palette = 'Paired') +
    	ylab('Rate') +
    	xlab('Metro-Type') + 
    	theme_bw() +
    	scale_x_discrete(limits = rev(levels(as.factor(p.dat$ur.code)))) +
    	coord_flip()
   dev.off()
  
  ## Figure 2: rate ratio plot
  # 1: calculate posterior rate ratios for (blk v wht) v. (lat v wht)
  bw.post = bind_cols(ur.division, as.data.frame(t(apply(
  	post.blk - post.wht, 2, function(x)quantile(x, probs=c(0.05, 0.5, 0.95))))))
  bw.post$rate = "Black - White"
  
  lw.post = bind_cols(ur.division, as.data.frame(t(apply(
  	post.lat - post.wht, 2, function(x)quantile(x, probs=c(0.05, 0.5, 0.95))))))
  lw.post$rate = "Latino - White"
  
  # 2: get data ready for plotting 
  plot.dat = bind_rows(bw.post, lw.post)
  
  plot.dat$ur.code<-factor(plot.dat$ur.code, 
  						 levels=sort(as.character(unique(plot.dat$ur.code))))
  plot.dat$division<-factor(plot.dat$division, 
  						 levels=sort(as.character(unique(plot.dat$division))))
  
  plot.dat<-plot.dat[order(plot.dat$ur.code, plot.dat$division), ]
  names(plot.dat)[3:6]<-c("lwr", "Median", "upr", 'comparison')
  
  # scale
  plot.dat = plot.dat %>%
  		  mutate_at(vars(lwr:upr), funs(./(1588/366)))
  
  # 3: add indicator for whether interval includes 0
  plot.dat$CI_includes_0 = ifelse(between(0, plot.dat$lwr, 
  										   plot.dat$upr) == TRUE, 'yes', 'no')
  
  # 4: plot it out 
  tiff(paste("fig2_hq", i, ".tif",sep=""),
       units="in", width=12, height=8.5, res=300)
  
  ggplot(plot.dat,
    aes(x = ur.code, y = Median, group = comparison, color = comparison, 
    	  shape = CI_includes_0)) +
    facet_wrap(~division) + 
    geom_point(size = 2.5) +
    scale_color_brewer(palette = 'Set2') +
    theme_bw() +
    xlab('Metro-Type') +
    ylab('Rate difference') + 
    geom_hline(yintercept = 0, lty = 2) +
    theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 9)) +
    geom_line()
  dev.off()
  
  
}  

# notes: 
# 1: ask about priors
# 2: are we really getting at year to year fluctuation?
# 3: any difference in table and plot values?



