library(MASS)
library(ggplot2)
library(date)
library(rstanarm)
library(dplyr)
library(tidyr)
library(xtable)
library(RColorBrewer)
library(gridExtra)

setwd("C:/Users/Mike/Dropbox/police-mort-bak/police-mort-master")
load("models.RData")

# 1: Bayesian estimates heat map
# ... set up scenarios
ur.code  = unique(tmp2$ur.code)
division = unique(tmp2$division)
ur.division = expand.grid(division, ur.code); 
names(ur.division) = c("division", "ur.code")
n = length(division)*length(ur.code)

### set up newdata for ppd - use 100,000,000 for offset to get enough trials, 
### then rescale to rate per 100,000 
# ... blacks 
newdata<-tmp2[1:n,]
newdata$fips<-0; newdata$tot.pop<-newdata$black<-newdata$white<-newdata$latino<-100000000; 
newdata$ur.code<-ur.division$ur.code; newdata$division<-ur.division$division

post.blk<-posterior_predict(blk.stan, newdata=newdata)/1000
post.blk.int<-bind_cols(ur.division, 
                        as.data.frame(t(apply(post.blk, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
post.blk.int = post.blk.int %>% mutate(race = 'Black')

# .. whites 
post.wht<-posterior_predict(wht.stan, newdata=newdata)/1000
post.wht.int<-bind_cols(ur.division, 
                        as.data.frame(t(apply(post.wht, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))

post.wht.int = post.wht.int %>% mutate(race = 'White')

# ... latinx 
post.lat<-posterior_predict(lat.stan, newdata=newdata)/1000
post.lat.int<-bind_cols(ur.division,
                        as.data.frame(t(apply(post.lat, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
post.lat.int = post.lat.int %>% mutate(race = 'Latino')

# ... gather estimates
post.estimates = bind_rows(post.wht.int, post.blk.int, post.lat.int) %>%
				 mutate(division2 = 
				 			ifelse(division == 'East South Central', 'E.S. Central',
				 			ifelse(division == 'West South Central', 'W.S. Central',
				 			ifelse(division == 'East Notyh Central', 'E.N. Central',
				 			ifelse(division == 'West North Central', 'W.N. Central',
				 			ifelse(division == 'East North Central', 'E.N. Central',
				 			ifelse(division == 'South Atlantic',     'S. Atlantic',
				 												  as.character(division))))))),

				 		ur.code2 = factor(ur.code, levels(ur.code)[c(3,5,2,1,4,6)])
				 		)
colnames(post.estimates) = c('division', 'ur.code', 'lwr', 
							 'rate', 'upr', 'race', 'div2', 'ur.code2')

# ... plot			
# can go any way w. the color; greyscale 2, just in case 
hm.palette  <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')
hm.palette2 <- colorRampPalette(brewer.pal(7, 'YlOrRd'), space='Lab') 
hm.palette3 <- colorRampPalette(brewer.pal(7, 'Greys'), space='Lab') 

ggplot(post.estimates, aes(y = ur.code2, x = div2, fill = rate)) +
	geom_tile(colour = "black") +
	theme_bw() +
	coord_equal() +
  	scale_fill_gradientn(colours = hm.palette(100)) +
  	xlab('Division') +
  	ylab('Urban/Rual Code') +
  	facet_wrap(~race, ncol = 2) +
  	theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 8)) 

# 2: b/l v white differences 
# get posterior rate ratios
bw.post<-bind_cols(ur.division, 
                   as.data.frame(t(apply(post.blk - post.wht, 2, function(x)quantile(x, probs=c(0.05, 0.5, 0.95))))))
bw.post$rate<-"Black - White"

lw.post<-bind_cols(ur.division, 
                   as.data.frame(t(apply(post.lat - post.wht, 2, function(x)quantile(x, probs=c(0.05, 0.5, 0.95))))))
lw.post$rate<-"Latino - White"

plot.dat<-bind_rows(bw.post, lw.post)

plot.dat$ur.code<-factor(plot.dat$ur.code, levels=sort(as.character(unique(plot.dat$ur.code))))
plot.dat$division<-factor(plot.dat$division, levels=sort(as.character(unique(plot.dat$division))))

plot.dat<-plot.dat[order(plot.dat$ur.code, plot.dat$division), ]
names(plot.dat)[3:5]<-c("Lower", "Median", "Upper")


# ... plot
ggplot(plot.dat,
  aes(x = ur.code, y = Median, group = rate, color = rate)) +
  facet_wrap(~division) + 
  geom_point(size = 2.5, position=position_dodge(width=0.5)) +
  scale_color_brewer(palette = 'Set2') +
  geom_errorbar(aes(ymin = Lower, ymax= Upper, color = rate), 
  					alpha = .6, width = .01, position=position_dodge(width=0.5)) +
  theme_bw() +
  #scale_color_brewer(palette = 'Set2') +
  xlab('County Type') +
  ylab('Rate difference') + 
  geom_hline(yintercept=0, lty=2)+
  theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 10)) 

# errorbars make hard to see differences, maybe instead just an indicator if diff from 0?
plot.dat$includes_0 = factor(ifelse(0 >= plot.dat$Lower & 0 <= plot.dat$Upper , 'yes', 'no'))
plot.dat$includes_0 = factor(plot.dat$includes_0, 
							  levels = levels(plot.dat$includes_0)[c(2,1)])

# ... plot
ggplot(plot.dat,
  aes(x = ur.code, y = Median, group = rate, color = rate, alpha = includes_0)) +
  facet_wrap(~division) + 
  geom_point(size = 2.5, position=position_dodge(width=0.5)) +
  scale_color_brewer(palette = 'Set2') +
  theme_bw() +
  xlab('County Type') +
  ylab('Rate difference') + 
  geom_hline(yintercept=0, lty=2) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 10)) +
  scale_alpha_discrete(range = c(0.3, 1)) +
  geom_line(position=position_dodge(width=0.5))



# ... or shapes
ggplot(plot.dat,
  aes(x = ur.code, y = Median, group = rate, color = rate, shape = includes_0)) +
  facet_wrap(~division) + 
  geom_point(size = 2.5, position=position_dodge(width=0.5)) +
  scale_color_brewer(palette = 'Set2') +
  theme_bw() +
  xlab('County Type') +
  ylab('Rate difference') + 
  geom_hline(yintercept=0, lty=2) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 10)) +
  scale_alpha_discrete(range = c(0.3, 1)) +
  geom_line(position=position_dodge(width=0.5))


 # ... or just forget it
ggplot(plot.dat,
  aes(x = ur.code, y = Median, group = rate, color = rate)) +
  facet_wrap(~division) + 
  geom_point(size = 2.5, position=position_dodge(width=0.5)) +
  scale_color_brewer(palette = 'Set2') +
  theme_bw() +
  xlab('County Type') +
  ylab('Rate difference') + 
  geom_hline(yintercept=0, lty=2) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 10)) +
  scale_alpha_discrete(range = c(0.3, 1)) +
  geom_line(position=position_dodge(width=0.5))
