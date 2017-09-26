#########################
# Fatal Encounters Data
# Main file, .R
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
library(xtable)
options(mc.cores = parallel::detectCores())

fdat<-read.csv("fe-clean.csv", stringsAsFactors = FALSE)
# 2: configure data ...
# ... attach fatal encounters data
#fdat = read.csv('fdat.csv',
#				 stringsAsFactors = FALSE) %>%
#	   filter(year >= 2013) %>%
#	   mutate(county = paste(county, 'county', sep = ' '))

fdat = fdat %>%
	   mutate(county = paste(county, 'county', sep = ' '),
	   	      year.death = year) %>%
	   dplyr::select(-year) %>%
	   mutate(race = ifelse(race == 'African-American/black', 
	  							    'African-American/Black', race))

# ... attach CDC urban-rual scheme
cdc = read_fwf(file = 'NCHSURCodes2013.txt', 
			   fwf_positions(c(1, 3, 7, 10, 47, 98, 107, 116, 118, 120), 
					  		 c(2, 5, 8, 45, 96, 105, 114, 116, 118, 120))
		) %>%
	  dplyr::select(X1, X2, X3, X4, X8) %>%
	  rename(s.fips = X1, c.fips = X2, state = X3, county = X4, ur.code = X8) %>%
	  mutate(county = tolower(county),
	  	     ur.code = ifelse(ur.code == 1, '1: large central metro',
	  	     		   ifelse(ur.code == 2, '2: large fringe metro',
	  	     		   ifelse(ur.code == 3, '3: medium metro',
	  	     		   ifelse(ur.code == 4, '4: small metro', 
	  	     		   ifelse(ur.code == 5, '5: micropolitan', 
	  	     		   ifelse(ur.code == 6, '6: noncore', NA))))))) %>%
	  mutate(fips = as.numeric(paste0(s.fips, c.fips))) %>%
	  dplyr::select(fips, ur.code)

# ... attach census division codes
regions = read.csv('regions.csv', 
					stringsAsFactors = FALSE) %>%
		  mutate(state = State.Code) %>%
		  dplyr::select(state, Division)

# ... demographics
pop<-read.csv("nhgis0022_ds215_20155_2015_county.csv", colClasses = "character")

pop$fips<-paste(pop$STATEA, pop$COUNTYA, sep="")

pop$latino<-as.numeric(pop$ADK5E012)
pop$white<-as.numeric(pop$ADK5E003)
pop$black<-as.numeric(pop$ADK5E004) + as.numeric(pop$ADK5E014)
pop$amind<-as.numeric(pop$ADK5E005) + as.numeric(pop$ADK5E015)
pop$api<-as.numeric(pop$ADK5E006)+
  as.numeric(pop$ADK5E007)+
  as.numeric(pop$ADK5E016)+as.numeric(pop$ADK5E017)
pop$tot.pop<-as.numeric(pop$ADK5E001)
pop<-pop%>%
  rename(fips.st=STATEA)%>%
  dplyr::select(fips, tot.pop, latino, white, black, amind, api, fips.st) %>%
				   mutate(fips = as.numeric(fips), fips.st=as.numeric(fips.st))%>%
  filter(fips.st!=72)

cw<-read.csv("fips-st-crosswalk.csv", stringsAsFactors = FALSE)%>%
  dplyr::select(state, fips)%>%rename(fips.st=fips)

pop<-left_join(pop, cw)%>%
  dplyr::select(-fips.st)

fdat$fips<-ifelse((fdat$county=="shannon county")&(fdat$state=="SD"), 46102, fdat$fips)
fdat$fips<-ifelse((fdat$city=="Chevak")&(fdat$state=="AK"), 2158, fdat$fips)

### got all of them matched
#z<-which(!(fdat$fips%in%pop$fips))

### rewrite for inclusion of all US counties - current setup miscalculates low pop based on zeroes in the data
tmp1 = left_join(pop, cdc, c('fips')) %>%
	   full_join(fdat, c('fips', 'state')) %>%
	   left_join(regions, 'state') %>% 
	   filter(!is.na(ur.code)) %>%
	   rename(division = Division) %>%
	   group_by(fips, race) %>%
	   mutate(d.count = n()) %>%
	   dplyr::select(fips, ur.code, division, race, d.count, tot.pop, latino, white, black, amind, api) %>%
	   arrange(fips, race) %>%
	   ungroup() %>%
	   distinct() %>%
	   group_by(fips) %>%
	   spread(race, d.count, fill = 0) %>% 
	   rename(d.black  = `African-American/Black`,
	  		  d.white  = `European-American/White`,
	  		  d.latino = `Hispanic/Latino`,
	  		  d.amind  = `Native American/Alaskan`,
	  		  d.api    = `Asian/Pacific Islander`,
	  		  d.mid    = `Middle Eastern`,
	  		  d.na     = `Race unspecified`) %>%
     mutate(d.total=d.black+d.api+d.white+d.latino+d.mid+d.amind+d.na)%>%
  dplyr::select(-`<NA>`)


#######################################
### models
#######################################
### drop na, only two cases
### setting intercept priors from http://harvardpublichealthreview.org/190/
### mort for black=0.94, white =0.37 from 2005
### B_0 +... =log(mort)-log(offset)
### high variance for weak information
### for offset=100000, p(B_0)~N(log(mort)-log(offset), 10)
### so for blk, p(B_0)~N(log(0.94)-log(100000), 10)
### for white, p(B_0)~N(log(0.37)-log(100000), 10)
### use white for all, use white for latino


tmp2<-tmp1

# all= glmer.nb(d.total ~  ur.code + (1|division),
#            data = tmp1, offset=log(tot.pop), verbose=TRUE, control=glmerControl(optimizer="bobyqa",
#                                                                                 optCtrl=list(maxfun=2e5)))

# all.stan = stan_glmer(d.black ~ ur.code + (1|division),
#                       prior_intercept=normal((log(0.37)-log(100000)), 10), #for prior intercept, based on krieger estimates
#                       prior = normal(0, 2.5), #weakly informative, no difference from big urban
#                       prior_covariance = decov(1, 1, 1, 1), #default
#                       data = tmp2, offset=log(tot.pop), family="neg_binomial_2", iter=2000, chains=4)


# blk = glmer.nb(d.black ~  ur.code + (1|division) + (1|fips),
#            data = tmp1, offset=I(log(black+1)), verbose=TRUE, control=glmerControl(optimizer="bobyqa",
#                                                                                    optCtrl=list(maxfun=2e5)))

tot.stan = stan_glmer(d.total ~ ur.code + (1|division),
                      prior_intercept=normal((log(0.37)-log(100000)), 10), #for prior intercept, based on krieger estimates
                      prior = normal(0, 2.5), #weakly informative, no difference from big urban
                      prior_covariance = decov(1, 1, 1, 1), #default
                      data = tmp2, offset=I(log(tot.pop+1)), family="neg_binomial_2", iter=2000, chains=4)



blk.stan = stan_glmer(d.black ~ ur.code + (1|division),
                      prior_intercept=normal((log(0.94)-log(100000)), 10), #for prior intercept, based on krieger estimates
                      prior = normal(0, 2.5), #weakly informative, no difference from big urban
                      prior_covariance = decov(1, 1, 1, 1), #default
                     data = tmp2, offset=I(log(black+1)), family="neg_binomial_2", iter=2000, chains=4)

# wht = glmer.nb(d.white ~  ur.code + (1|division) + (1|fips),
#                data = tmp1, offset=log(white), verbose=TRUE, control=glmerControl(optimizer="bobyqa",
#                                                                                   optCtrl=list(maxfun=2e5)))

wht.stan = stan_glmer(d.white ~ ur.code + (1|division),
                      prior_intercept=normal((log(0.37)-log(100000)), 10), #for prior intercept, based on krieger estimates
                      prior = normal(0, 2.5), #weakly informative, no difference from big urban
                      prior_covariance = decov(1, 1, 1, 1), #default
                      data = tmp2, offset=log(white), family="neg_binomial_2", iter=2000, chains=4)

# lat = glmer.nb(d.latino ~  ur.code + (1|division) + (1|fips),
#                data = tmp1, offset=I(log(latino+1)), verbose=TRUE, control=glmerControl(optimizer="bobyqa",
#                                                                                    optCtrl=list(maxfun=2e5)))

lat.stan = stan_glmer(d.latino ~ ur.code + (1|division),
                      prior_intercept=normal((log(0.37)-log(100000)), 10), #for prior intercept, based on krieger estimates
                      prior = normal(0, 2.5), #weakly informative, no difference from big urban
                      prior_covariance = decov(1, 1, 1, 1), #default
                      data = tmp2, offset=I(log(latino+1)), family="neg_binomial_2", iter=2000, chains=4)

save.image("models.RData")



### for natl descriptives
###Population estimates, July 1, 2015, (V2015)321,418,820


## ... populatuion counts 
#tmp2 = left_join(fdat, cdc, 'fips') %>%
#	   inner_join(regions, 'state') %>%
#	   left_join(pop, 'fips') %>% 
#	   rename(division = Division) %>%
#	   select(fips, ur.code, division, white, black, amind, api, latino) %>%
#	   distinct(.keep_all = TRUE) %>%
#	   group_by(ur.code, division) %>%
#	   mutate(cd_pop.black  = sum(black),
#	   		  cd_pop.white  = sum(white),
#	   		  cd_pop.latino = sum(latino),
#	   		  cd_pop.api    = sum(api), 
#	   		  cd_pop.amind  = sum(amind)) %>%
#	   distinct(.keep_all = TRUE) %>%
#	   select(ur.code, division, 9:13) %>%
#	   arrange(ur.code, division) #

## ... merge 
#tmp = inner_join(tmp1, tmp2, c('ur.code', 'division')) %>%
#	  spread(race, n) %>%
#	  arrange(ur.code, division) %>%
#	  rename(count.black  = `African-American/Black`,
#	  		 count.white  = `European-American/White`,
#	  		 count.latino = `Hispanic/Latino`,
#	  		 count.amind  = `Native American/Alaskan`,
#	  		 count.api    = `Asian/Pacific Islander`,
#	  		 count.mid    = `Middle Eastern`,
#	  		 count.na     = `Race unspecified`) %>%
#	  mutate(y.black  = (count.black/cd_pop.black)*100000,
#	  		 y.white  = (count.white/cd_pop.white)*100000,
#	  		 y.latino = (count.latino/cd_pop.latino)*100000,
#	  		 y.api    = (count.api/cd_pop.api)*100000,
#	  		 y.amind  = (count.amind/cd_pop.amind)*100000) %>%
#	  mutate(b.w = y.black/y.white) #

#dat = tmp %>%
#	  select(ur.code, division, count.black, count.white, y.black, y.white, )#

### Analysis
## 1: plot rates 
#plotDat = tmp %>%
#		  gather(race, rate, y.black:y.amind) %>%
#		  mutate(rate = ifelse(is.na(rate), 0, rate))#

#ggplot(plotDat, aes(y = rate, x = ur.code, group = race, 
#					color = race)) +
#	geom_point(alpha = .6, size = 4) +
#	facet_wrap(~division) + 
#	theme_bw() + 
#	coord_flip() +
#	ylab('Rate, per 100,000') +
#	xlab('CDC Type') +
#	scale_color_brewer(palette = 'Set2')#

## 2: plot black rate/white rate
#ggplot(tmp, aes(y = b.w, x = ur.code, group = division, color = division, 
#				size = log(cd_pop.black))) +
#	geom_point(alpha = .6, size = 4) + 
#	theme_bw() + 
#	#coord_flip() +
#	#geom_smooth(se = FALSE) +
#	ylab('B/W Rate') +
#	xlab('CDC Type') #

# 2: model


#a = tmp1 %>%
#	select(ur.code, race, count) %>% 
#	distinct(.keep_all = TRUE)#
#
#

## need the population of each ur.type#

#a = tmp1 %>%
#	ungroup() %>%
#	group_by(ur.code) %>%
#	mutate(ur.black = sum(black))#

## ... gather county population numbers
#tmp2 = tmp1 %>%
#	   gather(c.race, c.pop, c(white, black, latino, amind, api)) %>%
#	   gather(c.pop, value, c.pop) %>%
#	   mutate(i.race = ifelse(race == 'African-American/Black',  'black',
#	   				   ifelse(race == 'Asian/Pacific Islander',  'api',
#	   				   ifelse(race == 'European-American/White', 'white',
#	   				   ifelse(race == 'Native American/Alaskan', 'amind',
#	   				   ifelse(race == 'Hispanic/Latino',         'latino', 'other')))))) %>%
#	   ungroup() %>%
#	   select(ur.code, i.race, count, c.race, c.pop, value, division)#
#
#

##checks
#filter(tmp1, fips == 1003)#

#able(filter(fdat, fips == 1073)$race) #
#

#tmp2 = tmp1 %>% 
#	   group_by(Division) %>%
#	   summarise(black.d_pop  = sum(black,  na.rm = TRUE),
#	   			 white.d_pop  = sum(white,  na.rm = TRUE),
#	   			 latino.d_pop = sum(latino, na.rm = TRUE),
#	   			 amind.d_pop  = sum(amind,  na.rm = TRUE), 
#	   			 api.d_pop    = sum(api,    na.rm = TRUE))#

#tmp = left_join(tmp1, tmp2, 'Division') #
#
#

## ... get counts by county
#hold = tmp %>% 
#	   group_by(ur.code, race) %>%
#	   select(ur.code, race) %>%
#	   mutate(n = n())#

#tmp.black = tmp %>%
#			filter(race == 'African-American/Black') %>%
#			select(race, county, ur.code, Division, black, black.d_pop) %>%
#			mutate(n = n(),
#				   countcount.off  = n/black,
#				   divison.off = n/black.d_pop) %>%
#			rename(pop = black,
#				   black.d_pop = black_pop_division)#
#

#tmp.white = tmp %>%
#			filter(race == 'European-American/White') %>%
#			select(race, county, ur.code, Division, white, white.d_pop) %>%
#			mutate(n = n(),
#				   countcount.off  = n/white,
#				   divison.off = n/white.d_pop) #

#tmp.all = #
#

###########################
###########################
######### Analysis ########
###########################
############################

### 1: EDA
## 1a: cases by urban-rual
#table(tmp$ur.code) %>% plot()#

## 2a: ... by dividion 
#table(tmp$Division) %>% plot()#

## 3a: cases, by race, by urban-rual code x divison
#pDat1 = tmp %>%
#		select(race, ur.code, Division) %>%
#		group_by(race, ur.code, Division) %>%
#		summarise(n = n()) %>%
#		mutate(ur.div = paste(ur.code, Division, sep = ',')) #

#ggplot(pDat1, aes(x = ur.code, y = n, group = race, color = race)) +
#	geom_jitter(size = 4, width = 0.5, alpha = .8) +
#	theme_bw() +
#	scale_color_brewer(palette = 'Set2') +
#	facet_wrap(~ Division) +
#	coord_flip()


