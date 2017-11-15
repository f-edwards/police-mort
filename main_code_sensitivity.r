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

#########################################
### VERY UNIFORMATIVE PRIOR
#########################################

tot.stan.0 = stan_glmer(d.total ~ (1|ur.code) + (1|division),
                      prior_intercept=normal((log(0.37)-log(100000)), 2.5), #for prior intercept, based on krieger estimates
                      prior = normal(0, 2.5), #weakly informative, no difference from big urban
                      prior_covariance = decov(1, 1, 1, 1), #default
                      data = tmp2, offset=I(log(tot.pop+1)), family="neg_binomial_2", iter=2000, chains=4)



blk.stan.0 = stan_glmer(d.black ~ (1|ur.code) + (1|division),
                      prior_intercept=normal((log(0.94)-log(100000)), 2.5), #for prior intercept, based on krieger estimates
                      prior = normal(0, 2.5), #weakly informative, no difference from big urban
                      prior_covariance = decov(1, 1, 1, 1), #default
                     data = tmp2, offset=I(log(black+1)), family="neg_binomial_2", iter=2000, chains=4)

wht.stan.0 = stan_glmer(d.white ~ (1|ur.code) + (1|division),
                      prior_intercept=normal((log(0.37)-log(100000)), 2.5), #for prior intercept, based on krieger estimates
                      prior = normal(0, 2.5), #weakly informative, no difference from big urban
                      prior_covariance = decov(1, 1, 1, 1), #default
                      data = tmp2, offset=log(white), family="neg_binomial_2", iter=2000, chains=4)

lat.stan.0 = stan_glmer(d.latino ~ (1|ur.code) + (1|division),
                      prior_intercept=normal((log(0.37)-log(100000)), 2.5), #for prior intercept, based on krieger estimates
                      prior = normal(0, 2.5), #weakly informative, no difference from big urban
                      prior_covariance = decov(1, 1, 1, 1), #default
                      data = tmp2, offset=I(log(latino+1)), family="neg_binomial_2", iter=2000, chains=4)

save.image("models.RData")
