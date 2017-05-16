#########################
# Fatal Encounters Data
# Main file, .R
#########################

## Set-up
# 1: load packages; setwd etc. 
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(date)
setwd('C:/Users/Mike/Dropbox/police-mort') 
source('merge.r')

# 2: configure data ...
# ... attach fatal encounters data
#fdat = read.csv('fdat.csv',
#				 stringsAsFactors = FALSE) %>%
#	   filter(year >= 2013) %>%
#	   mutate(county = paste(county, 'county', sep = ' '))

fdat = fdat %>%
	   mutate(county = paste(county, 'county', sep = ' '),
	   	      year.death = year) %>%
	   select(-year) %>%
	   mutate(race = ifelse(race == 'African-American/black', 
	  							    'African-American/Black', race))

# ... attach CDC urban-rual scheme
cdc = read_fwf(file = 'NCHSURCodes2013.txt', 
			   fwf_positions(c(1, 3, 7, 10, 47, 98, 107, 116, 118, 120), 
					  		 c(2, 5, 8, 45, 96, 105, 114, 116, 118, 120))
		) %>%
	  select(X1, X2, X3, X4, X8) %>%
	  rename(s.fips = X1, c.fips = X2, state = X3, county = X4, ur.code = X8) %>%
	  mutate(county = tolower(county),
	  	     ur.code = ifelse(ur.code == 1, '1: large central metro',
	  	     		   ifelse(ur.code == 2, '2: large fringe metro',
	  	     		   ifelse(ur.code == 3, '3: medium metro',
	  	     		   ifelse(ur.code == 4, '4: small metro', 
	  	     		   ifelse(ur.code == 5, '5: micropolitan', 
	  	     		   ifelse(ur.code == 6, '6: noncore', NA))))))) %>%
	  mutate(fips = as.numeric(paste0(s.fips, c.fips))) %>%
	  select(fips, ur.code)

# ... attach census division codes
regions = read.csv('regions.csv', 
					stringsAsFactors = FALSE) %>%
		  mutate(state = State.Code) %>%
		  select(state, Division)

# ... demographics
demos = read.csv('demographics.csv', stringsAsFactors = FALSE) %>%
		rename(fips = FIPS) 

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
pop<-pop%>%dplyr::select(fips, tot.pop, latino, white, black, amind, api) %>%
				   mutate(fips = as.numeric(fips))

#... get death rates per county
# note: some missing ur codes
# see: tmp1 %>% filter(is.na(ur.code)) %>% select(race) %>% table()
tmp1 = left_join(fdat, cdc, 'fips') %>%
	   inner_join(regions, 'state') %>%
	   left_join(pop, 'fips') %>% 
	   filter(!is.na(ur.code)) %>%
	   rename(division = Division) %>%
	   group_by(fips, race) %>%
	   mutate(d.count = n()) %>%
	   select(fips, ur.code, division, race, d.count, tot.pop, latino, white, black, amind, api) %>%
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
	   mutate(y.black  = (d.black/black)	*100000,
	  		  y.white  = (d.white/white)	*100000,
	  		  y.latino = (d.latino/latino)	*100000,
	  		  y.api    = (d.api/api)		*100000,
	  		  y.amind  = (d.amind/amind)	*100000
	  	)

################################
########## Analysis ############
################################

# 1: EDA
# just pulling fips's over 1000 of each
tmp = tmp1 %>%
	  filter(black >= 1000, white >= 1000)

# keep most observations, but still
sum(tmp$d.black)
sum(tmp$d.white)

# ... distrbution of rates 
summary(tmp$y.black)
summary(tmp$y.white)

# ... distrbution of counts
# overwhelmingly 1-2 per county
summary(tmp$d.black)
summary(tmp$d.white)

plot(table(tmp$d.black))
plot(table(tmp$d.white))

# ... rate var by county
dat1 = tmp %>%
	   gather(y.race, y.rate, y.black: y.amind) %>%
	   select(fips, y.race, y.rate, ur.code, division) %>%
	   filter(y.race %in% c('y.black', 'y.white'))

ggplot(dat1, aes(x = fips, y = log(y.rate), color = y.race)) +
	geom_jitter(alpha = .35)  +
	theme_bw() +
	scale_color_brewer(palette = 'Set2') +
	coord_flip() +
	ylab('(log) Rate Killed')

# ... average rate by urban/rual type
dat2 = tmp %>%
	   group_by(ur.code) %>% 
	   summarise(y.black.mean  = mean(y.black,  na.rm = TRUE),
	  			 y.white.mean  = mean(y.white,  na.rm = TRUE)) %>%
	   mutate(y.bw = y.black.mean/y.white.mean) %>%
	   gather(rate, value, y.black.mean:y.white.mean)

ggplot(dat2, aes(x = ur.code, y = value, color = rate, group = rate)) +
	geom_point() +
	geom_line() +
	theme_bw() +
	scale_color_brewer(palette = 'Set2') +
	xlab('CDC Code') +
	ylab('Average Rate Killed, per 100,000')

# ... average rate by division 
dat3 = tmp %>%
	   group_by(division) %>% 
	   summarise(y.black.mean  = mean(y.black,  na.rm = TRUE),
	  			 y.white.mean  = mean(y.white,  na.rm = TRUE)) %>%
	   mutate(y.white_minus_black = y.white.mean - y.black.mean) %>%
	   gather(rate, value, y.black.mean:y.white_minus_black)

ggplot(filter(dat3, rate %in% c('y.black.mean', 'y.white.mean')),
	aes(x = division, y = value, color = rate, group = rate)) +
	geom_point(size = 4) +
	geom_line(data = filter(dat3, rate %in% c('y.white_minus_black')), 
			 aes(x = division, y = value), lwd = 1.5) +
	theme_bw() +
	scale_color_brewer(palette = 'Set2') +
	xlab('CDC Code') +
	ylab('Average Rate Killed, per 100,000') +
	geom_hline(yintercept = 0, lty = 'dashed')

# ... average rate by interaction 
dat4 = tmp %>%
	   group_by(ur.code, division) %>%
	   summarise(y.black.mean  = mean(y.black,  na.rm = TRUE),
	  			 y.white.mean  = mean(y.white,  na.rm = TRUE)) %>%
	   mutate(y.white_minus_black = y.white.mean - y.black.mean) %>%
	   gather(rate, value, y.black.mean:y.white_minus_black)

ggplot(filter(dat4, rate %in% c('y.black.mean', 'y.white.mean')),
	   aes(x = ur.code, y = value, group = rate, color = rate)) +
	facet_wrap(~division) + 
	geom_point(size = 2) +
	geom_line() +
	#coord_flip() +
	theme_bw() +
	scale_color_brewer(palette = 'Set2') +
	xlab('CDC Code') +
	ylab('Rate Killed, per 100,000') +
	theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))














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

## 2: model
#m1 = glm(b.w ~ ur.code + division, 
#		 data = tmp,
#	 	 family = Gamma(link = 'log'))



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


