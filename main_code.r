<<<<<<< HEAD
#########################
# Fatal Encounters Data
# Main Analysis file, .R
#########################

## Set-up
# 1: load packages; setwd etc. 
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(date)
setwd('C:/Users/Mike/Dropbox/PoliceShooting/police-mort-master') 
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
	   select(-year)

# ... attach CDC urban-rual scheme
cdc = read_fwf(file = 'NCHSURCodes2013.txt', 
			   fwf_positions(c(1, 3, 7, 10, 47, 98, 107, 116, 118, 120), 
					  		 c(2, 5, 8, 45, 96, 105, 114, 116, 118, 120))
		) %>%
	  select(X3, X4, X8) %>%
	  rename(state = X3, county = X4, ur.code = X8) %>%
	  mutate(county = tolower(county),
	  	     ur.code = ifelse(ur.code == 1, '1: large central metro',
	  	     		   ifelse(ur.code == 2, '2: large fringe metro',
	  	     		   ifelse(ur.code == 3, '3: medium metro',
	  	     		   ifelse(ur.code == 4, '4: small metro', 
	  	     		   ifelse(ur.code == 5, '5: micropolitan', 
	  	     		   ifelse(ur.code == 6, '6: noncore', NA)))))))

# ... attach census division codes
regions = read.csv('regions.csv', 
					stringsAsFactors = FALSE) %>%
		  mutate(state = State.Code) %>%
		  select(state, Division)

# ... demographics
demos = read.csv('demographics.csv', stringsAsFactors = FALSE) %>%
		rename(fips = FIPS) 

# ... merge everything 
tmp = left_join(fdat, cdc, c('state', 'county')) %>%
	  inner_join(regions, 'state') %>%
	  inner_join(demos, 'fips') %>%
	  filter(year == 2012) %>%
	  mutate(race = ifelse(race == 'African-American/black', 'African-American/Black',
	  					   race))

##########################
##########################
######## Analysis ########
##########################
##########################

## 1: EDA
# 1a: cases, by race, by urban-rual code x divison
pDat1 = tmp %>%
		select(race, ur.code, Division) %>%
		group_by(race, ur.code, Division) %>%
		summarise(n = n()) %>%
		mutate(ur.div = paste(ur.code, Division, sep = ','))

ggplot(pDat1, aes(x = ur.code, y = n, group = race, color = race)) +
	geom_jitter(size = 4, width = 0.5, alpha = .8) +
	theme_bw() +
	scale_color_brewer(palette = 'Set2') +
	facet_wrap(~ Division) +
	coord_flip()
=======
#########################
# Fatal Encounters Data
# Main Analysis file, .R
#########################

## Set-up
# 1: load packages; setwd etc. 
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(date)
setwd('C:/Users/Mike/Dropbox/PoliceShooting/police-mort-master') 
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
	   select(-year)

# ... attach CDC urban-rual scheme
cdc = read_fwf(file = 'NCHSURCodes2013.txt', 
			   fwf_positions(c(1, 3, 7, 10, 47, 98, 107, 116, 118, 120), 
					  		 c(2, 5, 8, 45, 96, 105, 114, 116, 118, 120))
		) %>%
	  select(X3, X4, X8) %>%
	  rename(state = X3, county = X4, ur.code = X8) %>%
	  mutate(county = tolower(county),
	  	     ur.code = ifelse(ur.code == 1, '1: large central metro',
	  	     		   ifelse(ur.code == 2, '2: large fringe metro',
	  	     		   ifelse(ur.code == 3, '3: medium metro',
	  	     		   ifelse(ur.code == 4, '4: small metro', 
	  	     		   ifelse(ur.code == 5, '5: micropolitan', 
	  	     		   ifelse(ur.code == 6, '6: noncore', NA)))))))

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
pop<-pop%>%dplyr::select(fips, tot.pop, latino, white, black, amind, api)

# ... merge everything 
tmp = left_join(fdat, cdc, c('state', 'county')) %>%
	  inner_join(regions, 'state') %>%
	  inner_join(demos, 'fips') %>%
	  filter(year == 2012)

##########################
##########################
######## Analysis ########
##########################
##########################

## 1: EDA
# 1a: cases by urban-rual
table(tmp$ur.code) %>% plot()

# 2a: ... by dividion 
table(tmp$Division) %>% plot 

# 3a: cases, by race, by urban-rual code x divison
pDat1 = tmp %>%
		select(race, ur.code, Division) %>%
		group_by(race, ur.code, Division) %>%
		summarise(n = n()) %>%
		mutate(ur.div = paste(ur.code, Division, sep = ','))


ggplot(pDat1, aes(x = ur.code, y = n, group = race, color = race)) +
	geom_jitter(size = 4, width = 0.5, alpha = .8) +
	theme_bw() +
	scale_color_brewer(palette = 'Set2') +
	facet_wrap(~ Division) +
	coord_flip()


>>>>>>> 90544bfb2024502c9b40cc08eeb761cbe59bf30a
