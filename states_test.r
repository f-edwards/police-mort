#########################
# Fatal Encounters Data
# division main file, .r
# ... revision edits 
# last edit 3/5 (by: ME)
########################
## Set-up
# 1: load packages; setwd etc. 
rm(list=ls())
set.seed(1)

library(tidyverse)
library(rstanarm)
library(parallel)
options(mc.cores = parallel::detectCores())
theme_set(theme_minimal())
setwd("~/Projects/police_mort")
select  = dplyr::select

fe_new<-read_csv("fatal-encounters-2-12-18.csv")
fe_new<-fe_new%>%
  filter(`Date (Year)`>=2012)
names(fe_new)<-c("id", "name", "age", "gender", "race", "URL", "death_date", 
                 "loc_address", "loc_city", "loc_state", "loc_zip", "loc_county", 
                 "loc_full_address", "Latitude", "Longitude", "agency", "cause_of_death",
                 "cause_description", "official_disposition", "news_url", "mental_illness", "video",
                 "null1", "dateanddesc", "null2", "id2", "year", "null3")

fe_new<-fe_new%>%
  select(id, name, age, gender, race, death_date, loc_state, loc_county, Latitude, Longitude,
         agency, cause_of_death, official_disposition, year)
fe_new<-fe_new%>%
  mutate(race = ifelse(race == "African-American/Black", "black", race),
         race = ifelse(race == "Asian/Pacific Islander", "asian", race),
         race = ifelse(race == "European-American/White", "white", race),
         race = ifelse(race == "Hispanic/Latino", "latino", race),
         race = ifelse(race == "Middle Eastern", "other", race),
         race = ifelse(race == "Native American/Alaskan", "other", race),
         race = ifelse(race == "Race unspecified", NA, race))


imputeds<-read_csv("predicted_race_05FalsePos.csv")

fdat<-imputeds%>%
  left_join(fe_new) # for surname/block imputation

############# for missing race data, replace with imputed, leave missings with classification above .05 false pos threshold missing

fdat<-fdat%>%
  mutate(race = ifelse(is.na(race), pred.race, race))%>%
  mutate(fips = as.numeric(fips))

fdat<-fdat%>%
  mutate(fips = ifelse(fips == 46113, 46102, fips))%>%
  mutate(fips = ifelse(fips == 2270, 2158, fips))

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
	  dplyr::select(fips, ur.code)%>%
  mutate(fips = ifelse(fips == 46113, 46102, fips))%>%
  mutate(fips = ifelse(fips == 2270, 2158, fips))

# ... attach census division codes
regions = read.csv('regions.csv', 
					stringsAsFactors = FALSE) %>%
		  mutate(state = State.Code) %>%
		  dplyr::select(state, Division)

# ... demographics

pop2<-read_csv("nhgis0029_ds216_20155_2015_county.csv")
pop2$fips<-paste(pop2$STATEA, pop2$COUNTYA, sep="")
pop2<-pop2%>%
  rename(fips.st=STATEA)%>%
  mutate(fips = as.numeric(fips), fips.st=as.numeric(fips.st))%>%
  filter(fips.st!=72)

cw<-read_csv("fips-st-crosswalk.csv")%>%
  dplyr::select(state, fips)%>%rename(fips.st=fips)

pop2<-left_join(pop2, cw)%>%
  dplyr::select(-fips.st)

pop<-pop2%>%
  mutate(black.men=ADT5E007+ADT5E008+ADT5E009+
           ADT5E010+ADT5E011+ADT5E012+ADT5E013+
           ADT5E014+ADT5E015+ADT5E016,
         white.men=ADUBE007+ADUBE008+ADUBE009+
           ADUBE010+ADUBE011+ADUBE012+ADUBE013+
           ADUBE014+ADUBE015+ADUBE016,
         latino.men=ADUCE007+ADUCE008+ADUCE009+
           ADUCE010+ADUCE011+ADUCE012+ADUCE013+
           ADUCE014+ADUCE015+ADUCE016,
         tot.men=ADURE008)%>%
  select(fips, state, black.men, white.men, 
         latino.men, tot.men)

### got all of them matched
#z<-which(!(fdat$fips%in%pop$fips))


### KIDS - START AT 18 or !5? Pop data may link up at 15

pop<-pop%>%
  left_join(pop)%>%
  left_join(cdc)%>%
  left_join(regions)

rm(cdc); rm(cw); rm(fe_new); rm(imputeds); rm(pop2); rm(regions)

# add in the new conditions: 
# ... filter out suicides
ns = c("Suicide", "Ruled suicide", "Ruled suicide by police", "Murder/suicide")
fdat = fdat %>% filter(!(official_disposition %in% ns)) 

# ... and reduce to men
fdat <- fdat %>% filter(gender == "Male")

# ... reduce ur codes to 3 (largest urban; medium urban; rural)
pop = pop %>%
      mutate(ur.code  = ifelse(ur.code %in% c('1: large central metro'), 'large urban', 
                        ifelse(ur.code %in% c('2: large fringe metro', '3: medium metro', '4: small metro'), 'medium urban',
                        ifelse(ur.code %in% c('5: micropolitan', '6: noncore'), 'rural', NA))))

tmp = fdat %>%
       group_by(fips, race) %>%
       summarise(deaths = n()) %>%
       spread(race, deaths, fill = 0) %>%
       rename(d.black = black,
              d.asian = asian,
              d.white = white,
              d.na    = `<NA>`,
              d.latino = latino,
              d.other  = other) %>%
       mutate(d.total = d.black + d.asian + d.white + d.latino + d.na + d.other)

tmp2 = left_join(pop, tmp, 'fips') %>%
       mutate_at(vars(d.asian:d.total), funs(replace(., which(is.na(.)), 0))) %>%
       tbl_df()

# modeling
# ... for blacks 
blk.stan.0 = stan_glmer(d.black ~ (1 | ur.code) + (1 | state),
                        prior_intercept = normal((log(0.94)-log(100000)), 2.5), 
                        prior = normal(0, 2.5),
                        prior_covariance = decov(1, 1, 1, 1),
                        data = tmp2, 
                        offset = I(log(black.men + 1)), 
                        family ="neg_binomial_2", 
                        iter = 500, 
                        chains = 2,
                        adapt_delta = .999
                        )


# ... OK, lets just run this a bunch
# condition -> result
# just 2 random effects -> 18 divergent
# with sparse on        -> 18 divergent
# with alpha at .999   -> so slow (30 mins); but works

save.image('black_model_999.RData')


# ... take a look at what this model gives
blk.stan = blk.stan.0

ur.code = unique(tmp2$ur.code)
state = unique(tmp2$state)
ur.state = expand.grid(state, ur.code)
names(ur.state) = c("state", "ur.code")
n = length(state)*length(ur.code)

newdata <- tmp2[1:n,]
newdata$fips <- 0; 
newdata$tot.men<-newdata$black.men<-newdata$white.men<-newdata$latino.men<-100000000; 
newdata$ur.code<-ur.state$ur.code; 
newdata$state<-ur.state$state

post.blk = posterior_predict(blk.stan, newdata = newdata)/1000

post.blk.sims = cbind(ur.state, t(post.blk))
post.blk.sims = gather(post.blk.sims, 
                       key = sim, 
                       value = sim.rate, 
                      -ur.code, -state)


# 3: calculate mean and 95% uncertainty intervals for each group 
post.blk.int = bind_cols(ur.state, as.data.frame(
  t(apply(post.blk, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))

holder = tmp2 %>% 
         select(state, Division) %>%
         distinct()

post.blk.int2 = left_join(post.blk.int, holder, 'state')

# ... unscaled! 
ggplot(post.blk.int2 %>% filter(ur.code == 'medium metro'), 
  aes(x = reorder(state, `50%`), y = `50%`, fill = Division, 
      ymin = `2.5%`, ymax = `97.5%`)) +
  geom_point() + 
  geom_errorbar()


# ... unscaled! 
ggplot(post.blk.int2 %>% filter(ur.code == 'medium urban'), 
  aes(x = reorder(state, `50%`), y = `50%`, fill = Division)) + #, #ymin = `2.5%`, ymax = `97.5%`)) + 
geom_point(color = 'grey10', pch = 21, size = 2.5) + 
coord_flip() + 
facet_wrap(~ur.code) +
scale_fill_brewer(palette = 'Paired')
#geom_errorbar(width = .05, alpha = .25)


# ... or 
# ... unscaled! 
ggplot(post.blk.int2 %>% filter(ur.code == 'large urban'), 
       aes(x = reorder(state, `50%`), y = `50%`, fill = Division)) + #, #ymin = `2.5%`, ymax = `97.5%`)) + 
    geom_point(color = 'grey10', pch = 21, size = 2.5) + 
    coord_flip() + 
    facet_wrap(~ur.code) +
    scale_fill_brewer(palette = 'Paired') + facet_wrap(~Division, scale = 'free_y')
#geom_errorbar(width = .05, alpha = .25)



# ur code, w/ only micro and non-core collapsed 


