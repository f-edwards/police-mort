#########################
# Fatal Encounters Data
# Main file, .R
#########################

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

#fe_new<-fe_new%>%
#  mutate(race = ifelse(race == "African-American/Black", "black", race),
#         race = ifelse(race == "Asian/Pacific Islander", "asian", race),
#         race = ifelse(race == "European-American/White", "white", race),
#         race = ifelse(race == "Hispanic/Latino", "latino", race),
#         race = ifelse(race == "Middle Eastern", "other", race),
#         race = ifelse(race == "Native American/Alaskan", "other", race),
#         race = ifelse(race == "Race unspecified", NA, race))

fe_new<-fe_new%>%
  mutate(race = ifelse(is.na(race), "Race unspecified", race),
         race = ifelse(race == "African-American/Black", "black", race),
         race = ifelse(race == "Asian/Pacific Islander", "asian", race),
         race = ifelse(race == "European-American/White", "white", race),
         race = ifelse(race == "Hispanic/Latino", "latino", race),
         race = ifelse(race == "Middle Eastern", "other", race),
         race = ifelse(race == "Native American/Alaskan", "other", race),
         race = ifelse(race == "Race unspecified", "Race unspecified", race))


imputeds<-read_csv("predicted_race_05FalsePos.csv")

fdat<-imputeds %>%
  left_join(fe_new) %>%
  mutate(pred.race = ifelse(is.na(pred.race), 
    'Race unspecified', pred.race)) # for surname/block imputation

# table(fdat$pred.race, fdat$pred.race2, useNA ='ifany')

############# for missing race data, replace with imputed, leave missings with classification above .05 false pos threshold missing

#fdat<-fdat%>%
#  mutate(race = ifelse(is.na(race), pred.race, race))%>%
#  mutate(fips = as.numeric(fips))

fdat = fdat %>%
  mutate(race = ifelse(race == 'Race unspecified', pred.race, race)) %>%
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

fdat<-fdat %>% filter(gender == "Male")

pop<-pop%>%
  left_join(pop)%>%
  left_join(cdc)%>%
  left_join(regions)

rm(cdc); rm(cw); rm(fe_new); rm(imputeds); rm(pop2); rm(regions)

# ... filter out suicides
ns = c("Suicide", "Ruled suicide", "Ruled suicide by police", "Murder/suicide")
fdat = fdat %>% filter(!(official_disposition %in% ns)) 

# ... and not self-inflicted (e.g., while fleeing) 
fdat = fdat %>% 
    filter(cause_of_death %in% 
         c('Asphyxiated/Restrained','Beaten/Bludgeoned with instrument', 
           'Chemical agent/Pepper spray', 'Medical emergency', 'Tasered', 
           'Gunshot'))

# ... compress urban-rual code, to 3 categories (large urban; med. urban; rural)
pop = pop %>%
      mutate(ur.code2 = ifelse(ur.code %in% c('1: large central metro', '2: large fringe metro'), 'large urban', 
                        ifelse(ur.code %in% c('3: medium metro', '4: small metro'), 'medium urban',
                        ifelse(ur.code %in% c('5: micropolitan', '6: noncore'), 'rural', NA))))

# ... join frames
tmp2 = pop %>%
       full_join(fdat, c('fips')) %>%
       filter(!is.na(ur.code2)) %>%
       group_by(fips, race) %>%
       mutate(d.count = n()) %>%
       select(fips, state, ur.code2, Division, race, d.count, tot.men,
              black.men, white.men, latino.men) %>%
       #arrange(fips, race) %>%
       ungroup() %>%
       distinct() %>%
       group_by(fips) %>%
       spread(race, d.count, fill = 0) %>%
       rename(d.black = black, 
              d.asian = asian, 
              d.white = white,
              d.latino = latino,
              d.other = other,
              d.na = `Race unspecified`) %>%
       mutate(d.total = d.black + d.white + d.latino + d.asian + d.other + d.na) %>%
       select(-`<NA>`)

# ... should pass all these conditions if done right
# ... MAYBE compare to NA coding
# ... check counts...
# ... of total deaths (should equal length fdat)
# sum(tmp2$d.total)
# dim(fdat)

# ... of fips (should equal # in pop)
# length(unique(tmp2$fips))
# length(unique(pop$fips))

# ... all fips not in fdat should have 0s
# (tmp2[which(!(pop$fips %in% fdat$fips)),]) %>% print %>% nrow
# tmp2$d.total %>% table(. == 0)



# 2: models
# ... for everyone
tot.stan.0 = stan_glmer(d.total ~ (1 | ur.code) + (1 | division),
            prior_intercept=normal((log(0.37)-log(100000)), 2.5), 
                        prior = normal(0, 2.5), 
                        prior_covariance = decov(1, 1, 1, 1),
                        data = tmp2, 
                        offset = I(log(tot.pop + 1)), 
                        family = "neg_binomial_2", 
                        iter = 2000, 
                        chains = 4, 
                        # ... need to increase 
                        adapt_delta = 0.8)

# ... for blacks 
blk.stan.0 = stan_glmer(d.black ~ (1 | ur.code2) + (1 | state),
                        prior_intercept = normal((log(0.94)-log(100000)), 2.5), 
                        prior = normal(0, 2.5),
                        prior_covariance = decov(1, 1, 1, 1),
                        data = tmp2, 
                        offset = I(log(black.men + 1)), 
                        family ="neg_binomial_2", 
                        iter = 2000, 
                        chains = 4,
                        # ... need to increase 
                        adapt_delta = 0.85)   

# ... for whites 
wht.stan.0 = stan_glmer(d.white ~ (1 | ur.code2) + (1 | state),
            prior_intercept = normal((log(0.37)-log(100000)), 2.5),
                        prior = normal(0, 2.5), 
                        prior_covariance = decov(1, 1, 1, 1),
                        data = tmp2, 
                        offset = log(white.men), 
                        family="neg_binomial_2", 
                        iter = 2000, 
                        chains = 4,
                        # ... need to increase 
                        adapt_delta = 0.85)

# ... for latinos
lat.stan.0 = stan_glmer(d.latino ~ (1|ur.code) + (1 | division),
                        prior_intercept = normal((log(0.37)-log(100000)), 2.5), 
                        prior = normal(0, 2.5), 
                        prior_covariance = decov(1, 1, 1, 1), 
                        data = tmp2, 
                        offset = I(log(latino.men + 1)), 
                        family="neg_binomial_2", 
                        iter = 2000, 
                        chains = 4,
                        # ... need to increase 
                        adapt_delta = 0.85)



