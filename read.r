########################## READ AND TRANSFORM TABLES

library(tidyverse)
setwd("~/Projects/police-mort")
select  = dplyr::select

# ... attach and configure mortality file
# .... subset years (2012 - 2018)
fe_new <- read_csv("./data/fatal-encounters-2-12-18.csv") %>%
  filter(`Date (Year)`>=2012)

# .... make names more user-friendly
names(fe_new)<-c("id", "name", "age", "gender", "race", "URL", "death_date", 
                 "loc_address", "loc_city", "loc_state", "loc_zip", "loc_county", 
                 "loc_full_address", "Latitude", "Longitude", "agency", 
                 "cause_of_death","cause_description", "official_disposition", 
                 "news_url", "mental_illness", "video", "null1", "dateanddesc", 
                 "null2", "id2", "year", "null3")

# .... re-label race variable
fe_new <- fe_new %>%
  select(id, name, age, gender, race, death_date, 
         loc_state, loc_county, Latitude, Longitude,
         agency, cause_of_death, official_disposition, year) %>%
  mutate(race = ifelse(race == "African-American/Black", "black", race),
         race = ifelse(race == "Asian/Pacific Islander", "asian", race),
         race = ifelse(race == "European-American/White", "white", race),
         race = ifelse(race == "Hispanic/Latino", "latino", race),
         race = ifelse(race == "Middle Eastern", "other", race),
         race = ifelse(race == "Native American/Alaskan", "other", race),
         race = ifelse(race == "Race unspecified", NA, race))

# .... merge in imputed races
imputeds <- read_csv("./data/predicted_race_05FalsePos.csv")

# .... for race == missing, replace w/ classification above threshold
# .... and fix fips codes
fdat <- imputeds %>% 
  left_join(fe_new) %>%
  mutate(race = ifelse(is.na(race), pred.race, race),
         fips = as.numeric(fips),
         fips = ifelse(fips == 46113, 46102, 
                       ifelse(fips == 2270,   2158, fips)))

# ... attach and configure population data 
# .... urban-rural county classifications 
cdc = read_fwf(file = './data/NCHSURCodes2013.txt', 
               fwf_positions(c(1, 3, 7, 10, 47, 98, 107, 116, 118, 120), 
                             c(2, 5, 8, 45, 96, 105, 114, 116, 118, 120))) %>%
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
  select(fips, ur.code)%>%
  mutate(fips = ifelse(fips == 46113, 46102, fips)) %>%
  mutate(fips = ifelse(fips == 2270, 2158, fips))

# .... census division codes
regions = read.csv('./data/regions.csv', stringsAsFactors = FALSE) %>%
  mutate(state = State.Code) %>%
  select(state, Division)

# ..,. demographics
pop2 <- read_csv("./data/nhgis0029_ds216_20155_2015_county.csv")
pop2$fips<-paste(pop2$STATEA, pop2$COUNTYA, sep="")
pop2<-pop2%>%
  rename(fips.st=STATEA)%>%
  mutate(fips = as.numeric(fips), fips.st=as.numeric(fips.st)) %>%
  filter(fips.st!=72)

# .... fips-state crosswalk
cw <- read_csv("./data/fips-st-crosswalk.csv") %>%
  select(state, fips) %>% 
  rename(fips.st = fips)

pop2 <- left_join(pop2, cw) %>%
  select(-fips.st)

# .... calculate men per county
pop <- pop2%>%
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
  select(fips, state, black.men, white.men, latino.men, tot.men)

# .... merge all population frames together
pop <- pop %>%
  left_join(pop) %>%
  left_join(cdc) %>%
  left_join(regions)

# ... clean up frames we're done with
# .... i.e., all but 1: fdat (mortality data) & 2: pop (population data)
rm(cdc); rm(cw); rm(fe_new); rm(imputeds); rm(pop2); rm(regions)

#########################################################
# ... selecting the data to analyze 
# .... 1: select only men from mortality data 
# .... (removes 889 women, 7 trans)
fdat <- fdat %>% filter(gender == "Male")

# .... and 2: remove children (anyone under age 18)
# ....  convert all ages to numeric for filter
# .... (removes 318 children)
fdat[grep("mon", fdat$age), "age"]<-"0"
ranges<-grep("-", fdat$age)
## substr and str_sub doing weird stuff with vector, going to do by row
for(i in 1:length(ranges)){
  fdat[ranges[i], "age"]<-substr(fdat[ranges[i], "age"], 1, 2)
}
s<-grep("s", fdat$age)
for(i in 1:length(s)){
  fdat[s[i], "age"]<-substr(fdat[s[i], "age"], 1, 2)
}
misc<-grep("`", fdat$age)
for(i in 1:length(misc)){
  fdat[misc[i], "age"]<-substr(fdat[misc[i], "age"], 1, 2)
}
fdat$age<-as.numeric(fdat$age)           
fdat<-fdat%>%
  filter(age>=18)

# .... and 3: filter out suicides from mortality file 
# .... (removes 810 adult male suicides)
ns = c("Suicide", "Ruled suicide", "Ruled suicide by police", "Murder/suicide")
fdat = fdat %>% filter(!(official_disposition %in% ns)) 

# .... and 4: filter out not-force deaths (ie., fleeing deaths)
# .... (removes 1476 non-force deaths)
fdat = fdat %>%  
  filter(cause_of_death %in% 
           c('Asphyxiated/Restrained','Beaten/Bludgeoned with instrument', 
             'Chemical agent/Pepper spray', 'Medical emergency', 'Tasered', 
             'Gunshot'))

# .... leaves 6295 adult male homicides recorded between 1/1/2012 and 2/12/2018

#########################################################

# ... merge the sample of mortalities w/ the population data
# .... get deaths per county, for counties w/ observed police mortalities...
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

# .... and join with full list of counties
tmp2 = left_join(pop, tmp, 'fips') %>%
  mutate_at(vars(d.asian:d.total), funs(replace(., which(is.na(.)), 0))) %>%
  tbl_df() %>%
  rename(division = Division)

