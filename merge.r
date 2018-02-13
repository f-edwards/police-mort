rm(list=ls())
library(tidyverse)
library(ggplot2)
library(date)
library(lubridate)
library(stringr)
library(wru)
library(jsonlite)
library(tigris)

#fe<-read_csv("./data/fatal-encounters-5-8-17.csv")
fe_new<-read_csv("./data/fatal-encounters-2-12-18.csv")


#### get block shapefiles

#### do some eda on race missingness, gets much better 2012 and forward, gonna start there for now
#### can do sensitivity with full data later
fe_new<-fe_new%>%
  filter(`Date (Year)`>=2012)%>%
  filter(!(is.na(Latitude))) ## only one legitimate case screened out

names(fe_new)<-c("id", "name", "age", "gender", "race", "URL", "death_date", 
                 "loc_address", "loc_city", "loc_state", "loc_zip", "loc_county", 
                 "loc_full_address", "Latitude", "Longitude", "agency", "cause_of_death",
                 "cause_description", "official_disposition", "news_url", "mental_illness", "video",
                 "null1", "dateanddesc", "null2", "id2", "year", "null3")

fe_new<-fe_new%>%
  select(id, name, age, gender, race, death_date, loc_state, loc_county, Latitude, Longitude,
         agency, cause_of_death, official_disposition, year)

####### run script to link lat/long -> FIPS block
####### first check to see if crosswalk file is in directory, only run on new FE data
files<-list.files("./data")
if(!("block_map.csv"%in%files)){
  coords<-fe_new%>%
    select(Latitude, Longitude)%>%
    mutate("FIPS_block" = NA,
           "FIPS_county" = NA,
           "FIPS_state" = NA,
           "STname" = NA,
           "API_status" = NA)
  for(i in 1:3747){
    url<-paste("https://geo.fcc.gov/api/census/block/find?latitude=",
               coords[i, 1], 
               "&longitude=", 
               coords[i, 2], 
               "&showall=true&format=json",
               sep="")
    
    temp<-fromJSON(url)
    print(i)
    print(temp$status)
    
    coords[i, 3:7]<-c(temp$Block$FIPS,
                      temp$County$FIPS,
                      temp$State$FIPS,
                      temp$State$code,
                      temp$status)
    
  }
  write.csv(coords, "./data/block_map.csv", row.names=FALSE)
}

block_map<-read_csv("./data/block_map.csv",
                    col_types = "ddccccc")


fe_new<-

#### get block-level data from census api
census_dat_block<-get_census_data(key="518b6e66ffa1857a9e4ffd5d894f2934bb06c045",
                          states=unique(coords$STname)[1],
                          age=TRUE, sex=TRUE, census.geo="block")


# 
# census_dat_county<-get_census_data(key="518b6e66ffa1857a9e4ffd5d894f2934bb06c045",
#                                   states=unique(coords$STname)[1],
#                                   age=TRUE, sex=TRUE, census.geo="county")


#### remove nicknames (between quotes)
test<-str_replace(fe_new$name, '["\047].*["\047]', "")


names.tmp<-fe_new%>%
  extract(name, c("first.name", "surname"), "([^ ]+) (.*)")

### remove jr, II, III, Sr. numerics are all for john does
names.tmp$surname<-ifelse(grepl("jr.", tolower(names.tmp$surname)), substr(names.tmp$surname, 1, nchar(names.tmp$surname)-4), names.tmp$surname) 
names.tmp$surname<-ifelse(grepl("jr", tolower(names.tmp$surname)), substr(names.tmp$surname, 1, nchar(names.tmp$surname)-3), names.tmp$surname) 
names.tmp$surname<-ifelse(grepl("sr.", tolower(names.tmp$surname)), substr(names.tmp$surname, 1, nchar(names.tmp$surname)-4), names.tmp$surname) 
names.tmp$surname<-ifelse(grepl("II", names.tmp$surname), substr(names.tmp$surname, 1, nchar(names.tmp$surname)-3), names.tmp$surname) 
names.tmp$surname<-ifelse(grepl("III", names.tmp$surname), substr(names.tmp$surname, 1, nchar(names.tmp$surname)-4), names.tmp$surname) 
names.tmp$surname<-ifelse(grepl("IV", names.tmp$surname), substr(names.tmp$surname, 1, nchar(names.tmp$surname)-3), names.tmp$surname) 


names.tmp<-names.tmp%>%
  mutate(surname=word(surname, -1))

#### put missing in for missing names

### remove trailing whitespace
names.tmp$surname<-trimws(names.tmp$surname, which = c("both"))


name_out<-predict_race(voter.file=fe_new, census.data = census_dat_block, 
                       census.key="518b6e66ffa1857a9e4ffd5d894f2934bb06c045")

##### CHECK ON THE LIST OF CAUSES, SUICIDES, SOME OTHERS SHOULD BE DROPPED
##### THE TRAFFIC DATA IS IMPROVED in 13-14 PER README, CHECK THAT TS FOR TRAFFIC DEATHS


### prep for surname matching
#### extract surnames


#### pull only final word

#### REMOVE NICKNAMES


missing<-c("\"\"Kenny\"\"", "1", "2", "police")

tmp$surname<-ifelse(tmp$surname%in%missing, NA, names.tmp$surname)
tmp$sex<-ifelse(tmp$gender=="Male", 0, ifelse(tmp$gender=="Female", 1, NA))

###################### PREDICT RACE ON NAMES, AGE, GENDER, BLOCK



# ### vis for accuracy
# ### performance is weak for af am, asian am, strong for latino, white, as we might expect
# ### set a cut off for assignment at 75 percent, will leave unknown otherwise
# name_tmp1<-name_out%>%filter(race=="Race unspecified")
# name_tmp1$race<-ifelse(name_tmp1$pred.whi>0.75, "European-American/White", name_tmp1$race)
# name_tmp1$race<-ifelse(name_tmp1$pred.bla>0.75, "African-American/Black", name_tmp1$race)
# name_tmp1$race<-ifelse(name_tmp1$pred.his>0.75, "Hispanic/Latino", name_tmp1$race)
# name_tmp1$race<-ifelse(name_tmp1$pred.asi>0.75, "Asian/Pacific Islander", name_tmp1$race)
# 
# name_tmp1<-name_tmp1%>%select(-county, -sex, -pred.whi, -pred.bla, -pred.his, -pred.asi, -pred.oth, -first.name, -surname)
# ### use this data frame to sub in for missing race in original file
# 
# tmp<-bind_rows(fdat%>%filter(!(race=="Race unspecified"))%>%
#                  select(-name), 
#                name_tmp1)%>%
#   filter(year>2012)

write.csv(tmp, "fe-clean-cause.csv", row.names = FALSE)
write.csv(fdat, "fe-noimp-cause.csv", row.names=FALSE)

