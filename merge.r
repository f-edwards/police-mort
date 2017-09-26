rm(list=ls())
library(dplyr)
library(ggplot2)
library(date)
library(lubridate)
library(data.table)
library(stringr)
library(tidyr)
library(wru)

setwd("C:/sync/police-mort/")

fe<-fread("fatal-encounters-5-8-17.csv", header=TRUE)

names(fe)<-c("id", "name", "age", "gender", "race", "image", "date", 
             "address", "city", "state", "zip", "county", "agency", "cause",
             "circumstance", "disposition", "news", "symptoms", "uniqueid",
             "data-status", "date-descript", "dum", "dum2", "year")

fe$county<-tolower(fe$county)


fe$county<-gsub("city of st. louis", "st. louis city", fe$county)
fe$county<-gsub("saint louis city", "st. louis city", fe$county)
fe$county<-gsub("st .louis", "st. louis city", fe$county)
fe$county<-gsub("city of st .louis", "st. louis city", fe$county)

fe$county<-gsub("lasalle", "la salle", fe$county)
fe$county<-gsub("prince georges", "prince george's", fe$county)
fe$county<-gsub("mcclennan", "mclennan", fe$county)
fe$county<-gsub("doña ana", "dona ana", fe$county)
fe$county<-gsub("yukon koyukuk", "yukon-koyukuk", fe$county)
fe$county<-gsub("city of baltimore", "baltimore", fe$county)
fe$county<-gsub("charelston", "charleston", fe$county)
fe$county<-gsub("athens-clarke", "clarke", fe$county)
fe$county<-gsub("prince of wales hyder", "prince of wales-hyder", fe$county)
fe$county<-gsub("laporte", "la porte", fe$county)
fe$county<-gsub("saint joseph", "st. joseph", fe$county)
fe$county<-gsub("brooklyn", "kings", fe$county)
fe$county<-gsub("clallam bay", "clallam", fe$county)
fe$county<-gsub("saint mary's", "st. mary's", fe$county)

fe$county<-gsub("city of newport news", "newport news city", fe$county)

fe$county<-gsub("kuai", "kauai", fe$county)
fe$county<-gsub("shively", "jefferson", fe$county)
fe$county<-gsub("saint clair", "st. clair", fe$county)
fe$county<-gsub("clarl", "clark", fe$county)
fe$county<-gsub("park hill", "denver", fe$county)
fe$county<-gsub("braziria", "brazoria", fe$county)
fe$county<-gsub("andalusia", "covington", fe$county)
fe$county<-gsub("oskaloosa", "okaloosa", fe$county)
fe$county<-gsub("du page", "dupage", fe$county)
fe$county<-gsub("comanche `", "comanche", fe$county)
fe$county<-gsub("philadephia", "philadelphia", fe$county)
fe$county<-gsub("oglala lakota", "shannon", fe$county)
fe$county<-gsub("montrose county", "montrose", fe$county)
fe$county<-gsub("dekalb", "de kalb", fe$county)
fe$county<-gsub("prince george's", "prince georges", fe$county)
fe$county<-gsub("east baton rouge", "e. baton rouge", fe$county)
fe$county<-gsub("west baton rouge", "w. baton rouge", fe$county)

fe$county<-gsub("lasalle", "la salle", fe$county)
fe$county<-gsub("queen anne's", "queen annes", fe$county)
fe$county<-gsub("matanuska susitna", "matanuska-susitna", fe$county)
fe$county<-gsub("district of columbia", "the district", fe$county)
fe$county<-gsub("dc", "the district", fe$county)

fe$county<-gsub("st. john the baptist", "st. john baptist", fe$county)
fe$county<-gsub("desoto", "de soto", fe$county)
fe$county<-gsub("dupage", "du page", fe$county)
fe$county<-gsub("yukon koyukuk", "yukon-koyukuk", fe$county)
fe$county<-gsub("la paz", "lapaz", fe$county)
fe$county<-gsub("mcdonough", "mc donough", fe$county)
fe$county<-gsub("doña ana", "dona ana", fe$county)
fe$county<-gsub("comanche `", "comanche", fe$county)
fe$county<-gsub("bedford city", "bedford", fe$county)
fe$county<-gsub("northumberland", "northumberlnd", fe$county)

fe$county<-gsub("northwest arctic", "northwest artic borough", fe$county)
fe$county<-gsub("o'brien", "obrien", fe$county)
fe$county<-gsub("north slope", "north slope borouh", fe$county)
fe$county<-gsub("mcclean", "mc lean", fe$county)

fe$county<-gsub("mclennan", "mc lennan", fe$county)
fe$county<-gsub("dolton", "cook", fe$county)

fe$county<-gsub("mcdowell", "mc dowell", fe$county)
fe$county<-gsub("mccook", "mc cook", fe$county)
fe$county<-gsub("scotts bluff", "scott bluff", fe$county)
fe$county<-gsub("prince of wales-hyder", "pr.of wales-hyder cns area", fe$county)
fe$county<-gsub("sitka", "sitka borough", fe$county)
fe$county<-gsub("mcdonald", "mc donald", fe$county)
fe$county<-gsub("petersburg", "petersburg census area", fe$county)
fe$county<-gsub("dewitt", "de witt", fe$county)
fe$county<-gsub("st. mary's", "st. marys", fe$county)

fe$county<-gsub("mcduffie", "mc duffie", fe$county)
fe$county<-gsub("beford", "bedford", fe$county)
fe$county<-gsub("mcminn", "mc minn", fe$county)
fe$county<-gsub("st lucie", "st. lucie", fe$county)

fe$county<-gsub("st joseph", "st. joseph", fe$county)
fe$county<-gsub("st clair", "st. clair", fe$county)

fe$county<-gsub("mccracken", "mc cracken", fe$county)
fe$county<-gsub("mcleod", "mc leod", fe$county)
fe$county<-gsub("acadiana", "acadia", fe$county)
fe$county<-gsub("chicago", "cook", fe$county)
fe$county<-gsub("do�a ana", "dona ana", fe$county)
fe$county<-gsub("leflore", "le flore", fe$county)

fe$county<-ifelse(fe$city=="Sioux City", "woodbury", fe$county)
fe$county<-ifelse(fe$city=="San Antonio", "bexar", fe$county)
fe$county<-ifelse(fe$city=="Hermon", "penobscot", fe$county)
fe$county<-ifelse(fe$city=="Ponca City", "kay", fe$county)
fe$county<-ifelse((fe$city=="Danbury")&(fe$state=="CT"), "fairfield", fe$county)

fe$county<-ifelse((fe$county=="bristol")&(fe$state=="VA"), "washington", fe$county)
fe$county<-ifelse((fe$county=="mclean")&(fe$state=="IL"), "mc lean", fe$county)
fe$county<-ifelse((fe$county=="stafford")&(fe$state=="NH"), "strafford", fe$county)
fe$county<-ifelse((fe$city=="Nashville")&(fe$state=="GA"), "berrien", fe$county)
fe$county<-ifelse((fe$county=="norfolk")&(fe$state=="VA"), "norfolk city", fe$county)
fe$county<-ifelse((fe$city=="Alton")&(fe$state=="IL"), "madison", fe$county)
fe$county<-ifelse((fe$city=="Bloomington")&(fe$state=="IL"), "mc lean", fe$county)
fe$county<-ifelse((fe$city=="Richmond")&(fe$state=="IL"), "mc henry", fe$county)
fe$county<-ifelse((fe$city=="El Cajon")&(fe$state=="CA"), "san diego", fe$county)
fe$county<-ifelse((fe$city=="Hammond")&(fe$state=="IN"), "lake", fe$county)

fe$county<-ifelse((fe$city=="Madison")&(fe$state=="WI"), "dane", fe$county)


################################## MATCH COUNTIES WITH FIPS CROSSWALK
crosswalk<-read.csv("ssa_fips_state_county2017.csv", stringsAsFactors = FALSE)%>%select(state, county, fipscounty)
crosswalk$county<-tolower(crosswalk$county)

### ID non-matches
# z<-(which(!(fe$county%in%crosswalk$county)))
# table(fe[z, "county"])
# cbind(fe[z, "city"], fe[z, "county"], fe[z, "state"])


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


fe$year<-as.numeric(paste("20", substrRight(as.character(fe$date), 2), sep=""))
fe[which(fe$year==2100), "year"]<-2001

fdat<-fe%>%
  dplyr::select(name, age, gender, race, address, zip, city, state, county, year, cause)

fdat$cause<-ifelse(fdat$cause == "Asph", "Asphyxiated/Restrained", 
             ifelse(fdat$cause == "Asphyxiated/restrained", "Asphyxiated/Restrained",
                    fdat$cause))

#### create county index, then join. a straight join was producing duplicates
c.index<-left_join(fdat%>%select(county, state), crosswalk)%>%rename(fips=fipscounty)%>%distinct()
fdat<-left_join(fdat, c.index)

### manually remove duplicated case
fdat<-fdat%>%filter(!(name=="Robert McAfee Jr"))

### prep for surname matching
names.tmp<-fdat%>%mutate(county=substrRight(fips,3))
#### extract surnames
names.tmp<-names.tmp%>%
  extract(name, c("first.name", "surname"), "([^ ]+) (.*)")
### remove jr, II, III, Sr. numerics are all for john does
names.tmp$surname<-ifelse(grepl("jr.", tolower(names.tmp$surname)), substr(names.tmp$surname, 1, nchar(names.tmp$surname)-4), names.tmp$surname) 
names.tmp$surname<-ifelse(grepl("jr", tolower(names.tmp$surname)), substr(names.tmp$surname, 1, nchar(names.tmp$surname)-3), names.tmp$surname) 
names.tmp$surname<-ifelse(grepl("sr.", tolower(names.tmp$surname)), substr(names.tmp$surname, 1, nchar(names.tmp$surname)-4), names.tmp$surname) 
names.tmp$surname<-ifelse(grepl("II", names.tmp$surname), substr(names.tmp$surname, 1, nchar(names.tmp$surname)-3), names.tmp$surname) 
names.tmp$surname<-ifelse(grepl("III", names.tmp$surname), substr(names.tmp$surname, 1, nchar(names.tmp$surname)-4), names.tmp$surname) 
names.tmp$surname<-ifelse(grepl("IV", names.tmp$surname), substr(names.tmp$surname, 1, nchar(names.tmp$surname)-3), names.tmp$surname) 

#### put missing in for missing names

### remove trailing whitespace
names.tmp$surname<-trimws(names.tmp$surname, which = c("both"))
#### pull only final word
tmp<-names.tmp%>%mutate(surname=word(surname, -1))

missing<-c("\"\"Kenny\"\"", "1", "2", "police")

tmp$surname<-ifelse(tmp$surname%in%missing, NA, names.tmp$surname)
tmp$sex<-ifelse(tmp$gender=="Male", 0, ifelse(tmp$gender=="Female", 1, NA))

name_out<-predict_race(voter.file=tmp%>%filter(race=="Race unspecified"), census.geo="county", 
                       census.key="518b6e66ffa1857a9e4ffd5d894f2934bb06c045")

### vis for accuracy
### performance is weak for af am, asian am, strong for latino, white, as we might expect
### set a cut off for assignment at 75 percent, will leave unknown otherwise
name_tmp1<-name_out%>%filter(race=="Race unspecified")
name_tmp1$race<-ifelse(name_tmp1$pred.whi>0.75, "European-American/White", name_tmp1$race)
name_tmp1$race<-ifelse(name_tmp1$pred.bla>0.75, "African-American/Black", name_tmp1$race)
name_tmp1$race<-ifelse(name_tmp1$pred.his>0.75, "Hispanic/Latino", name_tmp1$race)
name_tmp1$race<-ifelse(name_tmp1$pred.asi>0.75, "Asian/Pacific Islander", name_tmp1$race)

name_tmp1<-name_tmp1%>%select(-county, -sex, -pred.whi, -pred.bla, -pred.his, -pred.asi, -pred.oth, -first.name, -surname)
### use this data frame to sub in for missing race in original file

tmp<-bind_rows(fdat%>%filter(!(race=="Race unspecified"))%>%
                 select(-name), 
               name_tmp1)%>%
  filter(year>2012)

write.csv(tmp, "fe-clean-cause.csv", row.names = FALSE)
write.csv(fdat, "fe-noimp-cause.csv", row.names=FALSE)

######################## make a file with complete case states
### complete states per FE
### http://www.fatalencounters.org/people-search/#namesearch

complete_states<-c("AL", "AK", "AZ", "AR", "CA", "CO",
                   "CT", "DE", "DC", "FL", "GA", "HI",
                   "IA", "ID", "IL", "KS", "KY", "LA",
                   "ME", "MA", "MN", "MS", "MO", "MT",
                   "NE", "NV", "NH", "NM", "NY", "NC", 
                   "ND", "OH", "OK", "OR", "RI", "SC",
                   "SD", "TX", "UT", "VT", "WA", "WY")

tmp<-bind_rows(fdat%>%filter(!(race=="Race unspecified"))%>%
                 select(-name), 
               name_tmp1)%>%
  filter((state%in%complete_states)|(year>2012))%>%
  filter(year!=2017)

### do a ts plot for complete states to see if it looks right
ggplot(tmp%>%
         filter(state%in%complete_states)%>% 
         group_by(state, year)%>%
         summarise(deaths=n()), 
       aes(x=year, y=deaths^(1/4))) + 
  geom_line() +
  facet_wrap(~state)

write.csv(tmp, "fe-complete-states.csv", row.names=FALSE)
  