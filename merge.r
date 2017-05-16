rm(list=ls())
library(dplyr)
library(ggplot2)
library(date)
library(lubridate)
library(data.table)

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

fe$county<-ifelse(fe$city=="Sioux City", "woodbury", fe$county)
fe$county<-ifelse(fe$city=="San Antonio", "bexar", fe$county)
fe$county<-ifelse(fe$city=="Hermon", "penobscot", fe$county)
fe$county<-ifelse(fe$city=="Ponca City", "kay", fe$county)
fe$county<-ifelse((fe$city=="Danbury")&(fe$state=="CT"), "fairfield", fe$county)


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


fe$year<-as.numeric(substrRight(as.character(fe$date), 4))
fe[which(fe$year==2100), "year"]<-2001

fdat<-fe%>%select(name, age, gender, race, city, state, county, year)
fdat<-fdat%>%filter(year>2012)

fdat<-left_join(fdat, crosswalk)%>%rename(fips=fipscounty)