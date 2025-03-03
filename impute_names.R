rm(list=ls())
library(tidyverse)
library(ggplot2)
library(date)
library(lubridate)
library(stringr)
library(wru)
library(jsonlite)

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

#### errors on coordinates
##### SOME FAILED MATCHES
# z<-which(block_map$FIPS_block=="OK")
# block_map[z,]
# ids<-fe_new[z,"id"]
fe_new<-fe_new%>%
  mutate(Latitude = ifelse((id==11840)&(Latitude<1), 
                           Latitude + 47, 
                           ifelse((id == 11921)&(Latitude<1),
                                  Latitude + 41,
                                  ifelse((id == 12963)&(Latitude<1),
                                         Latitude + 38,
                                         ifelse((id==14073)&(Latitude<1),
                                                Latitude + 37,
                                                Latitude)))),
         Latitude = ifelse(id==20157, 32.7398191, Latitude),
         Longitude = ifelse(id==20157, -97.4412267, Longitude))

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
  for(i in 1:nrow(coords)){
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

fe_new<-bind_cols(fe_new, block_map)
#### matches just fine, difference in lat/long variables are just rounding

#### get clean surnames, do I want to deal with hyphenateds?
fe_new<-fe_new%>%
  mutate(name_mod = str_replace(fe_new$name, '[\"].*[\"]', ""))
fe_new<-fe_new%>%
  mutate(name_mod = ifelse(grepl("Name withheld", fe_new$name_mod),
                           NA, fe_new$name_mod))
fe_new<-fe_new%>%
         mutate(name_mod = ifelse(grepl("Jane Doe", fe_new$name_mod),
                           NA, fe_new$name_mod))
fe_new<-fe_new%>%
         mutate(name_mod = ifelse(grepl("John Doe", fe_new$name_mod),
                           NA, fe_new$name_mod))

fe_new<-fe_new%>%
         mutate(name_mod = ifelse(grepl("jr.", tolower(name_mod)), substr(name_mod, 1, nchar(name_mod)-4), name_mod),
         name_mod = ifelse(grepl("jr", tolower(name_mod)), substr(name_mod, 1, nchar(name_mod)-3), name_mod),
         name_mod = ifelse(grepl("sr.", tolower(name_mod)), substr(name_mod, 1, nchar(name_mod)-4), name_mod),
         name_mod = ifelse(grepl("II", name_mod), substr(name_mod, 1, nchar(name_mod)-3), name_mod),
         name_mod = ifelse(grepl("III", name_mod), substr(name_mod, 1, nchar(name_mod)-4), name_mod),
         name_mod = ifelse(grepl("IV", name_mod), substr(name_mod, 1, nchar(name_mod)-3), name_mod))

fe_new<-fe_new%>%
         mutate(name_mod = gsub("-" , " ", fe_new$name_mod), #for hyphenateds, take only the last name
         name_mod = trimws(name_mod, which="both"))

fe_new<-fe_new%>%
         mutate(surname = word(name_mod, -1))

fe_new<-fe_new%>%
  rename(state = STname,
         county = FIPS_county)%>%
  mutate(sex = ifelse(gender == "Male", 0,
         ifelse(gender == "Female", 1, NA)),
         age = as.integer(age))

fe_complete<-fe_new%>%
  # filter(!(is.na(surname)),
  #        !(is.na(age)),
  #        !(is.na(sex)))%>%
  select(surname, age, sex, county, state, id, race, FIPS_block)%>%
  mutate(st_fips = substr(FIPS_block, 1, 2),
         county = substr(FIPS_block, 3, 5),
         tract = substr(FIPS_block, 6, 11),
         block = substr(FIPS_block, 12, 15))



# census_dat_county<-get_census_data(key="518b6e66ffa1857a9e4ffd5d894f2934bb06c045",
#                                    states=unique(fe_new$STname),
#                                    age=TRUE, sex=TRUE, census.geo="county")
########################################################
### NAME ONLY
name_only<-predict_race(voter.file=fe_complete, 
                       census.key = "518b6e66ffa1857a9e4ffd5d894f2934bb06c045",
                       surname.only = TRUE)

########################################################
### WITH COUNTY, FULL DATA
county_full<-predict_race(voter.file=fe_complete%>%
                            mutate(age=ifelse(is.na(age), mean(age, na.rm=TRUE), age),
                                   sex=ifelse(is.na(sex),0, sex)),
                       census.key = "518b6e66ffa1857a9e4ffd5d894f2934bb06c045",
                       census.geo = "county",
                       age= TRUE, 
                       sex = TRUE)

########################################################
### WITH TRACT 
tract_full<-predict_race(voter.file=fe_complete%>%
                           mutate(age=ifelse(is.na(age), mean(age, na.rm=TRUE), age),
                                  sex=ifelse(is.na(sex),0, sex)),
                             census.key = "518b6e66ffa1857a9e4ffd5d894f2934bb06c045",
                             census.geo = "tract",
                             age= TRUE,
                             sex = TRUE)

simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels,
             probs=scores[order(scores, decreasing = TRUE)])
}

###########################################################
## Make roc curves
############### name_only
roc_white<-name_only%>%
  filter(race!="Race unspecified")%>%
  mutate(white = race=="European-American/White")%>%
  select(white, pred.whi)
roc_white<-simple_roc(roc_white$white, roc_white$pred.whi)%>%
  mutate(type="Surname only",
         race="white")

roc_black<-name_only%>%
  filter(race!="Race unspecified")%>%
  mutate(black = race=="African-American/Black")%>%
  select(black, pred.bla)
roc_black<-simple_roc(roc_black$black, roc_black$pred.bla)%>%
  mutate(type="Surname only",
         race="black")

roc_hispanic<-name_only%>%
  filter(race!="Race unspecified")%>%
  mutate(hispanic = race=="Hispanic/Latino")%>%
  select(hispanic, pred.his)
roc_hispanic<-simple_roc(roc_hispanic$hispanic, roc_hispanic$pred.his)%>%
  mutate(type="Surname only",
         race="hispanic")

roc_asian<-name_only%>%
  filter(race!="Race unspecified")%>%
  mutate(asian = race=="Asian/Pacific Islander")%>%
  select(asian, pred.asi)
roc_asian<-simple_roc(roc_asian$asian, roc_asian$pred.asi)%>%
  mutate(type="Surname only",
         race="asian")

roc_other<-name_only%>%
  filter(race!="Race unspecified")%>%
  mutate(other = race%in%c("Middle Eastern", "Native American/Alaskan"))%>%
  select(other, pred.oth)
roc_other<-simple_roc(roc_other$other, roc_other$pred.oth)%>%
  mutate(type="Surname only",
         race="other")

roc_out<-rbind(roc_white, roc_black, roc_hispanic, roc_asian, roc_other)
############### county_full
roc_white<-county_full%>%
  filter(race!="Race unspecified")%>%
  mutate(white = race=="European-American/White")%>%
  select(white, pred.whi)
roc_white<-simple_roc(roc_white$white, roc_white$pred.whi)%>%
  mutate(type="Surname + county",
         race="white")

roc_black<-county_full%>%
  filter(race!="Race unspecified")%>%
  mutate(black = race=="African-American/Black")%>%
  select(black, pred.bla)
roc_black<-simple_roc(roc_black$black, roc_black$pred.bla)%>%
  mutate(type="Surname + county",
         race="black")

roc_hispanic<-county_full%>%
  filter(race!="Race unspecified")%>%
  mutate(hispanic = race=="Hispanic/Latino")%>%
  select(hispanic, pred.his)
roc_hispanic<-simple_roc(roc_hispanic$hispanic, roc_hispanic$pred.his)%>%
  mutate(type="Surname + county",
         race="hispanic")

roc_asian<-county_full%>%
  filter(race!="Race unspecified")%>%
  mutate(asian = race=="Asian/Pacific Islander")%>%
  select(asian, pred.asi)
roc_asian<-simple_roc(roc_asian$asian, roc_asian$pred.asi)%>%
  mutate(type="Surname + county",
         race="asian")

roc_other<-county_full%>%
  filter(race!="Race unspecified")%>%
  mutate(other = race%in%c("Middle Eastern", "Native American/Alaskan"))%>%
  select(other, pred.oth)
roc_other<-simple_roc(roc_other$other, roc_other$pred.oth)%>%
  mutate(type="Surname + county",
         race="other")


roc_out<-rbind(roc_out,
               roc_white, roc_black, roc_hispanic, roc_asian, roc_other)
############### tract_full
roc_white<-tract_full%>%
  filter(race!="Race unspecified")%>%
  mutate(white = race=="European-American/White")%>%
  select(white, pred.whi)
roc_white<-simple_roc(roc_white$white, roc_white$pred.whi)%>%
  mutate(type="Surname + tract",
         race="white")

roc_black<-tract_full%>%
  filter(race!="Race unspecified")%>%
  mutate(black = race=="African-American/Black")%>%
  select(black, pred.bla)
roc_black<-simple_roc(roc_black$black, roc_black$pred.bla)%>%
  mutate(type="Surname + tract",
         race="black")

roc_hispanic<-tract_full%>%
  filter(race!="Race unspecified")%>%
  mutate(hispanic = race=="Hispanic/Latino")%>%
  select(hispanic, pred.his)
roc_hispanic<-simple_roc(roc_hispanic$hispanic, roc_hispanic$pred.his)%>%
  mutate(type="Surname + tract",
         race="hispanic")

roc_asian<-tract_full%>%
  filter(race!="Race unspecified")%>%
  mutate(asian = race=="Asian/Pacific Islander")%>%
  select(asian, pred.asi)
roc_asian<-simple_roc(roc_asian$asian, roc_asian$pred.asi)%>%
  mutate(type="Surname + tract",
         race="asian")

roc_other<-tract_full%>%
  filter(race!="Race unspecified")%>%
  mutate(other = race%in%c("Middle Eastern", "Native American/Alaskan"))%>%
  select(other, pred.oth)
roc_other<-simple_roc(roc_other$other, roc_other$pred.oth)%>%
  mutate(type="Surname + tract",
         race="other")

roc_out<-rbind(roc_out,
               roc_white, roc_black, roc_hispanic, roc_asian, roc_other)

race_labels<-list(
  "asian"="Asian",
  "black"="Black",
  "hispanic"="Latino",
  "other"="Other",
  "white"="White"
)

race_labeller<-function(variable, value){
  return(race_labels[value])
}

ggplot(roc_out, 
       aes(x=FPR, y=TPR, col=type))+
  geom_line()+
  theme_bw()+
  geom_abline(intercept=0,slope=1)+
  facet_wrap(~race, labeller = race_labeller)+
  ylab("True positive rate")+
  xlab("False positive rate")+
  geom_vline(xintercept = 0.05, lty=2)+
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels=c(0, 0.25, 0.5, 0.75, 1))+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())+
  ggsave("./visuals/surname_roc.png", width=6.5, height=6.5)

##### predicted vs observed at 0.8 acceptance threshold
##### predicted vs observed at 0.9 acceptance threshold
#### ROC CURVES
#### make acceptance threshold sequence
#### loop over county, tract
#### loop over thresholds
save.image("name.RData")

#### check tpr for fpr thresholds
#roc_hispanic[min(which(roc_hispanic$FPR>=.1)),]

thresholds_05FP<-data.frame("whi"=roc_white[max(which(roc_white$FPR<=.05)),"probs"], 
                       "bla"=roc_black[max(which(roc_black$FPR<=.05)),"probs"], 
                       "his"=roc_hispanic[max(which(roc_hispanic$FPR<=.05)),"probs"],
                       "asi"=roc_asian[max(which(roc_asian$FPR<=.005)),"probs"],
                       "oth"=roc_other[max(which(roc_other$FPR<=.005)),"probs"])

tract_preds<-tract_full%>%
  mutate(pred.whi = ifelse(is.na(pred.whi), 0, pred.whi),
         pred.bla = ifelse(is.na(pred.bla), 0, pred.bla),
         pred.his = ifelse(is.na(pred.his), 0, pred.his),
         pred.asi = ifelse(is.na(pred.asi), 0, pred.asi),
         pred.oth = ifelse(is.na(pred.oth), 0, pred.oth))

tract_preds<-tract_preds%>%
  mutate(pred.whi.05 = pred.whi>thresholds_05FP$whi,
         pred.bla.05 = pred.bla>thresholds_05FP$bla,
         pred.his.05 = pred.his>thresholds_05FP$his,
         pred.asi.05 = pred.asi>thresholds_05FP$asi,
         pred.oth.05 = pred.oth>thresholds_05FP$oth)

assign_race<-function(x, thresholds){ # return predicted race for missing cases
  x$pred.race<-NA
  index<-which(names(x)%in%(c("pred.whi", "pred.oth")))
  for(i in 1:nrow(x)){
  if(x$pred.whi.05[i] + x$pred.bla.05[i] + x$pred.his.05[i] + x$pred.asi.05[i] + x$pred.oth.05[i] <=1){
    if(x$pred.whi.05[i]) x$pred.race[i]<-"white"
    if(x$pred.bla.05[i]) x$pred.race[i]<-"black"
    if(x$pred.his.05[i]) x$pred.race[i]<-"latino"
    if(x$pred.asi.05[i]) x$pred.race[i]<-"asian"
    if(x$pred.oth.05[i]) x$pred.race[i]<-"other"
  } 
  else{ 
      max<-names(x[index[1]:index[2]])[which(x[i, index[1]:index[2]] == max(x[i, index[1]:index[2]]))]
      if(max == "pred.whi" & x$pred.whi.05[i]) x$pred.race[i]<-"white"
      if(max == "pred.bla" & x$pred.bla.05[i]) x$pred.race[i]<-"black"
      if(max == "pred.his" & x$pred.his.05[i]) x$pred.race[i]<-"latino"
      if(max == "pred.asi" & x$pred.asi.05[i]) x$pred.race[i]<-"asian"
      if(max == "pred.oth" & x$pred.asi.05[i]) x$pred.race[i]<-"other"
    }
  }
  return(x)
}

tract_preds<-assign_race(tract_preds)
tract_preds<-tract_preds%>%
  mutate(fips = paste(st_fips, county, sep=""))%>%
  select(fips, id, pred.race)



#### then join this with full data - need to get FIPS codes and imputed race for final data. should also include disposition, etc for criteria test


write.csv(tract_preds, "./data/predicted_race_05FalsePos.csv", row.names = FALSE)
