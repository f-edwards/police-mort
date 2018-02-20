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
### WITH COUNTY, NO AGE, SEX 
county_only<-predict_race(voter.file=fe_complete, 
                       census.key = "518b6e66ffa1857a9e4ffd5d894f2934bb06c045",
                       census.geo = "county")
                       
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
  mutate(type="name_only",
         race="white")

roc_black<-name_only%>%
  filter(race!="Race unspecified")%>%
  mutate(black = race=="African-American/Black")%>%
  select(black, pred.bla)
roc_black<-simple_roc(roc_black$black, roc_black$pred.bla)%>%
  mutate(type="name_only",
         race="black")

roc_hispanic<-name_only%>%
  filter(race!="Race unspecified")%>%
  mutate(hispanic = race=="Hispanic/Latino")%>%
  select(hispanic, pred.his)
roc_hispanic<-simple_roc(roc_hispanic$hispanic, roc_hispanic$pred.his)%>%
  mutate(type="name_only",
         race="hispanic")

roc_asian<-name_only%>%
  filter(race!="Race unspecified")%>%
  mutate(asian = race=="Asian/Pacific Islander")%>%
  select(asian, pred.asi)
roc_asian<-simple_roc(roc_asian$asian, roc_asian$pred.asi)%>%
  mutate(type="name_only",
         race="asian")

roc_other<-name_only%>%
  filter(race!="Race unspecified")%>%
  mutate(other = race%in%c("Middle Eastern", "Native American/Alaskan"))%>%
  select(other, pred.oth)
roc_other<-simple_roc(roc_other$other, roc_other$pred.oth)%>%
  mutate(type="name_only",
         race="other")

roc_out<-rbind(roc_white, roc_black, roc_hispanic, roc_asian, roc_other)
############### county_only
roc_white<-county_only%>%
  filter(race!="Race unspecified")%>%
  mutate(white = race=="European-American/White")%>%
  select(white, pred.whi)
roc_white<-simple_roc(roc_white$white, roc_white$pred.whi)%>%
  mutate(type="county_only",
         race="white")

roc_black<-county_only%>%
  filter(race!="Race unspecified")%>%
  mutate(black = race=="African-American/Black")%>%
  select(black, pred.bla)
roc_black<-simple_roc(roc_black$black, roc_black$pred.bla)%>%
  mutate(type="county_only",
         race="black")

roc_hispanic<-county_only%>%
  filter(race!="Race unspecified")%>%
  mutate(hispanic = race=="Hispanic/Latino")%>%
  select(hispanic, pred.his)
roc_hispanic<-simple_roc(roc_hispanic$hispanic, roc_hispanic$pred.his)%>%
  mutate(type="county_only",
         race="hispanic")


roc_asian<-county_only%>%
  filter(race!="Race unspecified")%>%
  mutate(asian = race=="Asian/Pacific Islander")%>%
  select(asian, pred.asi)
roc_asian<-simple_roc(roc_asian$asian, roc_asian$pred.asi)%>%
  mutate(type="county_only",
         race="asian")

roc_other<-county_only%>%
  filter(race!="Race unspecified")%>%
  mutate(other = race%in%c("Middle Eastern", "Native American/Alaskan"))%>%
  select(other, pred.oth)
roc_other<-simple_roc(roc_other$other, roc_other$pred.oth)%>%
  mutate(type="county_only",
         race="other")

roc_out<-rbind(roc_out,
               roc_white, roc_black, roc_hispanic, roc_asian, roc_other)
############### county_full
roc_white<-county_full%>%
  filter(race!="Race unspecified")%>%
  mutate(white = race=="European-American/White")%>%
  select(white, pred.whi)
roc_white<-simple_roc(roc_white$white, roc_white$pred.whi)%>%
  mutate(type="county_full",
         race="white")

roc_black<-county_full%>%
  filter(race!="Race unspecified")%>%
  mutate(black = race=="African-American/Black")%>%
  select(black, pred.bla)
roc_black<-simple_roc(roc_black$black, roc_black$pred.bla)%>%
  mutate(type="county_full",
         race="black")

roc_hispanic<-county_full%>%
  filter(race!="Race unspecified")%>%
  mutate(hispanic = race=="Hispanic/Latino")%>%
  select(hispanic, pred.his)
roc_hispanic<-simple_roc(roc_hispanic$hispanic, roc_hispanic$pred.his)%>%
  mutate(type="county_full",
         race="hispanic")

roc_asian<-county_full%>%
  filter(race!="Race unspecified")%>%
  mutate(asian = race=="Asian/Pacific Islander")%>%
  select(asian, pred.asi)
roc_asian<-simple_roc(roc_asian$asian, roc_asian$pred.asi)%>%
  mutate(type="county_full",
         race="asian")

roc_other<-county_full%>%
  filter(race!="Race unspecified")%>%
  mutate(other = race%in%c("Middle Eastern", "Native American/Alaskan"))%>%
  select(other, pred.oth)
roc_other<-simple_roc(roc_other$other, roc_other$pred.oth)%>%
  mutate(type="county_full",
         race="other")


roc_out<-rbind(roc_out,
               roc_white, roc_black, roc_hispanic, roc_asian, roc_other)
############### tract_full
roc_white<-tract_full%>%
  filter(race!="Race unspecified")%>%
  mutate(white = race=="European-American/White")%>%
  select(white, pred.whi)
roc_white<-simple_roc(roc_white$white, roc_white$pred.whi)%>%
  mutate(type="tract_full",
         race="white")

roc_black<-tract_full%>%
  filter(race!="Race unspecified")%>%
  mutate(black = race=="African-American/Black")%>%
  select(black, pred.bla)
roc_black<-simple_roc(roc_black$black, roc_black$pred.bla)%>%
  mutate(type="tract_full",
         race="black")

roc_hispanic<-tract_full%>%
  filter(race!="Race unspecified")%>%
  mutate(hispanic = race=="Hispanic/Latino")%>%
  select(hispanic, pred.his)
roc_hispanic<-simple_roc(roc_hispanic$hispanic, roc_hispanic$pred.his)%>%
  mutate(type="tract_full",
         race="hispanic")

roc_asian<-tract_full%>%
  filter(race!="Race unspecified")%>%
  mutate(asian = race=="Asian/Pacific Islander")%>%
  select(asian, pred.asi)
roc_asian<-simple_roc(roc_asian$asian, roc_asian$pred.asi)%>%
  mutate(type="tract_full",
         race="asian")

roc_other<-tract_full%>%
  filter(race!="Race unspecified")%>%
  mutate(other = race%in%c("Middle Eastern", "Native American/Alaskan"))%>%
  select(other, pred.oth)
roc_other<-simple_roc(roc_other$other, roc_other$pred.oth)%>%
  mutate(type="tract_full",
         race="other")

roc_out<-rbind(roc_out,
               roc_white, roc_black, roc_hispanic, roc_asian, roc_other)

ggplot(roc_out, 
       aes(x=FPR, y=TPR, col=type))+
  geom_line()+
  geom_abline(intercept=0,slope=1)+
  facet_wrap(~race)+
  ylab("True positive rate")+
  xlab("False positive rate")+
  ggsave("./visuals/surname_roc.pdf")

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

tract_preds<-assign_race(tract_preds)%>%
  select(id, pred.race)

write.csv(tract_preds, "./data/predicted_race_05FalsePos.csv", row.names = FALSE)




#################################
## PARKING LOT
# 
# 
# #########################################################
# ### MORE MISSING DATA DIAGNOSTICS
# #### check on patterns of missingness on race
# fe_missing<-fe_new%>%
#   mutate(race = ifelse(race=="Race unspecified", NA, race))%>%
#   mutate(age = as.numeric(age),
#          year = as.numeric(year))
# 
# library(mice)
# md.pairs(fe_missing)
# table(is.na(fe_missing$race), fe_missing$loc_state)
# table(is.na(fe_missing$race), fe_missing$cause_of_death)
# 
# #### missing race as regression outcome
# missing_model<-glm(is.na(race)~
#                      as.numeric(year) + as.numeric(age) + gender + loc_state  + cause_of_death,
#                    family="binomial",
#                    data = fe_missing)
# 
# 
# #############PARKING
# #### check imputations against observed
# make_pred_race<-function(x, threshold){
#   x$pred.race<-NA
#   for(i in 1:nrow(x)){
#     cnames<-which(names(x)%in%c("pred.whi", "pred.oth"))
#     max_temp<-max(x[i, cnames[1]:cnames[2]], na.rm=TRUE)
#     index<-which(x[i, cnames[1]:cnames[2]]==max_temp)
#     pred.race<-names(x)[cnames[1]:cnames[2]][index]
#     if(max_temp>threshold){
#       x[i, "pred.race"]<-pred.race}
#   }
#   return(x)
# }
# 
# 
# make_sensitivity<-function(x, geo, threshold){
#   x<-x%>%summarise(true_pos_black=sum(na.rm=TRUE,(race=="African-American/Black")&(pred.race=="pred.bla"))/
#                      sum(na.rm=TRUE,race=="African-American/Black"),
#                    true_pos_hisp=sum(na.rm=TRUE,(race=="Hispanic/Latino")&(pred.race=="pred.his"))/
#                      sum(na.rm=TRUE,race=="Hispanic/Latino"),
#                    true_pos_whi=sum(na.rm=TRUE,(race=="European-American/White")&(pred.race=="pred.whi"))/
#                      sum(na.rm=TRUE,race=="European-American/White"),
#                    false_pos_black=sum(na.rm=TRUE,(race!="African-American/Black")&(pred.race=="pred.bla"))/
#                      sum(na.rm=TRUE,pred.race=="pred.bla"),
#                    false_pos_white=sum(na.rm=TRUE,(race!="European-American/White")&(pred.race=="pred.whi"))/
#                      sum(na.rm=TRUE,pred.race=="pred.whi"),
#                    false_pos_his=sum(na.rm=TRUE,(race!="Hispanic/Latino")&(pred.race=="pred.his"))/
#                      sum(na.rm=TRUE,pred.race=="pred.his"))%>%
#     mutate(geo.level=geo, acceptance_thresh=threshold)
# }
# 
# 
# 
# threshold_class<-function(x, threshold){
#   sens<-x%>%
#     summarise(true_pos_black=sum(na.rm=TRUE,
#                                  (race=="African-American/Black")&(pred.bla>=threshold))/
#                 sum(na.rm=TRUE,race=="African-American/Black"),
#               false_pos_black=sum(na.rm=TRUE,
#                                   (race!="African-American/Black")&(pred.bla>=threshold))/
#                 sum(na.rm=TRUE,pred.bla>=threshold),
#               true_pos_hisp=sum(na.rm=TRUE,
#                                 (race=="Hispanic/Latino")&(pred.his>=threshold))/
#                 sum(na.rm=TRUE,race=="Hispanic/Latino"),
#               false_pos_his=sum(na.rm=TRUE,
#                                 (race!="Hispanic/Latino")&(pred.his>=threshold))/
#                 sum(na.rm=TRUE,pred.his>=threshold),
#               true_pos_whi=sum(na.rm=TRUE,
#                                (race=="European-American/White")&(pred.whi>=threshold))/
#                 sum(na.rm=TRUE,race=="European-American/White"),
#               false_pos_white=sum(na.rm=TRUE,
#                                   (race!="European-American/White")&(pred.whi>=threshold))/
#                 sum(na.rm=TRUE,pred.whi>=threshold))%>%
#     mutate(acceptance_thresh=threshold)
#   return(sens)
# }
# 
# threshold_seq<-seq(from=0.2,to=0.99,by=0.01)
# roc<-t(sapply(threshold_seq, function(x) threshold_class(name_only, x)))
# 
# #
# #roc_white
# 
# # 
# # sensitivity<-bind_rows(
# #   make_sensitivity(make_pred_race(name_out, 0.7), "county", 0.7),
# #   make_sensitivity(make_pred_race(name_out, 0.75), "county", 0.75),
# #   make_sensitivity(make_pred_race(name_out, 0.8), "county",0.8),
# #   make_sensitivity(make_pred_race(name_out, 0.85), "county",0.85),
# #   make_sensitivity(make_pred_race(name_out, 0.9), "county",0.9),
# #   make_sensitivity(make_pred_race(name_out, 0.95), "county",0.95),
# #   make_sensitivity(make_pred_race(name_out_tract, 0.7), "tract", 0.7),
# #   make_sensitivity(make_pred_race(name_out_tract, 0.75), "tract", 0.75),
# #   make_sensitivity(make_pred_race(name_out_tract, 0.8), "tract",0.8),
# #   make_sensitivity(make_pred_race(name_out_tract, 0.85), "tract",0.85),
# #   make_sensitivity(make_pred_race(name_out_tract, 0.9), "tract",0.9),
# #   make_sensitivity(make_pred_race(name_out_tract, 0.95), "tract",0.95)
# # )
# 
# ### want to make long by geo, acceptance, race, sensitivity measure type
# library(xtable)
# print.xtable(xtable(sensitivity), file="./visuals/sensitivity.html", type="html")
# 
# sens_blk<-sensitivity%>%
#   select(true_pos_black, false_pos_black, geo.level, acceptance_thresh)%>%
#   gather(key = race, -geo.level, -acceptance_thresh, value = value)%>%
#   mutate(sens_type = ifelse(grepl("true", race), "true_positive", "false_positive"),
#          race = "Black")
# 
# sens_wht<-sensitivity%>%
#   select(true_pos_whi, false_pos_white, geo.level, acceptance_thresh)%>%
#   gather(key = race, -geo.level, -acceptance_thresh, value = value)%>%
#   mutate(sens_type = ifelse(grepl("true", race), "true_positive", "false_positive"),
#          race = "White")
# 
# sens_hisp<-sensitivity%>%
#   select(true_pos_hisp, false_pos_his, geo.level, acceptance_thresh)%>%
#   gather(key = race, -geo.level, -acceptance_thresh, value = value)%>%
#   mutate(sens_type = ifelse(grepl("true", race), "true_positive", "false_positive"),
#          race = "Latinx")
# 
# sensitivity<-bind_rows(sens_blk, sens_wht, sens_hisp)
# 
# ggplot(sensitivity,
#        aes(x=acceptance_thresh, color = sens_type, linetype=geo.level))+
#   geom_line(aes(y=value))+
#   ggtitle("true and false positives in race imputation, full data")+
#   ylim(0,1)+
#   facet_wrap(~race)+
#   ggsave("./visuals/imputation_sensitivity.pdf")

