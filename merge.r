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
                           NA, fe_new$name_mod),
         name_mod = ifelse(grepl("Jane Doe", fe_new$name_mod),
                           NA, fe_new$name_mod),
         name_mod = ifelse(grepl("John Doe", fe_new$name_mod),
                           NA, fe_new$name_mod),
         name_mod = ifelse(grepl("jr.", tolower(fe_new$name_mod)), substr(fe_new$name_mod, 1, nchar(fe_new$name_mod)-4), fe_new$name_mod),
         name_mod = ifelse(grepl("jr", tolower(fe_new$name_mod)), substr(fe_new$name_mod, 1, nchar(fe_new$name_mod)-3), fe_new$name_mod),
         name_mod = ifelse(grepl("sr.", tolower(fe_new$name_mod)), substr(fe_new$name_mod, 1, nchar(fe_new$name_mod)-4), fe_new$name_mod),
         name_mod = ifelse(grepl("II", fe_new$name_mod), substr(fe_new$name_mod, 1, nchar(fe_new$name_mod)-3), fe_new$name_mod),
         name_mod = ifelse(grepl("III", fe_new$name_mod), substr(fe_new$name_mod, 1, nchar(fe_new$name_mod)-4), fe_new$name_mod),
         name_mod = ifelse(grepl("IV", fe_new$name_mod), substr(fe_new$name_mod, 1, nchar(fe_new$name_mod)-3), fe_new$name_mod),
         name_mod = trimws(name_mod, which="both"),
         surname = word(name_mod, -1))

#### to check imputation algorithm: impute w/counties on missing, impute on full data to check sensitivity on thresholds


#### get block-level data from census api
# census_dat_block<-get_census_data(key="518b6e66ffa1857a9e4ffd5d894f2934bb06c045",
#                           states=unique(coords$STname)[1],
#                           age=TRUE, sex=TRUE, census.geo="block")
# 
# 
# # 



#### remove nicknames (between quotes)
#review<-fe_new[grep('[\"].*[\"]', fe_new$name), "name"]
### remove jr, II, III, Sr. numerics are all for john does

         
fe_new<-fe_new%>%
  rename(state = STname,
         county = FIPS_county)%>%
  mutate(sex = ifelse(gender == "Male", 0,
         ifelse(gender == "Female", 1, NA)),
         age = as.integer(age))

fe_complete<-fe_new%>%
  filter(!(is.na(surname)),
         !(is.na(age)),
         !(is.na(sex)))%>%
  select(surname, age, sex, county, state, id, race, FIPS_block)%>%
  mutate(st_fips = substr(FIPS_block, 1, 2),
         county = substr(FIPS_block, 3, 5),
         tract = substr(FIPS_block, 6, 11),
         block = substr(FIPS_block, 12, 15))


# census_dat_county<-get_census_data(key="518b6e66ffa1857a9e4ffd5d894f2934bb06c045",
#                                    states=unique(fe_new$STname),
#                                    age=TRUE, sex=TRUE, census.geo="county")


########################################################
### WITH COUNTY 
name_out<-predict_race(voter.file=fe_complete, 
                       census.key = "518b6e66ffa1857a9e4ffd5d894f2934bb06c045",
                       census.geo = "county",
                       age= TRUE, 
                       sex = TRUE)

#### check imputations against observed
make_pred_race<-function(x, threshold){
  x$pred.race<-NA
  for(i in 1:nrow(x)){
    cnames<-which(names(x)%in%c("pred.whi", "pred.oth"))
      max_temp<-max(x[i, cnames[1]:cnames[2]], na.rm=TRUE)
    index<-which(x[i, cnames[1]:cnames[2]]==max_temp)
    pred.race<-names(x)[cnames[1]:cnames[2]][index]
    if(max_temp>threshold){
    x[i, "pred.race"]<-pred.race}
  }
  return(x)
}

make_sensitivity<-function(x, geo, threshold){
  x<-x%>%summarise(true_pos_black=sum(na.rm=TRUE,(race=="African-American/Black")&(pred.race=="pred.bla"))/
              sum(na.rm=TRUE,race=="African-American/Black"),
            true_pos_hisp=sum(na.rm=TRUE,(race=="Hispanic/Latino")&(pred.race=="pred.his"))/
              sum(na.rm=TRUE,race=="Hispanic/Latino"),
            true_pos_whi=sum(na.rm=TRUE,(race=="European-American/White")&(pred.race=="pred.whi"))/
              sum(na.rm=TRUE,race=="European-American/White"),
            false_pos_black=sum(na.rm=TRUE,(race!="African-American/Black")&(pred.race=="pred.bla"))/
              sum(na.rm=TRUE,pred.race=="pred.bla"),
            false_pos_white=sum(na.rm=TRUE,(race!="European-American/White")&(pred.race=="pred.whi"))/
              sum(na.rm=TRUE,pred.race=="pred.whi"),
            false_pos_his=sum(na.rm=TRUE,(race!="Hispanic/Latino")&(pred.race=="pred.his"))/
              sum(na.rm=TRUE,pred.race=="pred.his"))%>%
    mutate(geo.level=geo, acceptance_thresh=threshold)
}

##### predicted vs observed at 0.8 acceptance threshold

# ggplot(name_out, aes(x=pred.race))+
#   geom_bar()+
#   ggtitle("predictions at 80 percent acceptance vs observed data")+
#   facet_wrap(~race, scales="free")+
#   ggsave("./visuals/name_county_pred_80.pdf", height = 10, width =10)


##### predicted vs observed at 0.9 acceptance threshold

# ggplot(name_out, aes(x=pred.race))+
#   geom_bar()+
#   ggtitle("predictions at 90 percent acceptancevs observed data")+
#   facet_wrap(~race, scales="free")+
#   ggsave("./visuals/name_county_pred_90.pdf", height = 10, width =10)


########################################################
### WITH TRACT 
name_out_tract<-predict_race(voter.file=fe_complete,
                       census.key = "518b6e66ffa1857a9e4ffd5d894f2934bb06c045",
                       census.geo = "tract",
                       age= TRUE,
                       sex = TRUE)

sensitivity<-bind_rows(
  make_sensitivity(make_pred_race(name_out, 0.7), "county", 0.7),
  make_sensitivity(make_pred_race(name_out, 0.75), "county", 0.75),
  make_sensitivity(make_pred_race(name_out, 0.8), "county",0.8),
  make_sensitivity(make_pred_race(name_out, 0.85), "county",0.85),
  make_sensitivity(make_pred_race(name_out, 0.9), "county",0.9),
  make_sensitivity(make_pred_race(name_out, 0.95), "county",0.95),
  make_sensitivity(make_pred_race(name_out_tract, 0.7), "tract", 0.7),
  make_sensitivity(make_pred_race(name_out_tract, 0.75), "tract", 0.75),
  make_sensitivity(make_pred_race(name_out_tract, 0.8), "tract",0.8),
  make_sensitivity(make_pred_race(name_out_tract, 0.85), "tract",0.85),
  make_sensitivity(make_pred_race(name_out_tract, 0.9), "tract",0.9),
  make_sensitivity(make_pred_race(name_out_tract, 0.95), "tract",0.95)
)

### want to make long by geo, acceptance, race, sensitivity measure type
library(xtable)
print.xtable(xtable(sensitivity), file="./visuals/sensitivity.html", type="html")

sens_blk<-sensitivity%>%
  select(true_pos_black, false_pos_black, geo.level, acceptance_thresh)%>%
  gather(key = race, -geo.level, -acceptance_thresh, value = value)%>%
  mutate(sens_type = ifelse(grepl("true", race), "true_positive", "false_positive"),
         race = "Black")

sens_wht<-sensitivity%>%
  select(true_pos_whi, false_pos_white, geo.level, acceptance_thresh)%>%
  gather(key = race, -geo.level, -acceptance_thresh, value = value)%>%
  mutate(sens_type = ifelse(grepl("true", race), "true_positive", "false_positive"),
         race = "White")

sens_hisp<-sensitivity%>%
  select(true_pos_hisp, false_pos_his, geo.level, acceptance_thresh)%>%
  gather(key = race, -geo.level, -acceptance_thresh, value = value)%>%
  mutate(sens_type = ifelse(grepl("true", race), "true_positive", "false_positive"),
         race = "Latinx")

sensitivity<-bind_rows(sens_blk, sens_wht, sens_hisp)

ggplot(sensitivity,
       aes(x=acceptance_thresh, color = sens_type, linetype=geo.level))+
  geom_line(aes(y=value))+
  ggtitle("true and false positives in race imputation, full data")+
  ylim(0,1)+
  facet_wrap(~race)+
  ggsave("./visuals/imputation_sensitivity.pdf")



save.image("name.RData")
