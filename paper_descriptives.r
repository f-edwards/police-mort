rm(list=ls())
library(tidyverse)
library(rstanarm)
library(xtable)

#setwd("~/Projects/police-mort")
load("division_ur_models.RData")

#### read CDC Wonder homicide data
# files produced using https://wonder.cdc.gov detailed mortality table generator on 3/12/18
# query: group by division, race, ethnicity 
# filter age>=18, sex=Male, injury intent, homicide, all causes
# one file for each 2013 UR code

homicide<-read_tsv("./data/homicide_CDCWONDER_LargeCentral.txt")%>%
  filter(!(is.na(`Census Division`)))%>% # removes end of file notes
  mutate(ur.code = "1: large central metro")%>%
  rbind(read_tsv("./data/homicide_CDCWONDER_LargeFringe.txt")%>%
              filter(!(is.na(`Census Division`)))%>% # removes end of file notes
              mutate(ur.code = "2: large fringe metro"))%>%
  rbind(read_tsv("./data/homicide_CDCWONDER_Medium.txt")%>%
              filter(!(is.na(`Census Division`)))%>% # removes end of file notes
              mutate(ur.code = "3: medium metro"))%>%
  rbind(read_tsv("./data/homicide_CDCWONDER_Small.txt")%>%
              filter(!(is.na(`Census Division`)))%>% # removes end of file notes
              mutate(ur.code = "4: small metro"))%>%
  rbind(read_tsv("./data/homicide_CDCWONDER_Micro.txt")%>%
              filter(!(is.na(`Census Division`)))%>% # removes end of file notes
              mutate(ur.code = "5: micropolitan"))%>%
  rbind(read_tsv("./data/homicide_CDCWONDER_NonCore.txt")%>%
              filter(!(is.na(`Census Division`)))%>% # removes end of file notes
              mutate(ur.code = "6: noncore"))
    
### recode into race/ethnicity codes used in main analysis, select needed variables

homicide<-homicide%>%
  mutate(race = ifelse(Race=="Black or African American",
                       "Black", ifelse(Race!="Black or African American" & 
                                         `Hispanic Origin` == "Hispanic or Latino",
                                       "Latino",
                                       ifelse(Race == "White" &
                                                `Hispanic Origin` != "Hispanic or Latino",
                                              "White", 
                                              "Other"))))

homicide<-homicide%>%
  select(`Census Division`, Deaths, ur.code, race)%>%
  rename(division = `Census Division`,
         total.homicides = Deaths)%>%
  mutate(total.homicides = as.numeric(total.homicides))%>%
  group_by(division, ur.code, race)%>%
  summarise(total.homicides = sum(total.homicides, na.rm=TRUE))
  

### make total homicides

homicide<-bind_rows(homicide,
    homicide%>%
    group_by(division, ur.code)%>%
    summarise(total.homicides = sum(total.homicides))%>%
    mutate(race = "Total"))%>%
  ungroup()

### fill in zeroes
homicide<-homicide%>%
  complete(race, nesting(division, ur.code), fill=list(total.homicides = 0))

### match to division names
homicide<-homicide%>%
  mutate(division = substr(division, 13, nchar(division)))

### spread by race
homicide<-homicide%>%
  mutate(total.homicides = total.homicides / 5)%>% ### convert to 5 yr avg
  spread(race, total.homicides,
         sep="_")

### join with police mortality data
