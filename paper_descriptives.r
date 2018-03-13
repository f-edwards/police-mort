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
# NOTE THAT SOME DATA ARE SUPPRESSED IN X RACE X ETHNICITY X DIVISION X UR NUMBERS
# SEPARATE UNSUPPRESSED FILES FOR TOTAL x ALL RACE, X DIVISION, and X UR

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

### match to division names
homicide<-homicide%>%
  mutate(division = substr(`Census Division`, 13, nchar(`Census Division`)))


homicide_ur<-read_tsv("./data/homicide_CDCWONDER_UR.txt")%>%
  filter(!(is.na(Race)))%>%
  filter(!(is.na(`2013 Urbanization Code`)))%>%
  mutate(division = "TOTAL")%>%
  mutate(ur.code = ifelse(`2013 Urbanization Code`==1,
                          "1: large central metro",
                          ifelse(`2013 Urbanization Code`==2,
                                 "2: large fringe metro",
                                 ifelse(`2013 Urbanization Code`==3,
                                        "3: medium metro",
                                        ifelse(`2013 Urbanization Code`==4,
                                               "4: small metro",
                                               ifelse(`2013 Urbanization Code`==5,
                                                      "5: micropolitan",
                                                      ifelse(`2013 Urbanization Code`==6,
                                                             "6: noncore", NA)))))))

homicide_div<-read_tsv("./data/homicide_CDCWONDER_Div.txt")%>%
  filter(!(is.na(Race)))%>%
  filter(!(is.na(`Census Division`)))%>%
  mutate(ur.code = "TOTAL")%>%
  mutate(division = substr(`Census Division`, 13, nchar(`Census Division`)))
  
### read total homicides

total_homicide<-read_tsv("./data/homicide_CDCWONDER_Total.txt")%>%
  filter(!(is.na(`Census Division`)))%>%
  mutate(Race = "total")%>%
  mutate(ur.code = ifelse(`2013 Urbanization Code`==1,
                          "1: large central metro",
                          ifelse(`2013 Urbanization Code`==2,
                                 "2: large fringe metro",
                                 ifelse(`2013 Urbanization Code`==3,
                                        "3: medium metro",
                                        ifelse(`2013 Urbanization Code`==4,
                                               "4: small metro",
                                               ifelse(`2013 Urbanization Code`==5,
                                                      "5: micropolitan",
                                                      ifelse(`2013 Urbanization Code`==6,
                                                      "6: noncore", NA)))))))%>%
  mutate(division = substr(`Census Division`, 13, nchar(`Census Division`)))

homicide<-homicide%>%
  mutate(Deaths = as.integer(Deaths),
         Population = as.integer(Population),
         `Crude Rate` = as.integer(`Crude Rate`))

homicide_ur<-homicide_ur%>%
  mutate(Deaths = as.integer(Deaths),
         Population = as.integer(Population),
         `Crude Rate` = as.integer(`Crude Rate`))

homicide_div<-homicide_div%>%
  mutate(Deaths = as.integer(Deaths),
         Population = as.integer(Population),
         `Crude Rate` = as.integer(`Crude Rate`))

homicide<-homicide%>%
  bind_rows(homicide_ur)%>%
  bind_rows(homicide_div)

### recode into race/ethnicity codes used in main analysis, select needed variables
homicide<-homicide%>%
  mutate(race = ifelse(Race=="Black or African American",
                       "black", ifelse(Race!="Black or African American" & 
                                         `Hispanic Origin` == "Hispanic or Latino",
                                       "latino",
                                       ifelse(Race == "White" &
                                                `Hispanic Origin` != "Hispanic or Latino",
                                              "white", 
                                              "other"))))



homicide<-homicide%>%
  select(division, Deaths, ur.code, race)%>%
  rename(total.homicides = Deaths)%>%
  mutate(total.homicides = as.numeric(total.homicides))%>%
  group_by(division, ur.code, race)%>%
  summarise(total.homicides = sum(total.homicides, na.rm=TRUE))%>%
  ungroup()

total_homicide<-total_homicide%>%
  filter(!(is.na(ur.code)))%>%
  select(division, ur.code, Race, Deaths)%>%
  rename(race = Race,
         total.homicides = Deaths)

homicide<-homicide%>%
  bind_rows(total_homicide)

### fill in zeroes
homicide<-homicide%>%
  complete(race, nesting(division, ur.code), fill=list(total.homicides = NA))

### spread by race
homicide<-homicide%>%
  mutate(total.homicides = total.homicides / 5)%>% ### convert to 5 yr avg
  rename(homicide = race)%>%
  spread(homicide, total.homicides,
         sep="_")%>%
  filter(!(is.na(ur.code)))

### join with police mortality data, tmp2
pol.homicide<-tmp2%>%
  group_by(ur.code, division)%>%
  summarise(pol.black = sum(d.black) * (365/2234), # convert to single year average for comparability, ratio
            pol.latino = sum(d.latino) * (365/2234),
            pol.white = sum(d.white) * (365/2234),
            pol.total = sum(d.total) * (365/2234))%>%
  ungroup()

homicide_ratios<-homicide%>%
  left_join(pol.homicide)%>%
  mutate(black_ratio = pol.black / homicide_black,
         latino_ratio = pol.latino / homicide_latino,
         white_ratio = pol.white / homicide_white,
         total_ratio = pol.total / homicide_total) 




ur.homicide_ratios<-homicide_ratios%>%
  group_by(ur.code)%>%
  summarise(black_ratio = sum(pol.black, na.rm=TRUE) / sum(homicide_black, na.rm=TRUE),
            latino_ratio = sum(pol.latino, na.rm=TRUE) / sum(homicide_latino, na.rm=TRUE),
            white_ratio = sum(pol.white, na.rm=TRUE) / sum(homicide_white, na.rm=TRUE),
            total_ratio = sum(pol.total, na.rm=TRUE) / sum(homicide_total, na.rm=TRUE))
