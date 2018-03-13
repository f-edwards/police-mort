rm(list=ls())
gc()
library(tidyverse)
library(rstanarm)
library(xtable)

#setwd("~/Projects/police-mort")
load("division_ur_models.RData")

########################################################################################################################
### total homicide: read/transform data x race, x ur, x division

#### CDC Wonder homicide data
# files produced using https://wonder.cdc.gov detailed mortality table generator on 3/12/18
# query: group by division, race, ethnicity 
# filter age>=18, sex=Male, injury intent, homicide, all causes
# one file for each 2013 UR code
# NOTE THAT SOME DATA ARE SUPPRESSED IN X RACE X ETHNICITY X DIVISION X UR NUMBERS (esp rural)
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

#### add UR and division totals
total_ur<-total_homicide%>%
  group_by(ur.code, race)%>%
  summarise(total.homicides = sum(total.homicides))%>%
  mutate(division = "TOTAL")

total_div<-total_homicide%>%
  group_by(division, race)%>%
  summarise(total.homicides = sum(total.homicides))%>%
  mutate(ur.code = "TOTAL")

total_homicide<-total_homicide%>%
  bind_rows(total_ur)%>%
  bind_rows(total_div)
  
### bind total x division/ur, x ur, x division 

homicide<-homicide%>%
  bind_rows(total_homicide)

### fill in suppressed values as NAs
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
d.homicide<-tmp2%>%
  group_by(ur.code, division)%>%
  summarise(d.black = sum(d.black) * (365/2234), # convert to single year average for comparability, ratio
            d.latino = sum(d.latino) * (365/2234),
            d.white = sum(d.white) * (365/2234),
            d.total = sum(d.total) * (365/2234),
            black.men = sum(black.men),
            white.men = sum(white.men),
            latino.men = sum(latino.men),
            tot.men = sum(tot.men))%>%
  ungroup()

homicide_join<-homicide%>%
  left_join(d.homicide)%>%
  filter(division != "TOTAL" & ur.code!="TOTAL")

homicide_plot_dat<-homicide_join%>%
  mutate(Black = d.black,
         Latino = d.latino,
         White = d.white,
         Total = d.total)%>%
  select(division, ur.code, Black, Latino, White, Total)%>%
  gather(key = race, value = pol.deaths, -division, -ur.code)%>%
  left_join(homicide_join%>%
              mutate(Black =black.men,
                     Latino = latino.men,
                     White = white.men,
                     Total = tot.men)%>%
              select(division, ur.code, Black, Latino, White, Total)%>%
              gather(key = race, value = adult.men, -division, -ur.code))%>%
  left_join(homicide_join%>%
              mutate(Black = homicide_black,
                     Latino = homicide_latino,
                     White = homicide_white,
                     Total = homicide_total)%>%
              select(division, ur.code, Black, Latino, White, Total)%>%
              gather(key = race, value = homicide, -division, -ur.code))%>%
  mutate(`Homicide rate` = homicide / adult.men * 100000,
         `Police homicide rate` = pol.deaths / adult.men * 100000,
         Ratio = pol.deaths / homicide)

### create totals x UR, x Div, x Race
race_totals<-homicide%>%
  filter(division == "TOTAL")%>%
  summarise(homicide_black = sum(homicide_black),
            homicide_white = sum(homicide_white),
            homicide_latino = sum(homicide_latino),
            homicide_total = sum(homicide_total))%>%
  bind_cols(tmp2%>%
              summarise(d.black = sum(d.black) * (365/2234), # convert to single year average for comparability, ratio
                        d.latino = sum(d.latino) * (365/2234),
                        d.white = sum(d.white) * (365/2234),
                        d.total = sum(d.total) * (365/2234),
                        black.men = sum(black.men),
                        white.men = sum(white.men),
                        latino.men = sum(latino.men),
                        tot.men = sum(tot.men)))

ur_totals<-homicide%>%
  filter(division == "TOTAL")%>%
  select(-division)%>%
  left_join(tmp2%>%
              group_by(ur.code)%>%
              summarise(d.black = sum(d.black) * (365/2234), # convert to single year average for comparability, ratio
                        d.latino = sum(d.latino) * (365/2234),
                        d.white = sum(d.white) * (365/2234),
                        d.total = sum(d.total) * (365/2234),
                        black.men = sum(black.men),
                        white.men = sum(white.men),
                        latino.men = sum(latino.men),
                        tot.men = sum(tot.men)))

div_totals<-homicide%>%
  filter(ur.code == "TOTAL")%>%
  select(-ur.code)%>%
  left_join(tmp2%>%
              group_by(division)%>%
              summarise(d.black = sum(d.black) * (365/2234), # convert to single year average for comparability, ratio
                        d.latino = sum(d.latino) * (365/2234),
                        d.white = sum(d.white) * (365/2234),
                        d.total = sum(d.total) * (365/2234),
                        black.men = sum(black.men),
                        white.men = sum(white.men),
                        latino.men = sum(latino.men),
                        tot.men = sum(tot.men)))

########################################################################################################################
### police homicide / total homicide visuals, tables

hom_ratio_plot<-ggplot(homicide_plot_dat, 
                      aes(x = Ratio, y = ur.code, col=race))+
  geom_point() + 
  facet_wrap(~division)+
  xlab("Proportion of total homicides committed by police")+
  ylab("")

hom_ratio_plot
  
ggsave("./visuals/hom_ratio.tiff",
       hom_ratio_plot,
       width = 6.5,
       height = 6.5)
  
print.xtable(xtable(homicide_plot_dat), type="html", file = "./visuals/homicide_ratio_table.html")