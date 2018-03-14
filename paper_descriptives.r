rm(list=ls())
gc()
library(tidyverse)
library(rstanarm)
library(xtable)
set.seed(1)

setwd("~/Projects/police-mort")
source("division_ur_visuals.r")

### FE constant for FE counts -> deaths/yr
FE_constant<-(365/2234)

### for table 1

z.01<-tmp2%>%
  group_by(division, ur.code)%>%
  summarise(tot.deaths=sum(d.total))%>%
            #death.rt=sum(d.total)/sum(tot.men) * 100000* FE_constant)%>%
  ungroup()%>%
  spread(ur.code, tot.deaths)

z.02<-tmp2%>%
  group_by(division, ur.code)%>%
  summarise(death.rt=sum(d.total)/sum(tot.men) * 100000* FE_constant)%>%
  ungroup()%>%
  spread(ur.code, death.rt)

write.csv(z.01, "./visuals/table_01_count.csv", row.names=FALSE)
write.csv(z.02, "./visuals/table_01_rate.csv", row.names=FALSE)

z1<-tmp2%>%
  group_by(division)%>%
  summarise(tot.deaths=sum(d.total),
            death.rt=sum(d.total)/sum(tot.men) * 100000* FE_constant)%>%
  ungroup()

z2<-tmp2%>%
  group_by(ur.code)%>%
  summarise(tot.deaths=sum(d.total),
            death.rt=sum(d.total)/sum(tot.men) * 100000* FE_constant)%>%
  ungroup()

write.csv(z1, "./visuals/table_01_divtot.csv", row.names=FALSE)
write.csv(z2, "./visuals/table_01_urtot.csv", row.names=FALSE)
########################################################################################################################
### observed data descriptives
observed<-data.frame(Race = c("Black", "Latino", "White", "Total"),
                     Deaths = c(sum(tmp2$d.black), sum(tmp2$d.latino), sum(tmp2$d.white), sum(tmp2$d.total)),
                     Adult.men = c(sum(tmp2$black.men), sum(tmp2$latino.men), sum(tmp2$white.men), sum(tmp2$tot.men)))

observed<-observed%>%
  mutate(Annual.Rate = Deaths / Adult.men * 100000 * FE_constant)

write.csv(observed, 
          "./visuals/national_observed_rates.csv")

observed_ur_div<-tmp2%>%
  group_by(division, ur.code)%>%
  select(-fips, -state)%>%
  summarise_all(sum)%>%
  mutate(rate.black = FE_constant * d.black / black.men * 100000,
         rate.latino = FE_constant * d.latino / latino.men * 100000,
         rate.white = FE_constant * d.white / white.men * 100000,
         rate.tot = FE_constant * d.total / tot.men * 100000)

write.csv(observed_ur_div,
          "./visuals/observed_ur_div.csv")
  
########################################################################################################################
### posterior estimates, in-text descriptives
# obtain posterior predictions for deaths using observed data
post.blk.obs<-posterior_predict(blk.stan.0, newdata = tmp2)
post.lat.obs<-posterior_predict(lat.stan.0, newdata = tmp2)
post.wht.obs<-posterior_predict(wht.stan.0, newdata = tmp2)
post.tot.obs<-posterior_predict(tot.stan.0, newdata = tmp2)

post.blk.nat<-as.vector(apply(post.blk.obs,1,sum)) 
post.blk.nat<-post.blk.nat/sum(tmp2$black.men) * 100000 * FE_constant
post.blk.quantiles<-quantile(post.blk.nat, c(0.025, 0.5, .975))

post.lat.nat<-as.vector(apply(post.lat.obs,1,sum)) 
post.lat.nat<-post.lat.nat/sum(tmp2$latino.men) * 100000 * FE_constant
post.lat.quantiles<-quantile(post.lat.nat, c(0.025, 0.5, .975))

post.wht.nat<-as.vector(apply(post.wht.obs,1,sum)) 
post.wht.nat<-post.wht.nat/sum(tmp2$white.men) * 100000 * FE_constant
post.wht.quantiles<-quantile(post.wht.nat, c(0.025, 0.5, .975))

post.tot.nat<-as.vector(apply(post.tot.obs,1,sum)) 
post.tot.nat<-post.tot.nat/sum(tmp2$tot.men) * 100000 * FE_constant
post.tot.quantiles<-quantile(post.tot.nat, c(0.025, 0.5, .975))

black.one.year<-quantile(as.vector(apply(post.blk.obs,1,sum)) * FE_constant,
                         c(0.025, 0.5, .975))
latino.one.year<-quantile(as.vector(apply(post.lat.obs,1,sum)) * FE_constant,
                          c(0.025, 0.5, .975))
white.one.year<-quantile(as.vector(apply(post.wht.obs,1,sum)) * FE_constant,
                         c(0.025, 0.5, .975))
total.one.year<-quantile(as.vector(apply(post.tot.obs,1,sum)) * FE_constant,
                         c(0.025, 0.5, .975))


write.csv(rbind(post.blk.quantiles, 
                post.lat.quantiles, 
                post.wht.quantiles, 
                post.tot.quantiles,
                black.one.year,
                latino.one.year,
                white.one.year,
                total.one.year),
          "./visuals/national_post_predict.csv",
          row.names = TRUE)

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

### spread by race, standardize to annual average
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
                        tot.men = sum(tot.men)))%>%
  mutate(ratio_black = d.black / homicide_black,
         ratio_white = d.white / homicide_white,
         ratio_latino = d.latino / homicide_latino,
         ratio_total = d.total / homicide_total)

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
                        tot.men = sum(tot.men)))%>%
  mutate(ratio_black = d.black / homicide_black,
         ratio_white = d.white / homicide_white,
         ratio_latino = d.latino / homicide_latino,
         ratio_total = d.total / homicide_total)

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
                        tot.men = sum(tot.men)))%>%
  mutate(ratio_black = d.black / homicide_black,
         ratio_white = d.white / homicide_white,
         ratio_latino = d.latino / homicide_latino,
         ratio_total = d.total / homicide_total)

########################################################################################################################
### police homicide / total homicide visuals, tables

hom_ratio_plot<-ggplot(homicide_plot_dat, 
                      aes(x = Ratio, y = clean_ur(ur.code), col=factor(race, 
                                                                       levels = 
                                                                         c("Black", "Latino", "White", "Total"))))+
  geom_point(alpha = 0.7, position = position_jitter(width=0, height = 0.15)) + 
  facet_wrap(~division)+
  xlab("Homicides committed by police as proportion of total homicides")+
  ylab("")+
  scale_y_discrete(limits = rev(levels(clean_ur(homicide_plot_dat$ur.code))))+
  theme(legend.title = element_blank())
  
hom_ratio_plot
  
ggsave("./visuals/hom_ratio.tiff",
       hom_ratio_plot,
       width = 6.5,
       height = 6.5)
  
print.xtable(xtable(homicide_plot_dat), 
             type="html", 
             file = "./visuals/homicide_ratio_table.html")

########################################################################################################################
### rates for other groups
homicide_amind<-homicide_ur%>%
  filter(Race == "American Indian or Alaska Native")%>%
  summarise(homicide_amind = sum(Deaths, na.rm=TRUE)/5)

fe_new <- read_csv("./data/fatal-encounters-2-12-18.csv") %>%
  filter(`Date (Year)`>=2012)

# .... make names more user-friendly
names(fe_new)<-c("id", "name", "age", "gender", "race", "URL", "death_date", 
                 "loc_address", "loc_city", "loc_state", "loc_zip", "loc_county", 
                 "loc_full_address", "Latitude", "Longitude", "agency", 
                 "cause_of_death","cause_description", "official_disposition", 
                 "news_url", "mental_illness", "video", "null1", "dateanddesc", 
                 "null2", "id2", "year", "null3")

fe_filtered<-fe_new%>%
  filter(`Cause of death` %in% 
           c('Asphyxiated/Restrained','Beaten/Bludgeoned with instrument', 
             'Chemical agent/Pepper spray', 'Medical emergency', 'Tasered', 
             'Gunshot'))

d.amind<-fe_filtered%>%
  filter(`Subject's race` == "Native American/Alaskan")%>%
  summarise(d.amind = n() * FE_constant)

ratio_amind<-(d.amind)/(homicide_amind); names(ratio_amind)<-"ratio_amind"
### per american fact finder, 2015 pop estimates: 18+ Race alone or in combination
amind.men<-1429537

homicide_api<-homicide_ur%>%
  filter(Race == "Asian or Pacific Islander")%>%
  summarise(homicide_api = sum(Deaths, na.rm=TRUE)/5)

d.api<-fe_filtered%>%
  filter(`Subject's race` == "Asian/Pacific Islander")%>%
  summarise(d.api = n() * FE_constant)

ratio_api<-(d.api)/(homicide_api); names(ratio_api)<-"ratio_api"

api.men<-6645926+276693

nat_data<-cbind(race_totals, homicide_amind, d.amind, 
                ratio_amind, amind.men, homicide_api, 
                d.api, ratio_api, api.men)

nat_data<-nat_data%>%
  mutate(rate_black=d.black / black.men * 100000,
         rate_latino=d.latino / latino.men * 100000,
         rate_white=d.white / white.men * 100000,
         rate_amind=d.amind / amind.men * 100000,
         rate_api=d.api / api.men * 100000,
         rate_total=d.total / tot.men * 100000)

write.csv(nat_data,
          "./visuals/nat_observed.csv")

