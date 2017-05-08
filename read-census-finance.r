rm(list=ls())
# setwd("C:/Users/kilgore/Dropbox/CensusGovFinanceData/_IndFin_1967-2012/")
setwd("~/sync/data-police-shootings/finance-viol")


library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lme4)
library(stringdist)
library(fuzzyjoin)
# 
# infl<-read.csv("U:/cpi-us/data/cpiai.csv")
# 
# infl$year<-as.numeric(substr(infl$Dat, 1,4))
# infl$mont<-substr(infl$Dat, 6, 7)
# ##TOSS MONTHLY DATA, KEEP ONLY JANUARY
# infl<-infl[infl$mont=="01",]
# 
# ### MAKE THIS VECTORIZED IT IS SLOW AS SHIT
# inflate<-function(x, money){
#   total <-nrow(x)
#   # create progress bar
#   pb <- txtProgressBar(min = 0, max = total, style = 3)
#   base<-infl[infl$year==2014, "Index"]
#   for(i in 1:nrow(x)){
#     y.new<-as.numeric(x[i, "year"])
#     z<-which(infl$year==y.new)
#     new<-infl[z,"Index"]
#     adj<-(base-new)/new+1
#     for(s in 1:length(money)){
#       x[i,money[s]]<-as.numeric(x[i,money[s]])*adj
#     }
#     setTxtProgressBar(pb, i)
#   }
#   return(x)
# }
# 
# ### Rev data
# ##write read loop - write data out for quicker access
# years<-as.character(c(87:99, 00:12))
# for(y in 1:length(years)){
#   if(nchar(years[y])==1){
#     years[y]<-paste("0", years[y], sep="")}
# }
# 
# fin.data<-list()
# 
# for(y in 1:length(years)){
#   fin.data[[y]]<-as.data.frame(fread(paste("IndFin", years[y], "a.Txt", sep=""), sep=","))
# }
# 
# setwd("U:/CoG-finance/")
# 
# dat1<-do.call(rbind, fin.data)
# 
# fine.govs<-dat1%>%
#   # filter(`Type Code` %in% c(0,1,2,3))%>%
#   # #filter(Population>1000)%>%
#   # filter(`Total Revenue`>0)%>%
#   select(ID, Year4, `FIPS Code-State`, County, Name, Population, 
#          `Type Code`, `Fines and Forfeits`, `Total Revenue`, `Total Rev-Own Sources`)
# 
# ##remove community college districts with population "."
# fine.govs<-fine.govs[-(which(fine.govs$Population==".")),]
# 
# names(fine.govs)<-c("ID", "year", "FIPS", "County", "Name", "pop", "type", "fines.forfeits", "tot.rev", "tot.rev.own")
# 
# fine.govs[,6]<-as.numeric(fine.govs[,6])
# 
# fine.govs<-inflate(fine.govs, c("fines.forfeits", "tot.rev", "tot.rev.own"))
# 
# write.csv(fine.govs, "fine-govs-inflated2014.csv", row.names=FALSE)

fine.govs<-read.csv("fine-govs-inflated2014.csv", sep=",", nrows=10)

shootings<-read.csv("~/sync/data-police-shootings/fdat.csv", stringsAsFactors = FALSE)
shootings$county<-toupper(shootings$county)
shootings$age<-as.numeric(shootings$age)

s.cnty<-shootings%>%group_by(state, county, year, race)%>%summarise(total=n())

# st.ts<-ggplot(s.state, aes(x=year, y=total))+geom_line()+facet_wrap(~state)

crosswalk<-read.csv("ssa_fips_state_county2017.csv")%>%select(state, county, fipscounty)

merge<-left_join(s.cnty,crosswalk, by=c("state" , "county"))
merge<-merge%>%rename(FIPS=fipscounty)

dem<-read.csv("~/sync/ncands-csv/ncands-fc-merge.csv", stringsAsFactors=FALSE)%>%
  filter(year==2012)%>%select(-year, -county)

dem$state<-dem$stname

dat<-left_join(merge, dem)

dat<-dat%>%mutate(pctblk=(adult.blk+child.blk)/(adult+child), pov.rt=child.pov/child,
                  arrest.viol=viol.all/adult, murder.rt=MURDER_mav/adult, 
                  year.c=year-2000)%>%
  filter(!is.na(adult))

#### scratch
### full pooling
m.p.0<-glm(total~scale(arrest.viol)+scale(pov.rt)+scale(murder.rt)+
             scale(pctblk), data=dat, family=poisson, offset=log(adult))

### partial

m.ml.0<-glmer(total~scale(arrest.viol)+scale(pov.rt)+scale(murder.rt)+
            scale(pctblk)+scale(pop.density)+
              year.c+(1|FIPS)+(1|state), 
            data=dat, family=poisson, offset=log(adult))

##### need to do some kind of fuzzy matching to make this work completely

dat<-dat%>%
  mutate(rev.fines.tot.own=fines.forfeits/tot.rev.own,
         fine.percap=(fines.forfeits)*1000/pop)
dat<-as.data.frame(dat)
### drop post 2000 non-census years
dat<-dat%>%filter(year%in%c(2002, 2007, 2012))
dat$fipscounty<-paste(dat$FIPS, dat$County, sep="")
fine.sum<-dat%>%group_by(fipscounty, year)%>%summarise(fines.forfeits=sum(fines.forfeits))

### get full fips measure

### DO EDA ON STATE MISSINGNESS FOR YEARS PRIOR TO 1992

# test<-fin.data[[4]]
# 
# dat12<-as.data.frame(fread("IndFin12a.Txt", sep=","))
# dat07<-as.data.frame(fread("IndFin07a.Txt", sep=","))
# dat02<-as.data.frame(fread("IndFin02a.Txt", sep=","))
# dat97<-as.data.frame(fread("IndFin97a.Txt", sep=","))
# dat92<-as.data.frame(fread("IndFin92a.Txt", sep=","))
# dat87<-as.data.frame(fread("IndFin87a.Txt", sep=","))
# dat82<-as.data.frame(fread("IndFin82a.Txt", sep=","))

###Expense data

# dat1<-bind_rows(list(dat12, dat07, dat02, dat97, dat92, dat87, dat82))


# ### UCR FUNCTIONS
# read.ucr<-function(x, year){
#   if(year%in%c(2000,2001,2002,2003,2004,2005,2006,2007,2008)){
#     out<-read.fwf(x, widths=c(4,1,1,4,2,3,8,8,3,3,8,6,6,4,4,5,5,6,6,6,4),
#                   col.names=c("STUDYNO", "EDITION", "PART", "IDNO", "FIPS_ST", "FIPS_CTY", "CPOPARST", 
#                               "CPOCRIM","AG_ARRST", "AG_OFF", "COVIND", "INDEX",
#                               "MODINDX", "MURDER", "RAPE", "ROBBERY",
#                               "AGASLT","BURGLRY", "LARCENY","MVTHEFT", "ARSON"))
#   }
#   if(year%in%c(2009,2010,2011)){
#     out<-read.fwf(x, widths=c(4,1,1,4,2,3,8,8,3,3,8,6,6,4,4,5,5,6,6,6,4),
#                   col.names=c("STUDYNO", "EDITION", "PART", "IDNO", "FIPS_ST", "FIPS_CTY", "CPOPARST", 
#                               "CPOCRIM","AG_ARRST", "AG_OFF", "COVIND", "VIOL","PROP",
#                               "MURDER", "RAPE", "ROBBERY",
#                               "AGASLT","BURGLRY", "LARCENY","MVTHEFT", "ARSON"))
#     out$INDEX<-out$VIOL+out$PROP
#   }
#   
#   out$year<-year
#   crime<-crimestate(out)
#   return(crime)
# }


### FOR ALL STATE, COUNTY, MUNI GOVS, exclude special districts
### TYPE CODE: 0-state, 1-county, 2-muni, 3-township


### Do visuals on TS for all munis, all counties, big munis, big counties
### ALL CITIES WITH 0 FINE REV - IS THIS MISSING OR RweaveLatex

fine.govs.cities<-fine.govs%>%filter(`Type Code`==2)
fine.govs.cities<-inflate(fine.govs.cities, money="fine.percap")

percap.ts<-ggplot(data=fine.govs, aes(x=year, y=fines.forfeits/tot.rev))+geom_line()

f.m.c<-fine.govs.cities%>%group_by(year)%>%summarise(avg=mean(fine.percap))

fine.govs.states<-fine.govs%>%filter(`Type Code`==0)
fine.govs.states<-inflate(fine.govs.states, money="fine.percap")
percap.st.ts<-ggplot(data=fine.govs.states, aes(x=Year4, 
  y=fine.percap))+geom_line()+facet_wrap(~Name)+ggtitle("State govt alone")

fine.govs.sanctions<-fine.govs.states%>%filter(Name%in%c("TEXAS STATE GOVT", "WASHINGTON STATE GOVT", 
                                                  "ILLINOIS STATE GOVT", "CALIFORNIA STATE GOVT", "MINNESOTA STATE GOVT", 
                                                  "GEORGIA STATE GOVT", "MISSOURI STATE GOVT", "NEW YORK STATE GOVT", "NORTH CAROLINA STATE GOVT"))

percap.st.ts<-ggplot(data=fine.govs.sanctions, aes(x=Year4, 
                                                y=fine.percap))+geom_line()+facet_wrap(~Name)+ggtitle("State govt alone")

# pdf(file="state-percap-ts", width=12, height=12)
# percap.st.ts
# dev.off()

# pdf(file="state-pctfine-ts", width=12, height=12)
# pctfine.ts<-ggplot(data=fine.govs.states, 
#   aes(x=Year4, y=fine.percap))+geom_line()+facet_wrap(~Name)+
#   ggtitle("Fines as percent of all revenues")
# pctfine.ts
# dev.off()

fine.govs.counties<-fine.govs%>%filter(`Type Code`==1)%>%filter(Name=="TRAVIS COUNTY")
fine.govs.counties<-inflate(fine.govs.counties, money="fine.percap")
f.m.county<-fine.govs.counties%>%group_by(year)%>%summarise(avg=mean(fine.percap))

# pdf("county-ts")
# plot(f.m.county$year, f.m.county$avg, type="l", xlab="Year",
#   ylab="Fines per cap", main="Average county fines per capita")
# dev.off()

# pdf("city-ts")
# plot(f.m.c$year, f.m.c$avg, type="l", xlab="Year",
#   ylab="Fines per cap", main="Average city fines per capita")
# dev.off()

### CREATE ALL GOVS STATE TOTALS FOR MUNI, COUNTY, StatEcdf
st.tot<-fine.govs%>%
  group_by(`FIPS Code-State`, year)%>%
  summarise(tot.rev=sum(`Total Revenue`), tot.fine=sum(`Fines and Forfeits`), 
            tot.cor=sum(`Correct-Total Exp`),pop=max(Population))%>%
  mutate(rev.fines.tot=tot.fine/tot.rev,
         fine.percap=tot.fine/pop*1000, cor.percap=tot.cor/pop*1000)

st.tot<-inflate(st.tot, money=c("rev.fines.tot", "fine.percap", "cor.percap"))
names(st.tot)[1]<-"fips"
st.tot<-as.data.frame(st.tot)

# ### simple lme model 
# ### hypothesis is on time ordering. for now, get demographics and crime data in
# st.tot$year.c<-st.tot$year-1982
# m1<-lmer(scale(fine.percap)~scale(cor.percap)+(year.c)+(year.c|fips), data=st.tot)
# # st.tot<-st.tot[-which(st.tot$fips==11),]

p1<-ggplot(data=st.tot, aes(x=year, y=fine.percap))+geom_line()+facet_wrap(~fips)+ggtitle("State + County + Muni")

# 
# pdf("stateall", width=24, height=12)
# grid.arrange(percap.st.ts, p1, ncol=2)
# dev.off()

## city sandbox for TS checks
#z<-which(fine.govs$Name%in%c("CHICAGO CITY", "NEW YORK CITY", "LOS ANGELES CITY", "SEATTLE CITY"))
z<-which(fine.govs$Name%in%"NEW ORLEANS CITY")
city.ts<-fine.govs[z,]

percap.ts<-ggplot(data=city.ts, aes(x=Year4, y=fine.percap))+geom_line()

ggsave("NOLAfines.pdf", percap.ts)
write.csv(city.ts, "NOLAfines.csv", row.names=FALSE)