#########################
# Fatal Encounters Data
# division main file, .r
# ... revision edits 
# last edit 3/5 (by: ME)
########################

## Set-up
# 1: load packages; setwd etc. 
rm(list=ls())
set.seed(1)

library(tidyverse)
library(rstanarm)
library(parallel)
options(mc.cores = parallel::detectCores())
theme_set(theme_minimal())
setwd("~/Projects/police_mort")
select  = dplyr::select

fe_new<-read_csv("fatal-encounters-2-12-18.csv")
fe_new<-fe_new%>%
  filter(`Date (Year)`>=2012)
names(fe_new)<-c("id", "name", "age", "gender", "race", "URL", "death_date", 
                 "loc_address", "loc_city", "loc_state", "loc_zip", "loc_county", 
                 "loc_full_address", "Latitude", "Longitude", "agency", "cause_of_death",
                 "cause_description", "official_disposition", "news_url", "mental_illness", "video",
                 "null1", "dateanddesc", "null2", "id2", "year", "null3")

fe_new<-fe_new%>%
  select(id, name, age, gender, race, death_date, loc_state, loc_county, Latitude, Longitude,
         agency, cause_of_death, official_disposition, year)
fe_new<-fe_new%>%
  mutate(race = ifelse(race == "African-American/Black", "black", race),
         race = ifelse(race == "Asian/Pacific Islander", "asian", race),
         race = ifelse(race == "European-American/White", "white", race),
         race = ifelse(race == "Hispanic/Latino", "latino", race),
         race = ifelse(race == "Middle Eastern", "other", race),
         race = ifelse(race == "Native American/Alaskan", "other", race),
         race = ifelse(race == "Race unspecified", NA, race))

imputeds<-read_csv("predicted_race_05FalsePos.csv")

fdat <- imputeds %>% left_join(fe_new) # for surname/block imputation

############# for missing race data, replace with imputed, leave missings with classification above .05 false pos threshold missing

fdat<-fdat%>%
  mutate(race = ifelse(is.na(race), pred.race, race))%>%
  mutate(fips = as.numeric(fips))

fdat<-fdat%>%
  mutate(fips = ifelse(fips == 46113, 46102, fips))%>%
  mutate(fips = ifelse(fips == 2270, 2158, fips))

# ... attach CDC urban-rual scheme
cdc = read_fwf(file = 'NCHSURCodes2013.txt', 
			   fwf_positions(c(1, 3, 7, 10, 47, 98, 107, 116, 118, 120), 
					  		 c(2, 5, 8, 45, 96, 105, 114, 116, 118, 120))
		) %>%
	  dplyr::select(X1, X2, X3, X4, X8) %>%
	  rename(s.fips = X1, c.fips = X2, state = X3, county = X4, ur.code = X8) %>%
	  mutate(county = tolower(county),
	  	     ur.code = ifelse(ur.code == 1, '1: large central metro',
	  	     		   ifelse(ur.code == 2, '2: large fringe metro',
	  	     		   ifelse(ur.code == 3, '3: medium metro',
	  	     		   ifelse(ur.code == 4, '4: small metro', 
	  	     		   ifelse(ur.code == 5, '5: micropolitan', 
	  	     		   ifelse(ur.code == 6, '6: noncore', NA))))))) %>%
	  mutate(fips = as.numeric(paste0(s.fips, c.fips))) %>%
	  dplyr::select(fips, ur.code)%>%
  mutate(fips = ifelse(fips == 46113, 46102, fips))%>%
  mutate(fips = ifelse(fips == 2270, 2158, fips))

# ... attach census division codes
regions = read.csv('regions.csv', 
					stringsAsFactors = FALSE) %>%
		  mutate(state = State.Code) %>%
		  dplyr::select(state, Division)

# ... demographics
pop2<-read_csv("nhgis0029_ds216_20155_2015_county.csv")
pop2$fips<-paste(pop2$STATEA, pop2$COUNTYA, sep="")
pop2<-pop2%>%
  rename(fips.st=STATEA)%>%
  mutate(fips = as.numeric(fips), fips.st=as.numeric(fips.st)) %>%
  filter(fips.st!=72)

cw<-read_csv("fips-st-crosswalk.csv") %>%
  dplyr::select(state, fips)%>%rename(fips.st = fips)

pop2<-left_join(pop2, cw)%>%
  dplyr::select(-fips.st)

pop<-pop2%>%
  mutate(black.men=ADT5E007+ADT5E008+ADT5E009+
           ADT5E010+ADT5E011+ADT5E012+ADT5E013+
           ADT5E014+ADT5E015+ADT5E016,
         white.men=ADUBE007+ADUBE008+ADUBE009+
           ADUBE010+ADUBE011+ADUBE012+ADUBE013+
           ADUBE014+ADUBE015+ADUBE016,
         latino.men=ADUCE007+ADUCE008+ADUCE009+
           ADUCE010+ADUCE011+ADUCE012+ADUCE013+
           ADUCE014+ADUCE015+ADUCE016,
         tot.men=ADURE008)%>%
  select(fips, state, black.men, white.men, 
         latino.men, tot.men)

### got all of them matched
#z<-which(!(fdat$fips%in%pop$fips))

### KIDS - START AT 18 or !5? Pop data may link up at 15

pop<-pop%>%
  left_join(pop)%>%
  left_join(cdc)%>%
  left_join(regions)

rm(cdc); rm(cw); rm(fe_new); rm(imputeds); rm(pop2); rm(regions)

#########################################################
# add in the new conditions: 
# ... reduce to men
# removes 889 women, 7 trans
fdat <- fdat %>% filter(gender == "Male")


#######################################################
## FILTER BY AGE >= 18
## removes 318 children
### convert all ages to numeric for filter
# 
fdat[grep("mon", fdat$age), "age"]<-"0"
ranges<-grep("-", fdat$age)
## substr and str_sub doing weird stuff with vector, going to do by row
for(i in 1:length(ranges)){
  fdat[ranges[i], "age"]<-substr(fdat[ranges[i], "age"], 1, 2)
}
s<-grep("s", fdat$age)
for(i in 1:length(s)){
  fdat[s[i], "age"]<-substr(fdat[s[i], "age"], 1, 2)
}
misc<-grep("`", fdat$age)
for(i in 1:length(misc)){
  fdat[misc[i], "age"]<-substr(fdat[misc[i], "age"], 1, 2)
}
fdat$age<-as.numeric(fdat$age)           
fdat<-fdat%>%
  filter(age>=18)

# ... and filter out suicides
#  removes 810 adult male suicides
ns = c("Suicide", "Ruled suicide", "Ruled suicide by police", "Murder/suicide")
fdat = fdat %>% filter(!(official_disposition %in% ns)) 

# ... and filter out not-force deaths (ie., fleeing deaths)
# removes 1476 non-force deaths
fdat = fdat %>%  
        filter(cause_of_death %in% 
          c('Asphyxiated/Restrained','Beaten/Bludgeoned with instrument', 
            'Chemical agent/Pepper spray', 'Medical emergency', 'Tasered', 
            'Gunshot'))

############### FINAL SET OF CASES
### 6295 adult male homicides recorded between 1/1/2012 and 2/12/2018

# ... reduce ur codes to 3 (largest urban; medium urban; rural)
pop = pop %>%
      mutate(ur.code  = ifelse(ur.code %in% c('1: large central metro'), 'large urban', 
                        ifelse(ur.code %in% c('2: large fringe metro', '3: medium metro', '4: small metro'), 'medium urban',
                        ifelse(ur.code %in% c('5: micropolitan', '6: noncore'), 'rural', NA))))
##############################################################

# ... merge the data 
tmp = fdat %>%
       group_by(fips, race) %>%
       summarise(deaths = n()) %>%
       spread(race, deaths, fill = 0) %>%
       rename(d.black = black,
              d.asian = asian,
              d.white = white,
              d.na    = `<NA>`,
              d.latino = latino,
              d.other  = other) %>%
       mutate(d.total = d.black + d.asian + d.white + d.latino + d.na + d.other)

tmp2 = left_join(pop, tmp, 'fips') %>%
       mutate_at(vars(d.asian:d.total), funs(replace(., which(is.na(.)), 0))) %>%
       tbl_df() %>%
       rename(division = Division)

## Modeling
#  1. divison level 
# (running ur.code as fixed effect, becuase convergance tough at .999 if random)
# ... for blacks     
blk.stan.0 = stan_glmer(d.black ~ ur.code + (1|division),
                      prior_intercept=normal((log(0.94)-log(100000)), 2.5), #for prior intercept, based on krieger estimates
                      prior = normal(0, 2.5), #weakly informative, no difference from big urban
                      prior_covariance = decov(1, 1, 1, 1), #default
                      data = tmp2, offset=I(log(black.men+1)), 
                      family="neg_binomial_2", iter=2000, chains=4)#, 
                      #adapt_delta = 0.9999)

# ... for whites
wht.stan.0 = stan_glmer(d.white ~ ur.code + (1|division),
                      prior_intercept=normal((log(0.37)-log(100000)), 2.5), #for prior intercept, based on krieger estimates
                      prior = normal(0, 2.5), #weakly informative, no difference from big urban
                      prior_covariance = decov(1, 1, 1, 1), #default
                      data = tmp2, offset=log(white.men), 
                      family="neg_binomial_2", iter=2000, chains=4)#,
                      #adapt_delta = 0.999)

# ... for latinos
lat.stan.0 = stan_glmer(d.latino ~ ur.code + (1|division),
                      prior_intercept=normal((log(0.37)-log(100000)), 2.5), #for prior intercept, based on krieger estimates
                      prior = normal(0, 2.5), #weakly informative, no difference from big urban
                      prior_covariance = decov(1, 1, 1, 1), #default
                      data = tmp2, offset=I(log(latino.men+1)), 
                      family="neg_binomial_2", iter=2000, chains=4)#,
                      #adapt_delta = 0.999)

# converge?
# blk, yes at base delta
# wht, yes ...
# ltx, yes ... 

#save.image('bwl_fixed_mod.RData')


# model divisions as normal
# then, as exploratory, run state, w/ fixed ur effects (since it's operating that way)
# and just plot to show that getting same general idea

## Visuals 
# load('bwl_fixed_mod.RData')
blk.stan<-blk.stan.0
wht.stan<-wht.stan.0
lat.stan<-lat.stan.0
#tot.stan<-tot.stan.0
tmp2<-tmp2%>%as.data.frame()

# ...set up scenarios for prediction 
ur.code  = unique(tmp2$ur.code)
division = unique(tmp2$division)
ur.division = expand.grid(division, ur.code); 
names(ur.division) = c("division", "ur.code")
n = length(division)*length(ur.code)

# ... use 100,000,000 for offset to get enough trials
# ... then rescale to rate per 100,000 
newdata <- tmp2[1:n,]
newdata$fips <- 0; newdata$tot.men<-newdata$black.men<-newdata$white.men<-newdata$latino.men<-100000000; 
newdata$ur.code<-ur.division$ur.code; newdata$division<-ur.division$division

# ... estimate predictive posterior for each group 
# ... {blk: black; wht: white; lat: latinx}
# make a long file for ur.division
#post.tot = posterior_predict(tot.stan, newdata = newdata)/1000

# ... black
post.blk = posterior_predict(blk.stan, newdata = newdata)/1000
post.blk.sims<-cbind(ur.division, t(post.blk))
post.blk.sims<-gather(post.blk.sims, 
                      key=sim, 
                      value=sim.rate, 
                      -ur.code, -division)

# ... white
post.wht = posterior_predict(wht.stan, newdata = newdata)/1000
post.wht.sims<-cbind(ur.division, t(post.wht))
post.wht.sims<-gather(post.wht.sims, 
                      key=sim, 
                      value=sim.rate, 
                      -ur.code, -division)

# ... latino
post.lat = posterior_predict(lat.stan, newdata = newdata)/1000
post.lat.sims<-cbind(ur.division, t(post.lat))
post.lat.sims<-gather(post.lat.sims, 
                      key=sim, 
                      value=sim.rate, 
                      -ur.code, -division)

# 3: calculate mean and 95% uncertainty intervals for each group 
post.blk.int = bind_cols(ur.division, as.data.frame(
	t(apply(post.blk, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
post.blk.int = post.blk.int %>% mutate(race = 'Black')

post.wht.int = bind_cols(ur.division, as.data.frame(
	t(apply(post.wht, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
post.wht.int = post.wht.int %>% mutate(race = 'White')

post.lat.int = bind_cols(ur.division, as.data.frame(
	t(apply(post.lat, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
post.lat.int = post.lat.int %>% mutate(race = 'Latino')

# ... bind everything together & get ready for plotting
post.estimates = bind_rows(post.blk.int, post.wht.int, post.lat.int);
colnames(post.estimates)[3:5] = c('lwr', 'estimate', 'upr')

p.dat1 = post.estimates %>%
		 rename(rate = estimate) %>% 
		 mutate(type = 'estimated', 
		 		ur.code  = as.character(ur.code), 
		 		division = as.character(division)) %>%
		 select(ur.code, division, rate, race, type) 

# ... get observed, average rates by race + county type + division
p.dat2 = tmp2 %>%
	  	 group_by(ur.code, division) %>%
	  	 summarise(y.black.mean  =   sum(d.black)/sum(black.men)*100000,
	          	   y.latino.mean = sum(d.latino)/sum(latino.men)*100000,
	  			   y.white.mean  =   sum(d.white)/sum(white.men)*100000) %>%
	  	 gather(rate, value, y.black.mean:y.white.mean) %>%
	   	 mutate(race = ifelse(rate == 'y.black.mean',  'Black',
		 			   ifelse(rate == 'y.white.mean',  'White',
		 			   ifelse(rate == 'y.latino.mean', 'Latino', 'nah'))),
		 	  	type = 'observed') %>%
	   	select(ur.code, division, value, race, type) %>%
	   	rename(rate = value)


# ---- POST MEETING CODE --- # 

# box plot version: 
# NOTE: need to change the scaling, right?

# 1: bind to
p.dat = bind_rows(p.dat1, p.dat2) %>%
		mutate_at(vars(rate), funs(./(1588/365))) %>%
		tbl_df()

# estimated: 
# ... with free y
a = p.dat %>%
	filter(type == 'estimated') %>%
	mutate(race = ifelse(race == 'Black', 'Black', 
				  ifelse(race == 'White', 'White',
				  ifelse(race == 'Latino', 'Latino', 'HEY'))))

library(RColorBrewer)
my.cols = brewer.pal(8, "Set2")
my.cols

#fig1<-ggplot(data = a, aes(x = ur.code, y = rate)) +
#	geom_boxplot(aes(x = ur.code, y = rate), alpha = 1, 
#					 outlier.colour = NULL, fill = my.cols[8]) +
#	geom_jitter(aes(y = rate, group = type, color = division), 
#				    position = position_jitterdodge(dodge.width   = .5, 
#				   								    jitter.width  = .0,
#				   								    jitter.height = .0),
#				   size  = 2.5, 
#				   alpha = .95) +
#	facet_wrap(~race, scales = 'free_y') +
#  	scale_color_brewer(palette = 'Paired') +
#  	scale_fill_manual(values  = my.cols[2]) +
#  	ylab('Rate') +
#  	xlab('Metro-Type') + 
#  	theme_bw() +
#  	#scale_x_discrete(limits = rev(levels(as.factor(p.dat$ur.code)))) +
#  	theme(axis.text.x = element_text(angle=60, hjust=1)) +
#    theme(legend.position="bottom")+
#  	ggsave("fig1_men.tiff")


## Figure 2:
# 1: calculate posterior rate ratios for (blk v wht) v. (lat v wht)
bw.post = bind_cols(ur.division, as.data.frame(t(apply(
	post.blk/post.wht, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
bw.post$rate = "Black/White"

lw.post = bind_cols(ur.division, as.data.frame(t(apply(
	post.lat/post.wht, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
lw.post$rate = "Latino/White"

# ... get data ready for plotting 
plot.dat = bind_rows(bw.post, lw.post)

plot.dat$ur.code<-factor(plot.dat$ur.code, 
						 levels=sort(as.character(unique(plot.dat$ur.code))))
plot.dat$division<-factor(plot.dat$division, 
						 levels=sort(as.character(unique(plot.dat$division))))

plot.dat<-plot.dat[order(plot.dat$ur.code, plot.dat$division), ]
names(plot.dat)[3:6]<-c("lwr", "Median", "upr", 'comparison')

# ... add indicator for whether interval includes 1
plot.dat$CI_includes_1 = ifelse(data.table::between(1, plot.dat$lwr, 
										   plot.dat$upr) == TRUE, 'yes', 'no')

plot.dat2 = 
	plot.dat %>% 
	mutate(ci_includes_1 = ifelse(CI_includes_1 == 'no', 1.05, 1))%>%
    rename(`Interval\nIncludes 1`=CI_includes_1)%>%
    rename(Comparison=comparison)

# ... plot it out
ggplot(plot.dat2,
       aes(x = ur.code, y = Median, group = Comparison, fill = Comparison, color =  Comparison)) +
    facet_wrap(~division) + 
    geom_point(aes(size = `Interval\nIncludes 1`), alpha = .9, pch = 21, color = 'grey1') + 
    scale_fill_brewer(palette = 'Set2') +
    #theme_bw() +
    ylab("Mortality rate ratio") + 
    xlab(" ")+
    geom_hline(yintercept = 1, lty = 2) +
    #theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 9)) +
    #geom_line() +
    scale_size_discrete(range = c(5,2)) + 
    coord_flip() +
    scale_x_discrete(limits = rev(levels(as.factor(plot.dat2$ur.code))))+
    theme(legend.position="bottom") +
  ggsave("fig2_men.tiff", width=6.5, height=6.5, units="in")

###############################################################
## Figure 3: difference between observed, median estimated
# 1: calculate expected deaths from posterior, count observed deaths/pop
# 2: lump divisions, UR using expected counts
###############################################################
#### make observed deaths, pop DF
post.observed.blk<-tmp2%>%
  group_by(ur.code, division)%>%
  summarise(observed.deaths=sum(d.black),
            observed.pop=sum(black.men))%>%
  ungroup()

post.observed.lat<-tmp2%>%
  group_by(ur.code, division)%>%
  summarise(observed.deaths=sum(d.latino),
            observed.pop=sum(latino.men))%>%
  ungroup()

post.observed.wht<-tmp2%>%
  group_by(ur.code, division)%>%
  summarise(observed.deaths=sum(d.white),
            observed.pop=sum(white.men))%>%
  ungroup()

#### pull full posterior sim results, convert into expected deaths to aggregate to ur, div
blk.ur.post.mort<-post.observed.blk%>%
  left_join(post.blk.sims)%>%
  ungroup()%>%
  select(-division)%>%
  group_by(ur.code, sim)%>%
  summarise(sim.mort.rt=sum((sim.rate/100000) * observed.pop)/sum(observed.pop) * 100000,
            obs.mort.rt=sum(observed.deaths)/sum(observed.pop)*100000)%>%
  mutate(Race="Black")

lat.ur.post.mort<-post.observed.lat%>%
  left_join(post.lat.sims)%>%
  ungroup()%>%
  select(-division)%>%
  group_by(ur.code, sim)%>%
  summarise(sim.mort.rt=sum((sim.rate/100000) * observed.pop)/sum(observed.pop) * 100000,
            obs.mort.rt=sum(observed.deaths)/sum(observed.pop)*100000)%>%
  mutate(Race="Latino")

wht.ur.post.mort<-post.observed.wht%>%
  left_join(post.wht.sims)%>%
  ungroup()%>%
  select(-division)%>%
  group_by(ur.code, sim)%>%
  summarise(sim.mort.rt=sum((sim.rate/100000) * observed.pop)/sum(observed.pop) * 100000,
            obs.mort.rt=sum(observed.deaths)/sum(observed.pop)*100000)%>%
  mutate(Race="White")

ur.post<-bind_rows(blk.ur.post.mort, 
                   lat.ur.post.mort,
                   wht.ur.post.mort)

ur.post<-ur.post%>%
  ungroup()%>%
  mutate_at(vars(sim.mort.rt, obs.mort.rt), funs(./(1588/365)))

blk.div.post.mort<-post.observed.blk%>%
  left_join(post.blk.sims)%>%
  ungroup()%>%
  select(-ur.code)%>%
  group_by(division, sim)%>%
  summarise(sim.mort.rt=sum((sim.rate/100000) * observed.pop)/sum(observed.pop) * 100000,
            obs.mort.rt=sum(observed.deaths)/sum(observed.pop)*100000)%>%
  mutate(Race="Black")

lat.div.post.mort<-post.observed.lat%>%
  left_join(post.lat.sims)%>%
  ungroup()%>%
  select(-ur.code)%>%
  group_by(division, sim)%>%
  summarise(sim.mort.rt=sum((sim.rate/100000) * observed.pop)/sum(observed.pop) * 100000,
            obs.mort.rt=sum(observed.deaths)/sum(observed.pop)*100000)%>%
  mutate(Race="Latino")

wht.div.post.mort<-post.observed.wht%>%
  left_join(post.wht.sims)%>%
  ungroup()%>%
  select(-ur.code)%>%
  group_by(division, sim)%>%
  summarise(sim.mort.rt=sum((sim.rate/100000) * observed.pop)/sum(observed.pop) * 100000,
            obs.mort.rt=sum(observed.deaths)/sum(observed.pop)*100000)%>%
  mutate(Race="White")

div.post<-bind_rows(blk.div.post.mort, 
                    lat.div.post.mort,
                    wht.div.post.mort)

div.post <- div.post %>%
  	mutate_at(vars(sim.mort.rt, obs.mort.rt), funs(./(1588/365)))

# ... not sure if outlines make easier or harder to see
ggplot(ur.post, aes(sim.mort.rt, fill=Race))+
  geom_density(alpha=0.7, bw = .2, color = 'grey100')+
  geom_point(aes(x=obs.mort.rt, y=0, shape=Race), size=2)+
  facet_wrap(~ur.code)+
  xlab("Police-involved deaths per 100,000 per year")+
  scale_fill_brewer(palette = 'Set2') +
  ylab("")+
  coord_cartesian(xlim=c(0,7))+
  theme(legend.position="bottom")+
  ggsave("UR_Density_men.tiff",width=6.5, height=6.5, units="in")
  
ggplot(div.post, aes(sim.mort.rt, fill=Race))+
  geom_density(alpha=0.7, bw = .2, color = 'grey100')+
  geom_point(aes(x=obs.mort.rt, y=0, shape=Race), size=2)+
  facet_wrap(~division)+
  xlab("Police-involved deaths per 100,000 per year")+
  ylab("")+
  coord_cartesian(xlim=c(0,7))+
  scale_fill_brewer(palette = 'Set2') +
  theme(legend.position="bottom")+
  ggsave("Div_Density_men.tiff", width=6.5, height=6.5, units="in")


#### for appendix table 3

apTab3<-post.estimates%>%
  mutate(lwr=lwr*(365/1588),
         estimate=estimate*(365/1588),
         upr=upr*(365/1588))

apTabReformat<-apTab3%>%
  mutate(estimate=paste(
    round(estimate, 1), 
    " (",
    round(lwr, 1),
    ", ",
    round(upr, 1),
    ")",
    sep=""
  ))%>%
  select(- lwr, -upr)%>%
  spread(race, estimate)%>%
  mutate(division=as.character(division),
         ur.code=as.character(ur.code))%>%
  arrange(division, ur.code)