#################################
# Police Shooting Project
# cause of death filtering, .r
# last edit: 2/15 (M.E)
#################################

# 1: load data; attach packages; etc.
library(tidyverse)
library(RColorBrewer)
library(scales)

setwd("~/Projects/police_mort")
dat_hold = read_csv('fatal-encounters-2-12-18.csv')
cols = c(brewer.pal(8, name = 'Set2'), brewer.pal(8, name = 'Paired'))

# ... pull folks beyond 2012 (...as set up in merge.r file...)
fe_new = dat_hold %>%
  filter(`Date (Year)`>= 2012) %>%
  filter(!(is.na(Latitude)))

names(fe_new) <- c("id", "name", "age", "gender", "race", "URL", "death_date", 
                   "loc_address", "loc_city", "loc_state", "loc_zip", "loc_county", 
                   "loc_full_address", "Latitude", "Longitude", "agency", 
                   "cause_of_death","cause_description", "official_disposition", 
                   "news_url", "mental_illness", "video", "null1", "dateanddesc", 
                   "null2", "id2", "year", "null3")

# 2: take a look at distribution of reported causes of death
fe_new %>%
	group_by(cause_of_death) %>%
	summarise(count = n(), freq = count/nrow(fe_new))

# ... can visualize it, if we want
base_breaks <- function(n = 10){
    function(x) {
        axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
    }
}

fe_new %>%
    group_by(cause_of_death) %>%
    summarise(count = n(), 
              freq = count/nrow(fe_new)) %>%
    ggplot(., aes(x = count, y = cause_of_death)) +
    geom_point(size = 4.5, fill = cols[1], color = 'black', pch = 21) +
    theme_minimal() + 
    scale_x_continuous(trans = log_trans(), breaks = base_breaks()) +
    xlab('count') +
    ylab('cause of death') +
    expand_limits(x = c(10, 11000))

# notes: 
# ... shooting and vehicle deaths dominate the list (99%);
# ... first thought: trying to sort around idea of "use of force"
# .... e.g., shootings category seem, inarguably, most important to include in 'keep' category
# .... e.g., vehicle deaths seems like fall in same category, but a bit more distant from the core idea of that group
# .... e.g., drug overdose is very likely in the alternative group =
# ... first pass == including based off of these two categories
# ... will have to look into detailed descriptions for ambiguous groups

# 3: take a look at some of circumstances that define the more complicated causes
# ... looping through w/:
lookR = function(.cause) fe_new %>% filter(cause_of_death == .cause) %>% select(cause_description)

# ... a. Asphyxiated/Restrained
# ... (mostly death to use of force; some suicides though)							
tmp = lookR('Asphyxiated/Restrained');  nrow(tmp)*.10
tmp[sample(1:nrow(tmp), 1),] %>% data.frame

# ... b. Beaten/Bludgeoned with instrument
# ... almost exclusively death due to force
tmp = lookR('Beaten/Bludgeoned with instrument'); nrow(tmp)*.10
tmp[sample(1:nrow(tmp), 1),] %>% data.frame

# ... c.  Burned/Smoke inhalation
# ... (mostly suicide; some taser accidents)
tmp = lookR('Burned/Smoke inhalation'); nrow(tmp)*.10
tmp[sample(1:nrow(tmp), 1),] %>% data.frame

# ... d. Chemical agent/Pepper spray 
# ... (mostly force related)
tmp = lookR('Chemical agent/Pepper spray'); nrow(tmp)*.10
tmp[sample(1:nrow(tmp), 1),] %>% data.frame

# ... e. Drowned
# ... (almost all suicide)
tmp = lookR('Drowned'); nrow(tmp)*.10
tmp[sample(1:nrow(tmp), 1),] %>% data.frame

# ... f. Drug overdose 
# ... (mostly what the name says)
tmp = lookR('Drug overdose'); nrow(tmp)*.10
tmp[sample(1:nrow(tmp), 1),] %>% data.frame

# ... g. Fell from a height
# ... (more accidents, less suicides)
tmp = lookR('Fell from a height'); nrow(tmp)*.10
tmp[10,] %>% data.frame  
 
# ... h. Medical emergency
# ... (a complete grab bag; lots of "was beaten up; died later")
tmp = lookR('Medical emergency'); nrow(tmp)*.10
tmp[sample(1:nrow(tmp), 1),] %>% data.frame 

# ... i. Other
# ... (...grab bag is putting it lightly)
tmp = lookR('Other'); nrow(tmp)*.10
tmp[sample(1:nrow(tmp), 1),] %>% data.frame

# ... j. Stabbed
# ... (almost all suicides)
tmp = lookR('Stabbed'); nrow(tmp)*.10
tmp[sample(1:nrow(tmp), 1),] %>% data.frame

# ...k. Tasered 
# ... (mostly use of force)
tmp = lookR('Tasered'); nrow(tmp)*.10
tmp[sample(1:nrow(tmp), 1),] %>% data.frame 

# l. Undetermined
# ... (mix of not reported; multiple potential causes---e.g., shot and hung...)
tmp = lookR('Undetermined'); nrow(tmp)*.10
tmp[sample(1:nrow(tmp), 1),] %>% data.frame

# m. Vehicle
# ... (mostly chases; some deaths to bystanders)
tmp = lookR('Vehicle'); nrow(tmp)*.10
tmp[sample(1:nrow(tmp), 1),] %>% data.frame 

# n. GunShot
# ... (overwhelmingly "police shot and killed ..."
tmp = lookR('Gunshot'); nrow(tmp)*.10
tmp[sample(1:nrow(tmp), 1),] %>% data.frame

# some notes
# ... a handful of suicides in every category; variation in prop(suicide) 
# ..... can't really sort this programatically though
# ... some fairly clear characteristics in categories though
# .... i.e, {Asphyxiated: mostly force; +
# .... 		Beating: mostly force; +
# .... 		Burning: mix(suicide; force); +
# .... 		Chemical: mostly force; +
# .... 		Drowning: mostly suicide; +
# .... 		Drug overdose: mostly suicide; +
# .... 		Fall: mostly fleeing; +
# .... 		Medical emergency: mostly force (died after physical struggle) +
# .... 		Other: mix(grab-bag) +
# .... 		Stabbed: mostly suicide +
# .... 		Taser: mostly force +
# .... 		Undetermined: mix(not reported; multiple potential causes) +
# .... 		Vehicle: mostly fleeing +
# .... 		GunShot: mostly force +

# ... so, looks like a handful of consistent categories
# .... mostly force; mostly suicide; mostly fleeing; mixed 
# ... p(.) hierarchy: {full; force; fleeing; mixed; suicide}

# 4: create data sets, using different sorting criteria
data_scens = list(
	full  = fe_new,
	force = fe_new %>% 
		filter(cause_of_death %in% c('Asphyxiated/Restrained','Beaten/Bludgeoned with instrument', 
				'Chemical agent/Pepper spray', 'Medical emergency', 'Tasered', 'Gunshot')),
	flee = fe_new %>% filter(cause_of_death %in% c('Vehicle','Fell from a height')),
	mix = fe_new %>% filter(cause_of_death %in% c('Burned/Smoke inhalation', 'Other', 'Undetermined')),
	suicide = fe_new %>% filter(cause_of_death %in% c('Drowned', 'Drug overdose', 'Stabbed'))
	)

# ... bind everything together (and map over when ready)
dfs = tibble(id = names(data_scens), data_scens) 

# ... get different combos etc. etc.
force_flee = bind_rows(dfs$data_scens$force, dfs$data_scens$flee)
force_flee_mix = bind_rows(dfs$data_scens$force, dfs$data_scens$flee, dfs$data_scens$mix)