################################################################
rm(list=ls())
gc()
library(tidyverse)
library(rstanarm)
library(parallel)
library(data.table)
theme_set(theme_minimal())
setwd("~/Projects/police-mort")
select  = dplyr::select

FE_constant<-(365/2234)

#### function to clean ur.codes for plotting:
clean_ur<-function(x){
  x<-as.character(x)
  levels<-str_to_title(substr(sort(unique(x)), 4, nchar(sort(unique(x)))))
  output<-ordered(str_to_title(substr(x, 4, nchar(x))),
                   levels = levels)
  return(output)
}

results<-data.frame(results = c('division_ur_models.RData',
                                'division_ur_models_sensitivity_noimps.RData',
                                'division_ur_models_sensitivity_alldeaths.RData'),
                    labels = c("Main results",
                               "No imputation",
                               "All deaths"), 
                    stringsAsFactors = FALSE)

##### LOOP OVER ALL SENSITIVITY TESTS AND MAIN RESULTS
p.dat<-list()
for(t in 1:nrow(results)){
  print(t)
  load(results$results[[t]])
  
  blk.stan <- blk.stan.0
  wht.stan <- wht.stan.0
  lat.stan <- lat.stan.0
  #tot.stan<-tot.stan.0
  tmp2 <- tmp2 %>% as.data.frame()
  
  # ... set up scenarios for prediction
  # .... (9 divisions x 6 ur.codes)
  ur.code  = unique(tmp2$ur.code)
  division = unique(tmp2$division)
  ur.division = expand.grid(division, ur.code);
  names(ur.division) = c("division", "ur.code")
  n = length(division)*length(ur.code)
  
  # .... use 100,000,000 for offset to get enough trials
  # .... then rescale to rate per 100,000
  newdata <- tmp2[1:n,]
  newdata$fips <- 0; newdata$tot.men<-newdata$black.men<-newdata$white.men<-newdata$latino.men<-100000000;
  newdata$ur.code<-ur.division$ur.code; newdata$division<-ur.division$division
  
  # ... estimate predictive posterior for each group
  # .... for blacks
  post.blk = posterior_predict(blk.stan, newdata = newdata)/1000
  post.blk.sims <- cbind(ur.division, t(post.blk)) %>%
    gather(., key = sim, value = sim.rate, -ur.code, -division)
  
  # .... for whites
  post.wht = posterior_predict(wht.stan, newdata = newdata)/1000
  post.wht.sims <- cbind(ur.division, t(post.wht)) %>%
    gather(., key = sim, value = sim.rate, -ur.code, -division)
  
  # ... latino
  post.lat = posterior_predict(lat.stan, newdata = newdata)/1000
  post.lat.sims <- cbind(ur.division, t(post.lat)) %>%
    gather(., key = sim, value = sim.rate, -ur.code, -division)
  
  #########################################################
  
  # ... calculate mean and 95% uncertainty intervals for each group
  post.blk.int = bind_cols(ur.division, as.data.frame(
    t(apply(post.blk, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
  post.blk.int = post.blk.int %>% mutate(race = 'Black')
  
  post.wht.int = bind_cols(ur.division, as.data.frame(
    t(apply(post.wht, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
  post.wht.int = post.wht.int %>% mutate(race = 'White')
  
  post.lat.int = bind_cols(ur.division, as.data.frame(
    t(apply(post.lat, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
  post.lat.int = post.lat.int %>% mutate(race = 'Latino')
  
  ################################################################################################
  #### APPENDIX VIOLIN PLOT
  
  # 4: bind everything together & get ready for plotting
  post.estimates = bind_rows(post.blk.int, post.wht.int, post.lat.int);
  colnames(post.estimates)[3:5] = c('lwr', 'estimate', 'upr')
  
  print(t)
  p.dat[[t]] <- post.estimates %>%
       rename(rate = estimate) %>% 
       mutate(type = results$labels[t], 
          ur.code  = as.character(ur.code), 
          division = as.character(division)) %>%
       select(ur.code, division, rate, race, type) #
  print(head(p.dat[[t]]))
  
}

# 6: bind together estimated and observed rates and scale 
p.dat = bind_rows(p.dat[[1]], p.dat[[2]], p.dat[[3]]) %>%
  mutate_at(vars(rate), funs(. * FE_constant))%>%
  mutate(ur.code = clean_ur(ur.code))

# 7: plot
# ... set up plotting space for tifff image
dev.off()
tiff("./visuals/violin_sensitivity.tiff", units="in", width=12, height = 6.5, res = 300)#

ggplot(data = p.dat, aes(x = ur.code, y = rate)) +
geom_violin(aes(x = ur.code, y = rate, fill = type),
      position = position_dodge(width = .9),
      alpha = .3,
      scale = "width") +
geom_point(aes(y = rate, group = type, color = division),
         position = position_jitterdodge(dodge.width   = .9,
                           jitter.width  = .05,
                           jitter.height = .0),
         size  =  2,
         alpha = .9) +
facet_wrap(~race, scales = 'free_y') +
  scale_fill_brewer(palette  = 'Dark2') +
  scale_color_brewer(palette = 'Paired') +
  ylab('Police homicides per 100,000 per year') +
  xlab('Metro type') +
  theme_bw() +
  #scale_x_discrete(limits = rev(levels(as.factor(p.dat$ur.code)))) +
  #coord_flip() +
  theme(axis.text.x = element_text(angle=60, hjust=1),
      legend.position = 'bottom', legend.title = element_blank())#

dev.off()