library(MASS)

library(ggplot2)
library(readr)
library(date)
library(rstanarm)
library(dplyr)
library(tidyr)
library(xtable)
load("models-defaultpriors.RData")


######################
### Tables
########################

maketable<-function(x, label){
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  tab<-x
  names(tab)[3:5]<-c("Lower", "Median", "Upper")
  tab$ur.code<-as.character(tab$ur.code); tab$division<-as.character(tab$division)
  tab<-tab[order(tab$division, tab$ur.code), ]
  tab$ur.code<-substr(tab$ur.code, 4, nchar(tab$ur.code))
  for(i in 1:nrow(tab)){
    tab$ur.code[i]<-simpleCap(tab$ur.code[i])
    tab$ur.code[i]<-paste(" -", tab$ur.code[i])
  }
  tab[, 3:5]<-round(tab[, 3:5], 1)
  tab<-tab%>%mutate(value=paste(Median, " (", Lower, ", ", Upper, ")", sep=""))%>%
    select(-Median, -Lower, -Upper)
  div<-unique(tab$division)
  for(i in 1:length(div)){
    index<-min(which(tab$division==div[i]))
    if(index==1){
      tab<-rbind(c(NA, div[i], NA), tab[index:nrow(tab),])
    }
    if(index!=1){
      tab<-rbind(tab[1:(index-1),], c(NA, div[i], NA), tab[index:nrow(tab),])
    }
  }
  tab<-tab%>%select(-division)
  names(tab)<-c("County Type", label)
  return(tab)
}

### set up newdata for ppd - use 100,000,000 for offset to get enough trials, 
### then rescale to rate per 100,000 
# ... blacks 

ur.code  = unique(tmp2$ur.code)
division = unique(tmp2$division)
ur.division = expand.grid(division, ur.code); 
names(ur.division) = c("division", "ur.code")
n = length(division)*length(ur.code)

newdata<-tmp2[1:n,]
newdata$fips<-0; newdata$tot.pop<-newdata$black<-newdata$white<-newdata$latino<-100000000; 
newdata$ur.code<-ur.division$ur.code; newdata$division<-ur.division$division

post.blk<-posterior_predict(blk.stan, newdata=newdata)/1000
post.blk.int<-bind_cols(ur.division, 
                        as.data.frame(t(apply(post.blk, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
post.blk.int = post.blk.int %>% mutate(race = 'Black')

# .. whites 
post.wht<-posterior_predict(wht.stan, newdata=newdata)/1000
post.wht.int<-bind_cols(ur.division, 
                        as.data.frame(t(apply(post.wht, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))

post.wht.int = post.wht.int %>% mutate(race = 'White')

# ... latinx 
post.lat<-posterior_predict(lat.stan, newdata=newdata)/1000
post.lat.int<-bind_cols(ur.division,
                        as.data.frame(t(apply(post.lat, 2, function(x)quantile(x, probs=c(0.025, 0.5, 0.975))))))
post.lat.int = post.lat.int %>% mutate(race = 'Latinx')


tab.out<-cbind(maketable(post.blk.int, "Rate-Black")[, c(1,3)],
               maketable(post.lat.int, "Rate-Latinx")[,3],
               maketable(post.wht.int, "Rate-White")[,3])
names(tab.out)<-c("County Name", "Black", "Latinx", "White")


print(xtable(tab.out,
             caption="Posterior police related mortality estimates by race/ethnicity, census region, and metro type, 95 percent credible intervals",
             digits=1),
      include.rownames=FALSE,
      caption.placement="top",
      type="html",
      comment=FALSE,
      file="estimates-defaultpriors.html"
)




tabUR<-tmp2%>%group_by(ur.code, division)%>%
  summarise(total=sum(d.total))%>%
  spread(ur.code, total)

tabUR$Total<-apply(tabUR[, 2:ncol(tabUR)], 1, sum)
tabUR<-rbind(tabUR, c("Total", apply(tabUR[, 2:ncol(tabUR)], 2, sum)))

names(tabUR)<-c("Division", "Large Central Metro", "Large Fringe Metro",
                "Medium Metro", "Small Metro", "Micropolitan", "Noncore", "Total")

tab.out<-xtable(tabUR, 
                digits=1,
                caption = "Police related fatalities in the U.S. by metro type and Census divsiion, 1/1/2013 - 5/8/2017")
print(tab.out, type="html", 
      file="raw-counts-defaultpriors.html",
      include.rownames=FALSE)