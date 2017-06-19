library(MASS)

library(ggplot2)
library(readr)
library(date)
library(rstanarm)
library(dplyr)
library(tidyr)
library(xtable)
library(pander)
load("models.RData")


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
  tab[, 3:5]<-round(tab[, 3:5], 2)
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


tab.out<-cbind(maketable(post.blk.int, "Rate-Black"),
               maketable(post.lat.int, "Rate-Latino")[,2],
               maketable(post.wht.int, "Rate-White")[,2])
names(tab.out)<-c("County Name", "Black", "Latino", "White")


Here is a ridiculous table with posterior estimates of mortality by race, region and metro type.

{r, results="asis", message=FALSE, echo=FALSE}

print(xtable(tab.out,
             caption="Posterior police related mortality estimates by race/ethnicity, census region, and metro type, 95 percent credible intervals"),
      include.rownames=FALSE,
      size="\\fontsize{9pt}{10pt}\\selectfont",
      caption.placement="top",
      comment=FALSE
)




tabUR<-tmp2%>%group_by(ur.code, division)%>%
  summarise(total=sum(d.total))%>%
  spread(ur.code, total)

tabUR$Total<-apply(tabUR[, 2:ncol(tabUR)], 1, sum)
tabUR<-rbind(tabUR, c("Total", apply(tabUR[, 2:ncol(tabUR)], 2, sum)))

names(tabUR)<-c("Division", "Large Central Metro", "Large Fringe Metro",
                "Medium Metro", "Small Metro", "Micropolitan", "Noncore", "Total")

tab.out<-xtable(tabUR, caption = "Police related fatalities in the U.S. by metro type and Census divsiion, 1/1/2013 - 5/8/2017")
print(tab.out, type="html")


#### also make an appx table for 



