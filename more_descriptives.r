library(MASS)
library(ggplot2)
library(readr)
library(rstanarm)
library(dplyr)
library(tidyr)
library(xtable)
load("models.RData")

#### compare estimated mortality rates by race, modeled and observed

### observed

print("Total observed rate")
sum(tmp2$d.total)/sum(tmp2$tot.pop) * 100000 * (365/1588)

print("Black observed rate")
sum(tmp2$d.black)/sum(tmp2$black) * 100000 * (365/1588)

print("latino observed rate")
sum(tmp2$d.latino)/sum(tmp2$latino) * 100000 * (365/1588)

print("white observed rate")
sum(tmp2$d.white)/sum(tmp2$white) * 100000 * (365/1588)


post.blk<-posterior_predict(blk.stan, newdata=tmp2)
post.wht<-posterior_predict(wht.stan, newdata=tmp2)
post.lat<-posterior_predict(lat.stan, newdata=tmp2)
post.tot<-posterior_predict(tot.stan, newdata=tmp2)

post.blk.medians<-as.vector(apply(post.blk, 2, median))
plot(tmp2$d.black, post.blk.medians) #looks good to me

### posterior predictions on counts using observed data - 
### normalized into posterior risk rates

print("Black lower, median, upper")

post.blk.nat<-as.vector(apply(post.blk,1,sum))
post.blk.quantiles<-quantile(post.blk.nat, c(0.025, 0.5, .975))

sum(post.blk.quantiles[1])/sum(tmp2$black)* 100000 * (365/1588)
sum(post.blk.quantiles[2])/sum(tmp2$black)* 100000 * (365/1588)
sum(post.blk.quantiles[3])/sum(tmp2$black)* 100000 * (365/1588)


print("Latino lower, median, upper")

post.lat.nat<-as.vector(apply(post.lat,1,sum))
post.lat.quantiles<-quantile(post.lat.nat, c(0.025, 0.5, .975))

sum(post.lat.quantiles[1])/sum(tmp2$latino)* 100000 * (365/1588)
sum(post.lat.quantiles[2])/sum(tmp2$latino)* 100000 * (365/1588)
sum(post.lat.quantiles[3])/sum(tmp2$latino)* 100000 * (365/1588)

print("white lower, median, upper")
post.wht.nat<-as.vector(apply(post.wht,1,sum))
post.wht.quantiles<-quantile(post.wht.nat, c(0.025, 0.5, .975))

sum(post.wht.quantiles[1])/sum(tmp2$white)* 100000 * (365/1588)
sum(post.wht.quantiles[2])/sum(tmp2$white)* 100000 * (365/1588)
sum(post.wht.quantiles[3])/sum(tmp2$white)* 100000 * (365/1588)

print("total lower, median, upper")
post.tot.nat<-as.vector(apply(post.tot,1,sum))
post.tot.quantiles<-quantile(post.tot.nat, c(0.025, 0.5, .975))

sum(post.tot.quantiles[1])/sum(tmp2$tot.pop)* 100000 * (365/1588)
sum(post.tot.quantiles[2])/sum(tmp2$tot.pop)* 100000 * (365/1588)
sum(post.tot.quantiles[3])/sum(tmp2$tot.pop)* 100000 * (365/1588)

