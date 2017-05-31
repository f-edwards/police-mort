#devtools::install_github("hrbrmstr/albersusa")

library(albersusa)
library(sp)
library(rgeos)
library(maptools)
library(ggplot2)
library(ggalt)
library(ggthemes)
library(viridis)
library(scales)
library(RColorBrewer)
library(choroplethrMaps)


returnquant<-function(x){
  l<-5 ### number of quantiles
  temp<-cut_number(x,l)
  quant<-rep(NA, length(x))
  for(i in (1:l)){
    z<-which(levels(temp)[i]==temp)
    quant[z]<-i
  }
  return(as.factor(quant))
}

data(county.map)

#### add division and county classifiers
merge<-tmp1%>%
  dplyr::select(fips, ur.code, division)%>%
  dplyr::rename(region=fips)%>%
  left_join(county.map)


#try this for template
#https://github.com/hrbrmstr/rd3albers


# data("county.fips")
# 
# counties<-map_data("county")
# counties$polyname<-paste(counties$region, counties$subregion, sep=",")
# 
# 
# fcmap$state<-fcmap$St<-fcmap$stname
# fcmap<-StateNames(fcmap)
# fcmap$name<-tolower(fcmap$state)
# 
# #### load shapes from choroplethrMaps
# data(county.map)
# 
# n<-nrow(fcmap)
# 
# fclong<-with(fcmap, 
#              data.frame(name=rep(name, 4),
#                         q=as.factor(c(returnquant(bcl.rt), returnquant(acl.rt), returnquant(b.incarrt), returnquant(a.incarrt)))))
# 
# fclong$c<-c(rep("African American children in foster care per capita", n), 
#             rep("Native American children in foster care per capita", n), 
#             rep("African American Incarceration per capita", n),
#             rep("Native American incarceration per capita", n))
# fclong$c<-factor(fclong$c, levels=c("African American children in foster care per capita", 
#                                     "Native American children in foster care per capita", 
#                                     "African American Incarceration per capita", "Native American incarceration per capita"))
# 
# 
# us <- usa_composite()
# us_map <- fortify(us, region="name")
# us_map$name<-tolower(us_map$id)
# map.merge<-merge(us_map, fclong, by="name")
# 
# blue.pal<-c("#f1eef6", "#bdc9e1", "#74a9cf", "#2b8cbe", "#045a8d")
# 
# 
# gg <- ggplot()+ geom_map(data=map.merge, map=us_map,
#                     aes(x=long, y=lat, map_id=id, fill=q),
#                     color="black", size=0.2)+theme_map()+ coord_proj(us_laea_proj)+
#   facet_wrap(~c)+
#   scale_fill_manual(values = blue.pal,
#                     name="State Value\n2014", labels=c("Lowest 20%", " ", " ", " ", "Highest 20%"))
# 
# gg<-gg +theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
#               panel.border = element_blank(), panel.background=element_blank())+
#   scale_y_continuous(name="", breaks=NULL)+
#   scale_x_continuous(name="", breaks=NULL)+
#   theme(legend.title=element_text(size=10))+
#   theme(legend.text=element_text(size=10))+
#   theme(legend.position="bottom")+
#   theme(legend.key.size= unit(0.3, "cm"))+
#   theme(strip.background=element_blank(), 
#         strip.text.x=element_text(size=10),
#         strip.text.y=element_blank())+
#   theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))+
#   labs(x=NULL, y=NULL)
# 
# #print(gg)
# 
# ggsave(plot = gg, "RateMapBlue.pdf", height=5.5, width=7)
# 
# gg <- ggplot()+ geom_map(data=map.merge%>%filter(c%in%c("African American children in foster care per capita", 
#                                                         "Native American children in foster care per capita")), map=us_map,
#                          aes(x=long, y=lat, map_id=id, fill=q),
#                          color="black", size=0.2)+theme_map()+ coord_proj(us_laea_proj)+
#   facet_wrap(~c)+
#   scale_fill_manual(values = blue.pal,
#                     name="State Value\n2014", labels=c("Lowest 20%", " ", " ", " ", "Highest 20%"))
# 
# gg<-gg +theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
#               panel.border = element_blank(), panel.background=element_blank())+
#   scale_y_continuous(name="", breaks=NULL)+
#   scale_x_continuous(name="", breaks=NULL)+
#   theme(legend.title=element_text(size=10))+
#   theme(legend.text=element_text(size=10))+
#   theme(legend.position="bottom")+
#   theme(legend.key.size= unit(0.3, "cm"))+
#   theme(strip.background=element_blank(), 
#         strip.text.x=element_text(size=10),
#         strip.text.y=element_blank())
# 
# #print(gg)
# 
# ggsave(plot = gg, "FCRateMapBlue.pdf", width=7, height=3.5)
# 
# 
# fclong<-with(fcmap, 
#              data.frame(name=rep(name, 4),
#               q=as.factor(c(returnquant(bw.disp), returnquant(ami.disp), returnquant(b.incardisp), returnquant(a.incardisp)))))
#               
# fclong$c<-c(rep("Afr Am foster care disparity", n), 
#             rep("Native Am foster care disparity", n), 
#             rep("Afr Am incarceration disparity", n),
#             rep("Native Am incarceration disparity", n))
# fclong$c<-factor(fclong$c, levels=c("Afr Am foster care disparity", 
#                                     "Native Am foster care disparity", 
#                                     "Afr Am incarceration disparity", "Native Am incarceration disparity"))
# 
# map.merge<-merge(us_map, fclong, by="name")
# 
# gray.pal<-c("gray90", "gray65", "gray40", "gray15", "gray1")
# blue.pal<-c("#f1eef6", "#bdc9e1", "#74a9cf", "#2b8cbe", "#045a8d")
# 
# 
# gg <- ggplot()+ geom_map(data=map.merge, map=us_map,
#                          aes(x=long, y=lat, map_id=id, fill=q),
#                          color="black", size=0.2)+theme_map()+ coord_proj(us_laea_proj)+
#   facet_wrap(~c)+
#   scale_fill_manual(values = blue.pal,
#                     name="State Value\n2014", labels=c("Lowest 20%", " ", " ", " ", "Highest 20%"))
# 
# gg<-gg +theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
#               panel.border = element_blank(), panel.background=element_blank())+
#   scale_y_continuous(name="", breaks=NULL)+
#   scale_x_continuous(name="", breaks=NULL)+
#   theme(legend.title=element_text(size=10))+
#   theme(legend.text=element_text(size=10))+
#   theme(legend.position="bottom")+
#   theme(legend.key.size= unit(0.3, "cm"))+
#   theme(strip.background=element_blank(), 
#         strip.text.x=element_text(size=12),
#         strip.text.y=element_blank())
# 
# #print(gg)
# 
# ggsave(plot = gg, "DispMapBlue.pdf", height=5.5,width=7)
# 
# gg <- ggplot()+ geom_map(data=map.merge%>%filter(c%in%c("Afr Am foster care disparity", 
#                                                         "Native Am foster care disparity")), map=us_map,
#                          aes(x=long, y=lat, map_id=id, fill=q),
#                          color="black", size=0.2)+theme_map()+ coord_proj(us_laea_proj)+
#   facet_wrap(~c)+
#   scale_fill_manual(values = blue.pal,
#                     name="State Value\n2014", labels=c("Lowest 20%", " ", " ", " ", "Highest 20%"))
# 
# gg<-gg +theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
#               panel.border = element_blank(), panel.background=element_blank())+
#   scale_y_continuous(name="", breaks=NULL)+
#   scale_x_continuous(name="", breaks=NULL)+
#   theme(legend.title=element_text(size=12))+
#   theme(legend.text=element_text(size=12))+
#   theme(legend.position="bottom")+
#   theme(legend.key.size= unit(0.3, "cm"))+
#   theme(strip.background=element_blank(), 
#         strip.text.x=element_text(size=12),
#         strip.text.y=element_blank())
# 
# #print(gg)
# 
# ggsave(plot = gg, "FCDispMapBlue.pdf", width=7, height=3.5)
# 
# 
# fcmap$state<-fcmap$St<-fcmap$stname
# fcmap<-StateNames(fcmap)
# fcmap$name<-tolower(fcmap$state)
# 
# n<-nrow(fcmap)
# 
# fclong<-with(fcmap, 
#              data.frame(name=rep(name, 2),
#                         q=as.factor(c(returnquant(pov.rt), returnquant(cl.rt)))))
# 
# fclong$c<-c(rep("Child poverty per capita", n), 
#             rep("Children in foster care per capita", n))
# fclong$c<-factor(fclong$c, levels=c("Child poverty per capita", 
#                                     "Children in foster care per capita"))
# 
# us <- usa_composite()
# us_map <- fortify(us, region="name")
# us_map$name<-tolower(us_map$id)
# map.merge<-merge(us_map, fclong, by="name")
# 
# blue.pal<-c("#f1eef6", "#bdc9e1", "#74a9cf", "#2b8cbe", "#045a8d")
# 
# 
# gg <- ggplot()+ geom_map(data=map.merge, map=us_map,
#                          aes(x=long, y=lat, map_id=id, fill=q),
#                          color="black", size=0.2)+theme_map()+ coord_proj(us_laea_proj)+
#   facet_wrap(~c)+
#   scale_fill_manual(values = blue.pal,
#                     name="State Value\n2014", labels=c("Lowest 20%", " ", " ", " ", "Highest 20%"))
# 
# gg<-gg +theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
#               panel.border = element_blank(), panel.background=element_blank())+
#   scale_y_continuous(name="", breaks=NULL)+
#   scale_x_continuous(name="", breaks=NULL)+
#   theme(legend.title=element_text(size=10))+
#   theme(legend.text=element_text(size=10))+
#   theme(legend.position="bottom")+
#   theme(legend.key.size= unit(0.3, "cm"))+
#   theme(strip.background=element_blank(), 
#         strip.text.x=element_text(size=10),
#         strip.text.y=element_blank())
# 
# #print(gg)
# 
# ggsave(plot = gg, "PovMapBlue.pdf", width=7, height=3.5)
# 
# gg <- ggplot()+ geom_map(data=map.merge%>%filter(c=="Children in foster care per capita"), map=us_map,
#                          aes(x=long, y=lat, map_id=id, fill=q),
#                          color="black", size=0.2)+theme_map()+ coord_proj(us_laea_proj)+
#   facet_wrap(~c)+
#   scale_fill_manual(values = blue.pal,
#                     name="State Value\n2014", labels=c("Lowest 20%", " ", " ", " ", "Highest 20%"))
# 
# gg<-gg +theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
#               panel.border = element_blank(), panel.background=element_blank())+
#   scale_y_continuous(name="", breaks=NULL)+
#   scale_x_continuous(name="", breaks=NULL)+
#   theme(legend.title=element_text(size=10))+
#   theme(legend.text=element_text(size=10))+
#   theme(legend.position="bottom")+
#   theme(legend.key.size= unit(0.3, "cm"))+
#   theme(strip.background=element_blank(), 
#         strip.text.x=element_text(size=10),
#         strip.text.y=element_blank())
# 
# #print(gg)
# 
# ggsave(plot = gg, "CLMapBlue.pdf", width=7, height=5)