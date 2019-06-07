library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(readr)
library(dplyr)
library(viridis)
library(ggpubr)
library(ggthemes)
library(ggridges)
library(RColorBrewer)
options("scipen" = 100)

setwd("C:/Users/user/Desktop/I/ewha2/Graphics/Project/Data")
hour <- read.csv("CALL_Hourly_TOTAL.csv")
call <- read.csv("CALL_TOTAL.csv",stringsAsFactors = FALSE)
accident <- read_csv("C:/Users/user/Desktop/I/ewha2/DataMining/Project/traffic/data/motorcycle_accident.csv")
colnames(accident) <- c("gu","sum.accident")


ll <- read.csv('seoul.csv')
rr <- read_csv('id_seoul_gu.csv')
dong <- read_csv('id_dong.csv')
dong <- dong[,-3]
colnames(dong) <- c("id","dong","lat","lon")

### delivery ###
call <- call %>% group_by(gu) %>% summarise(sum.call = sum(call, na.rm = TRUE))
total <- merge(call,rr,by='gu',all=FALSE)
seoul <- merge(ll, total, by='id')
ggplot() + theme_void() +
  scale_fill_gradientn(colours=brewer.pal(5,"YlGnBu")) + 
  geom_polygon(data=seoul,alpha=.75,
                 aes(x=long, y=lat, group=group, fill=sum.call)) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5,size=20))

### motorcycle accident ### 
ggplot() + theme_void() +
  geom_polygon(data=seoul,alpha=.75,
               aes(x=long, y=lat, group=group, fill=sum.accident)) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5,size=20)) +
  scale_fill_gradientn(colours=brewer.pal(5,"YlOrRd"))

### hour ###
hour <- hour %>% group_by(time) %>% summarise(sum.call = sum(call, na.rm = TRUE))
ggplot(hour) + geom_line(aes(time,sum.call)) + theme_hc() + labs(x='시간',y='') +
  annotate("rect", xmin=0, xmax=9, ymin=0, ymax=Inf, alpha=0.1, fill="red")