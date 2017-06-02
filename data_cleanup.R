#########################
#Data clean up and Joins
#Toolik Alaska 2016
#Claudia Buszta
#May 27 2017
#########################

require('plyr')
#require('xlsx')
require('lubridate')
# clear workspace
rm(list=ls())

setwd('C:\\Users\\Dell Computer\\Documents\\Research')

#read in campbell loggers, create timestamps
tow.shrub <- read.table("shrub_all(2).csv",header=T, sep = ",", na.strings = c("NAN","NA","-7999","INF"))
tow.shrub$TIME <- as.POSIXct(tow.shrub$TIMESTAMP, format="%m/%d/%Y %H:%M")
tow.shrub$Doy <- yday(tow.shrub$TIME)
tow.shrub$year <- year(tow.shrub$TIME)
tow.shrub$hour <- hour(tow.shrub$TIME)
tow.shrub$minute <- minute(tow.shrub$TIME)
tow.shrub <- cbind(tow.shrub[,1:29],tow.shrub[,51:55])

tow.tus <- read.table("tussock_all(2).csv",header=T, sep = ",",na.strings = c("NAN","NA","-7999","INF"))
tow.tus$TIME <- as.POSIXct(tow.tus$TIMESTAMP, format="%m/%d/%Y %H:%M")
tow.tus$Doy <- yday(tow.tus$TIME)
tow.tus$year <- year(tow.tus$TIME)
tow.tus$hour <- hour(tow.tus$TIME)
tow.tus$minute <- minute(tow.tus$TIME)

#read in decagon loggers, create timestamps
dec.shr <- read.csv("CombinedShrub2.csv")
dec.shr$TIME <- as.Date(dec.shr$Measurement.Time, format="%m/%d/%Y %I:%M %p")
dec.shr$Doy <- yday(dec.shr$TIME)
dec.shr$year <- year(dec.shr$TIME)
dec.shr <- dec.shr[-c(20387:25873), ] #clear a bunch of NA rows at the end

dec.tus <- read.csv("CombinedTussock2.csv",header=T)
dec.tus$TIME <- as.POSIXct(dec.tus$Measurement.Time, format="%m/%d/%Y %I:%M %p")
dec.tus$Doy <- yday(dec.tus$TIME)
dec.tus$year <- year(dec.tus$TIME)

memory.limit(size = 100000)

#Join loggers for shrub files and tussock files
shrubjoin <- join(dec.shr, tow.shrub, by=c("Doy","hour","minute"), type="full")
tussjoin <- join(dec.tus, tow.tus, by=c("Doy","hour","minute"), type="full")
head(dec.shr)
head(tow.shrub)

##screen the albedo data using 10th and 90th percentiles
sq <- quantile(shrubjoin$Albedo_Avg, probs=c(.1,.9),na.rm=T)
tq <- quantile(tussjoin$Albedo_Avg, probs=c(.1,.9),na.rm=T)

shrubjoin$Albedo_Avg[shrubjoin$Albedo_Avg>sq[2]] <- NA
tussjoin$Albedo_Avg[tow.tus$Albedo_Avg<tq[1]] <- NA

## convert the longwave radiation to surface temp using 
## stefan-boltzman equation
pc <- 5.670373*10^-8
e <- 0.95
shrubjoin$Tsurf <- (((shrubjoin$IR01DnCo_Avg)/(pc*e))^(1/4))-273.15
tussjoin$Tsurf <- (((tussjoin$IR01DnCo_Avg)/(pc*e))^(1/4))-273.15

#save original joined files
write.table(tussjoin,file="tussJoin.csv",sep=",",row.names = F)
write.table(shrubjoin,file="shrubJoin.csv",sep=",",row.names = F)


write.table(shrubjoin[c(0:10000),], file = "shrubJoin.csv", sep=",", row.names = F)

while (nrow(shrubjoin) > 10000){
  write.table(shrubjoin[c(0:10000),], file = "shrubJoin.csv", append = TRUE, sep=",", row.names = F)
  shrubjoin <- shrubjoin[-c(0:10000),]
}



#aggregate joined files daily
#aggregate joined files half-hourly