#########################
#Data clean up and Joins
#Toolik Alaska 2016
#Claudia Buszta
#May 27 2017
#########################

require('stringi')
require('plyr')
#require('xlsx')
require('lubridate')
# clear workspace
rm(list=ls())

setwd('C:\\Users\\Dell Computer\\Documents\\Research')

#read in campbell loggers, create timestamps
tow.shrub <- read.table("shrub_all(2).csv",header=T, sep = ",", na.strings = c("NAN","Nan","NA","-7999","INF"))
tow.shrub$TIME <- as.POSIXct(tow.shrub$TIMESTAMP, format="%m/%d/%Y %H:%M")
tow.shrub$Doy <- yday(tow.shrub$TIME)
tow.shrub$year <- year(tow.shrub$TIME)
tow.shrub$hour <- hour(tow.shrub$TIME)
tow.shrub$minute <- minute(tow.shrub$TIME)
tow.shrub <- cbind(tow.shrub[,1:29],tow.shrub[,51:55])

tow.tus <- read.table("tussock_all(2).csv",header=T, sep = ",",na.strings = c("NAN","Nan","NA","-7999","INF"))
tow.tus$TIME <- as.POSIXct(tow.tus$TIMESTAMP, format="%m/%d/%Y %H:%M")
tow.tus$Doy <- yday(tow.tus$TIME)
tow.tus$year <- year(tow.tus$TIME)
tow.tus$hour <- hour(tow.tus$TIME)
tow.tus$minute <- minute(tow.tus$TIME)

#read in decagon loggers, create timestamps
dec.shr <- read.csv("CombinedShrub2.csv", header=T, na.strings = c("NAN","Nan","NA","-7999","INF"))
dec.shr$TIME <- as.POSIXct(dec.shr$Measurement.Time, format="%m/%d/%Y %I:%M %p")
dec.shr$Doy <- yday(dec.shr$TIME)
dec.shr$year <- year(dec.shr$TIME)
dec.shr$hour <- hour(dec.shr$TIME)
dec.shr$minute <- minute(dec.shr$TIME)

dec.shr <- dec.shr[1:20386, ] #clear a bunch of NA rows at the end

dec.tus <- read.csv("CombinedTussock2.csv",header=T, na.strings = c("NAN","Nan","NA","-7999","INF"))
dec.tus$TIME <- as.POSIXct(dec.tus$Measurement.Time, format="%m/%d/%Y %I:%M %p")
dec.tus$Doy <- yday(dec.tus$TIME)
dec.tus$year <- year(dec.tus$TIME)
dec.tus$hour <- hour(dec.tus$TIME)
dec.tus$minute <- minute(dec.tus$TIME)

#adjusting for join; aggregate every 5 minutes
tow.tus$tens <- tow.tus$minute%/%10
tow.tus$ones <- as.character(tow.tus$minute)
tow.tus$ones <- as.numeric(stringi::stri_sub(tow.tus$ones, from=-1, to=-1))
tow.tus$five[tow.tus$ones <= 4] <- 0
tow.tus$five[tow.tus$ones > 4] <- 5
tow.tus$ones <- (tow.tus$tens*10 + tow.tus$five)
tow.tus$TIME <- as.POSIXct(paste(tow.tus$year, tow.tus$Doy, tow.tus$hour, tow.tus$ones), format="%Y %j %H %M")
t.f <- substr(tow.tus$TIME,1,19)
tow.tus2 <- aggregate(tow.tus[,c(2:38)], by =list(t.f), FUN=mean, na.rm=T)

tow.shrub$tens <- tow.shrub$minute%/%10
tow.shrub$ones <- as.character(tow.shrub$minute)
tow.shrub$ones <- as.numeric(stringi::stri_sub(tow.shrub$ones, from=-1, to=-1))
tow.shrub$five[tow.shrub$ones <=4] <- 0
tow.shrub$five[tow.shrub$ones > 4] <- 5
tow.shrub$ones <- (tow.shrub$tens *10 +tow.shrub$five)
tow.shrub$TIME <- as.POSIXct(paste(tow.shrub$year, tow.shrub$Doy, tow.shrub$hour, tow.shrub$ones), format = "%Y %j %H %M")
s.f <- substr(tow.shrub$TIME,1,19)
tow.shrub2 <- aggregate(tow.shrub[,c(2:37)], by =list(s.f), FUN=mean, na.rm=T)

#adjust memmory for file processes
memory.limit(size = 100000)

#Join loggers for shrub files and tussock files
shrubjoin <- join(dec.shr, tow.shrub2, by=c("Doy","hour","minute"), type="full")
tussjoin <- join(dec.tus, tow.tus2, by=c("Doy","hour","minute"), type="full")

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

saveRDS(shrubjoin, "shrubJoin.rds")
saveRDS(tussjoin, "tussJoin.rds")


plot(tussjoin$TIME,tussjoin$Albedo_Avg,
     type ='l', # line graph
     ylim=c(0,0.4), #set y axis limits
     xlab="", ylab="Albedo") #axis label
