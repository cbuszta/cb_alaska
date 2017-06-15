################################
# filter and aggregate
# Claudia's Toolik data from 
# Summer 2016
# 
# MML 2 Dec 2016
# CB 2017
################################

# load necessary packages (install first, if needed)
require('plyr')
require('xlsx')
require('lubridate')
# clear workspace
rm(list=ls())

# set working directory
#setwd('/Users/mloranty/Google Drive/Documents/Research/student_projects/Summer_2016/Buszta/aggregate/')
setwd('C:\\Users\\Dell Computer\\Documents\\Research')

#pull out half hour into single column and then aggregate
tussall <- readRDS("tussJoin.rds")
shrall <- readRDS("shrubJoin.rds")

#aggregate to daily
t.day <- substr(tussall$Doy,1,3)
tussall_daily <- aggregate(tussall[,c(2:12,15,16,22:48)], by=list(t.day), FUN=mean,na.rm=T)

s.day <- substr(shrall$Doy,1,3)
shrall_daily <- aggregate(shrall[,c(2:11,15,21:46)], by=list(s.day), FUN=mean,na.rm=T)

#half-hour
tussall$hhr[tussall$minute>=30] <- 30
tussall$hhr[tussall$minute<30] <- 0

tussall$TIMEhhr <- as.POSIXct(paste(tussall$year, tussall$Doy, tussall$hour, tussall$hhr), format="%Y %j %H %M")
t.hhr <- substr(tussall$TIMEhhr,1,19)
tussall_hhr <- aggregate(tussall[,c(2:12,16,22:50)], by=list(t.hhr), FUN=mean, na.rm=T)

shrall$hhr[shrall$minute>=30] <- 30
shrall$hhr[shrall$minute<30] <- 0

shrall$TIMEhhr <- as.POSIXct(paste(shrall$year, shrall$Doy, shrall$hour, shrall$hhr), format="%Y %j %H %M")
s.hhr <- substr(shrall$TIMEhhr,1,19)
shrall_hhr <- aggregate(shrall[,c(2:11,15,22:48)], by=list(s.hhr), FUN=mean, na.rm=T)

# now we can make some plots with the dates on axes
plot(tussall_daily$TIME, tussall_daily$NDVI.2,
     type = "l",
     ylim=c(0,1),
     xlab="", ylab="NDVI", col = "blue")

plot(all.hr$date,all.hr$Albedo_Avg.shrub,
     type ='l', # line graph
     ylim=c(0.1,0.25), #set y axis limits
     xlab="", ylab="Albedo") #axis label
lines(all.hr$date,all.hr$Albedo_Avg.tus,col='red')

# plot soil heat flux, showing the mean from both plates

plot(all.hr$date,rowMeans(all.hr[,3:4]),
     type ='l',
     ylab="Soil Heat Flux")

lines(all.hr$date,rowMeans(all.hr[,7:8]),col='red')

# or plot the difference
plot(all.hr$date,rowMeans(all.hr[,7:8])-rowMeans(all.hr[,3:4]),
     ylim=c(-10,10),type='l')
abline(h=0)

plot(all.day$date,rowMeans(all.day[,7:8])-rowMeans(all.day[,3:4]),
     ylim=c(-10,10),type='l')
abline(h=0)

## surface temp
plot(all.hr$date,all.hr$Tsurf.shrub,
     type='l')
lines(all.hr$date,all.hr$Tsurf.tus,col='red')

plot(all.hr$date,all.hr$Tsurf.tus-all.hr$Tsurf.shrub, type='l')
abline(h=0)