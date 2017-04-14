################################
# filter and aggregate
# Claudia's Toolik data from 
# Summer 2016
# 
# MML 2 Dec 2016
################################

#new comment, practice Github

# load necessary packages (install first, if needed)
require('plyr')
#require('xlsx')
require('lubridate')

# clear workspace
rm(list=ls())

# set working directory
#setwd('/Users/mloranty/Google Drive/Documents/Research/student_projects/Summer_2016/Buszta/aggregate/')
setwd('C:\\Users\\Dell Computer\\Documents\\Research')

# read in the data from both towers
tow.shrub <- read.csv("shrub_all.csv",header=T,as.is=TRUE)
tow.tus <- read.csv("tussock_all.csv",header=T)

dec.shr <- read.csv("CombinedShrub2.csv")
dec.shr$TIME <- as.Date(dec.shr$Measurement.Time, format="%m/%d/%Y %I:%M %p")
dec.shr$Doy <- yday(dec.shr$TIME)
dec.shr$year <- year(dec.shr$TIME)


dec.tus <- read.csv("CombinedTussock2.csv",header=T)
dec.tus$TIME <- as.POSIXct(dec.tus$Measurement.Time, format="%m/%d/%Y %I:%M %p")
dec.tus$Doy <- yday(dec.tus$TIME)
dec.tus$year <- year(dec.tus$TIME)

joined <- join(dec.shr, dec.tus, by=c("Doy","hour","minute"), type="full")

##screen the albedo data using 10th and 90th percentiles
sq <- quantile(tow.shrub$Albedo_Avg,probs=c(.1,.9),na.rm=T)
tq <- quantile(tow.tus$Albedo_Avg,probs=c(.1,.9),na.rm=T)

tow.shrub$Albedo_Avg[tow.shrub$Albedo_Avg<sq[1]] <- NA
tow.shrub$Albedo_Avg[tow.shrub$Albedo_Avg>sq[2]] <- NA

tow.tus$Albedo_Avg[tow.tus$Albedo_Avg<tq[1]] <- NA
tow.tus$Albedo_Avg[tow.tus$Albedo_Avg>tq[2]] <- NA

## convert the longwave radiation to surface temp using 
## stefan-boltzman equation
pc <- 5.670373*10^-8
e <- 0.95

tow.shrub$Tsurf <- (((tow.shrub$IR01DnCo_Avg)/(pc*e))^(1/4))-273.15
tow.tus$Tsurf <- (((tow.tus$IR01DnCo_Avg)/(pc*e))^(1/4))-273.15

# create a vector of unique yr-mon-day hours
s.hr <- substr(tow.shrub$TIMESTAMP,1,13)
s.day <- substr(tow.shrub$TIMESTAMP,1,10)

t.hr <- substr(tow.tus$TIMESTAMP,1,13)
t.day <- substr(tow.tus$TIMESTAMP,1,10)

tstamp <- as.POSIXlt(tow.shrub$TIMESTAMP, format= "%m/%d/%Y %H:%M")
#tstamp
#pull out half hour into single column and then aggregate

s.hhr <- substr(tow.shrub$TIMESTAMP,1,13)
t.hhr <- substr(tow.shrub$TIMESTAMP,1,13)
test1 <- tow.shrub$TIMESTAMP
test1

# now aggregate to hourly & date

tow.shrub.hr <- aggregate(tow.shrub[,c(9,17,23:24,30)],by=list(s.hr),FUN=mean,na.rm=T)
colnames(tow.shrub.hr)[2:6] <- paste(colnames(tow.shrub.hr)[2:6],'shrub',sep='.')

tow.shrub.day <- aggregate(tow.shrub[,c(9,17,23:24,30)],by=list(s.day),FUN=mean,na.rm=T)
colnames(tow.shrub.day)[2:6] <- paste(colnames(tow.shrub.day)[2:6],'shrub',sep='.')

tow.tus.hr <- aggregate(tow.tus[,c(9,17,23:24,31)],by=list(t.hr),FUN=mean,na.rm=T)
colnames(tow.tus.hr)[2:6] <- paste(colnames(tow.tus.hr)[2:6],'tus',sep='.')

tow.tus.day <- aggregate(tow.tus[,c(9,17,23:24,31)],by=list(t.day),FUN=mean,na.rm=T)
colnames(tow.tus.day)[2:6] <- paste(colnames(tow.tus.day)[2:6],'tus',sep='.')

#Claudia

tow.shrub.hhr <- aggregate(tow.shrub[,c(9,17,23:24,30)],by=list(s.hhr),FUN=mean,na.rm=T)
colnames(tow.shrub.hr)[2:6] <- paste(colnames(tow.shrub.hhr)[2:6],'shrub',sep='.')

tow.tus.hhr <- aggregate(tow.tus[,c(9,17,23:24,31)],by=list(t.hhr),FUN=mean,na.rm=T)
colnames(tow.tus.day)[2:6] <- paste(colnames(tow.tus.hhr)[2:6],'tus',sep='.')


#now the Decagon loggers
dec.shr.hr <- aggregate(dec.shr[,c(4,8:11)],by=list(substr(dec.shr$TIME,1,13)),
                        FUN=mean,na.rm=T)
colnames(dec.shr.hr)[2:6] <- paste(colnames(dec.shr.hr)[2:6],'shrub',sep='.')

dec.shr.day <- aggregate(dec.shr[,c(4,8:11)],by=list(substr(dec.shr$TIME,1,10)),
                        FUN=mean,na.rm=T)
colnames(dec.shr.day)[2:6] <- paste(colnames(dec.shr.day)[2:6],'shrub',sep='.')

dec.tus.hr <- aggregate(dec.tus[,c(2:3,9:12)],by=list(substr(dec.tus$TIME,1,13)),
                        FUN=mean,na.rm=T)
colnames(dec.tus.hr)[2:3] <- paste(colnames(dec.tus.hr)[2:3],'tus',sep='.')

dec.tus.day <- aggregate(dec.tus[,c(2:3,9:12)],by=list(substr(dec.tus$TIME,1,10)),
                        FUN=mean,na.rm=T)
colnames(dec.tus.day)[2:3] <- paste(colnames(dec.tus.day)[2:3],'tus',sep='.')

all.hr <- join(tow.shrub.hr,tow.tus.hr,type='inner')
all.hr <- join(all.hr,dec.tus.hr,type='inner')
all.hr <- join(all.hr,dec.shr.hr,type='inner')

all.day <- join(tow.shrub.day,tow.tus.day,type='inner')
all.day <- join (all.day, dec.tus.day,type='inner')
all.day <- join (all.day, dec.shr.day,type='inner')

#Claudia
all.hhr <- join(tow.shrub.hhr,tow.tus.hhr,type='inner')
all.hhr <- join(all.hhr,dec.tus.hhr,type='inner')
all.hhr <- join(all.hhr,dec.shr.hhr,type='inner')

# make a date column that is actually a date
all.hr$date <- as.POSIXct(all.hr$Group.1, format="%Y-%m-%d %H")
all.day$date <- as.POSIXct(all.day$Group.1, format="%Y-%m-%d")
#make a hhr date column
all.hhr$date <- as.POSIXct(all.hhr$Group.1, format="%Y-%m-%d %M")

## write these tables to files ##
write.table(all.hr,file="hourly_energy_data.csv",sep=",",row.names = F)
write.table(all.day,file="daily_energy_data.csv",sep=",",row.names = F)
#me again
write.table(all.hhr,file="half_hourly_data.csv",sep=",",row.names = F)

# now we can make some plots with the dates on axes
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