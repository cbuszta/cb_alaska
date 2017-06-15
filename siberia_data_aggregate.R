#########################
#Data Joins and Aggregation
#Siberia 2016-2017
#Claudia Buszta
#June 5 2017
#########################

require('plyr')
#require('xlsx')
require('lubridate')
# clear workspace
rm(list=ls())

setwd('C:\\Users\\Dell Computer\\Documents\\Research\\siberia')
#read in data
hdAC <- read.table("Highdensity_abovecanopy.csv", skip= 4, header= T, sep=",", na.strings = c("NAN","Nan", "INF"))
hdSHF <- read.table("highdensity_soilheatflux.csv", skip= 4, header=T, sep=",", na.strings = c("NAN","Nan", "INF"))
hdSHF2 <- read.table("highdensity_soilheatflux2.csv", skip= 4, header= T, sep=",", na.strings = c("NAN", "Nan", "INF"))
colnames(hdSHF2)[2] <- "avg1"
colnames(hdSHF2)[3] <- "avg2"
hdUS <- read.table("highdensity_understory.csv",  skip= 4, header= T, sep=",", na.strings = c("NAN", "Nan", "INF"))

ldAC <- read.table("Lowdensity_abovecanopy.csv", skip= 3, header= T, sep=",", na.strings = c("NAN","Nan", "INF"))
ldSHF <- read.table("Lowdensity_soilheatflux.csv", skip= 4, header= T, sep=",", na.strings = c("NAN","Nan", "INF"))
ldSHF2 <- read.table("lowdensity_soilheatflux2.csv", skip= 4, header= T, sep=",", na.strings = c("NAN", "Nan", "INF"))
#fix column names for join later
colnames(ldSHF2)[2] <- "avg1"
colnames(ldSHF2)[3] <- "avg2"
ldUS <- read.table("Lowdensity_understory.csv", skip= 4, header= T, sep=",", na.strings = c("NAN", "Nan", "INF"))

#format timestamps for joins
hdAC$TIME <- as.POSIXct(hdAC$TIMESTAMP, format="%m/%d/%Y %H:%M")
hdAC$Doy <- yday(hdAC$TIME)
hdAC$year <- year(hdAC$TIME)
hdAC$hour <- hour(hdAC$TIME)
hdAC$minute <- minute(hdAC$TIME)

hdSHF$TIME <- as.POSIXct(hdSHF$TIMESTAMP, format="%m/%d/%Y %H:%M")
hdSHF$Doy <- yday(hdSHF$TIME)
hdSHF$year <- year(hdSHF$TIME)
hdSHF$hour <- hour(hdSHF$TIME)
hdSHF$minute <- minute(hdSHF$TIME)

hdSHF2$TIME <- as.POSIXct(hdSHF2$TIMESTAMP, format="%m/%d/%Y %H:%M")
hdSHF2$Doy <- yday(hdSHF2$TIME)
hdSHF2$year <- year(hdSHF2$TIME)
hdSHF2$hour <- hour(hdSHF2$TIME)
hdSHF2$minute <- minute(hdSHF2$TIME)

hdUS$TIME <- as.POSIXct(hdUS$TIMESTAMP, format="%m/%d/%Y %H:%M")
hdUS$Doy <- yday(hdUS$TIME)
hdUS$year <- year(hdUS$TIME)
hdUS$hour <- hour(hdUS$TIME)
hdUS$minute <- minute(hdUS$TIME)

ldAC$TIME <- as.POSIXct(ldAC$TIMESTAMP, format="%m/%d/%Y %H:%M")
ldAC$Doy <- yday(ldAC$TIME)
ldAC$year <- year(ldAC$TIME)
ldAC$hour <- hour(ldAC$TIME)
ldAC$minute <- minute(ldAC$TIME)

ldSHF$TIME <- as.POSIXct(ldSHF$TIMESTAMP, format="%m/%d/%Y %H:%M")
ldSHF$Doy <- yday(ldSHF$TIME)
ldSHF$year <- year(ldSHF$TIME)
ldSHF$hour <- hour(ldSHF$TIME)
ldSHF$minute <- minute(ldSHF$TIME)

ldSHF2$TIME <- as.POSIXct(ldSHF2$TIMESTAMP, format="%m/%d/%Y %H:%M")
ldSHF2$Doy <- yday(ldSHF2$TIME)
ldSHF2$year <- year(ldSHF2$TIME)
ldSHF2$hour <- hour(ldSHF2$TIME)
ldSHF2$minute <- minute(ldSHF2$TIME)

ldUS$TIME <- as.POSIXct(ldUS$TIMESTAMP, format="%m/%d/%Y %H:%M")
ldUS$Doy <- yday(ldUS$TIME)
ldUS$year <- year(ldUS$TIME)
ldUS$hour <- hour(ldUS$TIME)
ldUS$minute <- minute(ldUS$TIME)

#join shf files
ldSHFcomp <- join(ldSHF,ldSHF2, by=c("Doy","hour","minute"), type="full")
hdSHFcomp <- join(hdSHF, hdSHF2, by=c("Doy","hour","minute"), type="full")

#avg all shf values
ldSHFcomp$shfmean <- rowMeans(ldSHFcomp[,c("shf_Avg.1.","shf_Avg.2.","shf_Avg.3.","shf_Avg.4.","avg1","avg2")])

plot(ldSHFcomp$TIME,ldSHFcomp$shfmean,
     type="l",
     xlab="time", ylab="SHF")
