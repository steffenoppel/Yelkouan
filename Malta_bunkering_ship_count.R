#########################################################################################################################
##### COUNT THE NUMBER OF BUNKERING SHIPS PER NIGHT IN MALTA ####################################
#########################################################################################################################
### forked from 'RFID_bunkering_analysis_June2018.R' by steffen.oppel@rspb.org.uk in June 2018
### includes data sent by Martin Austad in June 2018

### modified on 18 June 2018 to count ships specifically between sunrise and sunset
### revised count to count individual ships only once, not repeatedly
### NOTE: cannot subset 'interval'-class data with 'filter()' in pipe (%>%) as for some reason these data won't be filtered and there is a data length mismatch

library(data.table)
library(tidyverse)
library(lubridate)
library(oce)
library(maptools)




################################################################################################################
###################### LOAD SHIP BUNKERING FROM MALTA   ######################################################
################################################################################################################

setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Malta\\Raw_data\\LightPollution")
setwd("C:\\STEFFEN\\RSPB\\Malta\\Raw_data\\LightPollution")

ships116 <- fread("2016_Area1_Bunkering.csv",fill=TRUE)
ships117 <- fread("2017_Area1_Bunkering.csv",fill=TRUE)
ships117$PortInMalta<-NULL
ships616 <- fread("2016_Area6_Bunkering.csv",fill=TRUE)
ships617 <- fread("2017_Area6_Bunkering.csv",fill=TRUE)
ships617$PortInMalta<-NULL


ships<-rbind(ships116,ships117,ships616,ships617)
ships <- ships %>%
  filter(PurposeOfCall=="BUNKERS") %>%
  mutate(Arrival=dmy_hm(paste(ArrivalDate, ArrivalTime,sep=" "),tz = "GMT")) %>%
  mutate(Year=year(Arrival)) %>%
  mutate(DepartureL=dmy_hm(paste(DepartureDate, DepartureTime,sep=" "))) %>%
  mutate(Departure=if_else(is.na(DepartureL),Arrival+hours(8),DepartureL)) %>%   ### FOR THREE SHIPS NO DEPARTURE TIME IS GIVEN SO WE ASSUME 8 HRS
  mutate(Arrival=with_tz(Arrival,tz = "UTC")) %>%
  mutate(Departure=with_tz(Departure,tz="UTC")) %>%
  mutate(duration=difftime(Departure,Arrival,units='hours')) %>%
  mutate(presence=interval(Arrival,Departure))%>%
  select(VesselName,Year,BerthInMalta,Arrival,Departure,duration,presence)

head(ships)
tail(ships)
dim(ships)

## check whether there are missing data
ships %>% filter(is.na(duration))

## check the longest mooring ships
ships %>% arrange(desc(duration))  %>% filter(duration>24)




################################################################################################################
###### ASSESS PROPORTION OF SHIPS THAT WERE PRESENT FOR AT LEAST ONE NIGHT  ##############################
################################################################################################################

## purpose of this analysis is to assess the economic impact of night-closure of areas 1 and 6
## irrelevant how many nights a ship was present, as assessment is based on number of individual ships

#### CREATE SUNSET AND SUNRISE INTERVALS
## requires complicated rules for ships that arrive before/after midnight and sunset/rise

NIGHTSHIPS <- ships %>%
  mutate(sunset=sunriset(matrix(c(14.3,35.9),ncol=2), as.POSIXct(Arrival, tz="UTC"),direction=c("sunset"), POSIXct.out=T)[,2])%>%
  mutate(sunrise=sunriset(matrix(c(14.3,35.9),ncol=2), as.POSIXct(Departure, tz="UTC"),direction=c("sunrise"), POSIXct.out=T)[,2])%>%
  mutate(sunset=with_tz(sunset,tz = "UTC")) %>%
  mutate(sunrise=with_tz(sunrise,tz = "UTC")) %>%
  
  ## the following rule checks ships arriving and departing on same day, and then defines the night as either before or after depending on whether arrival was before or after sunrise
  mutate(NIGHT=if_else(sunset>sunrise,if_else(Arrival>sunrise,interval(sunset,sunrise+days(1)),interval(sunset-days(1),sunrise)),interval(sunset,sunrise))) %>%
  mutate(ATNIGHT=int_overlaps(presence,NIGHT))
head(NIGHTSHIPS)

### CHECK THAT SHIPS NOT THERE AT NIGHT ARRIVED AND DEPARTED ON SAME DAY
NIGHTSHIPS %>% filter(ATNIGHT==FALSE) %>% select(VesselName,Arrival,Departure,sunset,sunrise)

### PRODUCE THE TABLE FOR REPORT ###
NIGHTSHIPS %>% mutate(count=1) %>%
  mutate(nightcount=ifelse(ATNIGHT==TRUE,1,0)) %>%
  group_by(BerthInMalta,Year) %>%
  summarise(Total=sum(count), Night=sum(nightcount)) 





################################################################################################################
###### SET DAILY TIME SERIES AND COUNT NUMBER OF SHIPS PER NIGHT  ##############################
################################################################################################################
## The above number does not consider the fact that some ships were present for multiple nights
## The following analysis counts the number of 'ship-nights', which is more relevant for shearwaters

#### CREATE TIME SERIES FOR EACH YEAR #####
TOTALTIME2016<-seq(min(ships$Arrival[ships$Year==2016]),max(ships$Departure[ships$Year==2016]),'1 day')
TOTALTIME2017<-seq(min(ships$Arrival[ships$Year==2017]),max(ships$Departure[ships$Year==2017]),'1 day')
TOTALTIME <- data.frame(NightStarting=c(TOTALTIME2016,TOTALTIME2017))


#### CREATE SUNSET AND SUNRISE INTERVALS
TOTALTIME<- TOTALTIME %>% mutate(NightEnding=NightStarting+days(1)) %>%
  mutate(sunset=sunriset(matrix(c(14.3,35.9),ncol=2), as.POSIXct(NightStarting, tz="UTC"),direction=c("sunset"), POSIXct.out=T)[,2])%>%
  mutate(sunrise=sunriset(matrix(c(14.3,35.9),ncol=2), as.POSIXct(NightEnding, tz="UTC"),direction=c("sunrise"), POSIXct.out=T)[,2])%>%
  mutate(sunset=with_tz(sunset,tz = "UTC")) %>%
  mutate(sunrise=with_tz(sunrise,tz = "UTC")) %>%
  mutate(NIGHT=interval(sunset,sunrise)) %>%
  mutate(ALL=interval(sunset,sunset+days(1)))
head(TOTALTIME)

#### CREATE DATA FRAME WITH ALL NIGHTLY TIME SLOTS FOR EACH AREA
monitornights1<- TOTALTIME %>%
  mutate(Year=year(NightStarting)) %>%
  mutate(BerthInMalta="AREA 1")
monitornights6<- TOTALTIME %>%
  mutate(Year=year(NightStarting)) %>%
  mutate(BerthInMalta="AREA 6")
monitornights<-rbind(monitornights1, monitornights6)
head(monitornights)
dim(monitornights)


#### FOR EACH NIGHT ASSESS NUMBER OF SHIPS PRESENT ###
monitornights$ALL_ships<-0
monitornights$NIGHT_ships<-0

for (n in 1:length(monitornights$NIGHT)){
  loc<-monitornights$BerthInMalta[n]
  shipsthisday<-int_overlaps(ships$presence[ships$BerthInMalta==loc],monitornights$ALL[n])
  shipsthisnight<-int_overlaps(ships$presence[ships$BerthInMalta==loc],monitornights$NIGHT[n])
  monitornights$ALL_ships[n]<- sum(shipsthisday, na.rm=T)
  monitornights$NIGHT_ships[n]<- sum(shipsthisnight, na.rm=T)
}


fwrite(monitornights[,c(8,7,1:4,9,10)],"Malta_N_bunkering_ships_per_night.csv")


#### PRODUCE TABLE FOR REPORT ###

monitornights %>% 
  group_by(BerthInMalta, Year) %>%
  summarise(N=sum(ALL_ships),N_NIGHT=sum(NIGHT_ships)) 






################################################################################################################
###### SET HOURLY TIME SERIES AND COUNT NUMBER OF SHIPS PER HOUR  ##############################
################################################################################################################

## similar analysis as above, but much higher resolution to relate to shearwater activity at hourly intervals

#### MAKE HOURLY TIME SERIES #####
TOTALTIME2016<-seq(min(ships$Arrival[ships$Year==2016]),max(ships$Departure[ships$Year==2016]),'1 hour')
TOTALTIME2017<-seq(min(ships$Arrival[ships$Year==2017]),max(ships$Departure[ships$Year==2017]),'1 hour')
TOTALTIME <- data.frame(Timestamp=c(TOTALTIME2016,TOTALTIME2017),NightStarting=0, ships=0)

#### CREATE DATA FRAME WITH ALL HOURLY TIME SLOTS FOR EACH AREA
monitornights1<- TOTALTIME %>%
  mutate(Year=year(Timestamp), Hour=hour(Timestamp)) %>%
  mutate(NightStarting=if_else(hour(Timestamp)>12,as.Date(Timestamp),as.Date(Timestamp-(24*3600)))) %>%
  mutate(BerthInMalta="AREA 1")
monitornights6<- TOTALTIME %>%
  mutate(Year=year(Timestamp), Hour=hour(Timestamp)) %>%
  mutate(NightStarting=if_else(hour(Timestamp)>12,as.Date(Timestamp),as.Date(Timestamp-(24*3600)))) %>%
  mutate(BerthInMalta="AREA 6")
monitornights<-rbind(monitornights1, monitornights6)
head(monitornights)



#### FOR EACH HOURLY TIME SLOT ASSESS WHETHER A SHIP WAS THERE ###

for (n in 1:length(monitornights$Timestamp)){
  x<-interval(monitornights$Timestamp[n],monitornights$Timestamp[n]+hours(1))
  loc<-monitornights$BerthInMalta[n]
  shipsthisnight<-int_overlaps(ships$presence[ships$BerthInMalta==loc],x)
  monitornights$ships[n]<- sum(shipsthisnight, na.rm=T)
}

fwrite(monitornights,"Malta_N_bunkering_ships_per_hour.csv")


#### SUMMARISE THE NUMBER OF SHIPS BY NIGHT, YEAR, AND AREA  ###

monitornights %>% group_by(BerthInMalta, NightStarting) %>%
  summarise(N_ships=max(ships))


  