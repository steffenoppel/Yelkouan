#########################################################################################################################
##### COMPARE COLONY ATTENDANCE TO BUNKERING DATA OF YELKOUAN SHEARWATER IN MALTA ####################################
#########################################################################################################################
### original code to clean RFID data provided by Martin Austad and updated by steffen.oppel@rspb.org.uk until Sept 2017
### trouble-shooting and plotting code in "RFIDdata_cleaning.R"

### updated on 1 Nov 2017 to include ship data and relate movements to bunkering events

## Steffen on 30-01-2018 suggested to look into how many service vessels are available - possible solution is to increase these vessels to finish bunkering before dark. Also to look into economic gain from bunkering areas
##check if RFID and ships are converted to UTC, UTC time is used for SQM

## updated 15 June 2018 to exclude one bird tagged with two RFID tags (8000E1349EA96521 and 8000E1349EA966FC)

library(data.table)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(lubridate)
library(oce)
library(maptools)
library(sp)
library(fasttime)




################################################################################################################
###################### LOAD LOCATION DATA FOR RFID SYSTEM AND SKY QUALITY METER   ##############################
################################################################################################################

setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Malta\\Raw_data\\LightPollution")
#setwd("C:\\STEFFEN\\RSPB\\Malta\\Raw_data\\LightPollution")
#setwd("C:\\Users\\Martin\\Documents\\working_folder\\RFID\\Bunkering_analysis\\data")

locs<-fread("Location_coordinates_SQM_RFID_2017.csv")
locs<-SpatialPoints(locs[,c(8,7)], proj4string=CRS("+proj=longlat +datum=WGS84"))



################################################################################################################
###################### READ IN INDIVIDUAL DEPLOYMENT DATA   ###########################################
################################################################################################################
setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Malta\\Raw_data\\RFID")
#setwd("C:\\STEFFEN\\RSPB\\Malta\\Raw_data\\RFID")

## read in deployments
deployments<-fread("PITdeployments2017.csv")


deployments<-deployments %>%
  select(TagID,PRn,Ring,Deploydate,Age,Sex,Breeding) %>%
  filter(Age==4) %>%  ### REMOVE AGE==3
  filter(TagID!='8000E1349EA966FC') %>%
  mutate(Deploydate=dmy(Deploydate, tz="Europe/Berlin")) %>%

  arrange(Deploydate)

n_deployed<- deployments %>%
  group_by(Deploydate) %>%
  summarise(n=length(unique(TagID)))

## this function allows to calculate the proportion of birds that had been tagged by a given date
taggedby<-function(x){sum(n_deployed$n[n_deployed$Deploydate<x])}



################################################################################################################
###################### YESH COLONY RFID DATA FROM MALTA   ######################################################
################################################################################################################


# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)


raw <- fread("RFID_Eggshellcave_22032017_27072017.txt",fill=TRUE)
#raw <- fread("RFID_data.csv",fill=TRUE)
#raw$DateTime<-fastPOSIXct(paste(raw$Date, raw$Time, " "),tz="GMT",required.components=6L)		### only works if string is in yyyy/mm/dd order

raw <- raw %>%
  mutate(Time=trim.trailing(Time)) %>%
  mutate(DateTimelocal=as.POSIXct(paste(raw$Date, raw$Time, " "), format="%d/%m/%Y %H:%M:%S", tz="Europe/Berlin")) %>%
  mutate(DateTime=with_tz(DateTimelocal, tz = "UTC"))

### INSERT THE MISSING TIME STAMPS ###
raw$DateTime[is.na(raw$DateTime)]<-as.POSIXct(paste(raw$Date[is.na(raw$DateTime)], raw$Time[is.na(raw$DateTime)], " "), format="%d/%m/%Y %H:%M:%S", tz="UTC")

  
#for(i in 1:nrow(raw)){ #this for-loop does not work(and similar versions of it); time was changed on RFID on 29-03-2017 at 15:50 UTC, so the conversion to UTC in line 79 is not correct for the period 26-03-2017 to 29-03-2017; but this period will not be used in analysis anyway
  #if(raw$DateTime[i] > ymd_hms("2017-03-29 17:50:00")){
   # raw$DateTime[i] <- raw$DateTime[i]-dhours(2)}else{
     # raw$DateTime[i] <- raw$DateTime[i]-dhours(1)
    #}
  #}

head(raw)



################################################################################################################
###################### MANIPULATE DATA AND DELETE UNNECESSARY ROWS   ###########################################
################################################################################################################

### ALL DONE IN ONE TIDY COMMAND
RFIDdata <- raw %>%
  filter(DATA == "D") %>%
  filter(TagType!="HW") %>%
  mutate(TagID=ifelse(TagID=='8000E1349EA966FC','8000E1349EA96521',TagID)) %>%   #### replace TagID of bird that was double-tagged
  filter(TagID %in% unique(deployments$TagID)) %>%
  select(DateTime,DATA,Duration,TagType,TagID,Ant,Count,Gap) %>%
  mutate(Gap=as.numeric(Gap))%>%
  arrange(DateTime)
head(RFIDdata)
tail(RFIDdata)

#RFIDdata[is.na(RFIDdata$DateTime),]   ## no missing DateTime data



################################################################################################################
###################### LOAD SHIP BUNKERING FROM MALTA   ######################################################
################################################################################################################

setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Malta\\Raw_data\\LightPollution")
#setwd("C:\\STEFFEN\\RSPB\\Malta\\Raw_data\\LightPollution")

ships <- fread("Malta_ship_bunkering2017.csv",fill=TRUE)
ships <- ships %>%
  filter(BerthInMalta=="AREA 6") %>%
  filter(PurposeOfCall=="BUNKERS") %>%
  mutate(Arrival=dmy_hm(paste(ArrivalDate, ArrivalTime,sep=" "),tz = "Europe/Berlin")) %>% #changed to Europe/Berlin time from GMT, now conversion is done to UTC with_tz function
  mutate(Departure=dmy_hm(paste(DepartureDate, DepartureTime,sep=" "), tz= "Europe/Berlin")) %>%
  mutate(Arrival=with_tz(Arrival,tz = "UTC")) %>%
  mutate(Departure=with_tz(Departure,tz="UTC")) %>%
  mutate(duration=difftime(Departure,Arrival,units='hours')) %>%
  mutate(presence=interval(Arrival,Departure))%>%
  select(VesselName,Arrival,Departure,duration,presence)




head(ships)
ships %>% filter(is.na(Departure))    ## three ships have no departure time


################################################################################################################
###### SPECIFY THE NIGHT TO AVOID GROUPING ISSUES AFTER MIDNIGHT  ##############################
################################################################################################################

midday<-as.POSIXct("12:00:00", format="%H:%M:%S",tz = "GMT")   # create a reference time to split the day
midday<-format(midday, format="%H:%M:%S",tz = "GMT") 

RFIDdata<- RFIDdata %>%
  mutate(DateTime=with_tz(DateTime,tz="UTC")) %>%
  mutate(Time=format(DateTime, format="%H:%M:%S",tz = "UTC")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600))))


#### CALCULATE LENGTH OF EACH MONITORED NIGHT #####
head(RFIDdata)

monitornights<- RFIDdata %>%
  group_by(NightStarting) %>%
  summarise(firstdet=min(DateTime), lastdet=max(DateTime))%>%
  mutate(NightEnding=NightStarting+days(1))%>%
  mutate(sunset=sunriset(locs[1,], as.POSIXct(NightStarting, tz="UTC"),direction=c("sunset"), POSIXct.out=T)[,2])%>%
  mutate(sunrise=sunriset(locs[1,], as.POSIXct(NightEnding, tz="UTC"),direction=c("sunrise"), POSIXct.out=T)[,2])%>%
  mutate(nightdur=interval(sunset,sunrise))%>%
  mutate(ships=0)


#### COUNT THE NUMBER OF SHIPS IN EACH MONITORED NIGHT ###

for (n in 1:length(monitornights$sunrise)){
  x<-monitornights$nightdur[n]
  shipsthisnight<-int_overlaps(ships$presence,x)
  monitornights$ships[n]<- sum(shipsthisnight, na.rm=T)
}



################################################################################################################
###### LOAD AND MANIPULATE THE SKY QUALITY METER DATA   ##############################
################################################################################################################
setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Malta\\Raw_data\\LightPollution")
#setwd("C:\\STEFFEN\\RSPB\\Malta\\Raw_data\\LightPollution")

sqm<-fread("MT24_SQM_alldata.csv")
names(sqm)[7]<-"MAG"

## need to remove all daytime measurements and ACCount for moon phase

SQM<-sqm %>%
  mutate(DateTime=dmy_hms(paste(date_UTC, time_UTC,sep=" "),tz = "UTC")) %>% 
  mutate(Time=format(DateTime, format="%H:%M:%S",tz = "UTC")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600)))) %>%
  filter(volts>4.5) %>%     ## remove locs when battery was dead
  mutate(sunset=crepuscule(locs[3,], DateTime,direction=c("dusk"), solarDep=18, POSIXct.out=T)[,2])%>%
  mutate(sunrise=crepuscule(locs[3,], DateTime,direction=c("dawn"), solarDep=18, POSIXct.out=T)[,2])%>%
  mutate(duringday=if_else(Time>midday,if_else(DateTime<sunset,1,0),if_else(DateTime>sunrise,1,0)))%>%
  filter(duringday==0) %>%
  mutate(prop.illuminated=moonAngle(t=DateTime, longitude=coordinates(locs)[3,1], latitude=coordinates(locs)[3,2])$illuminatedFraction)%>%### This is how 'full the moon is - 1 is full moon, 0 is new moon
  mutate(moon.elevation=moonAngle(t=DateTime, longitude=coordinates(locs)[3,1], latitude=coordinates(locs)[3,2])$altitude) %>%
  mutate(moon.light=prop.illuminated*(moon.elevation/coordinates(locs)[3,2]))

SQM <- SQM %>%
filter (!(NightStarting>="2017/07/24")) 


head(SQM)
tail(SQM)



################################################################################################################
###### QUESTION 1: DOES SHIP BUNKERING AFFECT SKY QUALITY METER DATA?   ##############################
################################################################################################################


### plot variation in darkness with ship bunkering events ###
## PLOT 1 - OVER TIME
shipnights<-monitornights %>% filter(ships>0) %>%select(sunset,ships)
shipnights$sunset <- as.numeric(as.POSIXct(shipnights$sunset)) #changed to as.numeric - graph was not complete 
#fwrite(shipnights,"Malta_bunkering_ships_2017.csv")

#pdf("SQM_season_ships.pdf", width=8, height=6)
SQM %>%
ggplot(aes(x=DateTime, y=MAG))+geom_point(colour="black", size=1.5) +
  geom_vline(aes(xintercept=shipnights$sunset[1]), color='red', size=shipnights$ships[1]/13) +
  geom_vline(aes(xintercept=shipnights$sunset[2]), color='red', size=shipnights$ships[2]/13) +
  geom_vline(aes(xintercept=shipnights$sunset[3]), color='red', size=shipnights$ships[3]/13) +
  geom_vline(aes(xintercept=shipnights$sunset[4]), color='red', size=shipnights$ships[4]/13) +
  geom_vline(aes(xintercept=shipnights$sunset[5]), color='red', size=shipnights$ships[5]/13) +
  geom_vline(aes(xintercept=shipnights$sunset[6]), color='red', size=shipnights$ships[6]/13) +
  geom_vline(aes(xintercept=shipnights$sunset[7]), color='red', size=shipnights$ships[7]/13) +
  geom_vline(aes(xintercept=shipnights$sunset[8]), color='red', size=shipnights$ships[8]/13) +
  geom_vline(aes(xintercept=shipnights$sunset[9]), color='red', size=shipnights$ships[9]/13) +
  geom_vline(aes(xintercept=shipnights$sunset[10]), color='red', size=shipnights$ships[10]/13) +
  geom_vline(aes(xintercept=shipnights$sunset[11]), color='red', size=shipnights$ships[11]/13) +
  geom_vline(aes(xintercept=shipnights$sunset[12]), color='red', size=shipnights$ships[12]/13) +
  geom_vline(aes(xintercept=shipnights$sunset[13]), color='red', size=shipnights$ships[13]/13) +
  xlab("Date") +
  ylab("Darkness of cliff face") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=20), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"),
        legend.background = element_rect(colour = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())


#dev.off()

## PLOT 2 - Darkness against moon - illumination
## need to make symbol size and colour reflect number of bunkering ships
SQM$ships<-0
x<-SQM$DateTime[1]-minutes(10)
x1<-SQM$DateTime[1]
sqmint<-interval(x,x1)
shipsthisinterval<-int_overlaps(ships$presence,sqmint)
SQM$ships[1]<- sum(shipsthisinterval, na.rm=T)

for (n in 2:length(SQM$DateTime)){
  x<-SQM$DateTime[n-1]
  x1<-SQM$DateTime[n]
  sqmint<-interval(x,x1)
  shipsthisinterval<-int_overlaps(ships$presence,sqmint)
  SQM$ships[n]<- sum(shipsthisinterval, na.rm=T)
}

#pdf("SQM_moonlight_ships.pdf", width=8, height=6)
SQM %>% mutate(ships=ships*2) %>%   ### usually 2 boats operate for a bunkering event
  ggplot(aes(x=moon.light, y=MAG))+
  geom_point(aes(size=ships, color=ships)) +
  scale_color_gradient(low="black", high="yellow")+
  scale_size(range=c(0,4)) +
  guides(color=guide_legend(title="Number of ships"), size = guide_legend(title="Number of ships"))+
  geom_vline(aes(xintercept=0), color='blue', size=1) +
  annotate("text", x=1, y=22, label= "moon in the sky", size=5) + 
  annotate("text", x = -0.5, y=22, label = "before moonrise", size=5)+
  xlab("level of moon light in the sky") +
  ylab("SQM measured darkness of cliff face") +

  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=20), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        legend.background = element_rect(fill="white", colour="white"),
        legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

#dev.off()



#########################################################
#### STATISTICAL TEST OF BUNKERING EFFECT ON DARKNESS ###
head(SQM)
SQM$bunker<-ifelse(SQM$ships>0,1,0)
hist(SQM$MAG)
Q1m<-glm(MAG~moon.light*bunker, data=SQM)
outQ1m<-summary(Q1m)
outQ1m$coefficients



################################################################################################################
###### PLOT ACTIVITY PATTERN OVER SEASON AND OVER NIGHTTIME  ##############################
################################################################################################################

season<- RFIDdata %>%
  filter(Ant=="A1") %>%
  mutate(count=1) %>%
  group_by(NightStarting,TagID) %>%
  summarise(activity=sum(count))

season %>% group_by(NightStarting) %>%
  summarise(activity=length(unique(TagID))) %>%
  mutate(n_deployed=sapply(NightStarting,taggedby)) %>%
  mutate(prop_moving=activity/n_deployed)%>%
  
  
  ggplot(aes(x=NightStarting, y=prop_moving))+
  geom_point(colour="black", size=1.5) +
  xlab("Date") +
  ylab("Prop of tagged individuals at A1") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=15, color="black"), 
        axis.title=element_text(size=15), 
        strip.text.x=element_text(size=15, color="black"), 
        strip.background=element_rect(fill="white", colour="black"),
        legend.background = element_rect(colour = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())



nighttime<- RFIDdata %>%
  filter(Ant=="A1") %>%
  mutate(count=1) %>%
  mutate(HR=as.factor(hour(DateTime)))%>%
  group_by(HR,TagID) %>%
  summarise(activity=sum(count)) %>%


  ggplot(aes(x=HR, y=activity, width=1))+
  geom_boxplot(colour="black") +
  xlab("hour of the day") +
  ylab("N detections at A1") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=15, color="black"), 
        axis.title=element_text(size=15), 
        strip.text.x=element_text(size=15, color="black"), 
        strip.background=element_rect(fill="white", colour="black"),
        legend.background = element_rect(colour = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())





### CONCLUSION - REMOVE RFID DATA BEFORE APRIL AND AFTER MAY

RFIDdata_all <-RFIDdata

goodinterval<-interval(ymd("2017-04-01", tz= "UTC"),ymd("2017-05-31", tz = "UTC"))
RFIDdata<- RFIDdata %>%
  filter(DateTime %within% goodinterval)

SQMall <-SQM
SQM<-SQM %>%
  filter(DateTime %within% goodinterval)

################################################################################################################
###### Moves for all period   ##############################
##########################################################################################
YESH_all<-unique(RFIDdata_all$TagID)

MOVES_all<-data.frame()

for (A in YESH_all){
  IND <- RFIDdata_all %>%
    filter(TagID == A) %>%
    arrange(DateTime) %>%
    
    #nextAntV<-RFIDdata$Ant[RFIDdata$TagID==A]
    
    mutate(nextAnt=c(Ant[-1],NA)) %>%
    mutate(nextTime=c(DateTime[-1],NA)) %>% ### insert column for date at next antenna to compare time  
    mutate(move=ifelse(Ant==nextAnt,0,1)) %>%
    mutate(direction=ifelse(move==1,ifelse(nextAnt=="A2","IN","OUT"),"STAY")) %>%
    mutate(timediff=difftime(nextTime,DateTime, units="secs")) %>% ### calculate time between two registrations
    mutate(DateTime=if_else(timediff>3600,nextTime,DateTime))%>% ### insert condition that if time between A1 and A2 is > 5min then take the date for the next registration
    mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600))))
  
  MOVES_all<-bind_rows(MOVES_all,IND)
  
}
sum(MOVES_all$move, na.rm=TRUE)





################################################################################################################
###### DETERMINE ARRIVAL AND DEPARTURE FROM SEQUENTAL REGISTRATION AT A1 and A2   ##############################
################################################################################################################

### LOOP OVER EACH ANIMAL ###
YESH<-unique(RFIDdata$TagID)


### IN for A1 followed by A2 for same tag

MOVES<-data.frame()

for (A in YESH){
  IND <- RFIDdata %>%
    filter(TagID == A) %>%
    arrange(DateTime) %>%
    
    #nextAntV<-RFIDdata$Ant[RFIDdata$TagID==A]
    
    mutate(nextAnt=c(Ant[-1],NA)) %>%
    mutate(nextTime=c(DateTime[-1],NA)) %>% ### insert column for date at next antenna to compare time  
    mutate(move=ifelse(Ant==nextAnt,0,1)) %>%
    mutate(direction=ifelse(move==1,ifelse(nextAnt=="A2","IN","OUT"),"STAY")) %>%
    mutate(timediff=difftime(nextTime,DateTime, units="secs")) %>% ### calculate time between two registrations
    mutate(DateTime=if_else(timediff>3600,nextTime,DateTime))%>% ### insert condition that if time between A1 and A2 is > 5min then take the date for the next registration
    mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600))))
  
  MOVES<-bind_rows(MOVES,IND)
  
}

head(MOVES)
#write.csv(MOVES, "Moves.csv")
sum(MOVES$move, na.rm=TRUE)

#at this point RFIDdata times are still correct but not sure if MOVES time are correct

################################################################################################################
###### QUESTION 2: ARE NUMBER OF MOVEMENTS RELATED TO SQM LIGHT LEVEL MEASUREMENTS? ###########
################################################################################################################


### summarise HOURLY light-level measurements
head(SQM)

SQMhr<- SQM %>%
  mutate(HR=as.factor(hour(DateTime)))%>%
  mutate(MONTH=(month(DateTime)))%>% #originally mutate(MONTH=(month(DateTime, label=T, abbr=T)))%>%
  group_by(MONTH,NightStarting,HR) %>%
  summarise(MAG=mean(MAG),prop.illuminated=mean(prop.illuminated), moon.elevation=mean(moon.elevation), ships=max(ships), bunker=max(bunker))%>%
  mutate(moon.light=prop.illuminated*(moon.elevation/coordinates(locs)[3,2]))

MOVEShr <- MOVES %>%
  filter(direction=="IN") %>%
  mutate(count=1) %>%
  mutate(HR=as.factor(hour(DateTime)))%>%
  mutate(MONTH=(month(DateTime)))%>%  #originally mutate(MONTH=(month(DateTime, label=T, abbr=T)))%>%
  group_by(MONTH,NightStarting,HR) %>%
  summarise(activity=length(unique(TagID)))


MOVE_SQM<-merge(SQMhr,MOVEShr,by=c('MONTH','NightStarting','HR'), all.x=T)
MOVE_SQM$activity[is.na(MOVE_SQM$activity)]<-0



#modified by Steffen on 29-01-2018 to incorporate hrs 19, 20 and 21 into one parameter, but does not influence model out put. Hr19to21 is still not identifiable
#head(MOVE_SQM)
#MOVE_SQM$HR<-as.numeric(as.character(MOVE_SQM$HR))
#MOVE_SQM$HR<-ifelse(MOVE_SQM$HR %in% c(19,20,21),21,MOVE_SQM$HR)
#MOVE_SQM$HR <- factor(MOVE_SQM$HR, levels = c("0","1","2","3","21","22","23"))

#old version before 29-01-2018 
MOVE_SQM$HRordered <- factor(MOVE_SQM$HR, levels = c("19","20","21","22","23","0","1","2","3"))
levels(MOVE_SQM$HR)


MOVE_SQM %>% #filter(HR %in% c(22,23,0,1))%>%
ggplot(aes(x=MAG, y=activity))+
  geom_point(colour="black") +
  ylab("N individuals entering the cave") +
  xlab("Darkness of cliff face (magnitude measured by SQM)") +
  #geom_boxplot(colour="black") +
  #facet_wrap('HRordered',scales = "fixed", shrink = TRUE, ncol=2)+
  geom_smooth(fill="lightblue", size=1.5, method='lm', se=T)+
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=15, color="black"), 
        axis.title=element_text(size=15),
        axis.title.y=element_text(margin=margin(0,15,0,0)),
        axis.title.x=element_text(margin=margin(15,0,0,0)), 
        strip.text.x=element_text(size=18, color="black"),
        strip.text.y=element_text(size=18, color="black"),
        strip.background=element_rect(fill="white", colour="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())


#########################################################
#### STATISTICAL TEST OF BUNKERING EFFECT ON DARKNESS ###

Q2m<-glm(activity~-1+HR+MONTH+MAG, data=MOVE_SQM[MOVE_SQM$HR %in% c("20", "21", "22","23","0","1","2","3"),], family=poisson)
outQ2m<-summary(Q2m)
outQ2m$coefficients


#-1 in model removes intercept, to compare estimates for hour on same scale
#in the current model hr 19 is not included because parameter was not identifiable 




################################################################################################################
###### QUANTIFY REGISTRATIONS AT A1 FOR EACH 10 MIN INTERVAL OF SQM   ##############################
################################################################################################################
head(SQM)
head(RFIDdata)

SQM$A1det<-0
SQM$A1ind<-0

for (n in 2:length(SQM$DateTime)){
  x<-SQM$DateTime[n-1]
  x1<-SQM$DateTime[n]
  
  movs<- RFIDdata %>%
    filter(DateTime>=x)%>%
    filter(DateTime<=x1)%>%
    filter(Ant=="A1")
  SQM$A1det[n]<-dim(movs)[1]
  
  inds<-movs %>%
    group_by(TagID)%>%
    mutate(count=1)%>%
    summarise(count=sum(count))
  SQM$A1ind[n]<-dim(inds)[1]
}



################################################################################################################
###### MANUALLY SELECTING TIME INTERVALS AROUND BUNKERING EVENTS   ##############################
################################################################################################################

withships<-as.numeric(rownames(monitornights)[monitornights$ships>0])
before<-withships-1
after<-withships+1
selection<-unique(c(withships,before,after))
selection<-selection[selection>0]

CaseControl<-monitornights%>%
  filter(row_number() %in% selection)%>%
  select(NightStarting,sunset,sunrise,ships,nightdur)%>%
  filter(sunrise %within% goodinterval)


### remove the lines outside the Case Control time periods
SQMcc<-data.frame()
for(i in selection){
  x<-SQM %>%
    filter(DateTime %within% monitornights$nightdur[i])
  SQMcc<-rbind(SQMcc,x)
}



#pdf("SQM_YESH_activity.pdf", width=8, height=6)
SQMcc %>% mutate(ships=ships*2) %>%   ### usually 2 boats operate for a bunkering event
  ggplot(aes(x=A1ind, y=MAG))+
  geom_point(aes(size=ships, color=ships)) +
  scale_color_gradient(low="black", high="yellow")+
  scale_size(range=c(0,4)) +
  guides(color=guide_legend(title="Number of ships"), size = guide_legend(title="Number of ships"))+
  xlab("Activity in YESH colony") +
  ylab("SQM measured darkness of cliff face") +
  
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=20), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        legend.background = element_rect(fill="white", colour="white"),
        legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

#dev.off()






################################################################################################################
###### QUESTION 3: SUMMARISE THE MOVEMENTS FOR EACH HOUR FOR NIGHTS WITH AND WITHOUT SHIPS BUNKERING   #####################
################################################################################################################

### remove the lines outside the Case Control time periods
MOVEScc<-data.frame()
for(i in selection){
  x<-MOVES %>%
    filter(DateTime %within% monitornights$nightdur[i]) %>%
    mutate(ships=monitornights$ships[i])%>%
    mutate(bunker=ifelse(ships>0,'with bunkering','no bunkering'))
  MOVEScc<-rbind(MOVEScc,x)
}



MOVEScc <- MOVEScc %>%
  filter(direction=="IN") %>%
  mutate(count=1) %>%
  mutate(HR=as.factor(hour(DateTime)))%>%
  mutate(MONTH=(month(DateTime)))%>% #label=T, abbr=T
  group_by(HR,MONTH,bunker,NightStarting) %>%
  summarise(activity=length(unique(TagID)))

MOVEScc_medians<-MOVEScc %>%
  group_by(MONTH,bunker) %>%
  summarise(MED=median(activity))

levels(MOVEScc$HR)

MOVEScc$HRordered <- factor(MOVEScc$HR, levels = c("19", "20","21","22","23","0","1","2","3","4")) #4 and 5 to be removed?
levels(MOVEScc$HRordered)

#pdf("Bunkering_YESH_activity.pdf", width=8, height=6)
  ggplot(data=MOVEScc, aes(x=HRordered, y=activity, width=1))+
  geom_boxplot(colour="black") +
  facet_grid(MONTH ~ bunker,scales = "fixed", shrink = TRUE)+
  ylab("N individuals entering the cave") +
  geom_hline(data=MOVEScc_medians,aes(yintercept=MED), color='red')+
  scale_x_discrete(name="Hour of day", labels=c(19,20,21,22,23,0,1,2,3,4,5))+ #,4,5 should be removed?
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=18, color="black"), 
        axis.title=element_text(size=20),
        axis.title.y=element_text(margin=margin(0,15,0,0)),
        axis.title.x=element_text(margin=margin(15,0,0,0)), 
        strip.text.x=element_text(size=18, color="black"),
        strip.text.y=element_text(size=18, color="black"),
        strip.background=element_rect(fill="white", colour="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())
#dev.off()



#########################################################
#### STATISTICAL TEST OF BUNKERING EFFECT ON DARKNESS ###
  
  Q3m<-glm(activity~-1+HR+MONTH+bunker, data=MOVEScc, family=poisson)
  outQ3m<-summary(Q3m)
  outQ3m$coefficients



  
  
  
  ##################################################################
  ### PRODUCE OUTPUT REPORT WITH KEY TABLES AND FIGURES ###
  ##################################################################
  #detach(packages:htmlwidgets)
  #detach(name="package:htmlwidgets", unload=TRUE, character.only=TRUE)
  #install.packages(c('plotly','htmlwidgets'), dependencies=T)
  
  library(markdown)
  library(rmarkdown)
  library(knitr)
  library(plotly)
  
  ### create HTML report for overall summary report
  #Sys.setenv(RSTUDIO_PANDOC="C:/Program Files (x86)/RStudio/bin/pandoc")
  Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
  
  
  rmarkdown::render('S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Malta\\Analysis\\ColonyAttendance\\Malta_LightPollution_Summary_June18.Rmd',
                    output_file = "Malta_LightPollution_Report_June2018.html",
                    output_dir = 'S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Malta\\Analysis\\ColonyAttendance')
  
  rmarkdown::render('C:\\Users\\Martin\\Documents\\working_folder\\RFID\\Bunkering_Analysis\\Malta_LightPollution_Summary_MA_Feb18.Rmd',
                    output_file = "Malta_LightPollution_Report_aftertimechange.html",
                    output_dir = 'C:\\Users\\Martin\\Documents\\working_folder\\RFID\\Bunkering_Analysis')
                    
  









################################################################################################################
###### SUMMARISE THE IN AND OUT MOVEMENTS FOR EACH NIGHT   ##############################
################################################################################################################
setwd("A:\\RSPB\\Malta\\Analysis\\ColonyAttendance")
setwd("C:\\STEFFEN\\RSPB\\Malta\\Analysis\\ColonyAttendance")


#### SUMMARISE THE MOVEMENTS FOR EACH NIGHT ACROSS ALL INDIVIDUALS ###

NIGHTMOVES<- MOVES %>%
  filter(move==1) %>%
  mutate(Time=format(DateTime, format="%H:%M:%S")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600)))) %>%  ### 'ifelse' in base converts date to number!!
  group_by(NightStarting,direction) %>%                              
  summarise(nmoves=sum(move)) %>%
  spread(key=direction, value=nmoves) %>%
  arrange(NightStarting)



#### MARTIN AUSTAD NOTICED ON 24 Aug 2017 that a lot of movements are based on few INDIVIDUALS ###
#### SUMMARISE THE N INDIVIDUALS MOVING EACH NIGHT ###

NIGHTINDIVIDUALS<- RFIDdata %>%
  mutate(n_detections=1) %>%
  mutate(Time=format(DateTime, format="%H:%M:%S")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600)))) %>%  ### 'ifelse' in base converts date to number!!
  group_by(NightStarting,TagID,Ant) %>%
  summarise(N_ind_det=max(n_detections)) %>%
  group_by(NightStarting,Ant) %>%
  summarise(N_individuals=sum(N_ind_det)) %>%
  spread(key=Ant, value=N_individuals) %>%
  arrange(NightStarting)

NIGHTIND<- MOVES %>%
  filter(move==1) %>%
  mutate(Time=format(DateTime, format="%H:%M:%S")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600)))) %>%  ### 'ifelse' in base converts date to number!!
  group_by(NightStarting) %>%                              
  summarise(nind=length(unique(TagID))) %>%
  arrange(NightStarting)

## merge with n movements
dim(NIGHTMOVES)
dim(NIGHTIND)           ## has more rows - solved on 4 Sept: NIGHTIND[!(NIGHTIND$NightStarting %in% NIGHTMOVES$NightStarting),]
dim(NIGHTINDIVIDUALS)   ## this is the number of individuals at each antenna
NIGHTMOVES<-as.data.frame(NIGHTMOVES)
NIGHTMOVES$N_individuals=NIGHTIND$nind[match(NIGHTMOVES$NightStarting,NIGHTIND$NightStarting)]
NIGHTMOVES$N_ind_A1=NIGHTINDIVIDUALS$A1[match(NIGHTMOVES$NightStarting,NIGHTINDIVIDUALS$NightStarting)]
NIGHTMOVES$N_ind_A2=NIGHTINDIVIDUALS$A2[match(NIGHTMOVES$NightStarting,NIGHTINDIVIDUALS$NightStarting)]


## merge with n ships bunkering
head(monitornights)
NIGHTMOVES$ships=monitornights$ships[match(NIGHTMOVES$NightStarting,monitornights$NightStarting)]



NIGHTMOVES <- NIGHTMOVES %>%
  mutate(n_deployed=sapply(NightStarting,taggedby)) %>%
  mutate(prop_moving=N_individuals/n_deployed)


#fwrite(NIGHTMOVES,"YESH_nightly_movements_RFID.csv")

head(NIGHTMOVES)





















################################################################################################################
######################  discarded: PLOT THE SHIP BUNKERING DATA AGAINST THE MOVEMENTS   ####################################
################################################################################################################

pdf("YESH_movements_bunkering_ships.pdf", width=7, height=6)
NIGHTMOVES %>% 
  
  ggplot(aes(x=ships, y=prop_moving))+
  geom_smooth(fill="lightblue", size=1.0, method='lm')+
  geom_point(colour="black", size=1.5) +
  xlab("Number of bunkering ships") +
  ylab("Proportion of tagged YESH moving") +
  scale_y_continuous(breaks=seq(0,0.6,0.1), labels=seq(0,0.6,0.1), limits=c(0,0.6))+
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=20), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

dev.off()


### TRY OTHER VARIABLES, BUT THE PATTERN IS SIMILAR

NIGHTMOVES %>% 
  
  ggplot(aes(x=ships, y=OUT))+
  geom_smooth(fill="lightblue", size=1.0, method='lm')+
  geom_point(colour="black", size=1.5) +
  xlab("Number of bunkering ships") +
  ylab("Number of tagged YESH at A1") +
 theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=20), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())









################################################################################################################
###### discarded: SUMMARISE THE SKY QUALITY METER DATA PER NIGHT AND RELATE TO SHIPS   ##############################
################################################################################################################
### much less powerful than above analysis!
### summarise by night ###

SQMsummary<-SQM %>%
  group_by(NightStarting)%>%
  summarise(min=min(MAG), mean=mean(MAG), max=max(MAG), sd=sd(MAG), moon=mean(moon.light))%>%
  select(NightStarting,min,max,mean,sd, moon)


LightPoll<-merge(monitornights,SQMsummary, by="NightStarting")
head(LightPoll)

LightPoll %>% 
  
  ggplot(aes(x=ships, y=max))+
  geom_smooth(fill="lightblue", size=1.0, method='lm')+
  geom_point(colour="black", size=1.5) +
  xlab("Number of bunkering ships") +
  ylab("Darkness of sky") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=20), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())



  