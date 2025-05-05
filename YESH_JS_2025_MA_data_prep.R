###############################################################################
##
##  YELKOUAN SHEARWATER SURVIVAL AND ABUNDANCE TREND ESTIMATION ON MALTA  ##
##
###############################################################################

## written by steffen.oppel@rspb.org.uk on 19 February 2019

## revised and split from YESH_survival_estimation.r on 25 March 2019
## re-arranged data structure to 1 occasion per year
## model with 32 occasions yielded consistent invalid parent error

## REMOVED CAVES ONLY MONITORED IN 2018-2019 TO ESTIMATE LONGER TREND
## UPDATED ON 6 AUG 2020 to include 2020 data 

## revised model on 10 Aug 2020 to include linear trend

##revised by Hannah Greetham & Martin Austad Aug 2021 to include 2021 data
##revised by Martin Austad September 2023 to include 2022 and 2023 data
##revised by Martin Austad August 2024 to include 2024 data

library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)
library(jagsUI)
library(readxl)
library(tidyr)

rm(list = ls())
################################################################################################################
###################### LOAD AND MANIPULATE DATA 	  ######################################################
################################################################################################################



##### LOAD RAW DATA  
#setwd("C:\\Users\\martin.austad\\Documents\\yesh_cmr_HG_02082021")
#setwd("C:\\Users\\hannah.greetham\\Documents\\yesh_cmr_HG_17082021")
#setwd("C:\\STEFFEN\\RSPB\\Malta\\Raw_data")
#setwd("C:\\Users\\Martin\\Documents\\MSFD_CMR\\YESH")

setwd("C:\\Users\\marti\\Documents\\CMR")

# yesh<-read_excel("YESH_2012_2018_cavestring_replacedrings.xlsx", sheet="with cave string")
# effort<-read_excel("Cave_Activity_complete_2012-2018.xlsx", sheet="Sheet1")

###read in CMR subsites
caves<-fread("Cave_ID.csv")

# to get list of all rings deployed, in the source file only original rings present and not their replacements
rings <- fread("2012_2025_replacement_and_tags.csv")

#####IMP. NOTE: All times in records and effort are in Malta time.

#to create Nightstarting for both ringing records and cave activity (Effort) to match effort on nightstarting
midday<-as.POSIXct("12:00:00", format="%H:%M:%S",tz = "UTC")   # create a reference time to split the day
midday<-format(midday, format="%H:%M:%S",tz = "UTC") 


R2022 <- fread("2022_YESH_Biometrics_RingingRecords.csv")

R2022$ringingDate <- as.Date(R2022$ringingDate, format="%d/%m/%Y")
  range(R2022$ringingDate)
  
  colnames(R2022)
  
  colnames(R2022)[28] <- "remarks"
  colnames(R2022)[27] <- "broodpatch"
  colnames(R2022)[26] <- "tags"

R2023 <- fread("2023_YESH_Biometrics_RingingRecords.csv") 

R2023$ringingDate <- as.Date(R2023$ringingDate, format="%d/%m/%Y")
range(R2023$ringingDate)

colnames(R2023)

colnames(R2023)[28] <- "remarks"
colnames(R2023)[27] <- "broodpatch"
colnames(R2023)[26] <- "tags"

#load 2024 ringing data
R2024 <- fread("2024_YESH_Biometrics_RingingRecords.csv")

R2024$ringingDate <- as.Date(R2024$ringingDate, format="%d/%m/%Y")
range(R2024$ringingDate)
colnames(R2024)

colnames(R2024)[28] <- "remarks"
colnames(R2024)[27] <- "broodpatch"
colnames(R2024)[26] <- "tags"

#load 2025 ringing data
R2025 <- fread("2025_YESH_Biometrics_RingingRecords.csv")
R2025$ringingDate <- as.Date(R2025$ringingDate, format="%d/%m/%Y")
range(R2025$ringingDate)
colnames(R2025)

colnames(R2025)[28] <- "remarks"
colnames(R2025)[27] <- "broodpatch"
colnames(R2025)[26] <- "tags"


#since ringing database extract for 2012-2016 gave time in hour only, :00:00 CONCATENATED onto hour value in excel
records <- fread("2012_2021_cavestring_ringingrecords_updated.csv")

#error in date format: two formats due to upload to onedrive
records <- records %>%
  mutate(monthextracted=month(ringingDate))%>%
  mutate(datecheck=if_else(month==monthextracted, "Correct", "Error"))

records <- records %>%
  mutate(ringingDateE = ringingDate)%>%
  mutate(ringingDate = if_else(datecheck=="Correct", as.Date(ringingDate, format="%d/%m/%Y"), as.Date(ringingDate, format="%m/%d/%Y")))
  
colnames(records)

records <- records %>%
  dplyr::select(Errors, site, Cave_string, ringingDate,      
month, Year, Hour, HourTime, type, ringnumber, Replacement, age,            
sex, winglength, weight, billlength, billheight, tarsus,
ringer, cave, nest_or_direction, remarks, tags, broodpatch, Prospector)

R2022 <- R2022 %>%
  dplyr::select(Errors, site, Cave_string, ringingDate,      
                month, Year, Hour, HourTime, type, ringnumber, Replacement, age,            
                sex, winglength, weight, billlength, billheight, tarsus,
                ringer, cave, nest_or_direction, remarks, tags, broodpatch, Prospector)


R2023 <- R2023 %>%
  dplyr::select(Errors, site, Cave_string, ringingDate,      
                month, Year, Hour, HourTime, type, ringnumber, Replacement, age,            
                sex, winglength, weight, billlength, billheight, tarsus,
                ringer, cave, nest_or_direction, remarks, tags, broodpatch, Prospector)

R2024 <- R2024 %>%
  dplyr::select(Errors, site, Cave_string, ringingDate,      
                month, Year, Hour, HourTime, type, ringnumber, Replacement, age,            
                sex, winglength, weight, billlength, billheight, tarsus,
                ringer, cave, nest_or_direction, remarks, tags, broodpatch, Prospector)

R2025 <- R2025 %>%
  dplyr::select(Errors, site, Cave_string, ringingDate,      
                month, Year, Hour, HourTime, type, ringnumber, Replacement, age,            
                sex, winglength, weight, billlength, billheight, tarsus,
                ringer, cave, nest_or_direction, remarks, tags, broodpatch, Prospector)

records <- rbind(records, R2022, R2023, R2024, R2025)

str(records)

unique(records$Cave_string)

records$HourTime

records$Errors[is.na(records$Errors)] <- 0

recordsbackup <- records

records <- records %>%
  mutate(DateTime=as.POSIXct(paste(ringingDate, HourTime, " "), format="%Y-%m-%d %H:%M:%S", tz="Europe/Berlin"))%>%
  #mutate(DateTime=ymd(ringingDate) + hms(HourTime), tz = "Europe/Berlin") 
  dplyr::rename(Cave_String=Cave_string) %>%
 # mutate(DateTime=with_tz(DateTime,tz="UTC")) %>% #does not work if using EuropeBerlin tz here
  mutate(Time=format(DateTime, format="%H:%M:%S",tz = "Europe/Berlin")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600))))%>%
  dplyr::filter(!(Cave_String=="Office")) %>% #"Office" refers to birds stranded or otherwise rescued for which there is no respective effort
  dplyr::filter(!(Cave_String=="office")) %>%  
  dplyr::filter(Errors!=1) #handful of records remain. these have been checked but could not be resolved. most likely are errors and should therefore not be included in analysis


effort <- fread("Cave_Activity_complete_2012-2025DT.csv")


##does the start and end time overlap midnight? 
##create an end date and correct for midnight overlap
effort <- effort %>%
  filter(!is.na(start_time) & !is.na(end_time)) %>%
  mutate(
    start=as.POSIXct(start_time, format="%H:%M",tz = "Europe/Berlin"),
    end=as.POSIXct(end_time, format="%H:%M", tz="Europe/Berlin"),
    st_minutes = hour(start) * 60 + minute(start),
    et_minutes = hour(end) * 60 + minute(end),
    overlaps_midnight = if_else(st_minutes > et_minutes, "YES", "NO"))%>%
  mutate(start_date = as.Date(Date, format="%d/%m/%Y"),
         end_date=if_else(overlaps_midnight=="NO", start_date, start_date+days(1))) %>%
  mutate(Start = as.POSIXct(paste(start_date, start_time, " "), format="%Y-%m-%d %H:%M", tz="Europe/Berlin"), 
         End= as.POSIXct(paste(end_date, end_time, " "), format="%Y-%m-%d %H:%M", tz="Europe/Berlin"), 
         SessionInt=interval(Start, End, tz="Europe/Berlin"))%>%
  mutate(NightStarting=if_else(start_time>midday,start_date,start_date-days(1)))%>%
  dplyr::select(Cave_String, Year, Date, start_date, end_date, start_time, end_time, hours, number_persons, net_length, SessionInt, NightStarting)

 
### EXTRACT RINGING RECORDS WHERE EFFORT DATA IS MISSING ###

head(records)

#error identification; does not work for 2012-2018 when records have time to the nearest hour

ms <- records %>%
  filter(!is.na(DateTime)) %>%
  rowwise() %>%
  mutate(
    missingeffort = if_else(any(DateTime %within% effort$SessionInt & Cave_String == effort$Cave_String),"no","yes")) %>%
  ungroup()%>%
  filter(missingeffort=="yes")


#misseff<-records %>% dplyr::select(Cave_String,NightStarting, ringnumber, ringer) %>%
  #left_join(effort[,c(1,12,8)],by=c("Cave_String","NightStarting")) %>%
  #mutate(hours=as.numeric(hours)) %>%
  #dplyr::filter(is.na(hours))

#fwrite(misseff,"YESH_ring_records_missing_effort_CHECK.csv") 

#to check in records for ringnumbers marked as 'new' more than once

recordsC <- records %>%
  group_by(ringnumber)%>%
  filter(type=="N")%>%
  mutate(count=1)%>%
  summarise(multipleN=sum(count))%>%
  filter(multipleN>1)

####

#to check wether some retrap records do not have a corresponding "new" record
#ok reason for this might be first ringing occured before 2012 and hence not in this dataset
#other ok reason for this is that it was a replacement ring
#or retrap record is an error in typing/reading ring

recordsC <- records %>%
  group_by(ringnumber)%>%
  mutate(types=if_else(type=="N", 1, 0))%>%
  summarise(e=sum(types), 
            r=max(Replacement))%>%
  filter(e!=1 & r==0)

#some retraps in period 2012-2019 remain in dataset even if the pre-2012 "new" record was not found in BLM ringing scheme database, records sent to John for checking but confident they are correct since retraped more than once

# ### CHANGED ON 22 NOV 2019 TO REMOVE THE COLONIES VISITED ONLY IN 2017-2019
# 
# RM01 <- c("MT09_RM01")
# RM03 <- c("MT09_RM03_18to22", "MT09_RM03", "MT09_RM03_North", "MT09_RM03_South") #if high transcience to remove "MT09_RM03_18to22" - lower effort than other sites
# RM05 <- c("MT09_RM05", "MT09_RM05BT", "MT09_RM05GC_Central", "MT09_RM05GC", "MT09_RM05GC_South", "MT09_RM05GC_North", "MT09_RM05BT_Lower", "MT09_RM05BT_Upper") #if high transcience to remove "MT09_RM05BT_Upper" - lower effort than other sites in last years but probably a lot of the effort made at MT09_RM05BT in 2013 was actually at lower
# RM04 <- c("MT09_RM04A", "MT09_RM04B", "MT09_RM04D", "MT09_RM04C") #RM04C lowest effort perhaps to remove
# Cominotto <- c("MT17_Cominotto_2", "MT17_Cominotto_1", "MT17_Cominotto_3", "MT17_Cominotto", "MT17_Cominotto_7", "MT17_Cominotto_4") # 4 only since 2017; #8 & 9 only since 2018 so better to remove these caves?
# StPauls <- c("MT22_StPauls_MainCave", "MT22_StPauls_WestCave")
# Majjistral_main <- c("MT24_Majjistral_Eggshell", "MT24_Majjistral_Thomas", "MT24_Majjistral_Subt", "MT24_Majjistral_NS2_NS3")             
# #Majjistral_south <- c("MT24_Majjistral_South") #probably too little effort           

adults <- c("6", "4", "2")

#unique(records$Cave_String[records$age %in% adults])

### REPLACE ORIGINAL CAVE STRING WITH POOLED SITES
## create lookup-table 
RM01 <- c("MT09_RM01")
RM03 <- c("MT09_RM03", "MT09_RM03_North", "MT09_RM03_South") # "MT09_RM03_18to22" - removed no effort in 2019 to 2023 for adults
RM05 <- c("MT09_RM05", "MT09_RM05BT", "MT09_RM05GC_Central", "MT09_RM05GC", "MT09_RM05GC_South", "MT09_RM05GC_North", "MT09_RM05BT_Lower", "MT09_RM05BT_Upper") 
RM04 <- c("MT09_RM04A", "MT09_RM04B", "MT09_RM04D", "MT09_RM04C") #RM04C lowest effort perhaps to remove
Cominotto <- c("MT17_Cominotto_2", "MT17_Cominotto_1", "MT17_Cominotto_3", "MT17_Cominotto_4","MT17_Cominotto_9") # 4 only since 2017; #8 & 9 only since 2018 so better to remove these caves? #"MT17_Cominotto_7" no real effort for adults after 2017
StPauls <- c("MT22_StPauls_MainCave", "MT22_StPauls_WestCave")
Majjistral_main <- c("MT24_Majjistral_Eggshell", "MT24_Majjistral_Thomas", "MT24_Majjistral_NS2_NS3") #"MT24_Majjistral_Subt" removed due to dificult access since 2020 
Majjistral_south <-c("MT24_Majjistral_South_4", "MT24_Majjistral_South_1", "MT24_Majjistral_South") # MT24_Majjistral_South_4 &1 remove for trend analysis
#Babu<- c("MT27_Babu") # remove for trend analysis #not enough birds here yet
all_lut<-data.frame(orig=as.character(c(RM01,RM03,RM04,RM05,Cominotto,StPauls,Majjistral_main,Majjistral_south)),
                    
                    poolloc=as.character(c("RM01",rep("RM03",3),rep("RM04",4),rep("RM05",8),rep("Cominotto",5),rep("StPauls",2),rep("Majjistral_main",3), rep("Majjistral_south", 3))), # number of subsites per location
                    
                    maincol=as.character(c(rep("RdumTalMadonna",16),rep("Cominotto",5),rep("StPauls",2),rep("Majjistral",6))))


effort <- effort %>%
  mutate(SITE=all_lut$poolloc[match(as.character(Cave_String),all_lut$orig)]) %>%
  mutate(COLO=all_lut$maincol[match(as.character(Cave_String),all_lut$orig)]) %>%
  dplyr::filter(!is.na(SITE))

records <- records %>%
  mutate(SITE=all_lut$poolloc[match(as.character(Cave_String),all_lut$orig)]) %>%
  mutate(COLO=all_lut$maincol[match(as.character(Cave_String),all_lut$orig)]) %>%
  dplyr::filter(!is.na(SITE)) %>%
  dplyr::filter(age %in% adults)%>%
  dplyr::filter(Errors!= 1)




#########################################################################
# ENSURE THAT RING REPLACEMENTS ARE INCORPORATED INTO RECORDS
#########################################################################
names(records)
length(unique(records$ringnumber))

## CREATE REPLACEMENT LIST
replist<-rings %>% #dplyr::filter(Replacement_ring!="") %>%
  rename(orig=Origninal_ring, repl=Replacement_ring) %>%
  select(orig,repl)

## FIND DOUBLE REPLACEMENTS AND UPDATE LIST
doublerep<-unique(rings$Replacement_ring)[unique(rings$Replacement_ring) %in% rings$Origninal_ring]
#IMPORTANT!:make sure that the 2nd replacement rings in the csv file, read in as 'rings', is in the same sequence as the original rings and double rep
replist$orig[replist$orig %in% doublerep]<-replist$orig[replist$repl %in% doublerep]
replist[replist$orig %in% doublerep,] ### this should be empty
replist[replist$repl %in% doublerep,] 


## UPDATE RECORDS
length(unique(records$ringnumber)) #2480
yesh<- records %>%
  mutate(ringnumber=ifelse(ringnumber %in% replist$repl,replist$orig[match(ringnumber,replist$repl)],ringnumber))
length(unique(yesh$ringnumber))  ## 2339

#########################################################################
# MAKE SURE THAT EACH Resighting is preceded by a New capture
#########################################################################
noringdepl<- yesh %>% group_by(ringnumber,type) %>%
  summarise(first=min(DateTime)) %>%
  spread(key=type, value=first) %>%
  mutate(tdiff=as.numeric(difftime(R,N,'days'))) %>%
  dplyr::filter(!is.na(R)) #%>%
 # dplyr::filter(tdiff<0 | is.na(tdiff)) #many of these are recruiting birds ringed as chicks, or adults ringed at sub-site not included in the analysis and recaptured at a different subsite 
#look for negative timediff

#fwrite(noringdepl,"YESH_recaptures_only.csv")

#########################################################################
##########check for recaptures in different colonies#####################
#########################################################################

recordsC <- yesh %>%
  group_by(ringnumber)%>%
  mutate(count=1)%>%
  reframe(multipleS=length(unique(COLO)))%>%
  filter(multipleS>1)

#prospectors? 

#########################################################################
# CREATE MATRIX OF ENCOUNTER DATA (0/1)
#########################################################################


### FIRST WE NEED TO CREATE A LIST OF OCCASIONS AND NUMBER THEM SEQUENTIALLY
## ANALYSE AT SEASON LEVEL
names(yesh)

#check dates #Year is inputted manually during data input
#yeshC <- yesh %>%
 # mutate(YearY=year(NightStarting))%>%
  #mutate(e=if_else(Year==YearY, "C", "E"))%>%
 # filter(e=="E")

OCC_lookup<- yesh %>%
  mutate(Year=year(NightStarting)) %>%
  mutate(month=month(NightStarting)) %>%
  #mutate(Occ=paste(Year,month,sep="_")) %>%
  mutate(SEASON=ifelse(month<10,(Year-1),Year)) %>% ###define a breeding season from Oct to Jul
  group_by(SEASON) %>%
  summarise(middate=median(NightStarting)) %>%
  arrange(middate) %>%
  mutate(OCC_NR=seq_along(SEASON))
# Recognises the numbers as characters so 10 follows 1, to rectify this we tried making then numerical (line227)
# or by adding 10 (line 229) as then the coloumns would always be correct in the lookup table - neither seem to rectify the error
    #OCC_lookup$OCC_NR<-as.numeric(OCC_lookup$OCC_NR)
    #OCC_lookup$OCC_NR<-OCC_lookup$OCC_NR +10

#OBS due to season naming '2011' season is mostly CMR in 2012 etc

YESH<- yesh  %>% #mutate(Colony=caves$Subcolony_Name[match(Cave_String,caves$Cave_String)]) %>%
  mutate(Year=year(NightStarting)) %>%
  mutate(month=month(NightStarting), seen=1) %>%
  #mutate(Occ=paste(Year,month,sep="_")) %>%
  mutate(SEASON=ifelse(month<10,(Year-1),Year)) %>% ###define a breeding season from Oct to Jul
  mutate(OCC_NR=OCC_lookup$OCC_NR[match(SEASON,OCC_lookup$SEASON)]) %>%
  group_by(COLO,SITE, OCC_NR,ringnumber) %>%   ## REPLACED Colony with SITE based on grouping 
  summarise(ALIVE=max(seen,na.rm=T)) %>%
  spread(key=OCC_NR, value=ALIVE) %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  mutate(COLO=as.character(COLO),SITE=as.character(SITE))


length(unique(yesh$ringnumber))
length(unique(YESH$ringnumber))
dim(YESH)

## THESE TWO DIMENSIONS DO NOT MATCH, BECAUSE SOME BIRDS WERE RECORDED IN >1 SITE
duplicates<-names(table(YESH$ringnumber))[table(YESH$ringnumber)>1]
transients <- YESH %>% dplyr::filter(ringnumber %in% duplicates) %>%
  arrange(ringnumber,SITE)

#fwrite(transients,"YESH_Malta_transients_EncHist.csv")

## REMOVE DUPLICATES AND TRANSFER ENCOUNTERS INTO OTHER ROWS

transientCH<-transients %>% gather(key='OCC', value='surv',-ringnumber,-SITE,-COLO) %>%
  arrange(ringnumber,OCC) %>%
  group_by(ringnumber,OCC) %>% summarise(surv=max(surv)) %>%
  spread(key=OCC,value=surv, fill=0)
#transientCH$OCC<-as.numeric(transientCH$OCC) # to try and change order of seasons

transientCH<-transients %>% gather(key='OCC', value='surv',-ringnumber,-SITE,-COLO) %>%
  dplyr::filter(surv==1) %>%
  arrange(ringnumber,OCC) %>%
  group_by(ringnumber) %>% summarise(COLO=last(COLO),SITE=last(SITE)) %>%
  left_join(transientCH, by="ringnumber") %>%
  dplyr::select(2,3,1,4:16)#increase last number as seasons increase


YESH<- YESH %>% dplyr::filter(!(ringnumber %in% duplicates)) %>%
 bind_rows(transientCH)
dim(YESH)


#above is the approach by Steffen but it assigns all encounters to one site, which is not necessarily correct especially for any movements between colonies
#remove transients instead? 
#dim(YESH)
#YESH<- YESH %>% dplyr::filter(!(ringnumber %in% duplicates))
#dim(YESH)
#########################################################################
# CREATE MATRIX OF EFFORT DATA AND DIFFERENCES BETWEEN OCCASIONS
#########################################################################

### FORMAT EFFORT DATA ###
min(effort$NightStarting)

#effC <- effort%>%
 # mutate(YearY=year(NightStarting))%>%
  #mutate(e=if_else(Year==YearY, "C", "E"))%>%
  #filter(e=="E")

eff<- effort %>% #mutate(Colony=caves$Subcolony_Name[match(Cave_String,caves$Cave_String)]) %>% ### REPLACED WITH THE GROUPING MARTIN PROVIDED IN OCTOBER 2019
  #mutate(Colony=Cave_String) %>%  ### this does not work unless the 'periods' are somehow adjusted'
  #mutate(Year=year(NightStarting)) %>%   
  #mutate(NightStarting=dmy(Date)) %>%
  mutate(month=month(NightStarting)) %>%
  mutate(SEASON=ifelse(month<10,(Year-1),Year)) %>% ###define a breeding season from Oct to Jul
  mutate(OCC_NR=OCC_lookup$OCC_NR[match(SEASON,OCC_lookup$SEASON)]) %>%
  mutate(hours=as.numeric(hours)) %>%
  group_by(COLO,SITE, SEASON,OCC_NR) %>%  ## REPLACED Colony with SITE based on Martin's grouping in October 2019
  summarise(effort=sum(hours,na.rm=T))
#eff$OCC_NR<-as.numeric(eff$OCC_NR) # again tryimg to orgainse seasons

### MATRIX OF EFFORT BY Cave_String ###
### CALCULATE INTERVAL BETWEEN OCCASIONS ###
survPeriods<-effort %>% 
  #mutate(Year=year(NightStarting)) %>%  
  mutate(month=month(NightStarting)) %>%
  mutate(SEASON=ifelse(month<10,(Year-1),Year)) %>% ###define a breeding season from Oct to Jul
  mutate(OCC_NR=OCC_lookup$OCC_NR[match(SEASON,OCC_lookup$SEASON)]) %>%
  group_by(COLO,SITE, SEASON,OCC_NR) %>%  ## REPLACED Colony with SITE based on Martin's grouping in October 2019
  summarise(first=min(NightStarting,na.rm=T), last=max(NightStarting,na.rm=T), mid=median(NightStarting, na.rm=T)) %>%
  arrange(SITE, SEASON,OCC_NR) %>%  ## REPLACED Colony with SITE based on Martin's grouping in October 2019
  mutate(surv_int=as.numeric(difftime(dplyr::lead(mid,1), mid,unit="days")))  

## calculate the difference in days between the mid days of subsequent sessions
## somehow the lead/lag does not work in: mutate(surv_int=as.numeric(difftime(lead(first), last,unit="days")))  ## calculates the difference in days between end of current session and beginning of next session

survPeriods$surv_int<-as.numeric(difftime(dplyr::lead(survPeriods$mid,1), survPeriods$mid,unit="days"))            
survPeriods$surv_int[survPeriods$surv_int<0]<-NA               

#fwrite(survPeriods,"Malta_intervals_sessions.csv")

#########################################################################
# PREPARE DATA FOR INPUT INTO JAGS
#########################################################################

names(YESH)
CH<-as.matrix(YESH[,4:16], dimnames=F) # changed to  16 to include 2024
dim(CH)

### check that there are contacts in every season

apply(CH,2,sum)

# Compute vector with occasion of first capture
get.first <- function(x) min(which(x==1))
f <- apply(CH, 1, get.first)
n.occ<-ncol(CH)

### CHECK WHETHER IT LOOKS OK ###
head(CH)

## PREPARE CONSTANTS
n.ind<-dim(CH)[1]		## defines the number of individuals
n.years<-dim(CH)[2]  ## defines the number of years
n.sites<-length(unique(YESH$SITE))
n.colonies<-length(unique(YESH$COLO))              


## CREATE MATRIX TO LOOK UP OBSERVATION EFFORT
## STANDARDISE HOURS TO AVOID INVALID PARENT ERROR IN JAGS
COLEFF<- eff %>% group_by(COLO,SITE, OCC_NR) %>%
  summarise(effort=sum(effort)) %>%
  mutate(effort=scale(effort, center=T, scale=T)) %>% #mean(effort))/sd(effort)) %>%
  spread(key=OCC_NR, value=effort) %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  #mutate(COL_NR=seq_along(COLO),SITE_NR=row_number())
  mutate(COL_NR=c(1,2,2,3,3,3,3,4),SITE_NR=c(1,1,2,1,2,3,4,1)) #mutate(COL_NR=c(1,2,3,3,3,3,4),SITE_NR=c(1,1,1,2,3,4,1)) 2020 grouping

effmat<-as.matrix(COLEFF[,3:15], dimnames=F)

#COLEFF<-as.numeric(colnames(COLEFF)) # again to organise the seasons

## CREATE LOOKUP VECTOR FOR WHICH COLONY BIRDS WERE CAUGHT IN
head(YESH)
sitevec<-COLEFF$SITE_NR[match(YESH$SITE,COLEFF$SITE)]
colvec<-match(YESH$COLO,unique(YESH$COLO))

length(f) 
length(colvec)
length(sitevec)


## CREATE LOOKUP VECTOR FOR WHICH COLONY BIRDS WERE CAUGHT IN
DURMAT<- survPeriods %>% group_by(COLO,SITE, OCC_NR) %>%
  summarise(int=sum(surv_int)) %>%
  spread(key=OCC_NR, value=int) %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  mutate(COL_NR=c(1,2,2,3,3,3,3,4),SITE_NR=c(1,1,2,1,2,3,4,1))# edited to 2021 sites

periods<-as.matrix(DURMAT[,3:15], dimnames=F)

### JAGS CRASHES WHEN period==0, so we need to remove intermittent 0 and split survival interval over two years
periods[1,4]<-periods[1,3]/2 #2020 sites
periods[1,3]<-periods[1,3]/2

##########################################################################################################
# Specify JS model with colony-specific ABUNDANCE TREND
##########################################################################################################
## GOAL IS TO CALCULATE TREND FOR FIVE MAIN COLONIES
#Cominotto
#StPauls
#Majjistral: Majjistral_main & Majjistral_south
#Rdum tal-Madonna: RM01 & RM03 & RM04 & RM05
#weid babu
#setwd("C:\\STEFFEN\\RSPB\\Malta\\Analysis\\Survival_analysis\\Yelkouan")
#setwd("C:\\Users\\martin.austad\\Documents\\yesh_cmr_HG_17082021\\trend")
#setwd("C:\\Users\\hannah.greetham\\Documents\\yesh_cmr_HG_17082021\\trend")
setwd("C:\\Users\\Martin\\Documents\\MSFD_CMR\\YESH")

sink("YESH_JS_abundance_trend_v6.jags")
cat("
    
    
    model
    {
    
    ##################### Priors and constraints #################################
    
    ### SURVIVAL PROBABILITY
    for (t in 1:(n.years-1)){
    ann.surv[t] ~ dunif(0.7, 1)                      # Priors for age-specific ANNUAL survival - this is the basic survival that is scaled to difference between occasions
    for (col in 1:n.cols) {
    for (s in 1:n.sites){
    phi[s,t,col] <- pow(pow(ann.surv[t],(1/365)),periods[s,t,col]) 
    } #s
    } #col
    } #t
    
    
    ### RECAPTURE PROBABILITY
    mean.p ~ dbeta(1.5, 4)                        # Prior for mean recapture switched to beta from unif
    logit.p <- log(mean.p / (1-mean.p))           # Logit transformation
    
    for (col in 1:n.cols){
    beta.effort[col] ~ dnorm(0,0.01)                   # Prior for trapping effort offset on capture probability
    }
    
    for (col in 1:n.cols) {
    for (i in 1:M){  
    for (t in 1:n.years){
    logit(p[i,t,col]) <- logit.p + beta.effort[col]*effmat[sitevec[i,col],t,col] + capt.raneff[i,t,col]   ## includes colony-specific effort and random effect for time and individual
    } # close t
    } #i
    } #col
    
    ## RANDOM INDIVIDUAL EFFECT ON CAPTURE PROBABILITY
    for (col in 1:n.cols) {
    for (i in 1:M){
    for (t in 1:n.years){
    capt.raneff[i,t,col] ~ dnorm(0, tau.capt)
    }
    }
    }
    
    ### PRIORS FOR RANDOM EFFECTS
    sigma.capt ~ dunif(0, 10)                     # Prior for standard deviation of capture
    tau.capt <- pow(sigma.capt, -2)
    
    
    ### RECRUITMENT PROBABILITY INTO THE MARKED POPULATION
    for (col in 1:n.cols){    
    for (t in 1:n.years){
    gamma[t,col] ~ dunif(0, 1)
    } #t
    } #col
    
    
    ##################### LIKELIHOOD #################################
    
    for (col in 1:n.cols) {
    for (i in 1:M){
    
    # First occasion
    # State process
    z[i,1,col] ~ dbern(gamma[1,col])
    
    # Observation process
    mu1[i,1,col] <- z[i,1,col] * p[i,1,col]
    y[i,1,col] ~ dbern(mu1[i,1,col])
    
    
    # Subsequent occasions
    for (t in 2:n.years){
    
    # State process
    recru[i,t-1,col] <- max(z[i,1:(t-1),col])		# Availability for recruitment - this will be 0 if the bird has never been observed before and 1 otherwise
    pot.alive[i,t,col] <- phi[sitevec[i,col],t-1,col] * z[i,t-1,col] + gamma[t,col] * (1-recru[i,t-1,col])
    z[i,t,col] ~ dbern(pot.alive[i,t,col])
    
    # Observation process
    mu1[i,t,col] <- z[i,t,col] * p[i,t,col]	
    y[i,t,col] ~ dbern(mu1[i,t,col])
    } #t
    } #i
    } #col
    
    
    ##################### DERIVED PARAMETERS #################################
    
    # POPULATION SIZE
    for (t in 1:n.years){
    for (col in 1:n.cols){
    N[t,col] <- sum(z[1:M,t,col])        # Actual population size per colony
    } #col
    } #t
    
    ##################### DERIVED TREND #################################
    
    for (col in 1:n.cols){
    col.trend[col]<-mean(trend[,col])
    for (t in 2:n.years){
    trend[t-1,col] <- N[t-1,col]/max(N[t,col],1)        # trend estimate, avoid division by 0
    } #t
    } #col
    
    
    }								#### END OF THE MODEL STATEMENT
    
    
    
    
    ",fill = TRUE)
sink()




#########################################################################
# PREPARE DATA FOR MODEL - INCLUDING DATA AUGMENTATION
#########################################################################


## CREATE MATRIX for INITIAL STATE Z (MATRIX WITH 0 and 1)
zinit<-CH
for (l in 1:nrow(zinit)){
  firstocc<-get.first(zinit[l,])
  if(firstocc<n.years){
    zinit[l,(firstocc):n.years]<-1  ## alive from first contact - DIFF FROM CJS where this is firstocc+1
  }else{
    zinit[l,firstocc]<-1
  }
  if(firstocc>1){zinit[l,1:(firstocc-1)]<-0}  ## sets everything up to first contact to - - DIFF FROM CJS where this is NA
}
dim(zinit)



### NEED TO CREATE 3-DIMENSIONAL ARRAY FOR y FOR EACH COLONY
### AUGMENT DATA TO HAVE 1000 INDIVIDUALS PER COLONY
### ARRAY must have identical dimensions
col.n<-as.numeric(table(colvec))
potYESH<-1500   ### changed to 1000 from 1500 on 12 Aug 2020 #increased back to 1500 in 2021; site 4 exceeded 1000
#CHcol<- simplify2array(by(YESH[,4:11], YESH$COLO, as.matrix),USE.NAMES==F)
#CHcol<- split(YESH[,4:11],f=YESH$COLO)

YESH$COL_NR<-COLEFF$COL_NR[match(YESH$SITE,COLEFF$SITE)]
YESH$SITE_NR<-COLEFF$SITE_NR[match(YESH$SITE,COLEFF$SITE)]


CHcol<-array(0,dim=c(potYESH,n.years,max(COLEFF$COL_NR)))   ## CAPTURE HISTORY FOR EACH INDIVIDUAL IN EACH COLONY
zinit.arr<-array(0,dim=c(potYESH,n.years,max(COLEFF$COL_NR)))                       # array for the initial values for z states for each individual in each colony
per.arr<-array(365,dim=c(max(DURMAT$SITE_NR),n.years,max(DURMAT$COL_NR)))						# array for the survival periods between recapture episodes
eff.arr<-array(0,dim=c(max(COLEFF$SITE_NR),n.years,max(COLEFF$COL_NR)))						# array for the trapping effort
site.arr<-array(1,dim=c(potYESH,max(COLEFF$COL_NR)))						                  # array for the colony-specific site at which each bird was captured


for (col in 1:max(COLEFF$COL_NR)) {
  
  ## populate capture history
  CHcol[1:col.n[col],,col]<-as.matrix(YESH[YESH$COL_NR==col,4:13]) #increased from 11 to 13
  
  ## populate capture effort
  sitespercol<-max(COLEFF$SITE_NR[COLEFF$COL_NR==col])
  eff.arr[1:sitespercol,,col]<-as.matrix(COLEFF[COLEFF$COL_NR==col,3:12]) #increased from 10 to 12
  
  ## populate period array for length of survival periods between occasions
  per.arr[1:sitespercol,,col]<-as.matrix(DURMAT[DURMAT$COL_NR==col,3:12])  #increased from 10 to 12
  
  ## populate array for individual-specific vectors
  site.arr[1:col.n[col],col]<-as.vector(YESH$SITE_NR[YESH$COL_NR==col])
  
  ## populate array for initial states
  zinit.arr[1:col.n[col],,col]<-zinit[YESH$COL_NR==col,]
  
}


### TO AVOID LOOPING OVER PARAMETER SPACE WITH NO DATA, WE CREATE A VECTOR THAT SPECIFIES WHEN DATA ARE AVAILABLE
## FOR EACH COLONY


col.first<- COLEFF %>% 
  dplyr::select(-SITE, -SITE_NR) %>%
  gather(key="OCC", value="effort",-COLO,-COL_NR) %>%
  dplyr::filter(effort!=0) %>%
  group_by(COLO,COL_NR) 

col.first$OCC<-as.numeric(col.first$OCC) ##changed OCC to numeric so that first OCC Is 2 or 3 not 10!
 col.first<- col.first%>%
  summarise(first=min(OCC)) %>%
  arrange(COL_NR)


# Bundle data
jags.data <- list(y = CHcol, n.years = n.years,col.first=as.numeric(col.first$first),
                  M = potYESH, sitevec=site.arr, 
                  periods=per.arr, effmat=eff.arr,
                  n.sites=max(COLEFF$COL_NR),n.cols=max(DURMAT$SITE_NR)) 

# Initial values 
inits <- function(){list(mean.phi = runif(1, 0.95, 1),
                         mean.p = rbeta(1, 1.5, 4),
                         z = zinit.arr,
                         beta.effort = rnorm(4, 0, 10))}


# Parameters monitored
parameters <- c("N", "ann.surv") # changed parameters to see if it could extract any data

# MCMC settings
# no convergence with ni=50,000, which took 760 minutes

ni <- 75000
nt <- 3 
nb <- 15000
nc <- 3

# Call JAGS from R
YESHabund <- jags(jags.data, inits, parameters, "C:\\Users\\Martin\\Documents\\MSFD_CMR\\YESH\\YESH_JS_abundance_trend_v6.jags",  #C:\\STEFFEN\\RSPB\\Malta\\Analysis\\Survival_analysis\\Yelkouan\\YESH_JS_abundance_trend_v6.jags
                  n.chains = nc, n.thin = nt, n.burnin = nb,parallel=T, n.iter=ni)



### ERROR OCCURS!!!!! ######
#Error in checkForRemoteErrors(val) : 
#3 nodes produced errors; first error: RUNTIME ERROR:
  #Compilation error on line 14.
#Index out of range taking subset of  periods
#########################################################################
# PRODUCE OUTPUT TABLE
#########################################################################
setwd("C:\\Users\\Martin\\Documents\\MSFD_CMR\\YESH")
save.image("YESH_JS_output2021.RData")

out<-as.data.frame(YESHabund$summary)
out$parameter<-row.names(YESHabund$summary)

export<-out %>% select(c(12,1,5,2,3,7)) %>%
  setNames(c('Parameter','Mean', 'Median','SD','lcl', 'ucl'))
fwrite(export,"YESH_Malta_Abundance_estimates2021.csv")

#########################################################################
# PRODUCE SURVIVAL GRAPH 
#########################################################################

ggplot(data=export[41:49,],aes(y=Mean, x=seq(2012.5,2020.5,1))) + geom_point(size=2)+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.1)+
  scale_x_continuous(name="Year", limits=c(2012,2021), breaks=seq(2012,2021,1), labels=seq(2012,2021,1))+
  scale_y_continuous(name="Annual survival probability", limits=c(0.5,1), breaks=seq(0.5,1,0.1), labels=seq(0.5,1,0.1))+
  #ggtitle(COLEFF$Colony)+
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=20, color="black", margin=6), 
        axis.title=element_text(size=22),
        plot.title=element_text(size=22), 
        strip.text.x=element_text(size=20, color="black", margin=6), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

ggsave("YESH_survival_2012_2021.pdf", device = "pdf", width=12, height=9)

#########################################################################
# PRODUCE ABUNDANCE GRAPH 
#########################################################################

abund<-out %>% select(c(12,1,5,2,3,7)) %>%
  setNames(c('Parameter','Mean', 'Median','SD','lcl', 'ucl')) %>%
  dplyr::filter(grepl("N",Parameter)) %>%
  mutate(Colony=substr(Parameter,5,5),Year=as.numeric(substr(Parameter,3,3))+2011) %>%
  mutate(Colony=COLEFF$COLO[match(Colony,COLEFF$COL_NR)]) %>%
  dplyr::filter(Year>2012)

###append 2021 data which due to '10' in character vector screws up the substr()above: 

abund21<-out %>% select(c(12,1,5,2,3,7)) %>%
  setNames(c('Parameter','Mean', 'Median','SD','lcl', 'ucl')) %>%
  dplyr::filter(grepl("N",Parameter)) %>%
  mutate(Colony=substr(Parameter,6,6),Year=as.numeric(substr(Parameter,3,4))+2011) %>%
  mutate(Colony=COLEFF$COLO[match(Colony,COLEFF$COL_NR)]) %>%
  dplyr::filter(Year>2012)

abund <- rbind(abund, abund21)

### plot output ###

ggplot(data=abund,aes(y=Mean, x=Year)) + geom_point(size=2)+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.1)+
  scale_x_continuous(name="Year", limits=c(2012,2021), breaks=seq(2012,2021,2), labels=seq(2012,2021,2))+
  facet_wrap(~Colony,ncol=2,scales="free_y") +
  ylab("N of adult Yelkouan Shearwaters") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=20, color="black"), 
        axis.title=element_text(size=22),
        strip.text.x=element_text(size=20, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

ggsave("YESH_abundance_2013_2021.pdf", device = "pdf", width=9, height=9)
