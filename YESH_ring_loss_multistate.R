###############################################################################
##
##  YELKOUAN SHEARWATER SURVIVAL AND RING LOSS ESTIMATION ON MALTA  ##
##
###############################################################################

## written by steffen.oppel@rspb.org.uk on 23 May 2025

rm(list = ls())
library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)
library(jagsUI)
library(readxl)
library(tidyr)
select<-dplyr::select
filter<-dplyr::filter



################################################################################################################
###################### LOAD AND MANIPULATE DATA 	  ######################################################
################################################################################################################

##### LOAD RAW DATA  
#setwd("C:\\Users\\martin.austad\\Documents\\yesh_cmr_HG_02082021")
#setwd("C:\\Users\\hannah.greetham\\Documents\\yesh_cmr_HG_17082021")
#setwd("C:\\STEFFEN\\RSPB\\Malta\\Raw_data")
#setwd("C:\\Users\\Martin\\Documents\\MSFD_CMR\\YESH")

try(setwd("C:\\Users\\martin.austad\\Documents\\PanPuffinus\\CMR"), silent=T)
try(setwd("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\Malta\\Analysis\\Survival_analysis\\2025\\CMR_data"), silent=T)
try(setwd("C:\\STEFFEN\\Vogelwarte\\YESH\\Yelkouan"), silent=T)

# yesh<-read_excel("YESH_2012_2018_cavestring_replacedrings.xlsx", sheet="with cave string")
# effort<-read_excel("Cave_Activity_complete_2012-2018.xlsx", sheet="Sheet1")

###read in CMR subsites
caves<-fread("data/Cave_ID.csv")

# to get list of all rings deployed, in the source file only original rings present and not their replacements
rings <- fread("data/2012_2024_replacement_and_tags.csv")

#####IMP. NOTE: All times in records and effort are in Malta time.

#to create Nightstarting for both ringing records and cave activity (Effort) to match effort on nightstarting
midday<-as.POSIXct("12:00:00", format="%H:%M:%S",tz = "UTC")   # create a reference time to split the day
midday<-format(midday, format="%H:%M:%S",tz = "UTC") 


R2022 <- fread("data/2022_YESH_Biometrics_RingingRecords.csv")

R2022$ringingDate <- as.Date(R2022$ringingDate, format="%d/%m/%Y")
range(R2022$ringingDate)

colnames(R2022)

colnames(R2022)[28] <- "remarks"
colnames(R2022)[27] <- "broodpatch"
colnames(R2022)[26] <- "tags"

R2023 <- fread("data/2023_YESH_Biometrics_RingingRecords.csv") 

R2023$ringingDate <- as.Date(R2023$ringingDate, format="%d/%m/%Y")
range(R2023$ringingDate)

colnames(R2023)

colnames(R2023)[28] <- "remarks"
colnames(R2023)[27] <- "broodpatch"
colnames(R2023)[26] <- "tags"

#load 2024 ringing data
R2024 <- fread("data/2024_YESH_Biometrics_RingingRecords.csv")

R2024$ringingDate <- as.Date(R2024$ringingDate, format="%d/%m/%Y")
range(R2024$ringingDate)
colnames(R2024)

colnames(R2024)[28] <- "remarks"
colnames(R2024)[27] <- "broodpatch"
colnames(R2024)[26] <- "tags"

#load 2025 ringing data
R2025 <- fread("data/2025_YESH_Biometrics_RingingRecords.csv")
R2025$ringingDate <- as.Date(R2025$ringingDate, format="%d/%m/%Y")
R2025$HourTime<-paste(R2025$HourTime,":00", sep="")   ## add missing seconds
range(R2025$ringingDate)
colnames(R2025)

colnames(R2025)[28] <- "remarks"
colnames(R2025)[27] <- "broodpatch"
colnames(R2025)[26] <- "tags"

#since ringing database extract for 2012-2016 gave time in hour only, :00:00 CONCATENATED onto hour value in excel
records <- fread("data/2012_2021_cavestring_ringingrecords_updated.csv")

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
  mutate(DateTime=as.POSIXct(paste(ringingDate, HourTime, " "), format="%Y-%m-%d %H:%M:%S", tz="Europe/Berlin")) %>%
  #mutate(DateTime=ymd(ringingDate) + hms(HourTime), tz = "Europe/Berlin") 
  dplyr::rename(Cave_String=Cave_string) %>%
  # mutate(DateTime=with_tz(DateTime,tz="UTC")) %>% #does not work if using EuropeBerlin tz here
  mutate(Time=format(DateTime, format="%H:%M:%S",tz = "Europe/Berlin")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600))))%>%
  dplyr::filter(!(Cave_String=="Office")) %>% #"Office" refers to birds stranded or otherwise rescued for which there is no respective effort
  dplyr::filter(!(Cave_String=="office")) %>%  
  dplyr::filter(Errors!=1) #handful of records remain. these have been checked but could not be resolved. most likely are errors and should therefore not be included in analysis


effort <- fread("data/Cave_Activity_complete_2012-2025DT.csv")


##does the start and end time overlap midnight? 
##create an end date and correct for midnight overlap
effort <- as.data.frame(effort) %>%   ### fixed this to data frame because data.table does not support multicolumn values such as intervals: https://github.com/tidyverse/dtplyr/issues/475
  dplyr::filter(!is.na(start_time) & !is.na(end_time)) %>%
  mutate(
    start=as.POSIXct(start_time, format="%H:%M",tz = "Europe/Berlin"),
    end=as.POSIXct(end_time, format="%H:%M", tz="Europe/Berlin"),
    st_minutes = hour(start) * 60 + minute(start),
    et_minutes = hour(end) * 60 + minute(end),
    overlaps_midnight = if_else(st_minutes > et_minutes, "YES", "NO")) %>%
  mutate(start_date = as.Date(Date, format="%d/%m/%Y"),
         end_date=if_else(overlaps_midnight=="NO", start_date, start_date+days(1))) %>%
  mutate(Start = as.POSIXct(paste(start_date, start_time, " "), format="%Y-%m-%d %H:%M", tz="Europe/Berlin"), 
         End= as.POSIXct(paste(end_date, end_time, " "), format="%Y-%m-%d %H:%M", tz="Europe/Berlin")) %>%
  mutate(SessionInt=interval(Start, End, tz="Europe/Berlin")) %>%
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

## temp fix for 2024 season (to accommodate ring loss data in 2025)
OCC_lookup

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
# READ IN RING LOSS DATA
#########################################################################


ringloss<-fread("data/YESH_doublemarks.csv") %>%
  rename(ringnumber=Original_ring) %>%
  filter(ringnumber %in% unique(YESH$ringnumber)) %>%
  group_by(ringnumber) %>%
  summarise(DRstart=min(DoubleTagON),DRend=max(DoubleTagOff), LostRing=max(LostRing),n=length(unique(DoubleTagOff))) %>%
  mutate(SEASONstart=ifelse(month(DRstart)<10,(year(DRstart)-1),year(DRstart))) %>% ###define a breeding season from Oct to Jul
  mutate(OCC_start=OCC_lookup$OCC_NR[match(SEASONstart,OCC_lookup$SEASON)]) %>%
  mutate(SEASONend=ifelse(month(DRend)<10,(year(DRend)-1),year(DRend))) %>% ###define a breeding season from Oct to Jul
  mutate(OCC_end=OCC_lookup$OCC_NR[match(SEASONend,OCC_lookup$SEASON)]) %>%
  arrange(ringnumber)

## CREATE RING LOSS ENCOUNTER HISTORY WITH STATES 1,2,3
# abandoned because the occasion-specific probability is very low
RL_CH<-YESH %>% #dplyr::filter(ringnumber %in% ringloss$ringnumber) %>%
  arrange(ringnumber)
DR_CH<-YESH %>% arrange(ringnumber)
get.first <- function(x) min(which(x==1))
f <- apply(as.matrix(RL_CH[4:dim(RL_CH)[2]]), 1, get.first)
for (i in unique(RL_CH$ringnumber)){
  DR_CH[DR_CH$ringnumber==i,(4):(dim(DR_CH)[2])]<-0
  if(i %in% unique(ringloss$ringnumber)){
    strt<-ringloss$OCC_start[ringloss$ringnumber==i]+3
    end<-ringloss$OCC_end[ringloss$ringnumber==i]+3
    DR_CH[DR_CH$ringnumber==i,strt:end]<-1

    if(ringloss$LostRing[ringloss$ringnumber==i]==1){
      loss_occ<-ringloss$OCC_end[ringloss$ringnumber==i]
      loss_occ<-ifelse(loss_occ==f[which(RL_CH$ringnumber==i)], loss_occ+1,loss_occ) # avoid loss on first occasion
      RL_CH[RL_CH$ringnumber==i,loss_occ+3]<-2
      if((loss_occ+3)<(dim(RL_CH)[2])){
        RL_CH[RL_CH$ringnumber==i,((loss_occ+4):(dim(RL_CH)[2]))]<-ifelse(RL_CH[RL_CH$ringnumber==i,((loss_occ+4):(dim(RL_CH)[2]))]==0,3,2)  ## birds cannot be seen with ring after ring loss
        DR_CH[DR_CH$ringnumber==i,((loss_occ+4):(dim(DR_CH)[2]))]<-ifelse(RL_CH[RL_CH$ringnumber==i,((loss_occ+4):(dim(RL_CH)[2]))]==0,3,1)  ## double ringing must be ON for birds to be recorded that had lost their ring previously
      }
    }
  }
}
RL_CH[100:105,]
DR_CH[100:105,]



#########################################################################
# PREPARE DATA FOR INPUT INTO JAGS
#########################################################################


## MAIN MATRIX OF RESIGHTINGS AND DOUBLE RINGING EFFORT
names(RL_CH)
CH<-as.matrix(RL_CH[,4:dim(RL_CH)[2]], dimnames=F) 
CHDR<-as.matrix(DR_CH[,4:dim(DR_CH)[2]], dimnames=F)
CH<-ifelse(CH==0,3,CH)
dim(CH)

# Compute vector with occasion of first capture
get.first <- function(x) min(which(x==1))
f <- apply(CH, 1, get.first)


### CHECK WHETHER IT LOOKS OK ###
head(CH)

## PREPARE CONSTANTS
## unlike the JS model we do not need sites structured within colonies - we just list 8 sites and loop over each site
n.ind<-dim(CH)[1]		## defines the number of individuals
n.occ<-ncol(CH)  ## defines the number of years

## PREPARE CONSTANTS
n.sites<-length(unique(YESH$SITE))


## CREATE MATRIX TO LOOK UP OBSERVATION EFFORT
## STANDARDISE HOURS TO AVOID INVALID PARENT ERROR IN JAGS
COLEFF<- eff %>% group_by(COLO,SITE, OCC_NR) %>%
  summarise(effort=sum(effort)) %>%
  mutate(effort=scale(effort, center=T, scale=T)) %>% #mean(effort))/sd(effort)) %>%
  spread(key=OCC_NR, value=effort) %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  mutate(SITE_NR=seq_along(COLO))

effmat<-as.matrix(COLEFF[,3:16], dimnames=F)

## CREATE LOOKUP VECTOR FOR WHICH COLONY BIRDS WERE CAUGHT IN
head(YESH)
sitevec<-COLEFF$SITE_NR[match(YESH$SITE,COLEFF$SITE)]

length(f) 
length(sitevec)


## CREATE MATRIX FOR THE DURATION OF THE PERIODS BETWEEN TRAPPING SESSIONS
DURMAT<- survPeriods %>% group_by(COLO,SITE, OCC_NR) %>%
  summarise(int=sum(surv_int)) %>%
  spread(key=OCC_NR, value=int) %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  mutate(SITE_NR=seq_along(COLO))
periods<-as.matrix(DURMAT[,3:16], dimnames=F)

### JAGS CRASHES WHEN period==0, so we need to remove intermittent 0 and split survival interval over two years
periods[1,4]<-periods[1,3]/2 #2020 sites
periods[1,3]<-periods[1,3]/2
  



##########################################################################################################
# MULTISTATE MODEL WITH RING LOSS PROBABILITY
##########################################################################################################

try(setwd("C:\\Users\\rita.matos\\Documents\\CMR"),silent=T)
try(setwd("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\Malta\\Analysis\\Survival_analysis\\2025"), silent=T)
try(setwd("C:\\STEFFEN\\Vogelwarte\\YESH\\Yelkouan"), silent=T)


# Specify model in JAGS language
cat(file = "YESH_ring_loss_temp_var.jags", "
model {

# -------------------------------------------------
# Parameters:
# ann.surv: annual apparent survival probability 
# loss: ring loss probability
# p: recapture probability
# -------------------------------------------------
# States (S):
# 1 alive with ring
# 2 alive without ring
# 3 dead
# Observations (O):  
# 1 seen with ring 
# 2 seen without ring (only possible if individual was doubleringed as specified in CHDR)
# 3 not seen
# -------------------------------------------------

# Priors and constraints
   # phi ~ dunif(0.7, 1)    # Priors for state-spec. survival
   loss ~ dnorm(0.068, 10)    # Prior for ring loss informed by actual data
   # p ~ dunif(0, 1)      # Priors for mean recapture

    ### SURVIVAL PROBABILITY
    for (t in 1:(n.occasions-1)){
      ann.surv[t] ~ dunif(0.7, 1)                      # Priors for age-specific ANNUAL survival - this is the basic survival that is scaled to difference between occasions
        for (s in 1:n.sites){
          phi[s,t] <- pow(pow(ann.surv[t],(1/365)),periods[s,t]) 
        } #s
    } #t
    
    
    ### SITE-SPECIFIC RECAPTURE PROBABILITY
    mean.p ~ dbeta(1.5, 4)                        # Prior for mean recapture switched to beta from unif
    logit.p <- log(mean.p / (1-mean.p))           # Logit transformation
    
    for (s in 1:n.sites){
      beta.effort[s] ~ dnorm(0,0.01)                   # Prior for trapping effort offset on capture probability
         for (t in 1:n.occasions){
          logit(p[s,t]) <- logit.p + beta.effort[s]*effmat[s,t]    ## includes colony-specific effort and random effect for time and individual
        } # close t
    } #s






# Define state-transition and observation matrices

  for (i in 1:nind){
   for (t in (f[i]):(n.occasions-1)){
   
   # Define probabilities of state S(t+1) [last dim] given S(t) [first dim]
      ps[1,i,t,1] <- phi[sitevec[i],t] * (1 - loss)
      ps[1,i,t,2] <- phi[sitevec[i],t] * loss
      ps[1,i,t,3] <- 1 - phi[sitevec[i],t]
      ps[2,i,t,1] <- 0
      ps[2,i,t,2] <- phi[sitevec[i],t]
      ps[2,i,t,3] <- 1 - phi[sitevec[i],t]
      ps[3,i,t,1] <- 0
      ps[3,i,t,2] <- 0
      ps[3,i,t,3] <- 1
   }
      # Define probabilities of O(t) [last dim] given S(t)  [first dim]¨
   for (t in (f[i]+1):n.occasions){
      po[1,i,t,1] <- p[sitevec[i],t]
      po[1,i,t,2] <- 0
      po[1,i,t,3] <- 1 - p[sitevec[i],t]
      po[2,i,t,1] <- 0
      po[2,i,t,2] <- p[sitevec[i],t]*dr[i,t]
      po[2,i,t,3] <- 1 - p[sitevec[i],t]*dr[i,t]
      po[3,i,t,1] <- 0
      po[3,i,t,2] <- 0
      po[3,i,t,3] <- 1
   }
  }

# Likelihood 
for (i in 1:nind){
   # Define latent state at first capture
   z[i,f[i]] <- y[i,f[i]]
   for (t in (f[i]+1):n.occasions){
      # State process: draw S(t) given S(t-1)
      z[i,t] ~ dcat(ps[z[i,t-1],i,t-1,1:3])
      # Observation process: draw O(t) given S(t)
      y[i,t] ~ dcat(po[z[i,t],i,t,1:3])
      } #t
   } #i
}
")


# Bundle data 
jags.data <- list(y = CH,
                  dr=CHDR,
                  f = f,
                  n.sites=n.sites,
                  sitevec=sitevec,
                  periods=periods,
                  effmat=effmat,
                  n.occasions = ncol(CH), 
                  nind = nrow(CH))


# SET Initial values
# Function to create initial values for unknown z
# Input: ch: multistate capture-recapture data (where "not seen" is not 0); f: vector with the occasion of marking
ms.init.z <- function(ch, f){
  states <- max(ch, na.rm = TRUE)   # identify the state that corresponds to "not seen"
  for (i in 1:nrow(ch)){
    ch[i,1:f[i]] <- NA
    if(f[i]<ncol(ch)){
      for(cl in (f[i]+1):ncol(ch))
        ch[i,cl] <- ifelse(ch[i,cl]==states,ch[i,cl-1],ch[i,cl])
      }
    }
  return(ch)
}

inits <- function(){list(ann.surv = runif((n.occ-1), 0.8, 1),
                         z = ms.init.z(CH, f),
                         loss = 0.068,
                         beta.effort=rnorm(n.sites,0,0.01),
                         mean.p = rbeta(1,1.5,4)
                         )}  

# SPECIFY Parameters monitored
parameters <- c("ann.surv", 
                "loss",
                "beta.effort",
                "mean.p")

# DEFINE MCMC settings
ni <- 40000
nt <- 10
nb <- 20000
nc <- 3

# Call JAGS from R
ms <- jags(data = jags.data, 
           inits = inits, 
           parameters.to.save = parameters, 
           model.file = "YESH_ring_loss_temp_var.jags", 
           n.chains = nc, 
           n.thin = nt, 
           n.iter = ni, 
           n.burnin = nb,
           parallel = TRUE)

# Inspect results
print(ms, digits = 3)

par(mfrow = c(3,3))
traceplot(ms)


#

#########################################################################
# PRODUCE OUTPUT TABLE
#########################################################################
try(setwd("C:\\Users\\rita.matos\\Documents\\CMR"), silent=T)
try(setwd("C:\\STEFFEN\\Vogelwarte\\YESH\\Yelkouan"), silent=T)
saveRDS(ms, "output/YESH_output_mutistate_ring_loss.rds")

out<-as.data.frame(ms$summary)
out$parameter<-row.names(ms$summary)

export<-out %>% select(c(12,1,5,2,3,7,8,9)) %>%
  setNames(c('Parameter','Mean', 'Median','SD','lcl', 'ucl','Rhat','n.eff'))
fwrite(export,"output/YESH_Malta_survival_estimates_with_ring_loss_multistate.csv")




#########################################################################
# PRODUCE SURVIVAL GRAPH 
#########################################################################

ggplot(data=export[1:13,],aes(y=Mean, x=seq(2012.5,2024.5,1))) + geom_point(size=2)+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.1)+
  scale_x_continuous(name="Year", limits=c(2012,2025), breaks=seq(2012,2025,1), labels=seq(2012,2025,1))+
  scale_y_continuous(name="Annual survival probability", limits=c(0.5,1), breaks=seq(0.5,1,0.1), labels=seq(0.5,1,0.1))+
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=20, color="black", margin=6), 
        axis.title=element_text(size=22),
        plot.title=element_text(size=22), 
        strip.text.x=element_text(size=20, color="black", margin=6), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

ggsave("output/YESH_survival_2012_2025_ring_loss_multistate.pdf", device = "pdf", width=12, height=9)



