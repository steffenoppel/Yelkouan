###############################################################################
##
##  YELKOUAN SHEARWATER SURVIVAL AND ABUNDANCE ESTIMATION ON MALTA  ##
##
###############################################################################

## written by steffen.oppel@rspb.org.uk on 19 February 2019

## updated on 13 March - new data included and model run successfully
## output alarming: very low adult survival probability

## revised and split from YESH_survival_estimation.r on 25 March 2019
## re-arranged data structure to 1 occasion per year
## model with 32 occasions yielded consistent invalid parent error

## data cleaning section updated on 02 Oct by MA to include other sub-sites

## ABUNDANCE JS MODEL for 4 major colonies introduced on 31 OCT 2019


library(tidyverse)
library(lubridate)
library(data.table)
library(jagsUI)
library(readxl)
library(tidyr)


################################################################################################################
###################### LOAD AND MANIPULATE DATA 	  ######################################################
################################################################################################################



##### LOAD RAW DATA       

setwd("C:\\STEFFEN\\RSPB\\Malta\\Raw_data")
#setwd("C:\\Users\\Martin\\Documents\\working_folder\\CaptureMarkRecapture\\Analysis_2019\\data")
#setwd("C:\\Users\\Martin\\Documents\\working_folder\\CaptureMarkRecapture\\Analysis_2019")
# 

# yesh<-read_excel("YESH_2012_2018_cavestring_replacedrings.xlsx", sheet="with cave string")
# effort<-read_excel("Cave_Activity_complete_2012-2018.xlsx", sheet="Sheet1")
caves<-fread("Cave_ID.csv")


#### replaced with Martins CMR cleaning code on 3 March 2019
# to get list of all rings deployed, in the source file only original rings present and not their replacements
rings <- fread("2012_2019_replacement_and_tags.csv")

#to update "2012_2018_replacement_and_tags.csv" with rings placed in 2019, "new" birds of 2019 were added in excel
#fwrite(unique_rings, "2012_2018_uniquerings.csv")

#####IMP. NOTE: All times in records and effort are in Malta time.



#to create Nightstarting for both ringing records and cave activity (Effort) to match effort on nightstarting
midday<-as.POSIXct("12:00:00", format="%H:%M:%S",tz = "UTC")   # create a reference time to split the day
midday<-format(midday, format="%H:%M:%S",tz = "UTC") 



#since ringing database extract for 2012-2016 gave time in hour only, :00:00 CONCATENATED onto hour value in excel
records <- fread("2012_2019_cavestring_ringingrecords.csv")

records <- records %>%
  mutate(DateTime=dmy_hms(paste(ringingDate,paste(HourTime, sep=":"), sep=" "),tz="Europe/Berlin")) %>%
  dplyr::rename(Cave_String=Cave_string) %>%
  mutate(DateTime=with_tz(DateTime,tz="UTC")) %>% #does not work if using EuropeBerlin tz here
  mutate(Time=format(DateTime, format="%H:%M:%S",tz = "UTC")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600))))%>%
  dplyr::filter(!(Cave_String=="Office")) %>% #"Office" refers to birds stranded or otherwise rescued for which there is no respective effort
  dplyr::filter(Errors!=1) #handful of records remain. these have been checked but could not be resolved. most likely are errors and should therefore not be included in analysis


effort <- fread("Cave_Activity_complete_2012-2019.csv")

effort <- effort %>%
  mutate(DateTime=dmy_hm(paste(Date,paste(start_time, sep=":"), sep=" "),tz="Europe/Berlin")) %>%
  dplyr::filter(!is.na(DateTime)) %>%
  mutate(DateTime=with_tz(DateTime,tz="UTC")) %>%
  mutate(Time=format(DateTime, format="%H:%M:%S",tz = "UTC")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600))))


### EXTRACT RINGING RECORDS WHERE EFFORT DATA IS MISSING ###

head(records)

misseff<-records %>% dplyr::select(Cave_String,NightStarting, ringnumber, ringer) %>%
  left_join(effort[,c(2,18,8,13)],by=c("Cave_String","NightStarting")) %>%
  mutate(hours=as.numeric(hours)) %>%
  dplyr::filter(is.na(hours))

#fwrite(misseff,"YESH_ring_records_missing_effort_CHECK.csv") #none with missing effort after cleaning Oct 2019

#to check in records for ringnumbers marked as 'new' more than once
#checked in excel

####

#to check wether some retrap records do not have a corresponding "new" record
#ok reason for this might be first ringing occured before 2012 and hence not in this dataset
#other ok reason for this is that it was a replacement ring
#or retrap record is an error in typing/reading ring

#checking in pivot by filtering rings without any "new" 
#to check retraps with only one #retrap event? - but if one retrap event is an error should not have corresponding new either
#some retraps in period 2012-2019 remain in dataset even if the pre-2012 "new" record was not found in BLM ringing scheme database, records sent to John for checking but confident they are correct since retraped more than once

#initialsites <- c("MT24_Majjistral_Eggshell", "MT24_Majjistral_Thomas", "MT24_Majjistral_Thomas_NS1", "MT24_Majjistral_Subt", "MT24_Majjistral_NS2_NS3")
#Majjistral_main <- initialsites

RM01 <- c("MT09_RM01")
RM03 <- c("MT09_RM03_18to22", "MT09_RM03", "MT09_RM03_North", "MT09_RM03_South") #if high transcience to remove "MT09_RM03_18to22" - lower effort than other sites
RM05 <- c("MT09_RM05", "MT09_RM05BT", "MT09_RM05GC_Central", "MT09_RM05GC", "MT09_RM05GC_South", "MT09_RM05GC_North", "MT09_RM05BT_Lower", "MT09_RM05BT_Upper") #if high transcience to remove "MT09_RM05BT_Upper" - lower effort than other sites in last years but probably a lot of the effort made at MT09_RM05BT in 2013 was actually at lower
RM04 <- c("MT09_RM04A", "MT09_RM04B", "MT09_RM04D", "MT09_RM04C") #RM04C lowest effort perhaps to remove
Cominotto <- c("MT17_Cominotto_2", "MT17_Cominotto_1", "MT17_Cominotto_3", "MT17_Cominotto", "MT17_Cominotto_7", "MT17_Cominotto_4", "MT17_Cominotto_8", "MT17_Cominotto_9") # 4 only since 2017; #8 & 9 only since 2018 so better to remove these caves?
StPauls <- c("MT22_StPauls_MainCave", "MT22_StPauls_WestCave")
Majjistral_main <- c("MT24_Majjistral_Eggshell", "MT24_Majjistral_Thomas", "MT24_Majjistral_Subt", "MT24_Majjistral_NS2_NS3")             
Majjistral_south <- c("MT24_Majjistral_South_1", 	"MT24_Majjistral_South", "MT24_Majjistral_South_4", "MT24_Majjistral_South_3") #probably too little effort           

adults <- c("6", "4", "2")
               

### REPLACE ORIGINAL CAVE STRING WITH POOLED SITES
## added on 28 Oct 2019 by steffen oppel based on Martin Austad's suggestion how to pool sites

## create lookup-table
## Making LUT
all_lut<-data.frame(orig=as.character(c(RM01,RM03,RM04,RM05,Cominotto,StPauls,Majjistral_main,Majjistral_south)),
                poolloc=as.character(c("RM01",rep("RM03",4),rep("RM04",4),rep("RM05",8),rep("Cominotto",8),rep("StPauls",2),rep("Majjistral_main",4),rep("Majjistral_south",4))),
                maincol=as.character(c(rep("RdumTalMadonna",17),rep("Cominotto",8),rep("StPauls",2),rep("Majjistral",8))))


               
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
replist$orig[replist$orig %in% doublerep]<-replist$orig[replist$repl %in% doublerep]
replist[replist$orig %in% doublerep,] ### this should be empty
replist[replist$repl %in% doublerep,] ### this should have two records


## UPDATE RECORDS
yesh<- records %>%
  mutate(ringnumber=ifelse(ringnumber %in% replist$repl,replist$orig[match(ringnumber,replist$repl)],ringnumber))
length(unique(yesh$ringnumber))  ## reduces number of individuals from 1619 to 1572





#########################################################################
# MAKE SURE THAT EACH Resighting is preceded by a New capture
#########################################################################
noringdepl<- yesh %>% group_by(ringnumber,type) %>%
  summarise(first=min(DateTime)) %>%
  spread(key=type, value=first) %>%
  mutate(tdiff=as.numeric(difftime(R,N,'days'))) %>%
  dplyr::filter(!is.na(R)) %>%
  dplyr::filter(tdiff<0 | is.na(tdiff))

#fwrite(noringdepl,"YESH_recaptures_only.csv")


               
#########################################################################
# CREATE MATRIX OF ENCOUNTER DATA (0/1)
#########################################################################
   

### FIRST WE NEED TO CREATE A LIST OF OCCASIONS AND NUMBER THEM SEQUENTIALLY
## ANALYSE AT SEASON LEVEL
names(yesh)
               
OCC_lookup<- yesh %>%
      mutate(Year=year(NightStarting)) %>%
      mutate(month=month(NightStarting)) %>%
      #mutate(Occ=paste(Year,month,sep="_")) %>%
      mutate(SEASON=ifelse(month<10,(Year-1),Year)) %>% ###define a breeding season from Oct to Jul
      group_by(SEASON) %>%
      summarise(middate=median(NightStarting)) %>%
      arrange(middate) %>%
      mutate(OCC_NR=seq_along(SEASON))
               
             
               
YESH<- yesh %>% #mutate(Colony=caves$Subcolony_Name[match(Cave_String,caves$Cave_String)]) %>%
           mutate(Year=year(NightStarting)) %>%
           mutate(month=month(NightStarting), seen=1) %>%
           #mutate(Occ=paste(Year,month,sep="_")) %>%
           mutate(SEASON=ifelse(month<10,(Year-1),Year)) %>% ###define a breeding season from Oct to Jul
           mutate(OCC_NR=OCC_lookup$OCC_NR[match(SEASON,OCC_lookup$SEASON)]) %>%
           group_by(COLO,SITE, OCC_NR,ringnumber) %>%   ## REPLACED Colony with SITE based on Martin's grouping in October 2019
           summarise(ALIVE=max(seen,na.rm=T)) %>%
           spread(key=OCC_NR, value=ALIVE) %>%
           replace(is.na(.), 0) %>%
           ungroup() %>%
           mutate(COLO=as.character(COLO),SITE=as.character(SITE))

#length is longer the unique(records$ringnumber) cause some rings are assigned to more than one cavestring but this should not be a problem
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

transientCH<-transients %>% gather(key='OCC', value='surv',-ringnumber,-SITE,-COLO) %>%
  dplyr::filter(surv==1) %>%
  arrange(ringnumber,OCC) %>%
  group_by(ringnumber) %>% summarise(COLO=last(COLO),SITE=last(SITE)) %>%
  left_join(transientCH, by="ringnumber") %>%
  dplyr::select(2,3,1,4:11)

YESH<- YESH %>% dplyr::filter(!(ringnumber %in% duplicates)) %>%
  bind_rows(transientCH)
dim(YESH)




#########################################################################
# CREATE MATRIX OF EFFORT DATA AND DIFFERENCES BETWEEN OCCASIONS
#########################################################################
               
### FORMAT EFFORT DATA ###
head(effort)
min(effort$Date)
               
eff<- effort %>% #mutate(Colony=caves$Subcolony_Name[match(Cave_String,caves$Cave_String)]) %>% ### REPLACED WITH THE GROUPING MARTIN PROVIDED IN OCTOBER 2019
         #mutate(Colony=Cave_String) %>%  ### this does not work unless the 'periods' are somehow adjusted'
         mutate(Year=year(NightStarting)) %>%   ## needs to be done due to conflicts between year=2017 for dates from 2015
         #mutate(NightStarting=dmy(Date)) %>%
         mutate(month=month(NightStarting)) %>%
         mutate(SEASON=ifelse(month<10,(Year-1),Year)) %>% ###define a breeding season from Oct to Jul
         mutate(OCC_NR=OCC_lookup$OCC_NR[match(SEASON,OCC_lookup$SEASON)]) %>%
         mutate(hours=as.numeric(hours)) %>%
         group_by(COLO,SITE, SEASON,OCC_NR) %>%  ## REPLACED Colony with SITE based on Martin's grouping in October 2019
         summarise(effort=sum(hours,na.rm=T))
               
               
### MATRIX OF EFFORT BY Cave_String ###
### CALCULATE INTERVAL BETWEEN OCCASIONS ###
survPeriods<-effort %>% 
              mutate(Year=year(NightStarting)) %>%   ## needs to be done due to conflicts between year=2017 for dates from 2015
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
CH<-as.matrix(YESH[,4:11], dimnames=F)
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
               
               
## CREATE MATRIX for INITIAL STATE Z
               
zinit<-CH
for (l in 1:nrow(zinit)){
     firstocc<-get.first(zinit[l,])
     if(firstocc<n.years){
            zinit[l,(firstocc):n.years]<-1  ## alive from first contact - DIFF FROM CJS where this is firstocc+1
     }else{
            zinit[l,firstocc]<-1
           }
     zinit[l,1:firstocc]<-NA #0  ## sets everything up to first contact to - - DIFF FROM CJS where this is NA
     }
dim(zinit)
               
               
## CREATE MATRIX TO LOOK UP OBSERVATION EFFORT
## STANDARDISE HOURS TO AVOID INVALID PARENT ERROR IN JAGS
COLEFF<- eff %>% group_by(COLO,SITE, OCC_NR) %>%
         summarise(effort=sum(effort)) %>%
         mutate(effort=scale(effort, center=T, scale=T)) %>% #mean(effort))/sd(effort)) %>%
         spread(key=OCC_NR, value=effort) %>%
         replace(is.na(.), 0) %>%
         ungroup() %>%
         #mutate(COL_NR=seq_along(COLO),SITE_NR=row_number())
        mutate(COL_NR=c(1,2,2,3,3,3,3,4),SITE_NR=c(1,1,2,1,2,3,4,1))
               
effmat<-as.matrix(COLEFF[,3:10], dimnames=F)
               
               
## CREATE LOOKUP VECTOR FOR WHICH COLONY BIRDS WERE CAUGHT IN
head(YESH)
sitevec<-COLEFF$COL_NR[match(YESH$SITE,COLEFF$SITE)]
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
         mutate(COL_NR=c(1,2,2,3,3,3,3,4),SITE_NR=c(1,1,2,1,2,3,4,1))
               
periods<-as.matrix(DURMAT[,3:10], dimnames=F)
               
### JAGS CRASHES WHEN period==0, so we need to remove intermittent 0 and split survival interval over two years
periods[1,4]<-periods[1,3]/2
periods[1,3]<-periods[1,3]/2













##########################################################################################################
# Specify JS model with colony-occasion-specific ABUNDANCE
##########################################################################################################
## GOAL IS TO CALCULATE ABUNDANCE FOR FOUR MAIN COLONIES
#Cominotto
#StPauls
#Majjistral: Majjistral_main & Majjistral_south
#Rdum tal-Madonna: RM01 & RM03 & RM04 & RM05

sink("YESH_JS_abundance_survival_v5.jags")
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
potYESH<-1500
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
  CHcol[1:col.n[col],,col]<-as.matrix(YESH[YESH$COL_NR==col,4:11])
  
  ## populate capture effort
  sitespercol<-max(COLEFF$SITE_NR[COLEFF$COL_NR==col])
  eff.arr[1:sitespercol,,col]<-as.matrix(COLEFF[COLEFF$COL_NR==col,3:10])
  
  ## populate period array for length of survival periods between occasions
  per.arr[1:sitespercol,,col]<-as.matrix(DURMAT[DURMAT$COL_NR==col,3:10])
  
  ## populate array for individual-specific vectors
  site.arr[1:col.n[col],col]<-as.vector(YESH$SITE_NR[YESH$COL_NR==col])
  
  ## populate array for initial states
  zinit.arr[1:col.n[col],,col]<-zinit[YESH$COL_NR==col,]
  
}


# Bundle data
jags.data <- list(y = CHcol, n.years = n.years, 
                  M = potYESH, sitevec=site.arr, 
                  periods=per.arr, effmat=eff.arr,
                  n.sites=max(COLEFF$COL_NR),n.cols=max(DURMAT$SITE_NR))

# Initial values 
inits <- function(){list(mean.phi = runif(1, 0.95, 1),
                         mean.p = rbeta(1, 1.5, 4),
                         z = zinit.arr,
                         beta.effort = rnorm(4, 0, 10))}


# Parameters monitored
parameters <- c("beta.effort","mean.p","ann.surv","N")

# MCMC settings
# no convergence with ni=50,000, which took 760 minutes

ni <- 150
nt <- 2
nb <- 70
nc <- 4

# Call JAGS from R
YESHabund <- autojags(jags.data, inits, parameters, "C:\\STEFFEN\\RSPB\\Malta\\Analysis\\Survival_analysis\\Yelkouan\\YESH_JS_abundance_survival_v5.jags",
                  n.chains = nc, n.thin = nt, n.burnin = nb,parallel=T)








#########################################################################
# PRODUCE OUTPUT TABLE
#########################################################################
## model converged with 150000 iterations in 270 minutes

out<-as.data.frame(YESHabund$summary)
out$parameter<-row.names(YESHabund$summary)
export<-out %>% select(c(12,1,5,2,3,7)) %>%
  setNames(c('Parameter','Mean', 'Median','SD','lcl', 'ucl'))
fwrite(export,"YESH_Malta_Abundance_estimates2019.csv")


### plot output ###

ggplot(data=export[8:13,],aes(y=Mean, x=seq(2013,2018,1))) + geom_point(size=2)+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.1)+
  geom_point(aes(y=apply(CHaug,2,sum), x=seq(2013,2018,1)), col='darkred',size=2, pch=3)+
  scale_x_continuous(name="Year", limits=c(2012.5,2018.5), breaks=seq(2013,2018,1), labels=seq(2013,2018,1))+
  scale_y_continuous(name="N Yelkouan Shearwaters", limits=c(0,400), breaks=seq(0,400,100), labels=seq(0,400,100))+
  ggtitle(COLEFF$Colony)+
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=20, color="black", margin=6), 
        axis.title=element_text(size=22),
        plot.title=element_text(size=22), 
        strip.text.x=element_text(size=20, color="black", margin=6), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

ggsave("YESH_abundance_Majjistral_2013_2018.pdf", device = "pdf")




ggplot(data=export[3:7,],aes(y=Mean, x=seq(2013.5,2017.5,1))) + geom_point(size=2)+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.1)+
  scale_x_continuous(name="Year", limits=c(2012.5,2018.5), breaks=seq(2013,2018,1), labels=seq(2013,2018,1))+
  scale_y_continuous(name="Annual survival probability", limits=c(0.5,1), breaks=seq(0.5,1,0.1), labels=seq(0.5,1,0.1))+
  ggtitle(COLEFF$Colony)+
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=20, color="black", margin=6), 
        axis.title=element_text(size=22),
        plot.title=element_text(size=22), 
        strip.text.x=element_text(size=20, color="black", margin=6), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

ggsave("YESH_survival_Majjistral_2013_2018.pdf", device = "pdf")









##########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########################################
#
# Specify CJS model to compare survival between 4 main colonies
#
##########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########################################


setwd("C:\\STEFFEN\\RSPB\\Malta\\Analysis\\Survival_analysis\\Yelkouan")
sink("YESH_CJS_simpletest.jags")
cat("
    model {
    
    # -------------------------------------------------
    # Parameters:
    # phi: survival probability, only adults 
    # p: recapture probability when breeding, depends on colony-occasion-specific effort
    # -------------------------------------------------
    
    
    ## Priors and constraints
    
    ### RECAPTURE PROBABILITY
    beta.effort ~ dnorm(0,0.01)                   # Prior for trapping effort offset on capture probability
    
    
    ## COLONY-SPECIFIC CAPTURE PROBABILITY 
    for (col in 1:n.colonies){
    cap.prob[col] ~ dunif(0, 1)                         # Priors for colony-specific capture probability
    mu.p[col] <- log(cap.prob[col] / (1-cap.prob[col]))       # Logit transformation
    for (t in 1:n.occasions){
    logit(p[col,t]) <- mu.p[col] + beta.effort*effmat[col,t]
    }
    }
    
    ### SURVIVAL PROBABILITY
    beta ~ dunif(0.9, 1)                       # Priors for age-specific DAILY survival - this is the basic survival that is scaled to difference between occasions
    for (i in 1:nind){
    for (t in f[i]:(n.occasions-1)){
    beta.int[i,t] <- pow(beta,periods[colvec[i],t])       ### exponentiates daily survival by the length between capture sessions
    mu[i,t] <- log(beta.int[i,t] / (1-beta.int[i,t]))       # Logit transformation
    logit(phi[i,t]) <- mu[i,t]
    } #t
    } #i
    
    # Likelihood 
    for (i in 1:nind){
    # Define latent state at first capture
    z[i,f[i]] <- 1
    for (t in (f[i]+1):n.occasions){
    # State process
    z[i,t] ~ dbern(mu1[i,t])
    mu1[i,t] <- phi[i,t-1] * z[i,t-1] 
    
    # Observation process
    y[i,t] ~ dbern(mu2[i,t])
    mu2[i,t] <- p[colvec[i],t] * z[i,t]
    } #t
    } #i
    
    # DERIVED SURVIVAL PROBABILITIES PER YEAR 
    ann.surv <- pow(beta,365)
    
    }
    ",fill = TRUE)
sink()





#########################################################################
# PREPARE DATA FOR SIMPLE CJS MODEL
#########################################################################

# Bundle data
jags.data <- list(y = CH, f = f, n.occasions = n.years, n.colonies=n.colonies,
                  nind = n.ind, #FULLAGEMAT=FULLAGEMAT,
                  periods=periods, colvec=colvec, effmat=effmat)

# Initial values 
inits <- function(){list(beta = runif(1, 0.9, 1),
                         cap.prob = runif(n.colonies, 0, 1),
                         z = zinit,
                         beta.effort = rnorm(1, 0, 10))}


# Parameters monitored
parameters <- c("ann.surv","beta.effort","p")

# MCMC settings
nt <- 1
nb <- 5
nc <- 4

# Call JAGS from R
YESHsurv <- autojags(jags.data, inits, parameters, "C:\\STEFFEN\\RSPB\\Malta\\Analysis\\Survival_analysis\\Yelkouan\\YESH_CJS_simpletest.jags", n.chains = nc, n.thin = nt, n.burnin = nb,parallel=T)




               