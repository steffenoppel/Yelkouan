###############################################################################
##
##  YELKOUAN SHEARWATER SURVIVAL COMPARISON FOR COLONIES ON MALTA  ##
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
## SPLIT FROM ABUNDANCE ESTIMATION on 8 NOV 2019

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
               
               
## CREATE MATRIX TO LOOK UP OBSERVATION EFFORT
## STANDARDISE HOURS TO AVOID INVALID PARENT ERROR IN JAGS
COLEFF<- eff %>% group_by(COLO,SITE, OCC_NR) %>%
         summarise(effort=sum(effort)) %>%
         mutate(effort=scale(effort, center=F, scale=T)) %>% #mean(effort))/sd(effort)) %>%  ## invalid parent error when center=T
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
periods[,8]<-365









##########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########################################
#
# Specify CJS model to compare survival between 4 main colonies
#
##########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########################################


setwd("C:\\STEFFEN\\RSPB\\Malta\\Analysis\\Survival_analysis\\Yelkouan")
sink("YESH_CJS_colony_surv.jags")
cat("
    model {
    
    # -------------------------------------------------
    # Parameters:
    # phi: survival probability, only adults 
    # p: recapture probability when breeding, depends on colony-occasion-specific effort
    # -------------------------------------------------
    
    
    ## Priors and constraints

    ### SITE-SPECIFIC CAPTURE PROBABILITY
    mean.p ~ dbeta(1.5, 4)                        # Prior for mean recapture switched to beta from unif
    logit.p <- log(mean.p / (1-mean.p))           # Logit transformation

    for (col in 1:n.cols) {
      for (s in 1:n.sites){
        beta.effort[s,col] ~ dunif(0,10)                   # Prior for trapping effort offset on capture probability
        for (t in 1:n.years){
          logit(p[s,t,col]) <- logit.p + beta.effort[s,col]*effmat[s,t,col] + capt.raneff[col,t]   ## includes site-specific effort and random effect for time and individual
        } # close t
      } #s
    } #col
    
    ## RANDOM INDIVIDUAL EFFECT ON CAPTURE PROBABILITY
    for (col in 1:n.cols) {
       for (t in 1:n.years){
          capt.raneff[col,t] ~ dnorm(0, tau.capt)
        }
      }
    
    ### PRIORS FOR RANDOM EFFECTS
    sigma.capt ~ dunif(0, 10)                     # Prior for standard deviation of capture
    tau.capt <- pow(sigma.capt, -2)

    
    ### COLONY- AND YEAR-SPECIFIC SURVIVAL PROBABILITY
    for (t in 1:(n.years-1)){
      for (col in 1:n.cols) {
        ann.surv[t,col] ~ dunif(0.7, 1)                      # Priors for colony-specific ANNUAL survival - this is the basic survival that is scaled to difference between occasions which are site-specific
        for (s in 1:n.sites){
          phi[s,t,col] <- pow(pow(ann.surv[t,col],(1/365)),periods[s,t,col]) 
        } #s
      } #col
    } #t
    
    
    # Likelihood 
    for (i in 1:n.ind){

        # Define latent state at first capture
        z[i,f[i]] <- 1
          for (t in (f[i]+1):n.years){

          # State process
            z[i,t] ~ dbern(mu1[i,t])
            mu1[i,t] <- phi[sitevec[i],t-1,colvec[i]] * z[i,t-1] 
    
          # Observation process
            y[i,t] ~ dbern(mu2[i,t])
            mu2[i,t] <- p[sitevec[i],t,colvec[i]] * z[i,t]
          } #t
    } #i
    
    }
    ",fill = TRUE)
sink()





#########################################################################
# PREPARE DATA FOR CJS MODEL TO COMPARE COLONY-SPECIFIC SURVIVAL
#########################################################################

names(YESH)
CH<-as.matrix(YESH[,4:11], dimnames=F)
dim(CH)

# Compute vector with occasion of first capture
get.first <- function(x) min(which(x==1))
f <- apply(CH, 1, get.first)
n.occ<-ncol(CH)

## PREPARE CONSTANTS
n.ind<-dim(CH)[1]		## defines the number of individuals
n.years<-dim(CH)[2]  ## defines the number of years
n.sites<-max(COLEFF$SITE_NR)
n.colonies<-max(COLEFF$COL_NR)              
sitevec<-COLEFF$SITE_NR[match(YESH$SITE,COLEFF$SITE)]
colvec<-COLEFF$COL_NR[match(YESH$SITE,COLEFF$SITE)]
YESH$COL_NR<-COLEFF$COL_NR[match(YESH$SITE,COLEFF$SITE)]
YESH$SITE_NR<-COLEFF$SITE_NR[match(YESH$SITE,COLEFF$SITE)]


## CREATE MATRIX for INITIAL STATE Z
zinit<-CH
for (l in 1:nrow(zinit)){
  firstocc<-get.first(zinit[l,])
  if(firstocc<n.years){
    zinit[l,(firstocc+1):n.years]<-1  ## alive from first contact - DIFF FROM JS where this is firstocc
  }else{
    zinit[l,firstocc]<-1
  }
  zinit[l,1:firstocc]<-NA           ## sets everything up to first contact to - DIFF FROM JS where this is 0
}
dim(zinit)



### CREATE 3-DIMENSIONAL ARRAY FOR EFFORT AT SITES WITHIN EACH COLONY
eff.arr<-array(0,dim=c(max(COLEFF$SITE_NR),n.years,max(COLEFF$COL_NR)))						# array for the trapping effort
per.arr<-array(365,dim=c(max(DURMAT$SITE_NR),n.years,max(DURMAT$COL_NR)))					# array for the survival periods between recapture episodes
DURMAT[,10]<-365
for (col in 1:max(COLEFF$COL_NR)) {
  sitespercol<-max(COLEFF$SITE_NR[COLEFF$COL_NR==col])
  eff.arr[1:sitespercol,,col]<-as.matrix(COLEFF[COLEFF$COL_NR==col,3:10])

  ## populate period array for length of survival periods between occasions
  per.arr[1:sitespercol,,col]<-as.matrix(DURMAT[DURMAT$COL_NR==col,3:10])
  
}



# Bundle data
jags.data <- list(y = CH, n.years = n.years,n.ind=dim(CH)[1],f=f,
                  sitevec=sitevec,colvec=colvec, 
                  periods=per.arr, effmat=eff.arr,
                  n.sites=max(COLEFF$COL_NR),n.cols=max(DURMAT$SITE_NR))

# Initial values 
inits <- function(){list(ann.surv = matrix(runif(1, 0.7, 1),ncol=max(DURMAT$COL_NR),nrow=(n.years-1)),
                         mean.p = rbeta(1, 1.5, 4),
				sigma.capt = runif(1, 0, 10),
                         z = zinit,
                         beta.effort = matrix(runif(1, 0, 10),ncol=max(DURMAT$COL_NR),nrow=max(DURMAT$SITE_NR)))}

# Parameters monitored
parameters <- c("ann.surv","mean.p")

# MCMC settings
ni<-5000
nt <- 2
nb <- 500
nc <- 4

# Call JAGS from R
YESHsurv <- jags(jags.data, inits, parameters, "C:\\STEFFEN\\RSPB\\Malta\\Analysis\\Survival_analysis\\Yelkouan\\YESH_CJS_colony_surv.jags",
			n.chains = nc, n.thin = nt, n.burnin = nb,n.iter=ni,parallel=T)




#########################################################################
# PRODUCE OUTPUT TABLE
#########################################################################
setwd("C:\\STEFFEN\\RSPB\\Malta\\Analysis\\Survival_analysis\\Yelkouan")
save.image("YESH_CJS_model_output.RData")

out<-as.data.frame(YESHsurv$summary)
out$parameter<-row.names(YESHsurv$summary)

surv<-out %>% select(c(12,1,5,2,3,7)) %>%
  setNames(c('Parameter','Mean', 'Median','SD','lcl', 'ucl')) %>%
  dplyr::filter(grepl("ann.surv",Parameter)) %>%
  mutate(Colony=substr(Parameter,12,12),Year=as.numeric(substr(Parameter,10,10))+2011) %>%
  mutate(Colony=COLEFF$COLO[match(Colony,COLEFF$COL_NR)]) 
  
fwrite(surv,"YESH_Malta_Colony_specific_survival2019.csv")





#########################################################################
# PRODUCE SURVIVAL ESTIMATES GRAPH 
#########################################################################


### plot output ###

ggplot(data=surv,aes(y=Mean, x=Year)) + geom_point(size=2)+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.1)+
  facet_wrap(~Colony,ncol=2,scales="fixed") +
  ylab("Annual survival probability") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=20, color="black"), 
        axis.title=element_text(size=22),
        strip.text.x=element_text(size=20, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

ggsave("YESH_Malta_Colony_specific_survival2019.pdf", device = "pdf", width=9, height=9)









               