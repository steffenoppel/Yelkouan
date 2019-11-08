###############################################################################
##  YELKOUAN SHEARWATER SURVIVAL AND ABUNDANCE ESTIMATION ON MALTA  ##
###############################################################################
## written by steffen.oppel@rspb.org.uk on 19 February 2019

## updated on 13 March - new data included and model run successfully
## output alarming: very low adult survival probability

## revised and split from YESH_survival_estimation.r on 25 March 2019
## re-arranged data structure to 1 occasion per year
## model with 32 occasions yielded consistent invalid parent error


library(tidyverse)
library(lubridate)
library(data.table)
library(jagsUI)
library(readxl)





################################################################################################################
###################### LOAD AND MANIPULATE DATA 	  ######################################################
################################################################################################################

##### LOAD RAW DATA       
# #setwd("A:\\RSPB\\Malta\\Raw_data")
setwd("C:\\STEFFEN\\RSPB\\Malta\\Raw_data")
# 
# yesh<-read_excel("YESH_2012_2018_cavestring_replacedrings.xlsx", sheet="with cave string")
# effort<-read_excel("Cave_Activity_complete_2012-2018.xlsx", sheet="Sheet1")
caves<-read_excel("Cave_ID.xlsx", sheet="Sheet1")


#### replaced with Martins CMR cleaning code on 3 March 2019


# to get list of all rings deployed, in the source file only original rings present and not their replacements
rings <- fread("2012_2018_replacement_and_tags.csv")

unique_rings <- as.data.frame(unique(rings$ringnumber))

#fwrite(unique_rings, "2012_2018_uniquerings.csv")

#####IMP. NOTE: All times in records and effort are in Malta time.

#to create Nightstarting for both ringing records and cave activity (Effort) to match effort on nightstarting

midday<-as.POSIXct("12:00:00", format="%H:%M:%S",tz = "UTC")   # create a reference time to split the day
midday<-format(midday, format="%H:%M:%S",tz = "UTC") 

#since ringing database extract for 2012-2016 gave time in hour only, :00:00 CONCATENATED onto hour value in excel
records <- fread("2012_2018_cavestring_ringingrecords.csv")

records <- records %>%
  mutate(DateTime=dmy_hms(paste(ringingDate,paste(HourTime, sep=":"), sep=" "),tz="Europe/Berlin")) 

records <- records %>%
  mutate(DateTime=with_tz(DateTime,tz="UTC")) %>% #does not work if using EuropeBerlin tz here
  mutate(Time=format(DateTime, format="%H:%M:%S",tz = "UTC")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600))))

effort <- fread("Cave_Activity_complete_2012-2019.csv")

effort <- effort %>%
  mutate(DateTime=dmy_hm(paste(Date,paste(start_time, sep=":"), sep=" "),tz="Europe/Berlin"))
effort[is.na(effort$DateTime),]

effort <- effort %>%
  mutate(DateTime=with_tz(DateTime,tz="UTC")) %>%
  mutate(Time=format(DateTime, format="%H:%M:%S",tz = "UTC")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600))))


initialsites <- c("MT24_Majjistral_Eggshell", "MT24_Majjistral_Thomas", "MT24_Majjistral_Thomas_NS1", "MT24_Majjistral_Subt", "MT24_Majjistral_NS2_NS3")
adults <- c("6", "4", "2")

effort <- effort %>%
  dplyr::filter(Cave_String %in% initialsites)

records <- records %>%
  dplyr::filter(Cave_string %in% initialsites) %>%
  dplyr::filter(age %in% adults)%>%
  dplyr::filter(ToCheck!= 1)




#########################################################################
# CREATE MATRIX OF ENCOUNTER DATA (0/1)
#########################################################################
yesh<-records

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

YESH<- yesh %>% mutate(Colony=caves$Colony_Name[match(Cave_string,caves$Cave_String)]) %>%
  mutate(Year=year(NightStarting)) %>%
  mutate(month=month(NightStarting), seen=1) %>%
  #mutate(Occ=paste(Year,month,sep="_")) %>%
  mutate(SEASON=ifelse(month<10,(Year-1),Year)) %>% ###define a breeding season from Oct to Jul
  mutate(OCC_NR=OCC_lookup$OCC_NR[match(SEASON,OCC_lookup$SEASON)]) %>%
  group_by(Colony, OCC_NR,ringnumber) %>%
  summarise(ALIVE=max(seen,na.rm=T)) %>%
  spread(key=OCC_NR, value=ALIVE) %>%
  replace(is.na(.), 0)





#########################################################################
# CREATE MATRIX OF EFFORT DATA AND DIFFERENCES BETWEEN OCCASIONS
#########################################################################

### FORMAT EFFORT DATA ###
head(effort)
min(effort$Date)
eff<- effort %>% mutate(Colony=caves$Colony_Name[match(Cave_String,caves$Cave_String)]) %>%
  #mutate(Colony=Cave_String) %>%  ### this does not work unless the 'periods' are somehow adjusted'
  mutate(Year=year(NightStarting)) %>%   ## needs to be done due to conflicts between year=2017 for dates from 2015
  #mutate(NightStarting=dmy(Date)) %>%
  mutate(month=month(NightStarting)) %>%
  mutate(SEASON=ifelse(month<10,(Year-1),Year)) %>% ###define a breeding season from Oct to Jul
  mutate(OCC_NR=OCC_lookup$OCC_NR[match(SEASON,OCC_lookup$SEASON)]) %>%
  mutate(hours=as.numeric(hours)) %>%
  group_by(Colony, SEASON,OCC_NR) %>%
  summarise(effort=sum(hours,na.rm=T))



### CALCULATE INTERVAL BETWEEN OCCASIONS ###

survPeriods<-effort %>% mutate(Colony=caves$Colony_Name[match(Cave_String,caves$Cave_String)]) %>%
  mutate(Year=year(NightStarting)) %>%   ## needs to be done due to conflicts between year=2017 for dates from 2015
  mutate(month=month(NightStarting)) %>%
  mutate(SEASON=ifelse(month<10,(Year-1),Year)) %>% ###define a breeding season from Oct to Jul
  mutate(OCC_NR=OCC_lookup$OCC_NR[match(SEASON,OCC_lookup$SEASON)]) %>%
  group_by(Colony, SEASON,OCC_NR) %>%
  summarise(first=min(NightStarting,na.rm=T), last=max(NightStarting,na.rm=T), mid=median(NightStarting, na.rm=T)) %>%
  arrange(Colony, SEASON,OCC_NR) %>%
  mutate(surv_int=as.numeric(difftime(dplyr::lag(mid), mid,unit="days")))  


## calculate the difference in days between the mid days of subsequent sessions
 ## some how the lead/lag does not work in: mutate(surv_int=as.numeric(difftime(lead(first), last,unit="days")))  ## calculates the difference in days between end of current session and beginning of next session
for (l in 1:(dim(survPeriods)[1]-1)){
  survPeriods$surv_int[l]=as.numeric(difftime(survPeriods$mid[l+1], survPeriods$mid[l],unit="days"))
}

#fwrite(survPeriods,"Malta_intervals_sessions.csv")


#########################################################################
# PREPARE DATA FOR INPUT INTO JAGS
#########################################################################

names(YESH)
CH<-as.matrix(YESH[,3:8], dimnames=F)
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
n.colonies<-length(unique(YESH$Colony))

## CREATE MATRIX for INITIAL STATE Z
zinit<-CH
for (l in 1:nrow(zinit)){
  firstocc<-get.first(zinit[l,])
  if(firstocc<n.years){
    zinit[l,(firstocc):n.years]<-1  ## alive from first contact - DIFF FROM CJS where this is firstocc+1
  }else{
    zinit[l,firstocc]<-1
  }
  zinit[l,1:firstocc]<-0  ## sets everything up to first contact to - - DIFF FROM CJS where this is NA
}
dim(zinit)




## CREATE MATRIX TO LOOK UP OBSERVATION EFFORT
## STANDARDISE HOURS TO AVOID INVALID PARENT ERROR IN JAGS

COLEFF<- eff %>% group_by(Colony, OCC_NR) %>%
  summarise(effort=sum(effort)) %>%
  mutate(effort=scale(effort, center=mean(effort))/sd(effort)) %>%
  spread(key=OCC_NR, value=effort) %>%
  replace(is.na(.), 0)
COLEFF$COL_NR<-1#seq(1:5)

effmat<-as.matrix(COLEFF[,2:7], dimnames=F)



## CREATE LOOKUP VECTOR FOR WHICH COLONY BIRDS WERE CAUGHT IN

head(YESH)
colvec<-COLEFF$COL_NR[match(YESH$Colony,COLEFF$Colony)]

length(f)
length(colvec)


## CREATE LOOKUP VECTOR FOR WHICH COLONY BIRDS WERE CAUGHT IN

DURMAT<- survPeriods %>% group_by(Colony, OCC_NR) %>%
  summarise(int=sum(surv_int)) %>%
  spread(key=OCC_NR, value=int) %>%
  replace(is.na(.), 0)

periods<-as.matrix(DURMAT[,2:7], dimnames=F)



##########################################################################################################
# Specify JS model with colony-occasion-specific recapture probability and random time effects
##########################################################################################################

setwd("C:\\STEFFEN\\RSPB\\Malta\\Analysis\\Survival_analysis\\YESH_survival")
sink("YESH_JS_abundance_survival_simp.jags")
cat("

model
{
  
  # Priors and constraints
  
  ### SURVIVAL PROBABILITY
  
  mean.phi ~ dunif(0.95, 1)                      # Priors for age-specific DAILY survival - this is the basic survival that is scaled to difference between occasions
  #mean.p ~ dbeta(1.5, 4)                          # Prior for mean recapture switched to beta from unif
  #logit.p <- log(mean.p / (1-mean.p))           # Logit transformation
  #beta.effort ~ dnorm(0,0.01)                   # Prior for trapping effort offset on capture probability
  
  for (i in 1:M){
    for (t in 1:(n.years-1)){
      beta.int[i,t] <- pow(mean.phi,periods[t])       ### exponentiates daily survival by the length between capture sessions
      mu[i,t] <- log(beta.int[i,t] / (1-beta.int[i,t]))       # Logit transformation
      logit(phi[i,t]) <- mu[i,t] + surv.raneff[t]
    } #t
  
  ### RECAPTURE PROBABILITY
  
    for (t in 1:n.years){
      #logit(p[i,t]) <- logit.p + capt.raneff[i] ###beta.effort*effort[t] + 
      p[i,t] ~ dbeta(1.5, 4) 
    }
  
  } #i
  
  
  ## RANDOM TIME EFFECT ON SURVIVAL
  for (t in 1:(n.years-1)){
    surv.raneff[t] ~ dnorm(0, tau.surv)
  }
  
  #for (i in 1:M){
  #  capt.raneff[i] ~ dnorm(0, tau.capt)
  #}
  
  
  ### PRIORS FOR RANDOM EFFECTS
  sigma.surv ~ dunif(0, 10)                     # Prior for standard deviation of survival
  tau.surv <- pow(sigma.surv, -2)
  
  #sigma.capt ~ dunif(0, 10)                     # Prior for standard deviation of capture
  #tau.capt <- pow(sigma.capt, -2)
  
  
  ####### LIKELIHOOD #############
  
  for (t in 1:n.years){
    gamma[t] ~ dunif(0, 1)
  } #t
  
  
  # Likelihood
  for (i in 1:M){
  # First occasion
  # State process
   z[i,1] ~ dbern(gamma[1])

  # Observation process
   mu1[i,1] <- z[i,1] * p[i,1]
   y[i,1] ~ dbern(mu1[i,1])

  # Subsequent occasions
    for (t in 2:n.years){
  # State process
  #q[i,t-1] <- 1-z[i,t-1]		# Availability for recruitment
      recru[i,t-1] <- max(z[i,1:(t-1)])		# Availability for recruitment - this will be 0 if the bird has never been observed before and 1 otherwise
      pot.alive[i,t] <- phi[i,t-1] * z[i,t-1] + gamma[t] * (1-recru[i,t-1])
      z[i,t] ~ dbern(pot.alive[i,t])
  
  # Observation process
      mu1[i,t] <- z[i,t] * p[i,t]	
      y[i,t] ~ dbern(mu1[i,t])
    } #t
  } #i 
  
  
  # DERIVED PARAMETERS TO REPORT POPULATION SIZE
  for (t in 1:n.years){
    N[t] <- sum(z[1:M,t])        # Actual population size
  } #t
  
  for (i in 1:M){
    Nind[i] <- sum(z[i,1:n.years])
    Nalive[i] <- 1-equals(Nind[i], 0)
  } #i
  
  # DERIVED SURVIVAL PROBABILITIES PER YEAR 
  ann.surv <- pow(mean.phi,365)

  }								#### END OF THE FUNCTION LOOP
  
    
    
    ",fill = TRUE)
sink()





#########################################################################
# PREPARE DATA FOR MODEL - INCLUDING DATA AUGMENTATION
#########################################################################

# Augment data set by potYESH potential individuals
# the JS model works by estimating the probability that these individuals exist
# if you make the number too small, the model will hit the ceiling and report the number you specified
# the larger you make the number the longer the model will run

potYESH<-15
CHpot.ind<-matrix(0,ncol=ncol(CH), nrow=potYESH)
CHaug<-rbind(CH,CHpot.ind)
dim(CHaug)

Zpot.ind<-matrix(0,ncol=ncol(CH), nrow=potYESH)
zinit.aug<-rbind(zinit,Zpot.ind)

## change zinit from NA to 0
#zinit.aug[is.na(zinit.aug)]<-0

# Bundle data
jags.data <- list(y = CHaug, n.years = n.years, 
                  M = n.ind+potYESH, 
                  periods=as.numeric(periods[1,]), effort=as.numeric(effmat[1,]))

# Initial values 
inits <- function(){list(mean.phi = runif(1, 0.95, 1),
                         #mean.p = rbeta(1, 1.5, 4),
                         #beta.effort = rnorm(1, 0, 10)
                         z = zinit.aug)}


# Parameters monitored
parameters <- c("ann.surv","beta.effort","mean.p","N")

# MCMC settings
ni <- 10
nt <- 1
nb <- 5
nc <- 4

# Call JAGS from R
YESHabund <- jags(jags.data, inits, parameters, "C:\\STEFFEN\\RSPB\\Malta\\Analysis\\Survival_analysis\\YESH_survival\\YESH_JS_abundance_survival_simp.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb,parallel=T)






