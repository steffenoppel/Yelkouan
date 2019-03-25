###############################################################################
##  YELKOUAN SHEARWATER SURVIVAL AND ABUNDANCE ESTIMATION ON MALTA  ##
###############################################################################
## written by steffen.oppel@rspb.org.uk on 19 February 2019

## updated on 13 March - new data included and model run successfully
## output alarming: very low adult survival probability

## multi-colony formulation: this only works if all colonies have capture effort in all sessions
## simply inserting a 0 for capture effort does not work, because survival probability is exponentiated with the interval, so the interval cannot be 0
## for multi-colony run, need to either fix periods to common interval or run colonies separately


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


### EXTRACT RINGING RECORDS WHERE EFFORT DATA IS MISSING ###
head(records)
misseff<-records %>% dplyr::select(Cave_string,NightStarting, ringnumber, ringer) %>%
  rename(Cave_String=Cave_string) %>%
  left_join(effort[,c(2,18,8)],by=c("Cave_String","NightStarting")) %>%
  mutate(hours=as.numeric(hours)) %>%
  dplyr::filter(is.na(hours))
fwrite(misseff,"YESH_ring_records_missing_effort.csv")


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
names(yesh)
OCC_lookup<- yesh %>%
  #mutate(NightStarting=if_else(timeHour<12,ymd(ringingDate)-days(1),ymd(ringingDate))) %>%
  mutate(Year=year(NightStarting)) %>%
  mutate(month=month(NightStarting)) %>%
  mutate(Occ=paste(Year,month,sep="_")) %>%
  group_by(Occ) %>%
  summarise(middate=median(NightStarting)) %>%
  arrange(middate) %>%
  mutate(OCC_NR=seq_along(Occ))

YESH<- yesh %>% mutate(Colony=caves$Colony_Name[match(Cave_string,caves$Cave_String)]) %>%
  #mutate(Colony=Cave_string) %>% ### this does not work unless the 'periods' are somehow adjusted'
  #mutate(NightStarting=if_else(timeHour<12,ymd(ringingDate)-days(1),ymd(ringingDate))) %>%
  mutate(month=month(NightStarting), seen=1) %>%
  mutate(Occ=paste(Year,month,sep="_")) %>%
  mutate(OCC_NR=OCC_lookup$OCC_NR[match(Occ,OCC_lookup$Occ)]) %>%
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
  mutate(hours=as.numeric(hours)) %>%
  group_by(Colony, Year, month) %>%
  summarise(effort=sum(hours,na.rm=T)) %>%
  mutate(Occ=paste(Year,month,sep="_")) %>%
  mutate(OCC_NR=OCC_lookup$OCC_NR[match(Occ,OCC_lookup$Occ)]) #%>%
  #dplyr::filter(effort>1)        ### minimum cut-off is to have at least 1 hr of mistnetting effort



### CALCULATE INTERVAL BETWEEN OCCASIONS ###

survPeriods<-effort %>% mutate(Colony=caves$Colony_Name[match(Cave_String,caves$Cave_String)]) %>%
  #mutate(Colony=Cave_String) %>%  ### this does not work unless the 'periods' are somehow adjusted'
  #mutate(Date=ymd(DateTime)) %>%   
  mutate(Year=year(NightStarting)) %>%   ## needs to be done due to conflicts between year=2017 for dates from 2015
  mutate(month=month(NightStarting)) %>%
  mutate(Occ=paste(Year,month,sep="_")) %>%
  group_by(Colony, Occ) %>%
  summarise(first=min(NightStarting,na.rm=T), last=max(NightStarting,na.rm=T), mid=median(NightStarting, na.rm=T)) %>%
  mutate(Year=year(first)) %>%
  mutate(month=month(first)) %>%
  mutate(OCC_NR=OCC_lookup$OCC_NR[match(Occ,OCC_lookup$Occ)]) %>%
  arrange(Colony, Year, month) %>%
  mutate(surv_int=as.numeric(difftime(lead(mid), mid,unit="days")))  ## calculates the difference in days between the mid days of subsequent sessions
  #mutate(surv_int=as.numeric(difftime(lead(first), last,unit="days")))  ## calculates the difference in days between end of current session and beginning of next session

#fwrite(survPeriods,"Malta_intervals_sessions.csv")



#########################################################################
# CREATE MATRIX OF AGE FOR EACH OCCASION AND INDIVIDUAL
#########################################################################

## this matrix will relate to the parameter estimates chosen in the model
## simple model only has 2 survival parameters:
## 1 - juvenile and immature survival (years 1-2)
## 2 - adult survival (birds ringed )


# unique(yesh$age)
# AGEMAT<- yesh %>% mutate(Colony=caves$Colony_Name[match(Cave_string,caves$Cave_String)]) %>%
#   #mutate(Colony=Cave_String) %>%  ### this does not work unless the 'periods' are somehow adjusted'
#   #mutate(NightStarting=if_else(timeHour<12,ymd(ringingDate)-days(1),ymd(ringingDate))) %>%
#   mutate(month=month(NightStarting)) %>%
#   mutate(Occ=paste(Year,month,sep="_")) %>%
#   mutate(OCC_NR=OCC_lookup$OCC_NR[match(Occ,OCC_lookup$Occ)]) %>%
#   group_by(Colony, OCC_NR,ringnumber) %>%
#   summarise(AGE=max(age,na.rm=T)) %>%
#   mutate(AGE=ifelse(AGE %in% c(1,3),1,2)) %>%     ### re-classify age into those ringed/observed as chicks/hatch-year birds and those observed later
#   spread(key=OCC_NR, value=AGE) 
# 
# AGEMAT<- as.matrix(AGEMAT[,3:34], dimnames=F)
# head(AGEMAT)

#########################################################################
# PREPARE DATA FOR INPUT INTO JAGS
#########################################################################

names(YESH)
CH<-as.matrix(YESH[,3:34], dimnames=F)
dim(CH)

### check that there are contacts in every season
apply(CH,2,sum)


# Compute vector with occasion of first capture
get.first <- function(x) min(which(x==1))
f <- apply(CH, 1, get.first)


## REMOVE individuals ringed in last occasion end of time series
# toolate<-ifelse(f==dim(CH)[2],1,0)
# CH<-CH[toolate==0,]

## CREATE BLANK AGE MATRIX
#FULLAGEMAT<-matrix(2,nrow=nrow(CH),ncol=ncol(CH))
n.occ<-ncol(CH)

# ## LOOP OVER EACH BIRD RINGED AND SET PRE-CAPTURE DATA TO NA AND ADJUST AGE
# ## THIS IS MASSIVELY COMPLICATED DUE TO THE DIFFERENT TIME INTERVALS BETWEEN OCCASIONS!!
# 
# 
# for (l in 1:nrow(FULLAGEMAT)){
#   firstocc<-f[l]
#   lastjuv<-firstocc+4     ## this is INCORRECT - depending on whether subsequent occasions are weeks or years apart this would need to be adjusted - major coding headache!!
#   lastjuv<-ifelse(lastjuv>n.occ,n.occ,lastjuv)
#   young<-AGEMAT[l,firstocc] ## the age of the bird when it was first encountered
#   if(firstocc>1){FULLAGEMAT[l,1:(firstocc-1)]<-NA}  ## sets everything before first contact to NA
#   if(young==1){FULLAGEMAT[l,firstocc:lastjuv]<-1}  ## sets all juvenile years to 1
# }

### CHECK WHETHER IT LOOKS OK ###
head(FULLAGEMAT)
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
    zinit[l,(firstocc+1):n.years]<-1  ## alive after first contact
  }else{
    zinit[l,firstocc]<-1
  }
  zinit[l,1:firstocc]<-NA  ## sets everything up to first contact to NA
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

effmat<-as.matrix(COLEFF[,2:34], dimnames=F)



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

periods<-as.matrix(DURMAT[,2:34], dimnames=F)

#periods[periods==0]<-0.001  ### because the number is used in a power function the 0 may cause problems?

##########################################################################################################
# Specify basic CJS model with colony-occasion-specific recapture probability and random time effects
##########################################################################################################
setwd("C:\\STEFFEN\\RSPB\\Malta\\Analysis\\Survival_analysis\\YESH_survival")
sink("YESH_CJS_adult_only.jags")
cat("
    model {
    
    # -------------------------------------------------
    # Parameters:
    # phi: survival probability, only adults 
    # p: recapture probability when breeding, depends on colony-occasion-specific effort
    # -------------------------------------------------
    
    
    ## Priors and constraints
    
    ### RECAPTURE PROBABILITY
    #mean.p ~ dunif(0, 1)                          # Prior for mean recapture
    #logit.p <- log(mean.p / (1-mean.p))           # Logit transformation
    beta.effort ~ dnorm(0,0.01)                   # Prior for trapping effort offset on capture probability


    ## COLONY-SPECIFIC CAPTURE PROBABILITY 
    for (col in 1:n.colonies){
      cap.prob[col] ~ dunif(0, 1)                         # Priors for colony-specific capture probability
      mu.p[col] <- log(cap.prob[col] / (1-cap.prob[col]))       # Logit transformation
        for (t in 1:n.occasions){
          logit(p[col,t]) <- mu.p[col] + beta.effort*effmat[col,t] + capt.raneff[t]
        }
    }
    
    ### SURVIVAL PROBABILITY
    beta ~ dunif(0.9, 1)                       # Priors for age-specific DAILY survival - this is the basic survival that is scaled to difference between occasions
    for (i in 1:nind){
      for (t in f[i]:(n.occasions-1)){
        beta.int[i,t] <- pow(beta,periods[colvec[i],t])       ### exponentiates daily survival by the length between capture sessions
        mu[i,t] <- log(beta.int[i,t] / (1-beta.int[i,t]))       # Logit transformation
        logit(phi[i,t]) <- mu[i,t] + surv.raneff[t]
      } #t
    } #i
    

    ## RANDOM TIME EFFECT ON SURVIVAL
    for (t in 1:(n.occasions-1)){
      surv.raneff[t] ~ dnorm(0, tau.surv)
    }

    for (t in 1:n.occasions){
      capt.raneff[t] ~ dnorm(0, tau.capt)
    }

    
    ### PRIORS FOR RANDOM EFFECTS
    sigma.surv ~ dunif(0, 10)                     # Prior for standard deviation of survival
    tau.surv <- pow(sigma.surv, -2)
    
    sigma.capt ~ dunif(0, 10)                     # Prior for standard deviation of capture
    tau.capt <- pow(sigma.capt, -2)
    

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
    #for (t in 1:(n.occasions-1)){
      #for (age in 1:2){
        #msurv[t]<-mean(phi[,t])
        ann.surv <- pow(beta,365)
        
      #}
    #}
    
    
    }
    ",fill = TRUE)
sink()





#########################################################################
# PREPARE DATA FOR MODEL
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
ni <- 150000
nt <- 4
nb <- 50000
nc <- 4

# Call JAGS from R
YESHsurv <- jags(jags.data, inits, parameters, "C:\\STEFFEN\\RSPB\\Malta\\Analysis\\Survival_analysis\\YESH_survival\\YESH_CJS_adult_only.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb,parallel=T)




#########################################################################
# PRODUCE OUTPUT TABLE
#########################################################################

out<-as.data.frame(YESHsurv$summary)
out$parameter<-row.names(YESHsurv$summary)
export<-out %>% select(c(1,5,2,3,7)) %>%
  setNames(c('Mean', 'Median','SD','lcl', 'ucl'))
write.table(export,"YESH_Malta_Survival_estimates2019.csv", sep=",", row.names=F)








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
  mean.p ~ dbeta(1.5, 4)                          # Prior for mean recapture switched to beta from unif
  logit.p <- log(mean.p / (1-mean.p))           # Logit transformation
  beta.effort ~ dnorm(0,0.01)                   # Prior for trapping effort offset on capture probability
  
  for (i in 1:M){
  for (t in 1:(n.years-1)){
  beta.int[i,t] <- pow(mean.phi,periods[t])       ### exponentiates daily survival by the length between capture sessions
  mu[i,t] <- log(beta.int[i,t] / (1-beta.int[i,t]))       # Logit transformation
  logit(phi[i,t]) <- mu[i,t] + surv.raneff[t]
  } #t
  
  ### RECAPTURE PROBABILITY
  
  for (t in 1:n.years){
  logit(p[i,t]) <- logit.p + beta.effort*effort[t] + capt.raneff[i]
  }
  
  } #i
  
  
  ## RANDOM TIME EFFECT ON SURVIVAL
  for (t in 1:(n.years-1)){
  surv.raneff[t] ~ dnorm(0, tau.surv)
  }
  
  for (i in 1:M){
    capt.raneff[i] ~ dnorm(0, tau.capt)
  }
  
  
  ### PRIORS FOR RANDOM EFFECTS
  sigma.surv ~ dunif(0, 10)                     # Prior for standard deviation of survival
  tau.surv <- pow(sigma.surv, -2)
  
  sigma.capt ~ dunif(0, 10)                     # Prior for standard deviation of capture
  tau.capt <- pow(sigma.capt, -2)
  
  
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
  #y[i,1] ~ dbern(gamma[1])
  #z[i,1] ~ dbern(gamma[1])  ## this is necessary to fill in the first column of the z-matrix, otherwise unresolved parameter error
  
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
  qgamma[t] <- 1-gamma[t]
  }
  cprob[1] <- gamma[1]
  
  for (t in 2:n.years){
  cprob[t] <- gamma[t] * prod(qgamma[1:(t-1)])
  } #t
  
  psi <- sum(cprob[])            # Inclusion probability
  for (t in 1:n.years){
  b[t] <- cprob[t] / psi      # Entry probability
  } #t
  
  # for (i in 1:M){
  #   recruit[i,1] <- z[i,1]
  #   for (t in 2:n.years){
  #     recruit[i,t] <- (1-z[i,t-1]) * z[i,t]
  #   } #t
  # } #i
  
  for (t in 1:n.years){
  N[t] <- sum(z[1:M,t])        # Actual population size
  #B[t] <- sum(recruit[1:M,t])  # Number of entries
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

Zpot.ind<-matrix(NA,ncol=ncol(CH), nrow=potYESH)
zinit.aug<-rbind(zinit,Zpot.ind)

## change zinit from NA to 0
zinit.aug[is.na(zinit.aug)]<-0

# Bundle data
jags.data <- list(y = CHaug, n.years = n.years, 
                  M = n.ind+potYESH, 
                  periods=as.numeric(periods[1,]), effort=as.numeric(effmat[1,]))

# Initial values 
inits <- function(){list(mean.phi = runif(1, 0.95, 1),
                         mean.p = rbeta(1, 1.5, 4),
                         #z = zinit.aug,
                         beta.effort = rnorm(1, 0, 10))}


# Parameters monitored
parameters <- c("ann.surv","beta.effort","mean.p","N")

# MCMC settings
ni <- 10
nt <- 1
nb <- 5
nc <- 4

# Call JAGS from R
YESHabund <- jags(jags.data, inits, parameters, "C:\\STEFFEN\\RSPB\\Malta\\Analysis\\Survival_analysis\\YESH_survival\\YESH_JS_abundance_survival_simp.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb,parallel=T)









####################### TROUBLESHOOTING RESOLVE PARAMETER PROBLEM #####################
## unable to resolve parameter - big problem
## persisted until ALL elements of z-matrix were filled


setwd("C:\\STEFFEN\\RSPB\\Statistics")
sink("TEST_MOD.jags")
cat("
    
    model
    {
    
    mean.phi ~ dunif(0.95, 1)                      # Priors for age-specific DAILY survival - this is the basic survival that is scaled to difference between occasions
    mean.p ~ dbeta(1.5, 4)                          # Prior for mean recapture switched to beta from unif
    
    # Likelihood
    for (i in 1:100){

      z[i,1] ~ dbern(mean.phi)
    
    
    # Subsequent occasions
        for (t in 2:20){
          pot.alive[i,t] <- z[i,(t-1)]*mean.phi
          z[i,t] ~ dbern(pot.alive[i,t])
          recru[i,t-1] <- max(z[i,1:(t-1)])	
        } # end t
    } # end i


    }								#### END OF THE FUNCTION LOOP
    
    
    ",fill = TRUE)
sink()


# MaKE UP BULLSHIT DATA
jags.data <- list(y = matrix(1,ncol=20, nrow=100))
inits <- function(){list(mean.phi = runif(1, 0.95, 1),
                         mean.p = rbeta(1, 1.5, 4),
                         z = matrix(0,ncol=20, nrow=100))}


# Parameters monitored
parameters <- c("recru")

# MCMC settings
ni <- 10
nt <- 1
nb <- 5
nc <- 4

# Call JAGS from R
YESHabund <- jags(jags.data, inits, parameters, "C:\\STEFFEN\\RSPB\\Statistics\\TEST_MOD.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb,parallel=T)

