#########################################################################################################################
##### YELKOUAN SHEARWATER SOUND RECORDING ANALYSIS ON MALTA ####################################
#########################################################################################################################

## written by steffen.oppel@rspb.org.uk on 3 Nov 2017
## based on data provided by BirdLife Malta (Martin Austad and Paulo Lago)
## uses output created by YESH_sound_analysis_v2.r
## re-used by MA 04/10/2018 for 2018 analysis


library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(lme4)
library(RODBC)

#TO DO: 

#- ultimately use sound-derived projections to calibrate camera trap data and project colony-size from relationship b/w sound and camera trap£

################################################################################################################
###################### combine ind csv files from soundecology analysis in YESH_sound_analysis_v2  ######################################################
################################################################################################################
setwd("C:/Users/Paulo/Desktop/arumerge")

files  <- list.files(pattern = '\\.csv')

tables <- lapply(files, read.csv, header = TRUE)

YESH_acoustic_diversity2018 <- do.call(rbind , tables)

fwrite(YESH_acoustic_diversity2018, "YESH_acoustic_diversity2018.csv")

################################################################################################################
###################### READ IN DATA OF ALL RECORDINGS   ######################################################
################################################################################################################

# Set working directory
setwd("C:\\STEFFEN\\RSPB\\Malta\\Raw_data")
setwd("A:\\RSPB\\Malta\\Raw_data")

### READ IN CSV FILES
#colony <- fread("YESH_colony_sizes_Malta.csv")
#YESH <- fread("YESH_acoustic_diversity2017.csv")

### OR LOAD DATA FROM DATABASE

db <- odbcConnectAccess2007('YESH_colony_acoustic_data_2018.accdb')
colony<- sqlQuery(db, "SELECT * FROM colony_sizes")
YESH<- sqlQuery(db, "SELECT * FROM sound_recordings")
odbcClose(db)

YESH <- YESH_acoustic_diversity2018 
head(YESH)
str(YESH)


################################################################################################################
###### SPECIFY THE NIGHT TO AVOID GROUPING ISSUES AFTER MIDNIGHT  ##############################
################################################################################################################

midday<-as.POSIXct("12:00:00", format="%H:%M:%S",tz = "UTC")   # create a reference time to split the day
midday<-format(midday, format="%H:%M:%S",tz = "UTC")

YESH$DateTime <- as.POSIXct(YESH$DateTime, format = "%Y-%m-%d %H:%M:%OS", tz="UTC")

YESH<- YESH %>%
  #mutate(DateTime=ymd_hms(paste(date,paste(hr,min,sec,sep=":"), sep=" "),tz="UTC"))%>%
  mutate(Time=format(DateTime, format="%H:%M:%S",tz = "UTC")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600))))

length(unique(YESH$ARU))


################################################################################################################
###################### CREATE HISTOGRAMS OF AVAILABLE SAMPLES OVER SEASON  ###############################
################################################################################################################

YESH %>% mutate(count=1) %>%

ggplot()+    ## colour=breed_stage looks shit
  geom_histogram(aes(x=NightStarting), binwidth=10)+                             
  facet_wrap("ARU", ncol=3, scales = "fixed")+
  ylab("Number of recordings") +
  xlab("Date") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.y=element_text(size=16, color="black"),
        axis.text.x=element_text(size=16, color="black", vjust=0.5), 
        axis.title=element_text(size=16), 
        strip.text.x=element_text(size=16, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())



YESH %>% mutate(count=1) %>%
	group_by(ARU,NightStarting) %>%
	summarise(BIX=mean(BIXmax)) %>%

ggplot(aes(x=NightStarting, y=BIX))+    
  geom_point(size=0.2)+                               
  facet_wrap("ARU", ncol=3, scales = "fixed")+
  geom_smooth(fill="lightblue", size=1.5, method="loess", se=T)+
  #geom_vline(aes(xintercept = as.numeric(as.Date("2017-01-15"))), colour="red", size=1) +
  #geom_vline(aes(xintercept = as.numeric(as.Date("2017-03-15"))), colour="red", size=1) +
  ylab("Bioacoustic Diversity Index (BIX)") +
  xlab("Date") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.y=element_text(size=16, color="black"),
        axis.text.x=element_text(size=16, color="black", vjust=0.5), 
        axis.title=element_text(size=16), 
        strip.text.x=element_text(size=16, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

YESH %>% mutate(count=1) %>%
  group_by(ARU,NightStarting) %>%
  summarise(ACD30=mean(ACD30max)) %>%
  
  ggplot(aes(x=NightStarting, y=ACD30))+    
  geom_point(size=0.2)+                               
  facet_wrap("ARU", ncol=3, scales = "fixed")+
  geom_smooth(fill="lightblue", size=1.5, method="loess", se=T)+
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-01-15"))), colour="red", size=1) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2017-03-15"))), colour="red", size=1) +
  ylab("Acoustic Diversity Index (-30dBFS)") +
  xlab("Date") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.y=element_text(size=16, color="black"),
        axis.text.x=element_text(size=16, color="black", vjust=0.5), 
        axis.title=element_text(size=16), 
        strip.text.x=element_text(size=16, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

YESH %>% mutate(count=1) %>%
  group_by(ARU,NightStarting) %>%
  summarise(ACD50=mean(ACD50max)) %>%
  
  ggplot(aes(x=NightStarting, y=ACD50))+    
  geom_point(size=0.2)+                               
  facet_wrap("ARU", ncol=3, scales = "fixed")+
  geom_smooth(fill="lightblue", size=1.5, method="loess", se=T)+
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-01-15"))), colour="red", size=1) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2017-03-15"))), colour="red", size=1) +
  ylab("Acoustic Diversity Index (-50dBFS)") +
  xlab("Date") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.y=element_text(size=16, color="black"),
        axis.text.x=element_text(size=16, color="black", vjust=0.5), 
        axis.title=element_text(size=16), 
        strip.text.x=element_text(size=16, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())
################################################################################################################
###################### CREATE HISTOGRAMS OF WINDSPEEDS  ###############################
################################################################################################################

YESH %>% mutate(count=1) %>%
  
  ggplot()+    ## colour=breed_stage looks shit
  geom_histogram(aes(x=v_wind), binwidth=3)+         
  ylab("Number of recordings") +
  xlab("Wind speed (m/s)") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.y=element_text(size=18, color="black"),
        axis.text.x=element_text(size=18, color="black", vjust=0.5), 
        axis.title=element_text(size=20), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())


################################################################################################################
###################### FIND RECORDINGS WITH VERY STRANGE TIMES  ###############################
################################################################################################################
YESH %>%
  filter(hr %in% c(15,16,5,6)) %>%
  mutate(count=1) %>%
  group_by(ARU)%>%
  summarise(n=sum(count), start=min(DateTime), end=max(DateTime))




################################################################################################################
###################### LIST THE DATA AVAILABILITY PERIODS FOR EACH ARU  ###############################
################################################################################################################

AVAILABILITY<-YESH %>%
  group_by(ARU) %>%
  summarise(start=min(DateTime), end=max(DateTime))

AVAILABILITY$RecInt=interval(AVAILABILITY$start,AVAILABILITY$end)			### this somehow got screwed up when it was in the pipe?!?
 
AllDataAvailable<-intersect(AVAILABILITY$RecInt[AVAILABILITY$ARU=="G1_isopu"],AVAILABILITY$RecInt[AVAILABILITY$ARU=="MT24_delli"])




################################################################################################################
###################### REMOVE DATA FROM OUTSIDE THE PERIOD WHEN ALL DATA ARE AVAILABLE  ###############################
################################################################################################################

### CREATE A WORKING DATA SET THAT REMOVES NOISY DATA

head(YESH)
dim(YESH)
dat<-YESH %>% 
  #filter(hr %in% c(21,22,23,0,1,2)) %>%
  filter(u_wind<9.72) %>%        ## 35km/hr used in 2017; using v_wind removes 334 recordings; using u_wind removes 571 recordings
  filter(DateTime %within% AllDataAvailable) %>%
  select(ARU,NightStarting,DateTime,month,hr,prop.illuminated,moon.elevation,sun.elevation,airtemp,u_wind,v_wind,BIXmax,ACD20max,ACD30max,ACD50max)
dim(dat)

fwrite(dat, "YESH_Acousticindex_2018_windfiltered.csv")


################################################################################################################
###################### CREATE HISTOGRAMS OF ACOUSTIC ACTIVITY OVER THE NIGHT  ###############################
################################################################################################################
unique(dat$hr)
dat$HRordered <- factor(dat$hr, levels = c("16","17","18","19","20","21","22","23","0","1","2","3","4","5","6"))
dat %>%
  filter(hr %in% c(17,18,19,20,21,22,23,0,1,2,3,4)) %>%
  ggplot()+ 
  geom_boxplot(aes(x=HRordered, y=BIXmax), width=1)+
  facet_wrap("month", scales="free", ncol=3)+       
  ylab("Bioacoustic Diversity Index") +
  xlab("hour of the day (UTC)") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.y=element_text(size=16, color="black"),
        axis.text.x=element_text(size=16, color="black", vjust=0.5), 
        axis.title=element_text(size=16), 
        strip.text.x=element_text(size=16, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

dat %>%
  filter(hr %in% c(17,18,19,20,21,22,23,0,1,2,3,4)) %>%
  ggplot()+ 
  geom_boxplot(aes(x=HRordered, y=ACD50max), width=1)+
  facet_wrap("month", scales="free", ncol=3)+       
  ylab("Acoustic Diversity Index (-50dBFS)") +
  xlab("hour of the day (UTC)") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.y=element_text(size=16, color="black"),
        axis.text.x=element_text(size=16, color="black", vjust=0.5), 
        axis.title=element_text(size=16), 
        strip.text.x=element_text(size=16, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())
### ONLY TIMES BETWEEN 20 and 2 HRS ARE SOMEWHAT CONSISTENT, so filter those: #19 also seems consistent in 2018
dat<-dat %>% 
  filter(hr %in% c(19,20,21,22,23,0,1,2)) %>%
  select(ARU,NightStarting,DateTime,month,hr,prop.illuminated,moon.elevation,sun.elevation,airtemp,u_wind,v_wind,BIXmax,ACD20max,ACD30max,ACD50max)
dim(dat)




################################################################################################################
###################### COMPARE ACOUSTIC DIVERSITY INDICES  ###############################
################################################################################################################



dat %>%
  gather(ACD20max,ACD30max,ACD50max,key="Index",value="ACT") %>%
  
  ggplot(aes(x=BIXmax, y=ACT,colour=as.factor(ARU)))+
  geom_point(size=0.2) +
  facet_wrap("Index", scales="free", ncol=3)+
  xlab("Bioacoustic diversity index") +
  ylab("Acoustic Diversity Index") +
  guides(fill=FALSE)+
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.y=element_text(size=16, color="black"),
        axis.text.x=element_text(size=16, color="black"), 
        axis.title=element_text(size=20),
        strip.text.x=element_text(size=18, color="black"),
        strip.text.y=element_text(size=18, color="black"),
        strip.background=element_rect(fill="white", colour="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        legend.position="none")




################################################################################################################
###################### SUMMARISE ACOUSTIC DIVERSITY BY ARU  ###############################
################################################################################################################
colony <- colony_sizes2018
dat$loc.type=colony$Locality_type[match(dat$ARU,colony$folder_name)]


dat %>% filter(loc.type==1)%>%
ggplot(aes(x=ARU, y=BIXmax, width=1))+
  geom_boxplot(colour="black") +
  ylab("Bioacoustic Diversity Index") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.y=element_text(size=18, color="black"),
        axis.text.x=element_text(size=14, color="black", angle=90, vjust=0.5), 
        axis.title=element_text(size=20),
        axis.title.y=element_text(margin=margin(0,15,0,0)),
        axis.title.x=element_text(margin=margin(15,0,0,0)), 
        strip.text.x=element_text(size=18, color="black"),
        strip.text.y=element_text(size=18, color="black"),
        strip.background=element_rect(fill="white", colour="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())




################################################################################################################
###################### COMBINE DATA WITH COLONY SIZE AND MODEL BIX FOR EACH RECORDING  #########################
################################################################################################################

dat$col.size=colony$SumOfn_nests[match(dat$ARU,colony$folder_name)]
dat$loc.type=factor(colony$Locality_type[match(dat$ARU,colony$folder_name)])
dat$dist.ARU=colony$MaxOfdistance_ARU[match(dat$ARU,colony$folder_name)]

head(dat[is.na(dat$col.size),])


### TEST WHETHER THERE IS A STATISTICAL EFFECT OF COLONY SIZE
fullmod<-lmer(BIXmax~month+hr+prop.illuminated+moon.elevation+sun.elevation+v_wind+col.size+(1|ARU), data=dat[!is.na(dat$col.size),])
redmod<-lmer(BIXmax~month+hr+prop.illuminated+moon.elevation+sun.elevation+v_wind+(1|ARU), data=dat[!is.na(dat$col.size),])
anova(fullmod, redmod)


### USE RANDOM FOREST TO EXPLORE THE MOST IMPORTANT VARIABLE
library(randomForest)
RF<-randomForest(BIXmax~month+hr+prop.illuminated+moon.elevation+sun.elevation+v_wind+airtemp+col.size+dist.ARU, data=dat[!is.na(dat$col.size),],ntree=1500,mtry=6,importance=T,replace=F)			###, strata=as.factor(ARU) - this will preclude prediction to other units
varImpPlot(RF)
RF
VAR<-importance(RF, nperm = 100, OOB=T)
IMP<-data.frame(variable=row.names(VAR), VAR)
IMP<-IMP[order(IMP$IncNodePurity, decreasing=T),]  ## SORTED BY node homogeneity
IMP$rel_imp<-round((IMP$IncNodePurity/IMP$IncNodePurity[1])*100,2)
IMP

par(mar=c(5,10,2,1))
barplot(IMP$rel_imp[10:1], horiz=T, names.arg=row.names(IMP)[10:1], xlim=c(0,100), las=1,xlab="Relative importance (%)", col='lightgray',main="")




PREDICTIONS<- dat %>%
	mutate(pred.size=predict(RF, newdat=dat,type="response"))%>%
	group_by(ARU) %>%
	summarise(pred.col.size=mean(pred.size), actual.col.size=mean(col.size))
names(PREDICTIONS$pred.col.size)<-NULL
PREDICTIONS<- PREDICTIONS[!(is.na(PREDICTIONS$pred.col.size)),] %>%
	mutate(error=pred.col.size/actual.col.size)

fwrite(PREDICTIONS, "predictedcolonysize_ARU.csv")



 
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
  
  
  rmarkdown::render('C:\\STEFFEN\\RSPB\\Malta\\Analysis\\Acoustic\\Malta_ColonySound_summary.Rmd',
                    output_file = "Malta_ARU_analysis_Report.html",
                    output_dir = 'C:\\STEFFEN\\RSPB\\Malta\\Analysis\\Acoustic')
  




################################################################################################################
###################### COMBINE DATA WITH COLONY SIZE AND PLOT  ###############################
################################################################################################################


summary<-dat %>%
  group_by(ARU) %>%
  summarise(BIX=mean(BIXmax),ACD20=mean(ACD20max),ACD30=mean(ACD30max),ACD50=mean(ACD50max))
  
summary$col.size=colony$SumOfn_nests[match(summary$ARU,colony$folder_name)]
summary$loc.type=colony$Locality_type[match(summary$ARU,colony$folder_name)]


summary %>% filter(col.size>0)%>%

ggplot(aes(x=col.size, y=BIX))+
  geom_point(colour="black") +
  geom_smooth(fill="lightblue", size=1.5, method='lm', se=T)+
  #facet_wrap("loc.type", scales="fixed")+
  xlab("Number of YESH nests") +
  ylab("Bioacoustic Diversity Index") +
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.y=element_text(size=16, color="black"),
        axis.text.x=element_text(size=16, color="black"), 
        axis.title=element_text(size=20),
        strip.text.x=element_text(size=18, color="black"),
        strip.text.y=element_text(size=18, color="black"),
        strip.background=element_rect(fill="white", colour="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())



### PLOT THE VARIOUS INDICES


summary %>% filter(col.size>0)%>%
  gather(BIX,ACD20,ACD30,ACD50,key="Index",value="ACT") %>%
  
  ggplot(aes(x=col.size, y=ACT,colour=as.factor(loc.type)))+
  geom_point() +
  geom_smooth(fill="lightblue", size=1.5, method='lm', se=T)+
  facet_wrap("Index", scales="free")+
  xlab("Number of YESH nests") +
  ylab("Acoustic Diversity Index") +
  guides(fill=FALSE)+
  theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text.y=element_text(size=16, color="black"),
        axis.text.x=element_text(size=16, color="black"), 
        axis.title=element_text(size=20),
        strip.text.x=element_text(size=18, color="black"),
        strip.text.y=element_text(size=18, color="black"),
        strip.background=element_rect(fill="white", colour="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        legend.position="none")
