######################CMR Data analysis#################################

##########################Data cleaning#################################
#Script started 20190302 by MA

library(data.table)
library(dplyr)
library(lubridate)

setwd("C:\\Users\\Martin\\Documents\\working_folder\\CaptureMarkRecapture\\yelkouans_all_files")

# to get list of all rings deployed, in the source file only original rings present and not their replacements
records <- fread("2012_2018_cavestring_replacedrings.csv")

unique_rings <- as.data.frame(unique(records$ringnumber))

fwrite(unique_rings, "2012_2018_uniquerings.csv")

#####IMP. NOTE: All times in records and effort are in Malta time.

#to create Nightstarting for both ringing records and cave activity (Effort) to match effort on nightstarting

midday<-as.POSIXct("12:00:00", format="%H:%M:%S",tz = "UTC")   # create a reference time to split the day
midday<-format(midday, format="%H:%M:%S",tz = "UTC") 

#since ringing database extract for 2012-2016 gave time in hour only, :00:00 CONCATENATED onto hour value in excel
records <- fread("2012_2018_cavestring_ringingrecords.csv")

records <- records %>%
  mutate(DateTime=dmy_hms(paste(ringingDate,paste(HourTime, sep=":"), sep=" ")),tz="Europe/Berlin") 

records <- records %>%
  mutate(DateTime=with_tz(DateTime,tz="UTC")) %>% #does not work if using EuropeBerlin tz here
  mutate(Time=format(DateTime, format="%H:%M:%S",tz = "UTC")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600))))
  
effort <- fread("Cave_Activity_complete_2012-2019.csv")

effort <- effort %>%
  mutate(DateTime=dmy_hm(paste(Date,paste(start_time, sep=":"), sep=" ")),tz="Europe/Berlin")


effort <- effort %>%
  mutate(DateTime=with_tz(DateTime,tz="UTC")) %>%
  mutate(Time=format(DateTime, format="%H:%M:%S",tz = "UTC")) %>%
  mutate(NightStarting=if_else(Time>midday,as.Date(DateTime),as.Date(DateTime-(24*3600))))


initialsites <- c("MT24_Majjistral_Eggshell", "MT24_Majjistral_Thomas", "MT24_Majjistral_Thomas_NS1", "MT24_Majjistral_Subt", "MT24_Majjistral_NS2_NS3")
adults <- c("6", "4", "2")

effort <- effort %>%
  filter(Cave_String %in% initialsites)

records <- records %>%
  filter(Cave_string %in% initialsites) %>%
  filter(age %in% adults)


