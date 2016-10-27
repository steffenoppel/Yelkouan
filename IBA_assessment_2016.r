########################################################################################################################################
#
#
#		ASSESSMENT OF MARINE IBAS AROUND MALTA BASED ON MODELLED OCCURRENCE DISTRIBUTION 
#
#
########################################################################################################################################
### written by steffen.oppel@rspb.org.uk on 11 Jan 2016
### distribution model output provided by Matthew Carroll
### requested by Maria Dias

library(rgeos)
library(SDMTools)
library(rgdal)
library(maptools)
require(maps)
require(mapdata)
library(ggmap)
library(ggplot2)
library(shapefiles)
library(raster)


#################################### LOADING DISTRIBUTION MODEL OUTPUT ###########################

setwd("A://RSPB/Malta/Analysis/Distribution_models/2015_ALL")
setwd("C:\\STEFFEN\\RSPB\\Malta\\Analysis\\Distribution_models\\2015_ALL")

FINAL_PREDICTION_MALTA<-data.frame()
#load("Cd_EVERYWHERE_all_predictions.RData")
load("Cd_EVERYWHERE_all_predictions_5km_interp.RData")
FINAL_PREDICTION_MALTA<-rbind(FINAL_PREDICTION_MALTA,FINAL_PREDICTION_EVERYWHERE[,c(1:4,31)])
names(FINAL_PREDICTION_MALTA)[5]<-'SCOP'
#load("Hp_EVERYWHERE_all_predictions.RData")
load("Hp_EVERYWHERE_all_predictions_5km_interp.RData")
FINAL_PREDICTION_MALTA<-merge(FINAL_PREDICTION_MALTA,FINAL_PREDICTION_EVERYWHERE[,c(1:4,31)], by=c("year","month","ET_X", "ET_Y"), all.x=T)
names(FINAL_PREDICTION_MALTA)[6]<-'MESP'
#load("Py_EVERYWHERE_all_predictions.RData")
load("Py_EVERYWHERE_all_predictions_5km_interp.RData")
FINAL_PREDICTION_MALTA<-merge(FINAL_PREDICTION_MALTA,FINAL_PREDICTION_EVERYWHERE[,c(1:4,31)], by=c("year","month","ET_X", "ET_Y"), all.x=T)
names(FINAL_PREDICTION_MALTA)[7]<-'YESH'
head(FINAL_PREDICTION_MALTA)
dim(FINAL_PREDICTION_MALTA)


hist(FINAL_PREDICTION_MALTA$MESP)


#################################### CROP DATA TO JUST 25 nm EFZ ###################################

setwd("A:\\RSPB\\Malta\\Raw_data\\GIS")
setwd("C:\\STEFFEN\\RSPB\\Malta\\Raw_data\\GIS")

coast<- readShapePoly("Malta_coastline_WGS84.shp")
proj4string(coast)<-CRS("+proj=longlat +ellps=WGS84")
EEZ<- readShapePoly("Malta_25nm_EFZ.shp")
proj4string(EEZ)<-CRS("+proj=utm +zone=33 +ellps=WGS84")
EEZw <- spTransform(EEZ, CRS=CRS("+proj=longlat +ellps=WGS84"))

MP<-SpatialPointsDataFrame(coords=SpatialPoints(FINAL_PREDICTION_MALTA[,3:4], proj4string=CRS("+proj=longlat +ellps=WGS84")), data=FINAL_PREDICTION_MALTA)

### check on a map ###
plot(MP, col=MP@data$SCOP*10)
plot(EEZw, add=T)
plot(coast, add=T, col='red')

crop<-MP %over% EEZw
FINAL_PREDICTION_MALTA<-FINAL_PREDICTION_MALTA[!is.na(crop),]			### remove everything OUTSIDE the EEZ
dim(FINAL_PREDICTION_MALTA)

MP<-SpatialPointsDataFrame(coords=SpatialPoints(FINAL_PREDICTION_MALTA[,3:4], proj4string=CRS("+proj=longlat +ellps=WGS84")), data=FINAL_PREDICTION_MALTA)

### check on a map ###
plot(MP)
plot(EEZw, add=T)
plot(coast, add=T, col='red')

crop<-MP %over% coast
dim(FINAL_PREDICTION_MALTA)
FINAL_PREDICTION_MALTA<-FINAL_PREDICTION_MALTA[is.na(crop$OBJECTID),]			### remove everything INSIDE the coast polygon
dim(FINAL_PREDICTION_MALTA)
head(FINAL_PREDICTION_MALTA)



##############################################################################################################
##########################	FIND MAXIMUM PREDICTED SUITABILITY FOR EACH GRID CELL  #########################
##############################################################################################################


sc<-aggregate(SCOP~ET_Y+ET_X, data=FINAL_PREDICTION_MALTA, FUN=max)
ye<-aggregate(YESH~ET_Y+ET_X, data=FINAL_PREDICTION_MALTA, FUN=max)
sp<-aggregate(MESP~ET_Y+ET_X, data=FINAL_PREDICTION_MALTA, FUN=max)
hist(sp$MESP)
hist(sc$SCOP)
hist(ye$YESH)




##############################################################################################################
##########################	PROJECTED POPULATION PROPORTION FOR EACH GRID CELL  #########################
##############################################################################################################
### TOTAL MALTESE POPULATION IS NUMBER OF BREEDING PAIRS*2.5 (to account for non-breeders)

scop_pop<-4500*3
yesh_pop<-2000*3
mesp_pop<-26447			## estimated via JS-model on Filfla

sc$SCOP<-sc$SCOP*scop_pop
ye$YESH<-ye$YESH*yesh_pop
sp$MESP<-sp$MESP*mesp_pop
par(mfrow=c(2,2))
hist(sp$MESP)
hist(sc$SCOP)
hist(ye$YESH)


##############################################################################################################
#########################	COMBINE DATA FOR OUTPUT			#########################################
##############################################################################################################

OUTPUT<-merge(sc,ye, by=c('ET_X','ET_Y'), all=T)
OUTPUT<-merge(OUTPUT,sp, by=c('ET_X','ET_Y'), all=T)
names(OUTPUT)[2:1]<-c("LAT","LONG")

head(OUTPUT)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXPORT AS SHAPEFILE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("A:\\RSPB\\Malta\\Raw_data\\GIS")

CG<-SpatialPointsDataFrame(coords=SpatialPoints(OUTPUT[,1:2], proj4string=CRS("+proj=longlat +ellps=WGS84")), data=OUTPUT[,3:5])

### write to ESRI shapefile
writeOGR(CG, dsn="MALTA_pred_bird_abundance", layer="MALTA_pred_bird_abundance",  driver="ESRI Shapefile")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXPORT AS TABLE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

write.table(OUTPUT, "MALTA_pred_abundance_per_gridcell.csv", sep=",", row.names=F)









#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#########################
#########	FOR GOVERNMENT: PROPORTION OF MALTESE BIRDS PROTECTED IN EACH GRID CELL  #########################
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#########################

### RE-CAST THE MAX PROBABILITY OF OCCURRENCE
sc<-aggregate(SCOP~ET_Y+ET_X, data=FINAL_PREDICTION_MALTA, FUN=max)
ye<-aggregate(YESH~ET_Y+ET_X, data=FINAL_PREDICTION_MALTA, FUN=max)
sp<-aggregate(MESP~ET_Y+ET_X, data=FINAL_PREDICTION_MALTA, FUN=max)

## convert predicted probabilities to proportion of population
sc$SCOP<-sc$SCOP/sum(sc$SCOP)
ye$YESH<-ye$YESH/sum(ye$YESH)
sp$MESP<-sp$MESP/sum(sp$MESP)


## then scale these standardised probabilities by the population size
sc$SCOP<-sc$SCOP*scop_pop
ye$YESH<-ye$YESH*yesh_pop
sp$MESP<-sp$MESP*mesp_pop
par(mfrow=c(2,2))
hist(sp$MESP)
hist(sc$SCOP)
hist(ye$YESH)


##############################################################################################################
#########################	COMBINE DATA FOR OUTPUT			#########################################
##############################################################################################################

OUTPUT<-merge(sc,ye, by=c('ET_X','ET_Y'), all=T)
OUTPUT<-merge(OUTPUT,sp, by=c('ET_X','ET_Y'), all=T)
names(OUTPUT)[2:1]<-c("LAT","LONG")

head(OUTPUT)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXPORT AS SHAPEFILE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("A:\\RSPB\\Malta\\Raw_data\\GIS")

CG<-SpatialPointsDataFrame(coords=SpatialPoints(OUTPUT[,1:2], proj4string=CRS("+proj=longlat +ellps=WGS84")), data=OUTPUT[,3:5])

### write to ESRI shapefile
writeOGR(CG, dsn="MALTA_cumulative_bird_abundance", layer="MALTA_cumulative_bird_abundance",  driver="ESRI Shapefile")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXPORT AS TABLE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

write.table(OUTPUT, "MALTA_cumulative_abundance_per_gridcell.csv", sep=",", row.names=F)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MAP IMPORTANCE ON GOOGLE MAP
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plotdat<-plotdat[!is.na(plotdat$ZONES),]
gplotdat<-plotdat[plotdat$ZONES>0.85,]

EEZ_df <- fortify(EEZw)

pdf("MALTA_marine_bird_abundance.pdf", width=12, height=11)

m <- get_map(location=c(lon = 14.4, lat = 35.9), source="google", zoom=9)
MaltaMap <- ggmap(m, extent = "device")
MaltaMap + geom_point(data=OUTPUT, aes(x=LONG, y=LAT, colour=SCOP), size = 4.5, pch=15) + scale_colour_gradientn("N Scopoli's Shearwater", colours=c("blue","red")) +
geom_polygon(data=EEZ_df, aes(x=long, y=lat),colour="black", lwd=1, lty=2, fill=NA)

MaltaMap + geom_point(data=OUTPUT, aes(x=LONG, y=LAT, colour=YESH), size = 4.5, pch=15) + scale_colour_gradientn("N Yelkouan Shearwater", colours=c("blue","red")) +
geom_polygon(data=EEZ_df, aes(x=long, y=lat),colour="black", lwd=1, lty=2, fill=NA)

MaltaMap + geom_point(data=OUTPUT, aes(x=LONG, y=LAT, colour=MESP), size = 4.5, pch=15) + scale_colour_gradientn("N Mediterranean Storm Petrel", colours=c("blue","red")) +
geom_polygon(data=EEZ_df, aes(x=long, y=lat),colour="black", lwd=1, lty=2, fill=NA)


dev.off()


