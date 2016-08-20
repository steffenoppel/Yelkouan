###Analysis of Yelkouan Shearwater survival rates using RMARK###
###script written by Steffen Oppel, RSPB, <steffen.oppel@rspb.org.uk>####


## SET WORK ENVIRONMENT

setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Malta\\Analysis\\Survival_analysis\\Final_analysis")
setwd("C:\\STEFFEN\\WORK\\RSPB\\Malta\\Analysis\\Survival_analysis\\Final_analysis")
library(RMark)


############################################################################################################
############## 1. JOINT SURVIVAL ANALYSIS WITH COVARIATES ##################################################
####### incorporate covariates, to allow for differences between Malta/France, sites, and breeding status ##
############################################################################################################


# DATA IMPORT

YESH<-import.chdata("YESH_input_final.txt", header = F, field.names= c("ch","site","breeder","country"), field.types = c("f","f","f"), use.comments=TRUE)
YESH.proc<-process.data(YESH, model="CJS", begin.time=2004, groups=c("site","breeder","country")) 
release.gof(YESH.proc)
YESH.ddl<-make.design.data(YESH.proc)


# DATA PROCESSING TO ACCOUNT FOR ABSENCE OF PROSPECTORS ON MALTA

YESH.ddl<-add.design.data(YESH.proc,YESH.ddl,parameter="Phi",name="Frapro", replace=T)
YESH.ddl$Phi$Frapro<-0
YESH.ddl$Phi$Frapro[YESH.ddl$Phi$country=="FRA"]=1
YESH.ddl$Phi$Frapro[YESH.ddl$Phi$breeder=="B"]=0
YESH.ddl<-add.design.data(YESH.proc,YESH.ddl,parameter="Phi",name="Frabre", replace=T)
YESH.ddl$Phi$Frabre<-0
YESH.ddl$Phi$Frabre[YESH.ddl$Phi$country=="FRA"]=1
YESH.ddl$Phi$Frabre[YESH.ddl$Phi$breeder=="P"]=0


# SURVIVAL ANALYSIS

do.analysis<-function()
{
Phi.breeder<-list(formula=~breeder)
Phi.breed.site<-list(formula=~-1+breeder:site)
Phi.site<-list(formula=~site:Frabre+site:Frapro)
Phi.country<-list(formula=~country)
Phi.country.site<-list(formula=~-1+country:breeder+site:Frabre+site:Frapro)
Phi.country.breeder.site<-list(formula=~-1+country:breeder+country:site)
Phi.global<-list(formula=~-1+country:breeder+site:breeder)
Phi.country.breeder<-list(formula=~-1+country:breeder)
Phi.country.site<-list(formula=~country+site:Frabre+site:Frapro)
p.all<-list(formula=~-1+time+country:breeder+site:breeder)
cml<-create.model.list("CJS")
model.list<-mark.wrapper(cml,data=YESH.proc,ddl=YESH.ddl)
return(model.list)
}
YESH_survival<-do.analysis()
YESH_survival
model.table<-model.table(YESH_survival,use.lnl=TRUE) 
model.table$EvidenceRatio<-2.71828182845904523536^(-0.5*model.table$DeltaAICc) 


# TEST FOR OVERDISPERSION
export.chdata(YESH.proc, 'Yesh_all', covariates = "all", replace = T)
export.model(YESH_survival, replace = T)
# I currently can't figure out how to use the output in MARK!!

############################################################################################################
############## 2. ESTIMATE MODEL-AVERAGED SURVIVAL PARAMETERS FOR EACH GROUP ##############################
############################################################################################################


## identifying the parameters from the all-different PIM to find the lines of interest in the model-average output
## this line will just print the PIMs to the screen and serves as a visual reality check, it is not required

PIMS<-PIMS(YESH_survival[[1]], "Phi", simplified=FALSE)  

## creating vector lists that specify the parameters of interest

par.index.france<-YESH.ddl$Phi[YESH.ddl$Phi$country=="FRA",]
par.index.malta<-YESH.ddl$Phi[YESH.ddl$Phi$country=="MAL",]
par.index.france.breeders<-par.index.france[par.index.france$breeder=="B",]
par.index.france.prospectors<-par.index.france[par.index.france$breeder=="P",]
PQbreed<-min(as.numeric(row.names(par.index.france.breeders[par.index.france.breeders$site=="PQ",])))
PQprosp<-min(as.numeric(row.names(par.index.france.prospectors[par.index.france.prospectors$site=="PQ",])))
PCbreed<-min(as.numeric(row.names(par.index.france.breeders[par.index.france.breeders$site=="PC",])))
PCprosp<-min(as.numeric(row.names(par.index.france.prospectors[par.index.france.prospectors$site=="PC",])))
Mcave<-min(as.numeric(row.names(par.index.malta[par.index.malta$site=="CAVE",])))
Mledge<-min(as.numeric(row.names(par.index.malta[par.index.malta$site=="LEDGE",])))

## model-average the results and compile the parameters of interest in a Table
## if you remove "drop=F" then models with ill-defined parameter estimates will be dropped, leading to slightly different output

parameter.estimates<-model.average(YESH_survival,parameter="Phi", vcv=T, drop=F)
recapture.estimates<-model.average(YESH_survival,parameter="p", vcv=T, drop=F)
recap<-recapture.estimates$estimates
min(recap$estimate)
max(recap$estimate)

### trying to estimate process variance
md=mark(YESH.proc,model.parameters=list(Phi=list(formula=~time)))
zz=get.real(md,"Phi",vcv=T)
z=zz$estimates$estimate[1:6]
vcv=zz$vcv.real
var.components(z,design=matrix(rep(1,length(z)),ncol=1),vcv) 


## creating and printing a results table with survival parameter estimates for each group of interest

Pooled_YESH_survival_estimates<-data.frame(Group=c("Porquerolles breeders", "Port-Cros breeders","Porquerolles prospectors", "Port-Cros prospectors", "Malta cave", "Malta ledge"))
Pooled_YESH_survival_estimates$mean_survival[1]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PQbreed,2]
Pooled_YESH_survival_estimates$mean_survival[2]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PCbreed,2]
Pooled_YESH_survival_estimates$mean_survival[3]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PQprosp,2]
Pooled_YESH_survival_estimates$mean_survival[4]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PCprosp,2]
Pooled_YESH_survival_estimates$mean_survival[5]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==Mcave,2]
Pooled_YESH_survival_estimates$mean_survival[6]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==Mledge,2]
Pooled_YESH_survival_estimates$stand_error[1]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PQbreed,3]
Pooled_YESH_survival_estimates$stand_error[2]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PCbreed,3]
Pooled_YESH_survival_estimates$stand_error[3]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PQprosp,3]
Pooled_YESH_survival_estimates$stand_error[4]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PCprosp,3]
Pooled_YESH_survival_estimates$stand_error[5]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==Mcave,3]
Pooled_YESH_survival_estimates$stand_error[6]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==Mledge,3]
Pooled_YESH_survival_estimates$lower95CI[1]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PQbreed,4]
Pooled_YESH_survival_estimates$lower95CI[2]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PCbreed,4]
Pooled_YESH_survival_estimates$lower95CI[3]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PQprosp,4]
Pooled_YESH_survival_estimates$lower95CI[4]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PCprosp,4]
Pooled_YESH_survival_estimates$lower95CI[5]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==Mcave,4]
Pooled_YESH_survival_estimates$lower95CI[6]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==Mledge,4]
Pooled_YESH_survival_estimates$upper95CI[1]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PQbreed,5]
Pooled_YESH_survival_estimates$upper95CI[2]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PCbreed,5]
Pooled_YESH_survival_estimates$upper95CI[3]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PQprosp,5]
Pooled_YESH_survival_estimates$upper95CI[4]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PCprosp,5]
Pooled_YESH_survival_estimates$upper95CI[5]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==Mcave,5]
Pooled_YESH_survival_estimates$upper95CI[6]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==Mledge,5]
Pooled_YESH_survival_estimates

## exporting the model table and the results table to a csv file

write.table(Pooled_YESH_survival_estimates,"Model_averaged_survival_estimates_all.csv", sep=",", row.names=F)
write.table(model.table,"YESH_model_comparison_all2.csv", sep=",", row.names=F)

## cleaning up the workspace

rm(YESH_survival)
cleanup(ask=FALSE)


############################################################################################################
############## 3. SEPARATE ANALYSIS FOR EACH COUNTRY #######################################################
####### incorporate covariates, to allow for differences between Malta/France, sites, and breeding status ##
############################################################################################################


# FRANCE

YESH<-import.chdata("YESH_data_France.txt", header = F, field.names= c("ch","site","breeder"), field.types = c("f","f"), use.comments=TRUE)
YESH.proc<-process.data(YESH, model="CJS", begin.time=2005, groups=c("site", "breeder")) 
release.gof(YESH.proc)
YESH.ddl<-make.design.data(YESH.proc)
do.analysis<-function()
{
Phi.dot<-list(formula=~1)
Phi.time<-list(formula=~time)
Phi.breeder<-list(formula=~breeder)
Phi.site<-list(formula=~site)
Phi.breeder.site<-list(formula=~-1+breeder:site)
p.breeder<-list(formula=~breeder)
p.dot<-list(formula=~1)
p.time<-list(formula=~time)
cml<-create.model.list("CJS")
model.list<-mark.wrapper(cml,data=YESH.proc,ddl=YESH.ddl)
return(model.list)
}
YESH_survival<-do.analysis()
model.table<-model.table(YESH_survival,use.lnl=TRUE) 
model.table$EvidenceRatio<-2.71828182845904523536^(-0.5*model.table$DeltaAICc) 
model.table

## identifying the parameters from the all-different PIM to find the lines of interest in the model-average output

PIMS<-PIMS(YESH_survival[[1]], "Phi", simplified=FALSE)  ## this line will just print the PIMs and serves for a visual reality check, it is not required

par.index.prospector<-YESH.ddl$Phi[YESH.ddl$Phi$breeder=="P",]
par.index.breeder<-YESH.ddl$Phi[YESH.ddl$Phi$breeder=="B",]
PQbreed<-min(as.numeric(row.names(par.index.breeder[par.index.breeder$site=="PQ",])))
PQprosp<-min(as.numeric(row.names(par.index.prospector[par.index.prospector$site=="PQ",])))
PCbreed<-min(as.numeric(row.names(par.index.breeder[par.index.breeder$site=="PC",])))
PCprosp<-min(as.numeric(row.names(par.index.prospector[par.index.prospector$site=="PC",])))

## model-average the results and compile the parameters of interest in a Table
parameter.estimates<-model.average(YESH_survival,parameter="Phi", vcv=T, drop=FALSE)

France_YESH_survival_estimates<-data.frame(Group=c("Porquerolles breeders", "Port-Cros breeders","Porquerolles prospectors", "Port-Cros prospectors"))
France_YESH_survival_estimates$mean_survival[1]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PQbreed,2]
France_YESH_survival_estimates$mean_survival[2]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PCbreed,2]
France_YESH_survival_estimates$mean_survival[3]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PQprosp,2]
France_YESH_survival_estimates$mean_survival[4]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PCprosp,2]
France_YESH_survival_estimates$stand_error[1]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PQbreed,3]
France_YESH_survival_estimates$stand_error[2]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PCbreed,3]
France_YESH_survival_estimates$stand_error[3]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PQprosp,3]
France_YESH_survival_estimates$stand_error[4]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PCprosp,3]
France_YESH_survival_estimates$lower95CI[1]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PQbreed,4]
France_YESH_survival_estimates$lower95CI[2]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PCbreed,4]
France_YESH_survival_estimates$lower95CI[3]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PQprosp,4]
France_YESH_survival_estimates$lower95CI[4]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PCprosp,4]
France_YESH_survival_estimates$upper95CI[1]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PQbreed,5]
France_YESH_survival_estimates$upper95CI[2]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PCbreed,5]
France_YESH_survival_estimates$upper95CI[3]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PQprosp,5]
France_YESH_survival_estimates$upper95CI[4]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==PCprosp,5]
France_YESH_survival_estimates

write.table(France_YESH_survival_estimates,"Model_averaged_survival_estimates_France.csv", sep=",", row.names=F)
write.table(model.table,"YESH_model_comparison_France.csv", sep=",", row.names=F)

rm(YESH_survival)
cleanup(ask=FALSE)


#### MALTA #####

setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Malta\\Analysis\\Survival_analysis\\RMark_attempts")
setwd("C:\\STEFFEN\\WORK\\RSPB\\Malta\\Analysis\\Survival_analysis\\RMark_attempts")

YESHm<-import.chdata("YESH_2010_Malta_only.txt", header = F, field.names= c("ch","site"), field.types = "f", use.comments=TRUE)
YESHm.proc<-process.data(YESHm, model="CJS", begin.time=2005, groups="site") 
release.gof(YESHm.proc)
YESHm.ddl<-make.design.data(YESHm.proc)
do.analysis<-function()
{
Phi.dot<-list(formula=~1)
Phi.time<-list(formula=~time)
p.time.site<-list(formula=~time+site)
p.site<-list(formula=~site)
p.time<-list(formula=~time)
cml<-create.model.list("CJS")
model.list<-mark.wrapper(cml,data=YESHm.proc,ddl=YESHm.ddl)
return(model.list)
}
YESHm_survival<-do.analysis()
model.table<-model.table(YESHm_survival,use.lnl=TRUE) 
model.table$EvidenceRatio<-2.71828182845904523536^(-0.5*model.table$DeltaAICc) 
model.table

## identifying the parameters from the all-different PIM to find the lines of interest in the model-average output

PIMS<-PIMS(YESHm_survival[[1]], "Phi", simplified=FALSE)  ## this line will just print the PIMs and serves for a visual reality check, it is not required

cave<-min(row.names(YESHm.ddl$Phi[YESHm.ddl$Phi$site=="3",]))
ledge<-min(row.names(YESHm.ddl$Phi[YESHm.ddl$Phi$site=="4",]))

## model-average the results and compile the parameters of interest in a Table
parameter.estimates<-model.average(YESHm_survival,parameter="Phi", vcv=T, drop=FALSE)
Malta_YESH_survival_estimates<-data.frame(Group=c("cave", "ledge"))
Malta_YESH_survival_estimates$mean_survival[1]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==cave,2]
Malta_YESH_survival_estimates$mean_survival[2]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==ledge,2]

Malta_YESH_survival_estimates$stand_error[1]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==cave,3]
Malta_YESH_survival_estimates$stand_error[2]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==ledge,3]

Malta_YESH_survival_estimates$lower95CI[1]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==cave,4]
Malta_YESH_survival_estimates$lower95CI[2]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==ledge,4]

Malta_YESH_survival_estimates$upper95CI[1]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==cave,5]
Malta_YESH_survival_estimates$upper95CI[2]<-parameter.estimates$estimates[parameter.estimates$estimates$par.index==ledge,5]

Malta_YESH_survival_estimates
setwd("C:\\STEFFEN\\WORK\\RSPB\\Malta\\Analysis\\Survival_analysis\\Final_analysis")
write.table(Malta_YESH_survival_estimates,"Model_averaged_survival_estimates_Malta.csv", sep=",", row.names=F)
write.table(model.table,"YESH_model_comparison_Malta.csv", sep=",", row.names=F)



rm(YESHm_survival)
cleanup(ask=FALSE)



###################################################################################
#### MALTA 1969-1995 #####
###################################################################################

setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Malta\\Analysis\\Survival_analysis\\Final_analysis")
setwd("C:\\STEFFEN\\WORK\\RSPB\\Malta\\Analysis\\Survival_analysis\\Final_analysis")
library(RMark)

YESHm<-import.chdata("YESH_data_Malta_preLIFE.txt", header = F, field.names= c("ch","frequency"), use.comments=TRUE)
YESHm.proc<-process.data(YESHm, model="CJS", begin.time=1969) 
release.gof(YESHm.proc)
YESHm.ddl<-make.design.data(YESHm.proc)
do.analysis<-function()
{
Phi.dot<-list(formula=~1)
Phi.time<-list(formula=~time)
Phi.trend<-list(formula=~Time)
p.time<-list(formula=~time)
p.dot<-list(formula=~1)
cml<-create.model.list("CJS")
model.list<-mark.wrapper(cml,data=YESHm.proc,ddl=YESHm.ddl)
return(model.list)
}
YESHm_survival<-do.analysis()
model.table<-model.table(YESHm_survival,use.lnl=TRUE) 
model.table$EvidenceRatio<-2.71828182845904523536^(-0.5*model.table$DeltaAICc) 
model.table

### trying to estimate process variance
### THIS DID NOT WORK BECAUSE THE VCV.real has non-positive variances!!!
md<-mark(YESHm.proc,model.parameters=list(Phi=list(formula=~1), p=list(formula=~time)))
PIMS<-PIMS(md, "Phi", simplified=FALSE) 
zz<-get.real(md,"Phi",vcv=T)
z<-zz$estimates$estimate[1:25]
vcv<-zz$vcv.real
var.components(z,design=matrix(rep(1,length(z)),ncol=1),matrix(rep(vcv,length(z)),nrow=length(z), ncol=length(z))) 
out<-zz$estimates[1:25,3:10]

## after including temporal trend model, model averaged estimates:
parameter.estimates<-model.average(YESHm_survival,parameter="Phi", vcv=T, drop=FALSE)
out<-parameter.estimates$estimates[1:25,2:7]


write.table(out,"Survival_estimates_Malta1969-1995.csv", sep=",", row.names=F)
write.table(model.table,"YESH_model_comparison_Malta1969-1995.csv", sep=",", row.names=F)


rm(md)
rm(YESHm_survival)
cleanup(ask=FALSE)



