###################################################################################################
########   YELKOUAN SHEARWATER POPULATION SIMULATION TO EXPLAIN COUNTS IN TURKEY   ################
###################################################################################################
# written by Dilek Sahin, 17 Apr 2016
# modified to include better demographic data in Aug 2016
# simulation and graphs added by Steffen Oppel on 19 Aug 2016 (steffen.oppel@rspb.org.uk)

library(popbio)
library(ggplot2)
library(doParallel)
library(foreach)




#################### SIMULATION ACROSS RANGE OF PARAMETERS ########################################

### SPECIFY RANGE OF PARAMETERS ###

pop.size<-seq(46000,92000,10000)				### population size in individuals
Sb<-seq(0.793,0.851,0.01)					### survival of adult breeders from Oppel et al. 2011
Snb<-seq(0.947,0.99,0.01)					### survival of adult non-breeders from Oppel et al. 2011
S1<-seq(0.434,0.853,0.01)					### survival of first year birds from Bonnaud et al. 2011 and Genovart et al. 2016
S2<-seq(0.853,0.922,0.01)					### survival of first year birds from Bonnaud et al. 2011
S0<-0.605								### juvenile survival from Bonnaud et al. 2009
r3<-0.03								### recruitment at age3 from Genovart et al. 2016
r4<-0.243								### recruitment at age4 from Genovart et al. 2016
r5<-0.431								### recruitment at age5 from Genovart et al. 2016
sk<-0.2								### adult non-breeding (skip) probability, from Brooke 1990
sk2<-0.04								### non-breeder skip probability, from Bonnaud et al. 2011 taken from Warham 1990
F<-seq(0.5,0.75,0.05)						### breeding success, specified as 0.728 from Bonnaud et al. 2009, on cat-free island




#################### CREATING THE POPULATION MATRIX ##################################################

seabird.matrix<-expression(
0,0,0,0,0,0,0,(F*0.5*S0),
S0,0,0,0,0,0,0,0,
0,S1,0,0,0,0,0,0,
0,0,S2,0,0,0,0,0,
0,0,0,(Snb*(1-r4)),0,0,0,0,
0,0,0,0,(Snb*(1-r5)),0,0,0,
0,0,0,0,0,0,(Snb*sk2),(Sb*sk),
0,0,(S2*r3),(Snb*r4),(Snb*r5),Snb,(Snb*(1-sk2)),(Sb*(1-sk))
)




#########################################################
###### LOOP OVER ALL COMBINATIONS OF DEMOGRAPHY #########
#########################################################

### COMPREHENSIVE TABLE OF ALL COMBINATIONS OF DEMOGRAPHIC PARAMETERS ###

simul_in<-expand.grid(pop.size, Sb, Snb, S1,S2,F)
dim(simul_in)
names(simul_in)<-c('pop.size','Sb','Snb','S1','S2','F')
simul_in$Nonbreed<-0
SIM_OUT<-data.frame()


### setup parallel backend to use 8 processors

cl<-makeCluster(8)
registerDoParallel(cl, cores=8)



### CALCULATING STABLE AGE DISTRIBUTION

SIM_OUT<-foreach(s=c(1:dim(simul_in)[1]), .packages='popbio',.combine=rbind) %dopar% {


### CREATE LESLIE MATRIX WITH SUBSET OF VITAL RATES

seabird.vr<-list(F=simul_in[s,6], S0=S0,S1=simul_in[s,4],S2=simul_in[s,5],Snb=simul_in[s,3],sk=sk,sk2=sk2,r3=r3,r4=r4,r5=r5,Sb=simul_in[s,2])
A<-matrix(sapply(seabird.matrix, eval,seabird.vr , NULL), nrow=sqrt(length(seabird.matrix)), byrow=TRUE)
x<-stable.stage(A)


### CALCULATING NON-BREEDING SEGMENT OF POPULATION

out<-simul_in[s,]
out$Nonbreed<-sum(x[1:7])*out$pop.size
out[,8:15]<-x
return(out)
}
stopCluster(cl)




### INSPECT RESULTS ###

names(SIM_OUT)[8:15]<-c('juv','N1','N2','N3nb','N4nb','N5nb','Nanb','Nab')
head(SIM_OUT)
SIM_OUT$NBfrac<-SIM_OUT$Nonbreed/SIM_OUT$pop.size
SIM_OUT$count.prop<-SIM_OUT$Nonbreed/90048
range(SIM_OUT$NBfrac)
range(SIM_OUT$count.prop)




### PLOT A HISTOGRAM TO SHOW RESULTS OF POPULATION SIZE

png("YESH_pop_simulations.png", units="px", width=2000, height=1200, res=600)
ggplot(data=SIM_OUT, aes(SIM_OUT$Nonbreed),xlim(20, 15)) + geom_histogram(fill="dodgerblue4", colour="white") +
	xlim(10000,95000) + geom_vline(xintercept = 90048, col='red', lwd=2)+
	theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=18, color="black"), 
        axis.title=element_text(size=20), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  ylab("N of simulations") +
  xlab("Number of Yelkouan Shearwaters")+
	annotate("text", x=40000, y=40000, label= "Simulated non-breeder fraction", col='dodgerblue4', size=7) + 
	annotate("text", x=80000, y=40000, label= "Observed migration", col='red', size=7)
dev.off()





### PLOT AGE DISTRIBUTION BOXPLOTS ###

### summarise dataset
boxplotdata<-melt(SIM_OUT, id.vars=names(SIM_OUT)[2:6], measure.vars=c('juv','N1','N2','N3nb','N4nb','N5nb','Nanb','Nab'), variable.name = "Prop")
head(boxplotdata)


png("YESH_age_distribution.png", units="px", width=2000, height=1200, res=600)
ggplot(data=boxplotdata, aes(x=variable, y=value)) + geom_boxplot() +
	theme(panel.background=element_rect(fill="white", colour="black"), 
        axis.text=element_text(size=18, color="black"), 
        axis.title=element_text(size=20), 
        strip.text.x=element_text(size=18, color="black"), 
        strip.background=element_rect(fill="white", colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  ylab("Proportion of population") +
  xlab("Life history stage")
dev.off()




### EXPORT OUTPUT TABLE ###

write.table(SIM_OUT, "YESH_pop_size_simulations.csv", sep=",")






