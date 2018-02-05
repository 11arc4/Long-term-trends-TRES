#I wonder if the reason we are having so much trouble fitting out data is that
#the early years have very sparse data.

setwd("~/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis")#Make sure you set the working directory to something seperarate otherwise MARK
nstgMark <- readRDS("Female Fledgling MARK Data.rda")
#This has only female nestlings that fledged, PLUS all the females. Unknown
#fledglings (ie those that didn't recruit) were set to male or female randomly
#(50-50) and males were removed.

#I'm going to chop the years at 1985 (10 years in)
nstgMark$Remove <- substring(nstgMark$ch,1,10)
nstgMark$Keep <-substring(nstgMark$ch, 11, 44)

for (i in 1:nrow(nstgMark)){
  if(as.numeric(nstgMark$Remove[2])>0){
    #If the previous year the bird was seen as a SY or HY then we need to up the age at first sight
    if(as.numeric(nstgMark$Remove[2])==1){
      if(nstgMark$AgeAtFirstSight=="SY" | nstgMark$AgeAtFirstSight=="ASY"){
        nstgMark$AgeAtFirstSight[2]<-"ASY"
      }
      if(nstgMark$AgeAtFirstSight=="HY"){
        nstgMark$AgeAtFirstSight[2]<-"SY"
      }
    }
    #If the bird was seen 2+ years ago
    if(as.numeric(nstgMark$Remove[2])>9){
      nstgMark$AgeAtFirstSight[2]<-"ASY"
    }
    
    
  }
  
}
#cut out the old columns and rename, recreate Age column
nstgMark_1985<- nstgMark %>% filter(as.numeric(Keep)>0)
nstgMark_1985<- nstgMark_1985[,c(2,3,6)]
colnames(nstgMark_1985)[3]<-"ch"

nstgMark_1985$age[which(nstgMark_1985$AgeAtFirstSight=="HY")]<-0
nstgMark_1985$age[which(nstgMark_1985$AgeAtFirstSight=="SY")]<-1
nstgMark_1985$age[which(nstgMark_1985$AgeAtFirstSight=="ASY")]<-2
nstgMark_1985$age[which(nstgMark_1985$AgeAtFirstSight=="AHY")]<-2




#OK Moment of truth. Does this data fit better? 

library(RMark)
#I've created multiple groups based on sex and at what age we first saw the
#bird. Also set the initial year to 1975.

# We're going to want a CJS #model because our data is recapture data, and we
# can't tell whether you've #actually died. 
tsprocess2 <-process.data(nstgMark_1985,model="CJS",
                         begin.time=1985, 
                         groups= ("age"),
                         initial.ages =c(0,1, 2))
RELEASEresults <- release.gof(tsprocess2, invisible = F)
chat <- RELEASEresults$Chi.square[3]/RELEASEresults$df[3]
#NOOOOO it's worse for the fully time variate model

ts.ddl2<- make.design.data(tsprocess2, parameters=list(Phi=list(age.bins=c(0, 0.8, 1.8, 44)),
                                                     p=list(age.bins=c(0, 0.8, 1.8, 44))))
ts.ddl2$Phi$popStatus<- rep(NA)
ts.ddl2$Phi$popStatus[which(ts.ddl2$Phi$Time<=11)]<- 0  #1985-1996 increased
ts.ddl2$Phi$popStatus[which(ts.ddl2$Phi$Time>11)]<- 1 #1997-2014 decreased
ts.ddl2$Phi$popStatus[which(ts.ddl2$Phi$Time>31)]<- 2 #2015-2017 have been increasing again

ts.ddl2$Phi$popStatus<- as.factor(ts.ddl2$Phi$popStatus)


#there are probably differences in capture ability by age-- often disappear as SY
p.timeage <- list(formula= ~time + age)
p.time <- list(formula= ~time)
p.dot <- list(formula= ~1)
p.age <- list(formula= ~age)


#We don't seem to have good enough data to test a linear regression well.
#Instead let's test whether there are three stages of recruitment estimates based on whether the population is growin or shrinking
Phi.popstatusbyage<- list(formula=~popStatus*age)
Phi.popstatus<- list(formula=~popStatus)
Phi.age<- list(formula=~age)
Phi.popstatusage<- list(formula=~popStatus+age)
Phi.dot<- list(formula=~1)

#Can we afford to include time? Does that blow up the model and stop fitting well? 
Phi.popstatusage <- list(formula=~popStatus * age + Time)
Phi.popstatus <- list(formula=~popStatus + age + Time)
Phi.popstatustime <- list(formula=~age + popStatus*Time)
Phi.popstatusagetime <- list(formula=~popStatus*age + popStatus*Time)





cml2 <- create.model.list("CJS")
results2 <- mark.wrapper(cml2, data=tsprocess2, ddl=ts.ddl2, output=F, adjust=F)


export.MARK(tsprocess2, "FemaleNestlings_1985", results2, replace=T)
#This actually looks a hell of a lot better in terms of the data. It's a bit
#more spread out, but it's not as bad. The median chat of my global model is
#1.32 which I think is perfectly good. I'll adjust the chat and then I feel
#pretty damn good about it.


adjust.chat(results2, chat=1.32)
#There is a clear winner-- model 16 is 4.5 times better than the alternatives.
#lets try to pull out our estimates from that model
bestmod <- adjust.chat(results2, chat=1.32)[[16]]


ModelData<- as.data.frame(matrix(nrow=96, ncol=6, NA))
colnames(ModelData)<- c("Year", "Age", "Estimate", "SE", "Point", "PointSE")
ModelData$Year<- rep(seq(1985,2016,1),3)
ModelData$Age<- c(rep("HY", 32), rep("SY", 32), rep("ASY", 32))
#Rows 1:32 are recruitment
ModelData[2:32,3:4]<- bestmod$results$real[seq(33,93,2),1:2 ] #1986-2016 recruitment
ModelData[1,3:4] <- bestmod$results$real[1,1:2 ] #1985 recruitment

#Rows 33:64 are SY return

ModelData[35:64,3:4]<- bestmod$results$real[seq(34,92,2),1:2 ] #1987-2016 SY return
ModelData[33,3:4] <- bestmod$results$real[94,1:2 ] #1985 SY return
ModelData[34,3:4] <- bestmod$results$real[2,1:2 ] #1986 SY return
#Rows 65:96 are ASY Return
ModelData[67:96, 3:4]<- bestmod$results$real[3:32,1:2 ] #1987-2016 ASY return
ModelData[65, 3:4] <- bestmod$results$real[96,1:2 ] #1985, ASY
ModelData[66, 3:4] <-bestmod$results$real[95,1:2 ] #1986, ASY

Points <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis/Model Averaged Yearly Apparent Survival for all records to get recruitment.csv", as.is=T) %>%
  filter(Year>=1985)

ModelData[1:32, 5:6]<- Points[,2:3] #recruitment
ModelData[33:64, 5:6]<- Points[,4:5] #SYReturn
ModelData[65:96, 5:6]<- Points[,6:7] #ASYReturn
ModelData$Age <- factor(ModelData$Age, levels=c("HY", "SY", "ASY"))

ggplot(ModelData %>% filter(Year<2016), aes(x=Year, fill=Age, color=Age))+
  geom_line(aes(y=Estimate))+
  geom_ribbon(aes(ymin=(Estimate-SE), ymax=Estimate+SE), alpha=0.2)+
  geom_line(aes(y=Point))+
  geom_segment(aes(y=Point-PointSE, yend=Point+PointSE, xend=Year), color="black")+
  geom_point(aes(y=Point))+
  facet_grid(~Age)



bestmod_withoutTime <- adjust.chat(results2, chat=1.32)[[24]]


