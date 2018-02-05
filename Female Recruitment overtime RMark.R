setwd("~/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis")#Make sure you set the working directory to something seperarate otherwise MARK
nstgMark <- readRDS("Female Fledgling MARK Data.rda")
#This has only female nestlings that fledged, PLUS all the females. Unknown
#fledglings (ie those that didn't recruit) were set to male or female randomly
#(50-50) and males were removed.

library(RMark)
#I've created multiple groups based on sex and at what age we first saw the
#bird. Also set the initial year to 1975.

# We're going to want a CJS #model because our data is recapture data, and we
# can't tell whether you've #actually died. 
tsprocess <-process.data(nstgMark,model="CJS",
                         begin.time=1975, 
                         groups= ("age"),
                         initial.ages =c(0,1, 2))
RELEASEresults <- release.gof(tsprocess, invisible = F)
chat <- RELEASEresults$Chi.square[3]/RELEASEresults$df[3]
#chat isn't great..... that's a problem


ts.ddl<- make.design.data(tsprocess, parameters=list(Phi=list(age.bins=c(0, 0.8, 1.8, 44)),
                                                     p=list(age.bins=c(0, 0.8, 1.8, 44))))
ts.ddl$Phi$popStatus<- rep(NA)
ts.ddl$Phi$popStatus[which(ts.ddl$Phi$Time<=21)]<- 0  #1975-1996 increased
ts.ddl$Phi$popStatus[which(ts.ddl$Phi$Time>21)]<- 1 #1997-2014 decreased
#ts.ddl$Phi$popStatus[which(ts.ddl$Phi$Time>41)]<- 2 #2015-2017 have been increasing again

ts.ddl$Phi$popStatus<- as.factor(ts.ddl$Phi$popStatus)




#Capture probability probably varies by year, but there should be no trend so we
#use time (discrete). There may also be a capture bias where nestlings the first
#year are transient? I think I can account for that using a cohort effect that only applies to the first group. I will do this in the future.  
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

# #Can we afford to include time? Does that blow up the model and stop fitting well? 
# Phi.popstatusage <- list(formula=~popStatus * age + Time)
# Phi.popstatus <- list(formula=~popStatus + age + Time)
# Phi.popstatustime <- list(formula=~age + popStatus*Time)
# Phi.popstatusagetime <- list(formula=~popStatus*age + popStatus*Time)
# #I think we cannot afford to do that. We don't have good enough data for this to fit well. 
# 
# #

cml <- create.model.list("CJS")
results <- mark.wrapper(cml, data=tsprocess, ddl=ts.ddl, output=F, adjust=F)
export.MARK(tsprocess, "FemaleNestlings", results, replace=T)


#median chat for the model phi=popStatus*age, p=time+age (ie our global model)=1.29 (model 20)


adjustedresults <- adjust.chat(1.29, results)
adjustedresults[20]$Phi.popstatusbyage.p.timeage$results


modAv <- model.average(results)

global <- results[[20]]

RecruitPIMS <- rep(NA,42)
SYPIMS <- rep(NA,42)
ASYPIMS <- rep(NA,42)

for (i in 1:42){
  #Group 1 is birds caught as nestlings first
  RecruitPIMS[i]<- global$pims$Phi[[1]]$pim[i,i]
  #group 2 is birds caught as SY first
  SYPIMS[i] <- global$pims$Phi[[2]]$pim[i,i]
  # group 3 is birds caught as full adults first
  ASYPIMS[i] <- global$pims$Phi[[3]]$pim[i,i]
}


# Model 10 (global) is easily the best model when we include time. I will just pull results from this
# model and graph those


#When we agree that we are shite at picking up linear trends and basically can't
#see them, then the best model isn't model 10, it's model 20

bestmod<- adjustedresults[20]

bestmod$Phi.popstatusbyage.p.timeage$results$real

DiscreteResults <- as.data.frame(matrix(ncol=6, nrow=6))
colnames(DiscreteResults)<- c("TimePeriod", "Age", "Estimate", "SE", "LowCI", "HighCI")
DiscreteResults$TimePeriod<- c(rep("Increasing",3), rep("Declining",3))
DiscreteResults$Age <- c("HY", "SY", "ASY", "ASY", "SY", "HY")
DiscreteResults[,3:6]<- bestmod$Phi.popstatusbyage.p.timeage$results$real[1:6,1:4]
DiscreteResults$TimePeriod<- factor(DiscreteResults$TimePeriod, levels=c("Increasing", "Declining"))
DiscreteResults$Age <- factor(DiscreteResults$Age, levels=c("HY", "SY", "ASY"))
AllResults$TimePeriod <- NA
AllResults$TimePeriod[which(AllResults$Year<=1996)]<- "Declining"
AllResults$TimePeriod[which(AllResults$Year>1996)]<- "Increasing"
AllResultsFemale$TimePeriod <- NA
AllResultsFemale$TimePeriod[which(AllResults$Year<=1996)]<- "Declining"
AllResultsFemale$TimePeriod[which(AllResults$Year>1996)]<- "Increasing"
AllResultsFemale$SYReturn[which(AllResultsFemale$seSYReturn > 0.2)]<- NA
AllResultsFemale$ASYReturn[which(AllResultsFemale$seASYReturn > 0.2)]<- NA
AllResults$Recruitment[which(AllResults$seRecruitment > 0.09)]<- NA

ggplot(data=DiscreteResults)+
  geom_segment(aes(x=TimePeriod, xend=TimePeriod, y=Estimate-SE, yend=Estimate+SE))+
  geom_point(aes(x=TimePeriod, color=Age, y=Estimate), size=3)+
  #geom_jitter(data=AllResults, aes(x=TimePeriod, y=Recruitment), width=0.1, color="red")+
  geom_jitter(data=AllResultsFemale, aes(x=TimePeriod, y=SYReturn), width=0.1, color="green")+
  #geom_jitter(data=AllResultsFemale, aes(x=TimePeriod, y=ASYReturn), width=0.1, color="blue")+
  ylab("Survival")
#I might buy these results? I'm not totally convinced yet

ggplot(data=DiscreteResults)+
  geom_segment(aes(x=TimePeriod, xend=TimePeriod, y=LowCI, yend=HighCI))+
  geom_point(aes(x=TimePeriod, color=Age, y=Estimate), size=3)+
    ylab("Survival")+
  xlab("Population Status")+
  scale_color_manual(labels=c("Recruitment", "1-year-old return", "Older female return"), 
                     values=c("azure4","burlywood4", "dodgerblue4"))+
  ggthemes::theme_few()



#Let's just plot the best linear trends for each of the ages that we currently have

ggplot(data=AllResultsFemale, aes(x=Year, y=SYReturn))+
  geom_line(color="brown")+
  geom_segment(aes(x=Year, xend=Year, y=SYReturn-seSYReturn, yend=SYReturn+seSYReturn), color="grey")+
  geom_point(color="brown")+
  ggthemes::theme_few()+
  geom_vline(xintercept=1996)+
  ylab("SY Survival")



ggplot(data=AllResultsFemale, aes(x=Year, y=ASYReturn))+
  geom_line(color="blue")+
  geom_segment(aes(x=Year, xend=Year, y=ASYReturn-seASYReturn, yend=ASYReturn+seASYReturn), color="grey")+
  geom_point(color="blue")+
  ggthemes::theme_few()+
  geom_vline(xintercept=1996)+
  ylab("ASY Survival")

ggplot(data=AllResults, aes(x=Year, y=Recruitment))+
  geom_line(color="black")+
  geom_segment(aes(x=Year, xend=Year, y=Recruitment-seRecruitment, yend=Recruitment+seRecruitment), color="grey")+
  geom_point(color="black")+
  ggthemes::theme_few()+
  geom_vline(xintercept=1996)+
  ylab("Recruitment")


cleanup(ask=T)
