###Doing mark capture analysis, testing to see whether there are consistant
###downward trends in recruitment  of nestlings

library(RMark)
library(ggplot2)
setwd("~/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis")
datMark <- readRDS("All birds MARK Data.rda")
#Set all the ages of males caught as adults to 2 (ie equivalent to an ASY)
datMark$age[which(datMark$AgeAtFirstSight=="AHY" & datMark$Sex=="M")]<- 2
datMark$age <- as.factor(datMark$age)


nestlingprocess <- process.data(datMark,model="CJS",begin.time=1975, groups= c("age"), initial.ages =c(0,1,2))
#initial age for all the hatchlings is 0 because here I am only looking at those birds we first saw as nestlings

RELEASEresults <- release.gof(nestlingprocess)
chat <- RELEASEresults$Chi.square[3]/RELEASEresults$df[3]
#This is below 3, which was the goal. It's still not perfect, but probably good enough to be going with

nestling.ddl<- make.design.data(nestlingprocess, parameters=list(Phi=list(age.bins=c(0,0.8,1.5,  44)),
                                                                 p=list(age.bins=c(0,0.8,1.5, 44)))) 
#we are binning the ages into HY, and adult since we don't know how old the males are when first captured. 

nestling.ddl$Phi$popStatus<- rep(NA)
nestling.ddl$Phi$popStatus[which(nestling.ddl$Phi$Time<21)]<- 0
nestling.ddl$Phi$popStatus[which(nestling.ddl$Phi$Time>=21)]<- 1
nestling.ddl$Phi$popStatus<- as.factor(nestling.ddl$Phi$popStatus)



#Capture probability probably varies by year, but there should be no trend so we
#use time (discrete). There may also be a capture bias where nestlings the first
#year are transient? I think I can account for that using a cohort effect that only applies to the first group. I will do this in the future.  

p.time <- list(formula= ~time)
p.dot <- list(formula= ~1)

#Survival probably varies by age, and we want to see if there is a difference in
#survival since the TerrOcc started to decline
Phi.dot <- list(formula=~1)
Phi.age <- list(formula=~age)
Phi.popStatus <- list(formula=~popStatus)
Phi.time <- list(formula=~time)
Phi.ageStaus <- list(formula= ~age + popStatus )
Phi.agebytime <- list(formula= ~age * popStatus)
Phi.ageStaustime <- list(formula= ~age + popStatus + time)
Phi.agebyStatustime <- list(formula= ~age * popStatus + time)





cml <- create.model.list("CJS")
nestlingresults <- mark.wrapper(cml, data=nestlingprocess, ddl=nestling.ddl, output=F, adjust=F)

adjustednestlingresults <- adjust.chat(chat, nestlingresults)

nestlingresults[10]



rm(ls=list())


#What about if it's a regression? Does that fit ok?
p.time <- list(formula= ~time)
p.dot <- list(formula= ~1)


Phi.dot <- list(formula=~1)
Phi.age <- list(formula=~age)
Phi.Time <- list(formula=~Time)
Phi.Timeage <- list(formula=~Time + age)
Phi.Timebyage <- list(formula=~Time * age)

cml <- create.model.list("CJS")
nestlingresults <- mark.wrapper(cml, data=nestlingprocess, ddl=nestling.ddl, output=F, adjust=F)

adjustednestlingresults <- adjust.chat(chat, nestlingresults)

modAv <- model.average(adjustednestlingresults)
global <- nestlingresults[[10]]

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


# Model 10 (global) is easily the best model. I will just pull results from this
# model and graph those
BestResults <- as.data.frame(matrix(nrow=42, ncol=7))
colnames(BestResults) <- c("Year", "Recruitment1", "seRecruitment1", "SYReturn1", "seSYReturn1","ASYReturn1", "seASYReturn1" )

BestResults$Year <- seq(1975, 2016, 1)
BestResults[,2:3]<- modAv[RecruitPIMS,2:3]
BestResults[,4:5]<- modAv[SYPIMS,2:3]
BestResults[,6:7]<- modAv[ASYPIMS,2:3]


AllResults <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis/Model Averaged Yearly Apparent Survival for all records to get recruitment.csv", as.is=T)
CombinedResults <- cbind(BestResults, AllResults[,2:7])

#Plot the different types of survival estimates with their linear regression
#line that we calculated to see if it looks like it fits well
ggplot(CombinedResults, aes(x=Year))+
  geom_line(aes(y=SYReturn1), color="brown")+
  geom_point(aes(y=SYReturn), color="brown")+
  geom_segment(aes(xend=Year, y=SYReturn-seSYReturn, yend=SYReturn+seSYReturn), color="brown")+
  geom_line( aes(y=ASYReturn1), color="blue")+
  geom_point(aes(y=ASYReturn), color="blue")+
  geom_segment( aes(xend=Year, y=ASYReturn-seASYReturn, yend=ASYReturn+seASYReturn), color="blue")+
  geom_line( aes(y=Recruitment1), color="black")+
  geom_point(aes(y=Recruitment), color="black")+
  geom_segment( aes(xend=Year, y=Recruitment-seRecruitment, yend=Recruitment+seRecruitment), color="black")+
  ylab("Survival Estimate")+
  ggthemes::theme_few()






PlotRecruit <- ggplot(CombinedResults, aes(x=Year))+
  geom_line( aes(y=Recruitment1), color="black")+
  geom_ribbon(aes(ymin= Recruitment1-seRecruitment1, ymax=Recruitment1 + seRecruitment1), alpha=0.5)+
  geom_point(aes(y=Recruitment), color="black")+
  geom_segment( aes(xend=Year, y=Recruitment-seRecruitment, yend=Recruitment+seRecruitment), color="black")+
  ylab("Recruitment Estimate")+
  xlab("")+
  ggthemes::theme_few()

PlotSYReturn <- ggplot(CombinedResults, aes(x=Year))+
  geom_line(aes(y=SYReturn1), color="brown")+
  geom_ribbon(aes(ymin= SYReturn1-seSYReturn1, ymax=SYReturn1 + seSYReturn1), alpha=0.5, fill="brown")+
  geom_point(aes(y=SYReturn), color="brown")+
  geom_segment(aes(xend=Year, y=SYReturn-seSYReturn, yend=SYReturn+seSYReturn), color="brown")+
  ylab("SY Return Estimate")+
  xlab("")+
  ggthemes::theme_few()

PlotASYReturn <- ggplot(CombinedResults, aes(x=Year))+
  geom_line( aes(y=ASYReturn1), color="blue")+
  geom_ribbon(aes(ymin= ASYReturn1-seASYReturn1, ymax=ASYReturn1 + seASYReturn1), alpha=0.5, fill="blue")+
  geom_point(aes(y=ASYReturn), color="blue")+
  geom_segment( aes(xend=Year, y=ASYReturn-seASYReturn, yend=ASYReturn+seASYReturn), color="blue")+
  ylab("ASY Return Estimate")+
  ggthemes::theme_few()

cowplot::plot_grid(PlotRecruit, PlotSYReturn, PlotASYReturn, align="v", nrow=3)
