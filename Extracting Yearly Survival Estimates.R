#Extracct our very best survival estimates for each year. 

#I will then use these estimates in some modelling to see if they are declining,
#and even more importantly, if they correlated to any environmental changes.

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


#Capture probability probably varies by year, but there should be no trend so we
#use time (discrete). There may also be a capture bias where nestlings the first
#year are transient? I think I can account for that using a cohort effect that only applies to the first group. I will do this in the future.  
p.timeage <- list(formula= ~time + age)
p.time <- list(formula= ~time)
p.dot <- list(formula= ~1)
p.age <- list(formula= ~age)

#We don't seem to have good enough data to test a linear regression well.
#Instead let's test whether there are three stages of recruitment estimates based on whether the population is growin or shrinking
Phi.timeage<- list(formula=~time*age)
Phi.time<- list(formula=~time)
Phi.age<- list(formula=~age)
Phi.timebyage<- list(formula=~time+age)
Phi.dot<- list(formula=~1)



cml <- create.model.list("CJS")
results <- mark.wrapper(cml, data=tsprocess, ddl=ts.ddl, output=F, adjust=F)
export.MARK(tsprocess, "Yearly Estimates", results, replace=T)


#There is a clear winner here (yearly varaition by age for both capture and survival). That is really to be expected. 
#Now we just need to make sure that our chat is low enough to be usefull, adjust if it isn't and extract estimates. 

#adjusted to the median chat for our best model.
adjustedresults <- adjust.chat(results, chat=1.187)


bestmod <- adjustedresults[16]$Phi.timeage.p.timeage


bestmod$results$real

#Extract PIMS
DiscreteResults <- data.frame(Year=seq(1975, 2016, 1), 
                              Recruit=NA,
                              SYReturn=NA, 
                              ASYReturn=NA,
                              RecruitSE=NA,
                              SYReturnSE=NA, 
                              ASYReturnSE=NA)


RealResults <- bestmod$results$real
RealResults$Row <- seq(1,210,1)

DiscreteResults$ASYReturn[3:42]<- RealResults$estimate[3:42]
DiscreteResults$ASYReturn[1]<- RealResults$estimate[126]
DiscreteResults$ASYReturn[2]<- RealResults$estimate[124]

DiscreteResults$Recruit[1]<- RealResults$estimate[1]
DiscreteResults$Recruit[2:42]<- RealResults$estimate[seq(43, 123, 2)]

DiscreteResults$SYReturn[2]<- RealResults$estimate[2]
DiscreteResults$SYReturn[3:42]<- RealResults$estimate[seq(44, 122, 2)]
DiscreteResults$SYReturn[1]<- RealResults$estimate[124]


DiscreteResults$ASYReturnSE[3:42]<- RealResults$se[3:42]
DiscreteResults$ASYReturnSE[1]<- RealResults$se[126]
DiscreteResults$ASYReturnSE[2]<- RealResults$se[124]

DiscreteResults$RecruitSE[1]<- RealResults$se[1]
DiscreteResults$RecruitSE[2:42]<- RealResults$se[seq(43, 123, 2)]

DiscreteResults$SYReturnSE[2]<- RealResults$se[2]
DiscreteResults$SYReturnSE[3:42]<- RealResults$se[seq(44, 122, 2)]
DiscreteResults$SYReturnSE[1]<- RealResults$se[124]


YearlySurvival <- DiscreteResults %>% gather(ASYReturn, SYReturn, Recruit, key="Age", value="Estimate" )%>%
  gather(RecruitSE, SYReturnSE, ASYReturnSE, key="AgeSE", value="SE")
YearlySurvival<- YearlySurvival[,-4]

YearlySurvival$SE


ggplot(YearlySurvival %>% filter(Year !=1975 & Year!=1976 & Year!=2016 & Estimate<.9 & SE<0.2))+
  geom_segment(aes(x=Year, xend=Year, y=Estimate-SE, yend=Estimate+SE, color=Age), alpha=0.6)+
  geom_point( aes(x=Year, y=Estimate, color=Age))+
  labs(x="Year", y="Apparent Survival" )+
  ggthemes::theme_few()


write.csv(YearlySurvival, file="file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/Yearly Survival Estimates.csv", na="", row.names=F)




cleanup(ask=F)


#Matt Guzzo reccomended running POPAN models to get abundance estimates that we
#could compare to the number of birds in boxes.
cml_popan <- create.model.list("POPAN")
results_popan <- mark.wrapper(cml_popan, data=tsprocess, ddl=ts.ddl, output=F, adjust=F)


