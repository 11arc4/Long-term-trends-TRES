#Doing mark capture analysis, testing to see whether there are consistant downward trends in adult  survival. 

setwd("~/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis")#Make sure you set the working directory to something seperarate otherwise MARK
#will make 1000 files that you'll want to get rid of later.

datMark_F <- readRDS("Adult Female MARK Data_updated.rda") 
#this data is only for females. All males have been removed as have observations
#of nestlings (if the bird was seen as a nestling the nestling observation is 
#removed and it shows up first at age 1). I did this because I don't know which 
#nestlings are female and which are not if they didn't recruit so I have to 
#consider nestling recruitment independent of sex. We could maye make the
#assumption that 50% of fledgelings are female because 50% of eggs are female,
#but there could easily be unequal survival since that occurs in other species.I
#thought fewer assumptions the better, so I will do a seperate analysis for the nestlings

library(RMark)
#I've created multiple groups based on sex and at what age we first saw the
#bird. Also set the initial year to 1975.

# We're going to want a CJS #model because our data is recapture data, and we
# can't tell whether you've #actually died. 
tsprocess <-process.data(datMark_F,model="CJS",
                         begin.time=1975, 
                         groups= ("age"),
                         initial.ages =c(1, 2))

#age vector is the initial ages, "initial.ages" assigns
# a dummy variable to #count up from for each of those. So I could use AHY ASY
# and SY and assign c(1, #2, 1) instead (it's done alphabetically) That
# condensed the data into that form
#where you have the different numbers of times you've seen a particular capture
#probability.

RELEASEresults <- release.gof(tsprocess, invisible = F)
#test 2 ok-- all individuals equally likely to be captured. 

#test 3 not good-- all individials NOT equally likely to survival. What if we
#include a pre-decline and post-decline thing in the process?




ts.ddl<- make.design.data(tsprocess, parameters=list(Phi=list(age.bins=c(0, 1.8, 44)),
                                                     p=list(age.bins=c(0.8, 1.8, 44))))
ts.ddl$Phi$popStatus<- rep(NA)
ts.ddl$Phi$popStatus[which(ts.ddl$Phi$Time<21)]<- 0
ts.ddl$Phi$popStatus[which(ts.ddl$Phi$Time>=21)]<- 1
ts.ddl$Phi$popStatus[which(ts.ddl$Phi$Time>41)]<- 2 #2015-2017 have been increasing again

ts.ddl$Phi$popStatus<- as.factor(ts.ddl$Phi$popStatus)




#Settin age bins to look at birds from 1-2 (SY) and then over 2 (ASY)

#Capture probability probably varies by year, but there should be no trend so we use time (discrete)
p.time <- list(formula= ~time)
p.dot <- list(formula= ~1)

#Survival probably varies by age, and we want to see if there is a difference in
#survival since the TerrOcc started to decline
Phi.dot <- list(formula=~1)
Phi.age <- list(formula=~age)
Phi.popStatus <- list(formula=~popStatus)
Phi.ageStaus <- list(formula= ~age + popStatus )
Phi.agebytime <- list(formula= ~age * popStatus)



#Build up all the possible models and collect them based on AIC

cml <- create.model.list("CJS")
female.results <- mark.wrapper(cml, data=tsprocess, ddl=ts.ddl, output=F, adjust=F)

#Let's check the global model's residuals to make sure that the global model seems to fit ok
export.MARK(tsprocess, "TSProject2", female.results)
#based on the model p=time and Phi=popstatus*age


adjusted <- adjust.chat(female.results, chat = 1.28)
#turns out we couldn't distinguish between models 2,4, and 6. Can't quite tell
#if they need the pop status and whether the interaction is required

AvResults_female <- model.average(adjusted)

ASYPIMs <- adjusted[[4]]$pims$Phi[[1]]$pim[1,]

#along the diagonal for SY
SYPIMs <- rep(NA, 42)
for(i in 1:42){
  SYPIMs[i] <- adjusted[[4]]$pims$Phi[[1]]$pim[i,i]
}
FemaleResults <- as.data.frame(matrix(nrow=42, ncol=5))
colnames(FemaleResults)= c("Year",  "SYReturn", "seSYReturn", "ASYReturn", "seASYReturn")
FemaleResults$Year<- seq(1975,2016, 1)

FemaleResults[,2:3]<- AvResults_female[SYPIMs,2:3]
FemaleResults[,4:5]<- AvResults_female[ASYPIMs,2:3]


library(ggplot2)
library(dplyr)


#There doesn't seem to be any real difference over the course of time. I'm going
#to go with this is an unproductive avenue. Unless of course our survival was
#always too low for sustainability
ggplot(FemaleResults )+
  geom_point(aes(y=SYReturn, x=Year+0.1), color="brown")+
  geom_segment(aes(x=Year+0.1, xend=Year+0.1, y=SYReturn+seSYReturn, yend=SYReturn-seSYReturn), color="brown")+
  geom_point(aes(y=ASYReturn, x=Year-0.1), color="blue")+
  geom_segment(aes(x=Year-0.1, xend=Year-0.1, y=ASYReturn+seASYReturn, yend=ASYReturn-seASYReturn), color="blue")+
  xlab("Year")+
  ylab("Apparent Survival")+
  #ylim(0,1)+
  geom_vline(xintercept=1996)+
  ggthemes::theme_few(base_size = 18)+
  geom_smooth(method="lm", aes(y=SYReturn, x=Year), data=FemaleResults%>% filter(Year<1996 & seSYReturn<0.25), color="brown")+
  geom_smooth(method="lm", aes(y=SYReturn, x=Year), data=FemaleResults%>% filter(Year>=1996 & seSYReturn<0.25), color="brown")+
  geom_smooth(method="lm", aes(y=ASYReturn, x=Year), data=FemaleResults%>% filter(Year<1996 & seASYReturn<0.25), color="blue")+
  geom_smooth(method="lm", aes(y=ASYReturn, x=Year), data=FemaleResults%>% filter(Year>=1996 & seASYReturn<0.25), color="blue")
  



#I'm just going to plot these survival estimates agains box-occupancy and
#TerrOcc size quickly to make sure that nothing seems to be there but I"m
#pretty sure theres nothing


boxOcc <- read.csv("~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/Box Occupancy using nestdata renest function.csv")


FemaleResults$TerrOcc <-boxOcc$BirdTotal[1:42] 
FemaleResults$BoxOcc <- boxOcc$BoxOccTotal[1:42]
FemaleResults$TerrOcc <- boxOcc$TerrOccTotal[1:42]





ggplot(FemaleResults )+
  geom_point(aes(y=SYReturn, x=TerrOcc+0.1), color="brown")+
  geom_segment(aes(x=TerrOcc+0.1, xend=TerrOcc+0.1, y=SYReturn+seSYReturn, yend=SYReturn-seSYReturn), color="brown")+
  geom_point(aes(y=ASYReturn, x=TerrOcc-0.1), color="blue")+
  geom_segment(aes(x=TerrOcc-0.1, xend=TerrOcc-0.1, y=ASYReturn+seASYReturn, yend=ASYReturn-seASYReturn), color="blue")+
  xlab("TerrOcc")+
  ylab("Apparent Survival")+
  #ylim(0,1)+
  ggthemes::theme_few(base_size = 18)+
  geom_smooth(method="lm", aes(y=SYReturn, x=TerrOcc), data=FemaleResults%>% filter( seSYReturn<0.25), color="brown")+
  geom_smooth(method="lm", aes(y=ASYReturn, x=TerrOcc), data=FemaleResults%>% filter(seASYReturn<0.25), color="blue")
#Flat across for both so really doesn't look promising for TerrOcc being driven by survival


ggplot(FemaleResults )+
  geom_point(aes(y=SYReturn, x=BoxOcc+0.1), color="brown")+
  geom_segment(aes(x=BoxOcc+0.1, xend=BoxOcc+0.1, y=SYReturn+seSYReturn, yend=SYReturn-seSYReturn), color="brown")+
  geom_point(aes(y=ASYReturn, x=BoxOcc-0.1), color="blue")+
  geom_segment(aes(x=BoxOcc-0.1, xend=BoxOcc-0.1, y=ASYReturn+seASYReturn, yend=ASYReturn-seASYReturn), color="blue")+
  xlab("Box Occupancy")+
  ylab("Apparent Survival")+
  #ylim(0,1)+
  ggthemes::theme_few(base_size = 18)+
  geom_smooth(method="lm", aes(y=SYReturn, x=BoxOcc), data=FemaleResults%>% filter( seSYReturn<0.25), color="brown")+
  geom_smooth(method="lm", aes(y=ASYReturn, x=BoxOcc), data=FemaleResults%>% filter(seASYReturn<0.25), color="blue")

ggplot(FemaleResults )+
  geom_point(aes(y=SYReturn, x=TerrOcc+0.1), color="brown")+
  geom_segment(aes(x=TerrOcc+0.1, xend=TerrOcc+0.1, y=SYReturn+seSYReturn, yend=SYReturn-seSYReturn), color="brown")+
  geom_point(aes(y=ASYReturn, x=TerrOcc-0.1), color="blue")+
  geom_segment(aes(x=TerrOcc-0.1, xend=TerrOcc-0.1, y=ASYReturn+seASYReturn, yend=ASYReturn-seASYReturn), color="blue")+
  xlab("Territory Occupancy")+
  ylab("Apparent Survival")+
  #ylim(0,1)+
  ggthemes::theme_few(base_size = 18)+
  geom_smooth(method="lm", aes(y=SYReturn, x=TerrOcc), data=FemaleResults%>% filter( seSYReturn<0.25), color="brown")+
  geom_smooth(method="lm", aes(y=ASYReturn, x=TerrOcc), data=FemaleResults%>% filter(seASYReturn<0.25), color="blue")


#It really doesn't look like adult survival, of either age class is declinging.
#Appears pretty steady across time although variable year to year.






#Make a plot for just the discrete results


DiscreteResults <- as.data.frame(matrix(ncol=6, nrow=4))
colnames(DiscreteResults)<- c("TimePeriod", "Age", "Estimate", "SE", "LowCI", "HighCI")
DiscreteResults$TimePeriod<- c(rep("Increasing",2), rep("Declining",2))
DiscreteResults$Age <- c( "SY", "ASY", "SY", "ASY")
DiscreteResults[1,3:4]<- FemaleResults[1,2:3]
DiscreteResults[2,3:4]<- FemaleResults[2,4:5]
DiscreteResults[3,3:4]<- FemaleResults[22,2:3]
DiscreteResults[4,3:4]<- FemaleResults[22,4:5]


DiscreteResults$TimePeriod<- factor(DiscreteResults$TimePeriod, levels=c("Increasing", "Declining"))
DiscreteResults$Age <- factor(DiscreteResults$Age, levels=c("HY", "SY", "ASY"))
AllResults <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis/Model Averaged Yearly Apparent Survival for all records to get recruitment.csv", as.is=T)

AllResults$TimePeriod <- NA
AllResults$TimePeriod[which(AllResults$Year<=1996)]<- "Declining"
AllResults$TimePeriod[which(AllResults$Year>1996)]<- "Increasing"

ggplot(data=DiscreteResults)+
  geom_segment(aes(x=TimePeriod, xend=TimePeriod, y=Estimate-SE, yend=Estimate+SE))+
  geom_point(aes(x=TimePeriod, color=Age, y=Estimate), size=3)+
  #geom_jitter(data=AllResults, aes(x=TimePeriod, y=SYReturn), width=0.1, color="blue")+
  #geom_jitter(data=AllResults, aes(x=TimePeriod, y=ASYReturn), width=0.1, color="red")+
  ylab("Survival")+
  ggthemes::theme_few()
