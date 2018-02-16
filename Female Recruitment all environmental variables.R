#Which of the different environmental covariates predicts survival the best? 
#If we include multiple, can we remove popStatus?
setwd("~/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis")

library(dplyr)
library(RMark)
library(ggplot2)
#Fledglings are trying to feed themselves, and mothers are trying to feed them
#as well for the first 2 weeks at least. Thats energetically demanding and might
#reduce survival

#Load in the data (already filtering out the males, and half of the nestlings for each year that we guess were male)
datMark <- readRDS("~/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis/Female Fledgling MARK Data.rda")

#Load in all the environmental data
YearlyFledge <- readRDS("LocalWeather")
YearlyFledge <- YearlyFledge %>% arrange(Year)

presugar <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Sugar Cane Acreage Available USDA survey.csv", as.is=T, na.strings =c(""))
sugar <- presugar[,c(2, 19, 21, 23,25)]
colnames(sugar) <- c("year", "acreCaneSeed", "tonsCaneSeed", "acreCaneSeedSugar", "acreCaneSugar")

sugar$acreCaneSeed[which(sugar$year>1974 & sugar$year<1997)]/10000
sugar$acreCaneSeed[which(sugar$year>1996)]/10000

sugar$acreCaneSeed <- as.numeric(gsub(',', '', sugar$acreCaneSeed))

ENSOdat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Monthly El Nino Southern Oscillation index.csv", as.is=T)[,1:13]
colnames(ENSOdat) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
ENSOdat<- ENSOdat %>% arrange(Year)


for(i in 2:nrow(ENSOdat)){
  
  ENSOdat$ENSOWinter[i] <-  mean(c(ENSOdat$Mar[i-1], ENSOdat$Dec[i-1], ENSOdat$Jan[i], ENSOdat$Feb[i]), na.rm=T)
  
}
hurricane <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Hurricanes.csv")


#process the data and make a design data table.

tsprocess <-process.data(datMark,model="CJS",
                         begin.time=1975, 
                         groups= ("age"),
                         initial.ages =c(0,1, 2))
ts.ddl<- make.design.data(tsprocess, parameters=list(Phi=list(age.bins=c(0, 0.8, 1.8, 44)),
                                                     p=list(age.bins=c(0, 0.8, 1.8, 44))))


#set up the population status variable
ts.ddl$Phi$popStatus<- rep(NA)
ts.ddl$Phi$popStatus[which(ts.ddl$Phi$Time<=21)]<- 0  #1975-1996 increased
ts.ddl$Phi$popStatus[which(ts.ddl$Phi$Time>21)]<- 1 #1997-2017 decreased

ts.ddl$Phi$Hurricanes<- rep(NA)
ts.ddl$Phi$ENSOWinter<- rep(NA)
ts.ddl$Phi$SugarAcres<- rep(NA)
ts.ddl$Phi$DaysBelow18<- rep(NA)


for (i in 1:nrow(ts.ddl$p)){
  ts.ddl$Phi$Hurricanes[i] <- as.numeric (hurricane$Hurricanes[which(hurricane$Year+1==ts.ddl$Phi$time[i])])
  ts.ddl$Phi$ENSOWinter[i] <- as.numeric (ENSOdat$ENSOWinter[which(ENSOdat$Year==ts.ddl$Phi$time[i])])
  ts.ddl$Phi$SugarAcres[i] <- as.numeric (sugar$acreCaneSeed[which(sugar$year+1==ts.ddl$Phi$time[i])])/10000
  ts.ddl$Phi$DaysBelow18[i] <- as.numeric (YearlyFledge$DaysBelow18[which(YearlyFledge$Year==ts.ddl$Phi$time[i])])
}


#Set up all the possible combos that you're interested in testing. 
p.timeage <- list(formula= ~time + age)

Phi.1 <- list(formula= ~ popStatus * age)

Phi.2 <- list(formula= ~ SugarAcres*age + Hurricanes*age + ENSOWinter*age + DaysBelow18*age) #this has very low support


Phi.3 <- list(formula= ~ SugarAcres*age*popStatus + Hurricanes*age*popStatus + ENSOWinter + DaysBelow18*age*popStatus)

Phi.4 <- list(formula= ~ SugarAcres*age*popStatus + Hurricanes*age*popStatus  + DaysBelow18*age*popStatus)
Phi.5 <- list(formula= ~ SugarAcres*age*popStatus +  ENSOWinter + DaysBelow18*age*popStatus)
Phi.9 <- list(formula= ~ SugarAcres*age*popStatus + Hurricanes*age*popStatus + ENSOWinter )
Phi.12 <- list(formula= ~ Hurricanes*age*popStatus + ENSOWinter + DaysBelow18*age*popStatus)



Phi.6 <- list(formula= ~ SugarAcres*age*popStatus +  ENSOWinter )
Phi.7 <- list(formula= ~ SugarAcres*age*popStatus + DaysBelow18*age*popStatus)
Phi.8 <- list(formula= ~ ENSOWinter + DaysBelow18*age*popStatus)
Phi.10 <- list(formula= ~ SugarAcres*age*popStatus + Hurricanes*age*popStatus )
Phi.11 <- list(formula= ~ Hurricanes*age*popStatus + ENSOWinter )
Phi.13 <- list(formula= ~ Hurricanes*age*popStatus  + DaysBelow18*age*popStatus)




Phi.14 <- list(formula= ~ Hurricanes*age*popStatus )
Phi.15 <- list(formula= ~  age*popStatus + ENSOWinter)
Phi.16 <- list(formula= ~  DaysBelow18*age*popStatus)
Phi.17 <- list(formula= ~ SugarAcres*age*popStatus)


cml<- create.model.list("CJS")
results <- mark.wrapper(cml, data=tsprocess, ddl=ts.ddl, output=F, adjust=F)

#lets find the chat to pick based on the global model and adjust
export.MARK(tsprocess, "FemaleNestlings_combinedfactors", results, replace=T)

adjustedresults <- adjust.chat(results, chat=1.16)

#I think that these results mean that sugar acreage and days below 18 have the
#biggest impact on survival but we can't rule out the impact of ENSO in
#particular but also hurricanes. ie they have a smallish impact but not huge


#I suspect that the reason that we can't get rid of popStatus is that condition
#during breeding have resulted in lower quality juveniles.



#Now we should pull out the estimates for survival and plot those, ideally with
#some indication of at least what sugar acreage and local weather was common
#that year
modAv<- model.average(adjustedresults)

global <- adjustedresults[11]$Phi.3.p.timeage
global$results$beta




#The PIMs look just like any other model. 

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
sugar <- sugar %>% filter(year>1974) %>% arrange(year)

AvResults <- data.frame(Year= rep(seq(1975, 2016, 1), 3), 
                        Age= c(rep("HY", 42), rep("SY", 42), rep("ASY", 42)),
                        Estimate= NA, 
                        SE=NA, 
                        SugarAcres=rep(sugar$acreCaneSeed[1:42], 3), 
                        DaysBelow18=rep(YearlyFledge$DaysBelow18[1:42], 3))

AvResults[1:42, 3:4] <- modAv[RecruitPIMS,][2:3]
AvResults[43:84, 3:4] <- modAv[SYPIMS,2:3]
AvResults[85:126, 3:4] <- modAv[ASYPIMS, 2:3]
AvResults$popStatus <- "Growing"
AvResults$popStatus[which(AvResults$Year>1996)] <- "Declining"


AvResults$Age <- factor(AvResults$Age, levels=c("HY", "SY", "ASY"))

ggplot(AvResults , aes(x=Year, y=Estimate, color=Age))+
  geom_segment(aes(x=Year, xend=Year, y=Estimate-1.96*SE, yend=Estimate+1.96*SE), color="black")+
  geom_point(size=3)+
  labs(x="Year", y="Survival", color="Age")+
  scale_color_manual(labels=c("Recruitment", "1-year-old return", "Older female return"), 
                     values=c("azure4","burlywood4", "dodgerblue4"))+
  ggthemes::theme_few(base_size = 16)


ggplot(AvResults , aes(x=DaysBelow18, y=Estimate, color=Age))+
  geom_segment(aes(x=DaysBelow18, xend=DaysBelow18, y=Estimate-1.96*SE, yend=Estimate+1.96*SE), color="black")+
  geom_point(size=3)+
  labs(x="Days below 18", y="Survival", color="Age")+
  scale_color_manual(labels=c("Recruitment", "1-year-old return", "Older female return"), 
                     values=c("azure4","burlywood4", "dodgerblue4"))+
  facet_grid(~popStatus)+
  ggthemes::theme_few(base_size = 16)

 
ggplot(AvResults , aes(x=SugarAcres, y=Estimate, color=Age))+
  geom_segment(aes(x=SugarAcres, xend=SugarAcres, y=Estimate-1.96*SE, yend=Estimate+1.96*SE), color="black")+
  geom_point(size=3)+
  labs(x="Acres sugar cane and seed", y="Survival", color="Age")+
  scale_color_manual(labels=c("Recruitment", "1-year-old return", "Older female return"), 
                     values=c("azure4","burlywood4", "dodgerblue4"))+
  facet_grid(~popStatus)+
  ggthemes::theme_few(base_size = 16)





Estimates <- global$results$beta[1:25,] 
Estimates$parameter <- rownames(Estimates)


ggplot(Estimates, aes(x=estimate, y=parameter))+
  geom_point()+
  geom_segment(aes(xend=lcl, x=ucl, yend=parameter))+
  labs(x="Beta with 95% CI (logit scale)", y="")+
  geom_vline(xintercept = 0)+
  ggthemes::theme_few(base_size = 16)
