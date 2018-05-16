#Linear regression analysis of SY survival with environmental covariates. 

library(MuMIn)
library(tidyverse)

############MAKE THE DATA
Survival <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/Yearly Survival Estimates.csv", na.strings="", as.is=T)

Survival$TimePeriod <- NA
Survival$TimePeriod[which(Survival$Year<1992)]<- "Growing"
Survival$TimePeriod[which(Survival$Year>1991)]<- "Declining"
#Survival$TimePeriod[which(Survival$Year>2014)]<- "PostDecline"
#We don't have enough post decline data to really use this. 
Survival$TimePeriod <- factor(Survival$TimePeriod, levels=c("Growing", "Declining"))
Survival$Age <- factor(Survival$Age)

#Remove the years and specific estimates that aren't any good (not enough birds
#went into them, or we are at a boundary)
Survival2 <- Survival %>% filter( Estimate<.9 & Age=="SYReturn" & SE<0.2  & Year!=2016 & Year !=2011 )

Survival2$Year2 <- (Survival2$Year-1975 )/10

#Do any of the environmental varaibles that we have data for predict survival well? 
#Load in all the environmental data
#weather on the breeding ground
YearlyFledge <- readRDS("~/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis/LocalWeather")
YearlyFledge <- YearlyFledge %>% arrange(Year)
#acreage sugar cane
presugar <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Sugar Cane Acreage Available USDA survey.csv", as.is=T, na.strings =c(""))
sugar <- presugar[,c(2, 19, 21, 23,25)]
colnames(sugar) <- c("year", "acreCaneSeed", "tonsCaneSeed", "acreCaneSeedSugar", "acreCaneSugar")
rm(presugar)
sugar$acreCaneSeed[which(sugar$year>1974 & sugar$year<1997)]/10000
sugar$acreCaneSeed[which(sugar$year>1996)]/10000
sugar$acreCaneSeed <- as.numeric(gsub(',', '', sugar$acreCaneSeed))
#ENSO
ENSOdat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Monthly El Nino Southern Oscillation index.csv", as.is=T)[,1:13]
colnames(ENSOdat) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
ENSOdat<- ENSOdat %>% arrange(Year)
for(i in 2:nrow(ENSOdat)){
  ENSOdat$ENSOWinter[i] <-  mean(c(ENSOdat$Mar[i-1], ENSOdat$Dec[i-1], ENSOdat$Jan[i], ENSOdat$Feb[i]), na.rm=T)
}
#Hurricanes
hurricane <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Hurricanes.csv")

Survival2$WinterENSO <- NA
Survival2$DaysBelow18_mean <- NA
Survival2$DaysBelow18_max <- NA
Survival2$Hurricanes <- NA
Survival2$SugarAcreage <- NA

#Match all of these parameters of interest up with the survival estimates. 
for (i in 1:nrow(Survival2)){
  Year<- Survival2$Year[i]
  Survival2$SugarAcreage[i] <- sugar$acreCaneSeed[Year==sugar$year]
  Survival2$DaysBelow18_mean[i] <- YearlyFledge$DaysBelow18_mean [Year==YearlyFledge$Year]
  Survival2$DaysBelow18_max[i] <- YearlyFledge$DaysBelow18_max [Year==YearlyFledge$Year]
  Survival2$Hurricanes[i] <- hurricane$Hurricanes[Year==hurricane$Year]
  Survival2$WinterENSO[i] <- ENSOdat$ENSOWinter[Year==ENSOdat$Year]
}

Survival2$SugarAcreage2 <- Survival2$SugarAcreage/ 10000
Survival2$DaysAbove18_max <- 28-Survival2$DaysBelow18_max


Survival2$SYReturn <- Survival2$Estimate*100 #Swap to percentages


###########Does sugar acreage predict SY return?
mod <- lm(log(SYReturn)~TimePeriod*SugarAcreage2, data=Survival2, na.action="na.fail")
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~Survival2$TimePeriod)
plot(resid(mod)~Survival2$SugarAcreage2)
#Doesn't fit perfectly but it's a hell of a lot better so I'll live with it!
dredge(mod)
anova(mod)
#nothing

###########Does hurricane predict SY return?
mod <- lm(log(SYReturn)~TimePeriod*Hurricanes, data=Survival2, na.action="na.fail")
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~Survival2$TimePeriod)
plot(resid(mod)~Survival2$Hurricanes)
#Doesn't fit perfectly but it's a hell of a lot better so I'll live with it!
dredge(mod)
anova(mod)
#Slight suggestion hurricanes might be important, but overal not a thing

###########Does winter ENSO predict SY return?
mod <- lm(log(SYReturn)~TimePeriod*WinterENSO, data=Survival2, na.action="na.fail")
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~Survival2$TimePeriod)
plot(resid(mod)~Survival2$WinterENSO)
#Doesn't fit perfectly but it's a hell of a lot better so I'll live with it!
dredge(mod)
anova(mod)
#Winter ENSO is just barely not significant, and it's in the top dredged model. 
mam <- lm(log(SYReturn)~WinterENSO, data=Survival2, na.action="na.fail")
anova(mam)
newdata <- data.frame(WinterENSO=seq(min(Survival2$WinterENSO), max(Survival2$WinterENSO), length.out=30), 
                      predicted=NA, 
                      UCL=NA, 
                      LCL=NA)
newdata[2:4] <- predict(mam, newdata = newdata, interval = "conf")

PanelC <- ggplot()+
  geom_point(data=Survival2, aes(x=WinterENSO, y=SYReturn))+
  geom_ribbon(data=newdata, aes(x=WinterENSO, ymin=exp(UCL), ymax=exp(LCL)), alpha=0.3)+
  geom_line(data=newdata, aes(x=WinterENSO, y=exp(predicted)))+
  labs(x="Mean ENSO (Dec-Mar) \n ", y="1-yr-old-female \nsurvival (%)", color="")+
  ggthemes::theme_few(base_size = 16, base_family = "serif")
PanelC


###########Does post-fledging weather predict SY return?
mod <- lm(log(SYReturn)~TimePeriod*DaysAbove18_max, data=Survival2, na.action="na.fail")
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~Survival2$TimePeriod)
plot(resid(mod)~Survival2$DaysAbove18_max)
#Doesn't fit great but I don't know how to make it fit better. I think it's just wonky
dredge(mod)
anova(mod)
