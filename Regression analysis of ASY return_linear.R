#Linear regression analysis of ASY survival with environmental covariates. 

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
Survival2 <- Survival %>% filter( Estimate<.9 & Age=="ASYReturn" & SE<0.2  & Year!=2016 & Year !=2011 )

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


Survival2$ASYReturn <- Survival2$Estimate*100 #Swap to percentages

##############Does sugar acreage predict ASY return? 
mod <- lm(ASYReturn~TimePeriod*SugarAcreage2, data=Survival2, na.action="na.fail")
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~Survival2$TimePeriod)
plot(resid(mod)~Survival2$SugarAcreage2)
#This looks OK

anova(mod)
dredge(mod)
#Nope not at all

##############Does hurricanes predict ASY return?
mod <- lm(ASYReturn~TimePeriod*Hurricanes, data=Survival2, na.action="na.fail")
plot(mod) #plot looks a bit shite but the shapiro.test and hist look OK for normality. 
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~Survival2$TimePeriod)
plot(resid(mod)~Survival2$Hurricanes)

dredge(mod)
anova(mod)
#Nope again not much!

##############Does winter ENSO predict ASY return? 
mod <- lm(ASYReturn~TimePeriod*WinterENSO, data=Survival2, na.action="na.fail")
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~Survival2$TimePeriod)
plot(resid(mod)~Survival2$SugarAcreage2)
anova(mod)
dredge(mod)
#nada

##############Does post fledging weather predict ASY return? 
mod <- lm(ASYReturn~TimePeriod*DaysAbove18_max, data=Survival2, na.action="na.fail")
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~Survival2$TimePeriod)
plot(resid(mod)~Survival2$SugarAcreage2)
anova(mod)
dredge(mod)
summary(mod)

#The only one that even kind of predicts anything. 
mam <- lm(ASYReturn~TimePeriod*DaysAbove18_max, data=Survival2, na.action="na.fail")
anova(mam)

PanelB <- ggplot(Survival2, aes(x=DaysAbove18_max, y=ASYReturn))+
  geom_smooth(method="lm", aes(linetype=TimePeriod, color=TimePeriod), show.legend = F, alpha=0.3)+
  
  geom_smooth(method="lm", aes(linetype=TimePeriod), color="black", se=F)+
  geom_point(aes(color=TimePeriod))+
  
  labs(x="Mean days of good \nweather post-fledging", y="Older-female \nsurvival (%)", color="", linetype="")+
  ggthemes::theme_few(base_size = 16, base_family = "serif")+
  scale_color_grey(start=0.2, end=0.6)+
  theme(legend.position = c(0.2, 0.2))
PanelB



#While the population was growing, ASY birds had higher survival when there were
#more days with good weather during the post fledging period.
#THis is EXtReMELY consistant with juvenile survival. 
