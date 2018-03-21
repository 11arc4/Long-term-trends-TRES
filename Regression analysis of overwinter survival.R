#Regression analysis of survival estimates
library(tidyverse)
library(betareg)
library(MuMIn)
#Matt Guzzo suggested pulling the MARK estimates of survival out and using those as data points in a regression analysis. 
#Then we can follow a similar analysis as the fledging analysis. 

Survival <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/Yearly Survival Estimates.csv", na.strings="", as.is=T)

Survival$TimePeriod <- NA
Survival$TimePeriod[which(Survival$Year<1997)]<- "Growing"
Survival$TimePeriod[which(Survival$Year>1996)]<- "Declining"
#Survival$TimePeriod[which(Survival$Year>2013)]<- "PostDecline"
Survival$TimePeriod <- factor(Survival$TimePeriod, levels=c("Growing", "Declining"))

Survival$Age <- factor(Survival$Age)

#Remove the years and specific estimates that aren't any good (not enough birds
#went into them, or we are at a boundary)
Survival2 <- Survival %>% filter(Year !=1975 & Year!=1976 & Year!=2016 & Estimate<.9 & SE<0.2)

ggplot(Survival2, aes(x=Year, y=Estimate, color=Age))+
  #geom_segment(aes(x=Year, xend=Year, y=Estimate-SE, yend=Estimate+SE, color=Age), alpha=0.6)+
  geom_point( )+
  labs(x="Year", y="Apparent Survival" )+
  ggthemes::theme_few()+
  #geom_smooth()+
  geom_smooth(data=Survival2 %>% filter(TimePeriod=="Growing"), aes(x=Year, y=Estimate, color=Age),method="lm")+
  geom_smooth(data=Survival2 %>% filter(TimePeriod=="Declining"), aes(x=Year, y=Estimate, color=Age),method="lm")+
  geom_smooth(data=Survival2 %>% filter(TimePeriod=="PostDecline"), aes(x=Year, y=Estimate, color=Age),method="lm")+
  facet_grid(~Age)


Survival2$Year2 <- (Survival2$Year-1975 )/10

#############################
#Has survival declined? 
mod <- lm(Estimate ~ Year2*TimePeriod*Age, data=Survival2)
plot(mod)#Normality might be a bit of an issue......
hist(resid(mod)) 
shapiro.test(resid(mod))
plot(resid(mod)~Survival2$TimePeriod) #Good
plot(resid(mod)~Survival2$Age) #Much lower variation for recruit than for ASY and SY return BUT we aren't overestimating or underestimating anyone
plot(resid(mod)~Survival2$Year2) #Fine

#Lack of normality is an issue

ggplot(Survival2, aes(Estimate))+
  geom_histogram()+
  facet_grid(Age~.)
#Probably beta distributed. 


fitdistrplus::descdist(Survival2$Estimate) #Maybe it's a beta distribution


bmod <- betareg(Estimate ~ Year2*Age*TimePeriod, data=Survival2, link="loglog") #We will keep the precision parameter the same for all-- no reason to expect otherwise
plot(bmod) #We have 3 poits with high leverage. Might want to run with and without
plot(bmod, which = 5, type = "deviance", sub.caption = "")
plot(bmod, which = 1, type = "deviance", sub.caption = "")
plot(resid(bmod)~Survival2$Year2)
plot(resid(bmod)~Survival2$Age)
plot(resid(bmod)~Survival2$TimePeriod)
#Holy shit this looks SOOOOOO much better. I think this actually fits. Amazing
#what using the proper assumptions does!



#Which link function should we use?
bmod1 <- betareg(Estimate ~ Year2*Age*TimePeriod, data=Survival2, link="logit")
bmod2 <- betareg(Estimate ~ Year2*Age*TimePeriod, data=Survival2, link="loglog")
bmod3 <- betareg(Estimate ~ Year2*Age*TimePeriod, data=Survival2, link="probit")
bmod4 <- betareg(Estimate ~ Year2*Age*TimePeriod, data=Survival2, link="cloglog")
#bmod5 <- betareg(Estimate ~ Year2*Age*TimePeriod, data=Survival2, link="cauchit")
bmod6 <- betareg(Estimate ~ Year2*Age*TimePeriod, data=Survival2, link="log")


AICc(bmod1, bmod2, bmod3, bmod4, bmod6) #all very similar in terms of AICc-- within 2
summary(bmod1)$pseudo.r.squared
summary(bmod2)$pseudo.r.squared #LIKE WAYYYYYYYY better. We will use this one. 
summary(bmod3)$pseudo.r.squared #this is a close second but not as good as bmod2-- looks like we prefer the loglog link
summary(bmod4)$pseudo.r.squared
summary(bmod6)$pseudo.r.squared


options(na.action = "na.fail")
dredge(bmod2)
car::Anova(bmod2)


newdata <- data.frame(Year2=rep(seq(0, 4.2, 0.1), 3),
                      Year=rep(seq(1975, 2017, 1), 3),
                      TimePeriod=rep(c(rep("Growing", 22), rep("Declining", 21)),3),
                      Age = c(rep("Recruit", 43), rep("SYReturn", 43), rep("ASYReturn", 43)), 
                      predicted=NA, 
                      se=NA,
                      ucl=NA, 
                      lcl=NA)

newdata$predicted <- predict(bmod2, newdata, type="response")
newdata$variance <- predict(bmod2, newdata, type="variance")

ggplot()+
  geom_line(data=newdata, aes(x=Year, y=predicted, color=Age), size=1)+
  geom_ribbon(aes(x=Year, ymin=predicted-variance, ymax=predicted+variance, fill=Age), alpha=0.4, data=newdata)+
  #geom_segment(data=Survival2, aes(x=Year, xend=Year, y=Estimate-SE, yend=Estimate+SE) )+
  geom_point(data=Survival2, aes(x=Year, y=Estimate, color=Age) )+
  labs(y="Predicted Survival Overwinter")+
  ggthemes::theme_few(base_size = 16)+
  facet_grid(~Age)

#The SE on these points is process variance not sampling variance, so I think it
#would be ok to use means as points for the population mean survival that year.
#Some animals will be above or below but overall should be good. Kid et al 2014
#(estrogen paper with Mike Rennie that Matt Guzzo recommended) treats estimates
#of abundance from MARK as point abundances without SE.


###############
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
sugar$
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
Survival2$DaysBelow18 <- NA
Survival2$Hurricanes <- NA
Survival2$SugarAcreage <- NA

#Match all of these parameters of interest up with the survival estimates. 
for (i in 1:nrow(Survival2)){
  Year<- Survival2$Year[i]
  Survival2$SugarAcreage[i] <- sugar$acreCaneSeed[Year==sugar$year]
  Survival2$DaysBelow18[i] <- YearlyFledge$DaysBelow18 [Year==YearlyFledge$Year]
  Survival2$Hurricanes[i] <- hurricane$Hurricanes[Year==hurricane$Year]
  Survival2$WinterENSO[i] <- ENSOdat$ENSOWinter[Year==ENSOdat$Year]
}

#Winter ENSO? 
ggplot(Survival2, aes(x=WinterENSO, y=Estimate, color=Age))+
  geom_point()+
  facet_grid(~Age)+
  geom_smooth()

bmod_ENSO <- betareg(Estimate ~ WinterENSO*Age*TimePeriod, data=Survival2, link="loglog")
plot(bmod_ENSO) #Looks pretty good. Could remove 2 outliers
plot(resid(bmod_ENSO)~Survival2$Age)
plot(resid(bmod_ENSO)~Survival2$WinterENSO)
plot(resid(bmod_ENSO)~Survival2$TimePeriod)
#Looks fine

car::Anova(bmod_ENSO)
dredge(bmod_ENSO)
#Basically WinterENSO doesn't do shit


#Sugar acreage? 
ggplot(Survival2, aes(x=SugarAcreage, y=Estimate, color=Age))+
  geom_point()+
  facet_grid(TimePeriod~Age)+
  geom_smooth()
Survival2$SugarAcreage2 <- Survival2$SugarAcreage/10000

bmod_sugar <- betareg(Estimate ~ SugarAcreage2*Age*TimePeriod, data=Survival2, link="loglog")
plot(bmod_sugar) #Looks pretty good. Could remove 2 outliers
plot(resid(bmod_sugar)~Survival2$Age)
plot(resid(bmod_sugar)~Survival2$SugarAcreage2)
plot(resid(bmod_sugar)~Survival2$TimePeriod)
#this fits well

car::Anova(bmod_sugar)
dredge(bmod_sugar)
#Sugar acreage makes it into the model. 
bmam_sugar<- betareg(Estimate ~ SugarAcreage* TimePeriod + Age*TimePeriod, data=Survival2, link="loglog")
summary(bmam_sugar)


#Days below 18
ggplot(Survival2, aes(x=DaysBelow18, y=Estimate, color=Age))+
  geom_point()+
  facet_grid(TimePeriod~Age)+
  geom_smooth()

bmod_days <- betareg(Estimate ~ DaysBelow18*Age*TimePeriod, data=Survival2, link="loglog")
plot(bmod_days) 
plot(resid(bmod_days)~Survival2$Age)
plot(resid(bmod_days)~Survival2$DaysBelow18)
plot(resid(bmod_days)~Survival2$TimePeriod)


car::Anova(bmod_days) #keep all
dredge(bmod_days)
#Keep all is good!!!
summary(bmod_days) #61% variation explained which is really quite good

newdata2 <- data.frame(DaysBelow18=rep(seq(0, 13, 1), 6),
                       TimePeriod=rep(c(rep("Growing", 14), rep("Declining", 14)),3),
                       Age = c(rep("Recruit", 28), rep("SYReturn", 28), rep("ASYReturn", 28)), 
                       predicted=NA, 
                       variance=NA)
newdata2$predicted<- predict(bmod_days, newdata=newdata2, type="response")
newdata2$variance<- predict(bmod_days, newdata=newdata2, type="variance")

newdata2$TimePeriod <- factor(newdata2$TimePeriod, levels=c("Growing", "Declining"))

ggplot()+
  geom_line(data=newdata2, aes(x=DaysBelow18, y=predicted, color=TimePeriod))+
  #geom_ribbon(data=newdata2, aes(x=DaysBelow18, ymin=predicted-variance, ymax=predicted+variance, fill=TimePeriod), alpha=0.4)+
  geom_point(data=Survival2, aes(x=DaysBelow18, y=Estimate, color=TimePeriod))+
  facet_grid(Age~TimePeriod)
#Looks like whent the population was growing cold weather on the breeding ground
#used to impact survival, but that's not really a thing anymore. SOmething else
#much be explaining it?


#Hurricanes
ggplot(Survival2, aes(x=Hurricanes, y=Estimate, color=Age))+
  geom_point()+
  facet_grid(~Age)+
  geom_smooth()
bmod_hurricanes <- betareg(Estimate ~ Hurricanes*Age*TimePeriod, data=Survival2, link="loglog")
plot(bmod_hurricanes) 
plot(resid(bmod_hurricanes)~Survival2$Age)
plot(resid(bmod_hurricanes)~Survival2$Hurricanes)
plot(resid(bmod_hurricanes)~Survival2$TimePeriod)
#This fits fine

car::Anova(bmod_hurricanes)
dredge(bmod_hurricanes)
#hurricanes aren't great