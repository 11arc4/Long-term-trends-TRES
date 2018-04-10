#Regression analysis of environmental causes of recruitment decline using simple linear regression

#this is based on my "Regression analysis of overwinter survival" where I did
#all 3 age classes. I thought that the first graph shows that really only
#recruitment has crashed, so perhaps interpretation would be easier if I only
#looked at the recruits in environmental models. They might well be affected
#differently.
library(MuMIn)
library(tidyverse)

############MAKE THE DATA
Survival <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/Yearly Survival Estimates.csv", na.strings="", as.is=T) %>% filter(Age=="Recruit")

Survival$TimePeriod <- NA
Survival$TimePeriod[which(Survival$Year<1992)]<- "Growing"
Survival$TimePeriod[which(Survival$Year>1991)]<- "Declining"
#Survival$TimePeriod[which(Survival$Year>2014)]<- "PostDecline"
#We don't have enough post decline data to really use this. 
Survival$TimePeriod <- factor(Survival$TimePeriod, levels=c("Growing", "Declining"))
Survival$Age <- factor(Survival$Age)

#Remove the years and specific estimates that aren't any good (not enough birds
#went into them, or we are at a boundary)
Survival2 <- Survival %>% filter( Estimate<.9 & Age=="Recruit" & SE<0.1 & Year!=2016 & Year !=2011 )

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

ggplot(Survival2, aes(x=Year, y=Estimate))+
  geom_point()+
  geom_point(data=Survival2 %>%filter(SE>0.1),aes(x=Year, y=Estimate), color="red" )+
  geom_vline(xintercept = 1991)+
  stat_smooth(method="lm", aes(group=TimePeriod))


#WIll simple linear regression fit our data here?

#Let's swap estimates into percentages.
Survival2$Recruitment<- Survival2$Estimate*100



#Does winter ENSO predict recruitment?
ggplot(Survival2, aes(x=WinterENSO, y=Recruitment, color=TimePeriod))+
  geom_point()+
  #geom_smooth(se=F)+
  stat_smooth(method="lm", aes(group=TimePeriod), se=F)
  #facet_grid(~TimePeriod)

mod_ENSO <- lm(Recruitment ~ WinterENSO*TimePeriod, data=Survival2)
plot(mod_ENSO)
hist(resid(mod_ENSO))
shapiro.test(resid(mod_ENSO))
plot(resid(mod_ENSO)~Survival2$WinterENSO)
plot(resid(mod_ENSO)~Survival2$TimePeriod)
#THis really looks like maybe it fits good! Residuals appear normal and there are no leverage points. 

summary(aov(mod_ENSO))
car::Anova(mod_ENSO)
dredge(mod_ENSO)

mam_ENSO <- lm(Recruitment ~ WinterENSO+TimePeriod, data=Survival2)
summary(mam_ENSO)

ggplot(Survival2, aes(x=WinterENSO, y=Recruitment))+
  geom_point()+
  #geom_smooth(se=F)+
  stat_smooth(method="lm", se=F)+
  facet_grid(~TimePeriod)


#Does sugar acreage predict recruitment?
ggplot(Survival2, aes(x=SugarAcreage2, y=Recruitment))+
  geom_point()+
  stat_smooth(method="lm", se=F)+
  facet_grid(~TimePeriod)
#This looks like it probably won't show anything but we will see. 

mod_sugar <- lm(Recruitment ~ SugarAcreage2*TimePeriod, data=Survival2)
plot(mod_sugar)
hist(resid(mod_sugar))
shapiro.test(resid(mod_sugar))
plot(resid(mod_sugar)~Survival2$TimePeriod)
plot(resid(mod_sugar)~Survival2$SugarAcreage2)
#This all looks ok. we aren't violating assumptions and appear to predict things pretty well. 

dredge(mod_sugar)
summary(aov(mod_sugar))
summary(mod_sugar)
car::Anova(mod_sugar)

#sugar acreage isn't a good predictor, but the best model with it in there is this
mam_sugar <- lm(Recruitment ~ SugarAcreage2 + TimePeriod, data=Survival2)
summary(mam_sugar)


#Do hurricanes predict recruitment?
ggplot(Survival2, aes(x=Hurricanes, y=Recruitment))+
  geom_point()+
  stat_smooth(method="lm", se=F)+
  facet_grid(~TimePeriod)

mod_hurricane<- lm(Recruitment ~ TimePeriod*Hurricanes, data=Survival2)
plot(mod_hurricane)
hist(resid(mod_hurricane))
shapiro.test(resid(mod_hurricane))
plot(resid(mod_hurricane)~Survival2$TimePeriod)
plot(resid(mod_hurricane)~Survival2$Hurricanes)

dredge(mod_hurricane)
summary(aov(mod_hurricane))
car::Anova(mod_hurricane)

#hurricanes are shitty predictors. Time period alone is better
mam_hurricanes <- lm(Recruitment ~ TimePeriod+Hurricanes, data=Survival2)
summary(mam_hurricanes)

#Does days where the max temp stays below 18 or it rains post fledging predict recruitment?
ggplot(Survival2, aes(x=DaysBelow18_max, y=Recruitment))+
  geom_point()+
  stat_smooth(method="lm", se=F)+
  facet_grid(~TimePeriod)
#Probably not going to really predict much. 

mod_days <- lm(Recruitment~DaysBelow18_max*TimePeriod, data=Survival2)
plot(mod_days)
hist(resid(mod_days))
shapiro.test(resid(mod_days))
plot(resid(mod_days)~Survival2$DaysBelow18_max)
plot(resid(mod_days)~Survival2$TimePeriod)
#All looks good

dredge(mod_days)
summary(aov(mod_days))
car::Anova(mod_days)
anova(mod_days)

mam_days <- lm(Recruitment~DaysBelow18_max+TimePeriod, data=Survival2)


#

mam_null <- lm(Recruitment~TimePeriod, data=Survival2)
AICc(mam_ENSO, mam_sugar, mam_hurricanes, mam_days, mam_null)



#What about if we combined all the best bits?
mod_all <- lm(Recruitment~TimePeriod+WinterENSO+SugarAcreage2+Hurricanes+DaysBelow18_max, data=Survival2)
plot(mod_all)
hist(resid(mod_all))
shapiro.test(resid(mod_all))
plot(resid(mod_all)~Survival2$TimePeriod)
plot(resid(mod_all)~Survival2$SugarAcreage2)
plot(resid(mod_all)~Survival2$Hurricanes)
plot(resid(mod_all)~Survival2$WinterENSO)
plot(resid(mod_all)~Survival2$DaysBelow18_max)

dredge(mod_all)
summary(aov(mod_all))
car::Anova(mod_all)
summary(mod_all)

mam_all <- lm(Recruitment~TimePeriod+SugarAcreage2+Hurricanes, data=Survival2)
summary(mam_all)

AICc(mam_all)


ggplot(Survival2, aes(x=SugarAcreage2, y=Recruitment, color=Hurricanes))+
  geom_point()+
  stat_smooth(method="lm", se=F)+
  facet_grid(~TimePeriod)+
  ggthemes::theme_few()



library(car)
scatter3d(formula= Recruitment ~SugarAcreage2 + Hurricanes, data=Survival2 %>% filter(TimePeriod=="Declining") )

scatter3d(formula= Recruitment ~SugarAcreage2 + Hurricanes|TimePeriod, data=Survival2  )


scatter3d(formula= Recruitment ~SugarAcreage2 + Hurricanes, data=Survival2 %>% filter(TimePeriod=="Growing") )



