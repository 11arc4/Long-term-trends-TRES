#Regression analysis of survival estimates
library(tidyverse)
library(betareg)
library(MuMIn)
#Matt Guzzo suggested pulling the MARK estimates of survival out and using those
#as data points in a regression analysis. Then we can follow a similar analysis
#as the fledging analysis.

Survival <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/Yearly Survival Estimates.csv", na.strings="", as.is=T)


Survival$TimePeriod <- NA
Survival$TimePeriod[which(Survival$Year<1992)]<- "Growing"
Survival$TimePeriod[which(Survival$Year>1991)]<- "Declining"
#Only have 1 year post decline so let's definitel not group it off seperately!
#Survival$TimePeriod[which(Survival$Year>2014)]<- "PostDecline"
Survival$TimePeriod <- factor(Survival$TimePeriod, levels=c("Growing", "Declining"))
Survival$Age <- factor(Survival$Age)


#Remove the years and specific estimates that aren't any good (not enough birds
#went into them, or we are at a boundary), or there is no environmental data
Survival2 <- Survival %>% filter( Estimate<.9 & ((SE<0.2 & Age!="Recruit" ) |(SE<0.1 & Age=="Recruit")) & Year!=2016 & Year !=2011 )

ggplot(Survival2, aes(x=Year, y=Estimate, color=Age))+
  geom_segment(aes(x=Year, xend=Year, y=Estimate-SE, yend=Estimate+SE, color=Age), alpha=0.6)+
  geom_point( )+
  labs(x="Year", y="Apparent Survival" )+
  ggthemes::theme_few()+
  geom_smooth(method="lm", formula=y~x, aes(group=TimePeriod))+
  facet_grid(~Age)
#Damn that recruitment looks a hell of a lot like the curve the population is showing......


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
plot(bmod) #We have 1 poits with high leverage. Might want to run with and without
plot(bmod, which = 5, type = "deviance", sub.caption = "")
plot(bmod, which = 1, type = "deviance", sub.caption = "")
plot(resid(bmod)~Survival2$Year2)
plot(resid(bmod)~Survival2$Age)
plot(resid(bmod)~Survival2$TimePeriod)
#Holy shit this looks SOOOOOO much better. I think this actually fits. Amazing
#what using the proper assumptions does!

Survival2$Year[which(cooks.distance(bmod)>0.1)]
Survival2$Age[which(cooks.distance(bmod)>0.1)]



#Which link function should we use?
bmod1 <- betareg(Estimate ~ Year2*Age*TimePeriod, data=Survival2, link="logit")
bmod2 <- betareg(Estimate ~ Year2*Age*TimePeriod, data=Survival2, link="loglog")
bmod3 <- betareg(Estimate ~ Year2*Age*TimePeriod, data=Survival2, link="probit")
bmod4 <- betareg(Estimate ~ Year2*Age*TimePeriod, data=Survival2, link="cloglog")
#bmod5 <- betareg(Estimate ~ Year2*Age*TimePeriod, data=Survival2, link="cauchit")
#bmod6 <- betareg(Estimate ~ Year2*Age*TimePeriod, data=Survival2, link="log")


AICc(bmod1, bmod2, bmod3, bmod4) #Bmod 2 is the best. 
summary(bmod1)$pseudo.r.squared
summary(bmod2)$pseudo.r.squared #LIKE WAYYYYYYYY better. We will use this one. 
summary(bmod3)$pseudo.r.squared #this is a close second but not as good as bmod2-- looks like we prefer the loglog link
summary(bmod4)$pseudo.r.squared


options(na.action = "na.fail")
dredge(bmod) #Full model is easily the best model. 


#Need to test significance of 3 way interaction
bmod2_2 <- betareg(Estimate ~ Year2*Age+ Age*TimePeriod+ TimePeriod*Year2, data=Survival2, link="loglog")

lmtest::lrtest(bmod2, bmod2_2)



bmam <-  betareg(Estimate ~ Year2*Age*TimePeriod, data=Survival2, link="loglog")
summary(bmam)

newdata <- data.frame(Year2=rep(seq(0, 4.2, 0.1), 3),
                      Year=rep(seq(1975, 2017, 1), 3),
                      TimePeriod=c(rep("Growing", 17), rep("Declining", 26)),
                      Age = c(rep("Recruit", 43), rep("SYReturn", 43), rep("ASYReturn", 43)), 
                      predicted=NA, 
                      se=NA,
                      ucl=NA, 
                      lcl=NA)

newdata$predicted <- predict(bmam, newdata, type="response")
newdata$variance <- predict(bmam, newdata, type="variance")
newdata$Age <- factor(newdata$Age, levels=c("Recruit", "SYReturn", "ASYReturn"))
ggplot()+
  geom_line(data=newdata, aes(x=Year, y=predicted, group=TimePeriod), size=1)+
  #geom_ribbon(aes(x=Year, ymin=predicted-variance, ymax=predicted+variance, group=TimePeriod), alpha=0.4, data=newdata)+
  #geom_segment(data=Survival2, aes(x=Year, xend=Year, y=Estimate-SE, yend=Estimate+SE) )+
  geom_point(data=Survival2, aes(x=Year, y=Estimate, color=Age), show.legend = F)+
  labs(y="Apparent Survival")+
  ggthemes::theme_few(base_size = 16)+
  geom_vline(xintercept = 1991)+
  facet_grid(~Age)



#Presentation quality
ggplot()+
  geom_line(data=newdata %>% filter(TimePeriod=="Declining" & Age=="Recruit"), aes(x=Year, y=predicted), size=1)+
  geom_line(data=newdata %>% filter(TimePeriod=="Growing" & Age=="Recruit"), aes(x=Year, y=predicted), size=1)+
  geom_point(data=Survival2 %>% filter(Age=="Recruit"), aes(x=Year, y=Estimate), show.legend = F)+
  labs(y="Juvenile \nSurvival")+
  ggthemes::theme_few(base_size = 16)+
  geom_vline(xintercept=1991)+
  theme(text = element_text(size=20), axis.title.y = element_text(angle=0, vjust=0.5))


#The SE on these points is process variance not sampling variance, so I think it
#would be ok to use means as points for the population mean survival that year.
#Some animals will be above or below but overall should be good. Kid et al 2014
#(estrogen paper with Mike Rennie that Matt Guzzo recommended) treats estimates
#of abundance from MARK as point abundances without SE.

#looks like Recruitment increased during the time period the population was
#growing but tanked when the population started to decline. This looks so
#similar to what happened to box occupancy there is just no way this isn't
#causing it. There isn't really mcuh going on with SY return or ASY return-- they aren't changing significantly through time. 
summary(bmam)











#This hasn't been corrected for the new time periods yet. 
#########################################################
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

#Winter ENSO? 
ggplot(Survival2, aes(x=WinterENSO, y=Estimate, color=Age))+
  geom_point()+
  facet_grid(~Age)+
  geom_smooth()

bmod_ENSO <- betareg(Estimate ~ WinterENSO*Age*TimePeriod, data=Survival2, link="loglog")
plot(bmod_ENSO) #Looks pretty good. Could remove 1 outliers
plot(resid(bmod_ENSO)~Survival2$Age)
plot(resid(bmod_ENSO)~Survival2$WinterENSO)
plot(resid(bmod_ENSO)~Survival2$TimePeriod)
#Looks fine

car::Anova(bmod_ENSO)
dredge(bmod_ENSO)
#Winter ENSO is doing something now but it looks like it probably only affects recrutiment! WOW
bmam_ENSO <- bmod_ENSO
summary(bmam_ENSO)

#Sugar acreage? 
ggplot(Survival2, aes(x=SugarAcreage, y=Estimate, color=Age))+
  geom_point()+
  facet_grid(TimePeriod~Age)+
  geom_smooth()
Survival2$SugarAcreage2 <- Survival2$SugarAcreage/10000

bmod_sugar <- betareg(Estimate ~ SugarAcreage2*Age*TimePeriod, data=Survival2, link="loglog")
plot(bmod_sugar) #Looks pretty good. Could remove 1 outlier....
plot(resid(bmod_sugar)~Survival2$Age)
plot(resid(bmod_sugar)~Survival2$SugarAcreage2)
plot(resid(bmod_sugar)~Survival2$TimePeriod)
#this fits well

car::Anova(bmod_sugar)
dredge(bmod_sugar)
#Sugar acreage makes it into the model. 
#Sugar also doens't make it into the top model
bmam_sugar<- betareg(Estimate ~ SugarAcreage2*Age+ Age*TimePeriod, data=Survival2, link="loglog")
summary(bmam_sugar)
#Sugar acreage looks like it has a positive effect for recruits, but little to no effect for SY and ASYs

 
#Days where mean temp was below 18 in the first 4 weeks after fledging. I wonder
#if the max temp is a better predictor (it is for nestling growth and all these
#other things) but unfortunently we don't have that long term data available
ggplot(Survival2, aes(x=DaysBelow18_mean, y=Estimate, color=Age))+
  geom_point()+
  facet_grid(TimePeriod~Age)+
  geom_smooth()
ggplot(Survival2, aes(x=DaysBelow18_max, y=Estimate, color=Age))+
  geom_point()+
  facet_grid(TimePeriod~Age)+
  geom_smooth()

bmod_mean <- betareg(Estimate ~ DaysBelow18_mean*Age*TimePeriod, data=Survival2, link="loglog")
plot(bmod_mean) 
plot(resid(bmod_mean)~Survival2$Age)
plot(resid(bmod_mean)~Survival2$DaysBelow18_mean)
plot(resid(bmod_mean)~Survival2$TimePeriod)


car::Anova(bmod_mean) #keep all
dredge(bmod_mean)
bmam_mean <- betareg(Estimate ~ DaysBelow18_mean*TimePeriod+ Age*TimePeriod, data=Survival2, link="loglog")
summary(bmam_mean)

#Based on max temp
bmod_max <- betareg(Estimate ~ DaysBelow18_max*Age*TimePeriod, data=Survival2, link="loglog")
plot(bmod_max) #There's one obvious outlier I should consider removing
plot(resid(bmod_max)~Survival2$Age)
plot(resid(bmod_max)~Survival2$DaysBelow18_mean)
plot(resid(bmod_max)~Survival2$TimePeriod)
#Fit's fine

car::Anova(bmod_max)
dredge(bmod_max)
bmam_max <- betareg(Estimate ~ DaysBelow18_max*TimePeriod +Age*TimePeriod, data=Survival2, link="loglog")
summary(bmam_max)

#Hurricanes
ggplot(Survival2, aes(x=Hurricanes, y=Estimate, color=Age))+
  geom_point()+
  facet_grid(TimePeriod~Age)+
  geom_smooth(method="lm")
bmod_hurricanes <- betareg(Estimate ~ Hurricanes*Age*TimePeriod, data=Survival2, link="loglog")
plot(bmod_hurricanes) #There's an outlier
plot(resid(bmod_hurricanes)~Survival2$Age)
plot(resid(bmod_hurricanes)~Survival2$Hurricanes)
plot(resid(bmod_hurricanes)~Survival2$TimePeriod)
#This fits fine

car::Anova(bmod_hurricanes)
dredge(bmod_hurricanes)

bmam_hurricanes <- betareg(Estimate ~ Hurricanes+Age*TimePeriod, data=Survival2, link="loglog")
summary(bmam_hurricanes)
#hurricanes aren't a great predictor and really shouldn't go into the model delta =0.89 and 2 null models are better. 

bnull <- betareg(Estimate ~ Age*TimePeriod, data=Survival2, link="loglog")

AICc(bmam_hurricanes, bmam_sugar, bmam_mean, bmam_max, bmam_ENSO, bnull)

#Days is easily the best model by a whole lot (based on max temp). INcluding ENSO and days below 18
#(mean) is the next best.



newdata2 <- data.frame(DaysBelow18_max=rep(seq(6, 20, 1), 6),
                       TimePeriod=rep(c(rep("Growing", 15), rep("Declining", 15)),3),
                       Age = c(rep("Recruit", 30), rep("SYReturn", 30), rep("ASYReturn", 30)), 
                       predicted=NA, 
                       variance=NA)
newdata2$predicted<- predict(bmam_max, newdata=newdata2, type="response")
newdata2$variance<- predict(bmam_max, newdata=newdata2, type="variance")

newdata2$TimePeriod <- factor(newdata2$TimePeriod, levels=c("Growing", "Declining"))

ggplot()+
  geom_line(data=newdata2  , aes(x=DaysBelow18_max, y=predicted, color=TimePeriod))+
  #geom_ribbon(data=newdata2, aes(x=DaysBelow18_max, ymin=predicted-variance, ymax=predicted+variance, fill=TimePeriod), alpha=0.4)+
  geom_point(data=Survival2 , aes(x=DaysBelow18_max, y=Estimate, color=TimePeriod))+
  facet_grid(Age~TimePeriod)+
  labs(x="Days below 18 \nduring peak breeding", y="Apparent Survival")+
  ggthemes::theme_few(base_size = 16)


#It really looks like survival used to be driven by conditions post fledging on
#the breeding ground, but now is driven by something else! What????


bmod_all <- betareg(Estimate ~ Hurricanes + WinterENSO*Age*TimePeriod + SugarAcreage2*Age + DaysBelow18_max*Age*TimePeriod, data=Survival2, link="loglog")
plot(bmod_all) 
plot(resid(bmod_all)~Survival2$Age)
plot(resid(bmod_all)~Survival2$Hurricanes)
plot(resid(bmod_all)~Survival2$TimePeriod)
plot(resid(bmod_all)~Survival2$WinterENSO)
plot(resid(bmod_all)~Survival2$SugarAcreage2)
plot(resid(bmod_all)~Survival2$DaysBelow18_mean)

car::Anova(bmod_all)
dredge(bmod_all)

bmam_all <- betareg(Estimate ~ Age*TimePeriod + DaysBelow18_max*TimePeriod+ SugarAcreage2+ Hurricanes + , data=Survival2, link="loglog")
summary(bmam_all) #explains 58% of variation



#Putting both in together DOES improve out our predictive power. weather on the
#breeding ground and ENSo are probably driving things during the growing period,
#and surgar acreage during the decline. That's what it looks like at least. 

plot(Survival2$SugarAcreage ~ Survival2$Year) #this has a trend
plot(Survival2$DaysBelow18_max ~ Survival2$Year) #this explains variation


AICc(bmam_hurricanes, bmam_sugar, bmam_mean,bmam_max, bmam_ENSO, bnull, bmam_all, bmam)
#YESSSS our weather predictors are now better than simply year.


newdata_days <- data.frame(Age=c(rep("Recruit", 30), rep("SYReturn", 30), rep("ASYReturn", 30)),
                           TimePeriod=rep(c(rep("Growing", 15), rep("Declining", 15)), 3), 
                           DaysBelow18_max=rep(seq(min(Survival2$DaysBelow18_max), max(Survival2$DaysBelow18_max), length.out = 15), 6 ), 
                           SugarAcreage2 = mean(Survival2$SugarAcreage2), 
                           Predicted=NA, 
                           Variance=NA
                           )

newdata_days$Predicted<- predict(bmam_all, newdata=newdata_days, type="response")
newdata_days$Variance<- predict(bmam_all, newdata=newdata_days, type="variance")



ggplot()+
  geom_line(data=newdata_days, aes(x=DaysBelow18_max, y=Predicted, color=TimePeriod))+
  geom_point(data=Survival2, aes(x=DaysBelow18_max, y=Estimate, color=TimePeriod))+
  facet_grid(~Age)+
  labs(y="Apparent Survival", x="Days of poor weather post fledging", color="Time Period")




newdata_sugar <- data.frame(Age=c(rep("Recruit", 30), rep("SYReturn", 30), rep("ASYReturn", 30)),
                           TimePeriod=rep(c(rep("Growing", 15), rep("Declining", 15)), 3), 
                           SugarAcreage2=rep(seq(min(Survival2$SugarAcreage2), max(Survival2$SugarAcreage2), length.out = 15), 6 ), 
                           DaysBelow18_max = mean(Survival2$DaysBelow18_max), 
                           Predicted=NA, 
                           Variance=NA
)

newdata_sugar$Predicted<- predict(bmam_all, newdata=newdata_sugar, type="response")
newdata_sugar$Variance<- predict(bmam_all, newdata=newdata_sugar, type="variance")


ggplot()+
  geom_line(data=newdata_sugar, aes(x=SugarAcreage2*10000, y=Predicted, color=TimePeriod))+
  geom_point(data=Survival2, aes(x=SugarAcreage2*10000, y=Estimate, color=TimePeriod))+
  facet_grid(~Age)+
  labs(y="Apparent Survival", x="Acreage of of sugar cane", color="Time Period")
