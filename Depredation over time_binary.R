#How does predation rate (Y/N) change over the years? 
library(tidyverse)
library(MuMIn)
library(car)
#Read in the binary fledging data
dat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/Binary Fledge Success wo experimental nests.csv", as.is=T, na.strings = "" ) %>% filter(Daysabove18 <10)
dat$TimePeriod<-factor(dat$TimePeriod, levels=c("Growing", "Declining", "PostDecline"))
dat$TimePeriod<-factor(dat$TimePeriod, levels=c("PostDecline", "Declining", "Growing"))

#make a dataset containing only nests that weren't predated and nests that were
#predated. Remove all other causes of death because they obscure whether the
#nest would ultimately have been depredated
Pred <- dat %>% filter(!is.na(Daysabove18) & (Fledge2==1 | (FailureCause2=="PREDATION" & Fledge2==0))) 
#Want to use the same dataset here as later

#Make a column for whether they were depredated or not (yes=1). This is the
#opposite of fledge2, but is a better more intuitive way of plotting that I'm
#sure.
Pred$Depredated <- NA
Pred$Depredated[Pred$Fledge2==0]<- 1
Pred$Depredated[Pred$Fledge2==1]<- 0

Pred$TimePeriod <- factor(Pred$TimePeriod)


ggplot(Pred, aes(x=Year, y=Depredated))+
  #geom_point()+
  geom_smooth()+
  geom_vline(xintercept = c(1991, 2014))+
  stat_smooth(method="lm", formula=y~x, aes(group=TimePeriod))+
  geom_point(data=PredationSummary, aes(x=Year, y=RatioPred))

#This looks like there might actually be 3 change points, and 4 time periods for
#predation risk rather than just 3 time periods. Lets double check with some raw
#data points. nah it actually looks OK. I'm not super worried. It maybe doesn't
#fit perfectly, but it's not too bad to treat linearly.THere is a whole lost
#more variation in predation rates from 2005 on though. That might be a problem.
#We will see. I think the only reason the GAM is showing more curves is because
#it needs to go back down to deal with the low predation in the post decline
#period

PredationSummary <- Pred %>% group_by(Year, TimePeriod)%>%
  summarise(RatioPred= sum(Depredated==1)/length(Depredated), 
            TotalNests=length(Depredated)) %>% filter(TotalNests>25)

ggplot(PredationSummary, aes(x=Year, y=RatioPred))+
         geom_point()+
         #geom_smooth(se=F)+
  stat_smooth(method="lm", aes(group=TimePeriod), se=F)+
  geom_vline(xintercept = c(1991, 2014))+
  ylim(0, 0.5)
  
  


########################################
#Question: Are there more predated nests in years during the decline? 

mod_pred1 <- glm(Depredated ~ TimePeriod*Year2, family=binomial(link="logit"), data=Pred)
mod_pred2 <- glm(Depredated ~ TimePeriod*Year2, family=binomial(link="log"), data=Pred)
mod_pred3 <- glm(Depredated ~ TimePeriod*Year2, family=binomial(link="cauchit"), data=Pred)
mod_pred4 <- glm(Depredated ~ TimePeriod*Year2, family=binomial(link="probit"), data=Pred)
mod_pred5 <- glm(Depredated ~ TimePeriod*Year2, family=binomial(link="cloglog"), data=Pred)
AICc(mod_pred1, mod_pred2, mod_pred3, mod_pred4, mod_pred5) #Again we should use the cauchit link
summary(mod_pred1)$psuedo.r
library(DescTools)
PseudoR2(mod_pred1) 
PseudoR2(mod_pred2) 
PseudoR2(mod_pred3) 
PseudoR2(mod_pred4) 
PseudoR2(mod_pred5) 

Pred$TimePeriod <- factor(Pred$TimePeriod, levels=c("Growing", "Declining", "PostDecline"))
Pred$TimePeriod <- factor(Pred$TimePeriod, levels=c("Declining", "Growing", "PostDecline"))
Pred$TimePeriod <- factor(Pred$TimePeriod, levels=c("PostDecline", "Declining", "Growing"))

mod_pred <- glm(Depredated ~ Year2*TimePeriod, family=binomial(link="cauchit"), data=Pred)

plot(mod_pred)
plot(resid(mod_pred)~Pred$TimePeriod) #this isn't ideal
plot(resid(mod_pred)~Pred$Year2) #Think it's not so bad. Not perfect but not so bad

options(na.action="na.fail")
dredge(mod_pred)
summary(mod_pred) #these summary estimates will differ depending on what you're comparing and what level is the default. DOn't worry
summary(aov(mod_pred))



oddsRat <- exp(coef(mod_pred))
get.or.se <- function(model) {
  broom::tidy(model) %>% 
    mutate(or = exp(estimate),
           var.diag = diag(vcov(model)),
           or.se = sqrt(or^2 * var.diag)) %>%
    select(or.se) %>% unlist %>% unname
}

oddsRatSE <- get.or.se(mod_pred)


newdata <- data.frame(Year2=rep(seq(0, 4.2, 0.1)),
                      TimePeriod=c(rep("Growing", 17), rep("Declining", 23), rep("PostDecline", 3)),
                      predicted_logit=NA,
                      predicted=NA, 
                      se_logit=NA,
                      use=NA, 
                      lse=NA)


newdata$predicted_logit <- predict(mod_pred, newdata, se.fit = T)$fit
newdata$se_logit <- predict(mod_pred, newdata, se.fit = T)$se.fit

newdata$predicted <- arm::invlogit(newdata$predicted_logit)
newdata$use <- arm::invlogit(newdata$predicted_logit+ (newdata$se_logit))
newdata$lse <- arm::invlogit(newdata$predicted_logit- (newdata$se_logit))

ggplot(newdata, aes(y=predicted, x=Year2*10+1975, group=TimePeriod))+
  geom_line()+
  geom_ribbon(aes(ymin=lse, ymax=use),alpha=0.2)+
  #ylim(0, 1)+
  labs(y="Predation Rate", x="Year")+
  geom_vline(xintercept = c(1991, 2014 ))+
  geom_vline(xintercept = 1998, color="darkgreen", size=2)+ #In 1998, ratsnakes got  listed. 
  ggthemes::theme_few(base_size = 16)
  #geom_point(data=PredationSummary, aes(x=Year, y=RatioPred))

#Predation rate was high and growing during the time period while the population
#was crashing. I feel pretty good with this model. It seems to match the data pretty well. 



#Question 2: Is predation somehow related to weather? I'm not really expecting
#it to be at the binary level, but perhaps.

ggplot(Pred, aes(x=Daysabove18, y=Depredated))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(~TimePeriod)





mod_preddays <-glm(Depredated ~ Daysabove18*TimePeriod, family=binomial(link="cauchit"), data=Pred)
plot(mod_pred)
plot(resid(mod_pred)~Pred$TimePeriod) #Not great but not really worse than before.  
plot(resid(mod_pred)~Pred$Daysabove18)  #Looks ok

options(na.action="na.fail")
dredge(mod_preddays) #this one says that we cant rule out that there was no effecct of days, but the null model is slightly better
summary(aov(mod_preddays)) #this says we shoudl only keep time period. 
car::Anova(mod_preddays)
mam_preddays <- glm(Depredated ~ TimePeriod+Daysabove18, family=binomial(link="cauchit"), data=Pred)


summary(mam_preddays)


oddsRat_days <- exp(coef(mam_preddays))
oddsRatSE_days <- get.or.se(mam_preddays)

newdata_days <- data.frame(Daysabove18=rep(seq(0, 9, 1),3),
                           TimePeriod=c(rep("Growing", 10), rep("Declining", 10), rep("PostDecline", 10)),
                           predicted_logit=NA,
                           predicted=NA, 
                           se_logit=NA,
                           use=NA, 
                           lse=NA)


newdata_days$predicted_logit <- predict(mam_preddays, newdata_days, se.fit = T)$fit
newdata_days$se_logit <- predict(mam_preddays, newdata_days, se.fit = T)$se.fit

newdata_days$predicted <- arm::invlogit(newdata_days$predicted_logit)
newdata_days$use <- arm::invlogit(newdata_days$predicted_logit+ (newdata_days$se_logit))
newdata_days$lse <- arm::invlogit(newdata_days$predicted_logit- (newdata_days$se_logit))

ggplot(newdata_days, aes(x=Daysabove18, y=predicted, fill=TimePeriod))+
  geom_line(aes(color=TimePeriod), size=1)+
  geom_ribbon(aes(ymin=use, ymax=lse), alpha=0.4)+
  labs(x="Days of good weather", y="Predation rate", fill="Population \nStatus", color="Population \nStatus")+
  ggthemes::theme_few(base_size = 16)+
  #geom_point(data=YearSummary%>% filter(Nests>25), aes(x=MeanDaysabove18, y=RatioFledgeFail, color=TimePeriod))+
  facet_grid(~TimePeriod)

#Predation rates were much higher when the population was declining than when
#the population wasn't growing. Weather conditions don't really play any part. 

#Cosewic designates gray ratsnakes as threatened in april 1998-- which is right after we see the population start declining. 




YearSummary <- Pred %>% 
  group_by(Year2, TimePeriod) %>% 
  summarise(MeanHatchDate=mean(HatchDate), 
            MeanDaysabove18= mean(Daysabove18, na.rm=T), 
            Daysabove18_2 = NA,
            RatioFledgeFail= sum(Depredated>0)/length(Depredated),
            Year=NA, 
            Nests=length(Depredated)) 

YearSummary$Year <- YearSummary$Year2*10+1975



weather_pre <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Hartington IHD Weather Station Daily Data 1975 to  2017.csv", as.is=T)
weather <- weather_pre[26:nrow(weather_pre), c(1,2,6,8,10,12,14,16,18)]
rm(weather_pre)
colnames(weather) <- c("Date", "Year",  
                       "MaxTemp", "MinTemp", "MeanTemp", "HeatDegDays", "CoolDegDays", "TotRain", "TotPrecip") 

weather$JDate <- lubridate::yday(as.Date(weather$Date, format="%m/%d/%Y"))
weather$MeanTemp <- as.numeric(weather$MeanTemp)
weather$MaxTemp <- as.numeric(weather$MaxTemp)


for (i in 1:nrow(YearSummary)){
  YearSummary$Daysabove18_2[i]<- calculateDaysabove18(HatchDate = YearSummary$MeanHatchDate[i], Year=YearSummary$Year[i])
}




calculateDaysabove18 <- function(HatchDate, Year){
  if(is.na(HatchDate)){
    return(NA)
  }
  weatherPoik <- weather[which(Year==weather$Year & weather$JDate>=HatchDate & weather$JDate<(HatchDate+9)),c(3, 8)]
  if (anyNA(weatherPoik)){
    #if we are missing data for any of the days, then we'll just return NA
    return(NA)
  } else {
    #how many of those days were warm enough for insects, and not raining (more than 1mm)
    return(length(which(weatherPoik$MaxTemp>18.5 & weatherPoik$TotRain==0)))
  }
}