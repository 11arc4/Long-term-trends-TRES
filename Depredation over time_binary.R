#How does predation rate (Y/N) change over the years? 
library(tidyverse)
library(MuMIn)
library(car)
get.or.se <- function(model) {
  broom::tidy(model) %>% 
    mutate(or = exp(estimate),
           var.diag = diag(vcov(model)),
           or.se = sqrt(or^2 * var.diag)) %>%
    select(or.se) %>% unlist %>% unname
}
#Read in the binary fledging data
dat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/Binary Fledge Success wo experimental nests.csv", as.is=T, na.strings = "" ) %>% filter(Daysabove18 <10)
dat$TimePeriod<-factor(dat$TimePeriod, levels=c("Growing", "Declining", "PostDecline"))
dat$TimePeriod<-factor(dat$TimePeriod, levels=c("PostDecline", "Declining", "Growing"))


#Make a column for whether they were depredated or not (yes=1). This is the
#opposite of fledge2, but is a better more intuitive way of plotting that I'm
#sure.
dat$Depredated <- NA
dat$Depredated[dat$Fledge2==0]<- 1
dat$Depredated[dat$Fledge2==1]<- 0


# #Are there any times where a nest looks like it fledged earlier than 16 days
# #old? Because those probably got predated and should be fixed.
# dat %>% filter(Fledge2==1 & (FledgeDate-HatchDate)<16 & FledgeDate>HatchDate)
# dat$Fledge2[]<- 0
# dat$Fledge[] <- 0 #I'm not sure how many would have fledged without checkin long term records. 
# dat$FailureCause[]<- "PREDATION"
# 
# #Alternatively are there any nests that look like they were predated after say
# #18 days? Because those probably force fledged!
# 
# dat %>% filter(Fledge2==0 & (FledgeDate-HatchDate)>16 & !is.na(FledgeDate) & !is.na(HatchDate) & FailureCause2=="PREDATION")
# dat$Fledge2[]<- 1
# dat$Fledge[] <- NA #I'm not sure how many would have fledged without checkin long term records. 
# dat$FailureCause[]<- NA


#make a dataset containing only nests that weren't predated and nests that were
#predated. Remove all other causes of death because they obscure whether the
#nest would ultimately have been depredated
Pred <- dat %>% filter(!is.na(Daysabove18) & (Fledge2==1 | (FailureCause2=="PREDATION" & Fledge2==0))) 
#Want to use the same dataset here as later







ggplot(Pred, aes(x=Year, y=Depredated))+
  #geom_point()+
  geom_smooth()+
  geom_vline(xintercept = c(1991, 2014))+
  stat_smooth(method="glm", formula=y~x, method.args=list(family=binomial(link="cauchit")), aes(group=TimePeriod))

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
  stat_smooth(method="glm", formula=y~x, method.args=list(family=binomial(link="cauchit")), aes(group=TimePeriod))+
  geom_vline(xintercept = c(1991, 2014))+
  ylim(0, 0.5)
  
  

#######################################
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
#Shouldn't report these F stats since it's a binomal GLM. 
summary(aov(mod_pred))
#Better to report the chi squared, which can come from either of these places
car::Anova(mod_pred)
anova(mod_pred, test="Chisq")

#OR and beta values for reporting. 
summary(mod_pred) #these summary estimates will differ depending on what you're comparing and what level is the default. DOn't worry
oddsRat <- exp(coef(mod_pred))
oddsRatSE <- get.or.se(mod_pred)



ggplot(Pred, aes(y=as.numeric(Depredated), x=Year2*10+1975))+
  labs(y="Predation Rate", x="Year")+
  stat_smooth(method="glm", method.args = list(family=binomial(link="cauchit")), aes(group=TimePeriod), color="black")+
  geom_vline(xintercept = c(1991, 2014), linetype="dashed")+
  #geom_vline(xintercept = 1998, color="darkgreen", size=2)+ #In 1998, ratsnakes got  listed. 
  ggthemes::theme_few(base_size = 16, base_family = "serif")
ggsave(filename='~/Masters Thesis Project/Long term trends paper/Plots for paper/Predation plot.jpeg', width=5, height=3, units="in", device="jpeg")
#Predation rate was high and growing during the time period while the population
#was crashing. I feel pretty good with this model. It seems to match the data pretty well. 



#Question 2: Is predation somehow related to weather? I'm not really expecting
#it to be at the binary level, but perhaps.

#We will look at weather conditions during the first 16 days of nestling
#development, and look at 2 cutoffs (15 degrees and 20 degrees based on Pat
#Weatherhead)

weather_pre <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Harington Weather Station Daily Data 1975 to  2017.csv", as.is=T)
weather <- weather_pre[26:nrow(weather_pre), c(1,2,6,8,10,12,14,16,18)]
rm(weather_pre)
colnames(weather) <- c("Date", "Year",  
                       "MaxTemp", "MinTemp", "MeanTemp", "HeatDegDays", "CoolDegDays", "TotRain", "TotPrecip") 

weather$JDate <- lubridate::yday(as.Date(weather$Date, format="%Y-%m-%d"))
weather$MeanTemp <- as.numeric(weather$MeanTemp)
weather$MaxTemp <- as.numeric(weather$MaxTemp)



calculatePredDays <- function(HatchDate, Year){
  if(is.na(HatchDate)){
    return(NA)
  }
  weatherYoung <- weather[which(Year==weather$Year & weather$JDate>=HatchDate & weather$JDate<(HatchDate+16)),c(3, 8)]
  if (anyNA(weatherYoung)){
    #if we are missing data for any of the days, then we'll just return NA
    return(NA)
  } else {
    #how many of those days were warm enough for insects, and not raining (more than 1mm)
    return(list("PredDays15"=length(which(weatherYoung$MaxTemp>15)), "PredDays20"=length(which(weatherYoung$MaxTemp>20))))
  }
}


for (i in 1:nrow(Pred)){
  PredDays <- calculatePredDays(HatchDate = Pred$HatchDate[i], Year=Pred$Year[i])
  Pred$PredDays15[i]<- PredDays[1][[1]]
  Pred$PredDays20[i]<- PredDays[2][[1]]
}

#It's because this month was in the dataset twice. I will divide by 2
Pred$PredDays15[which(Pred$PredDays15>16)] <- Pred$PredDays15[which(Pred$PredDays15>16)]/2
Pred$PredDays20[which(Pred$PredDays20>16)] <- Pred$PredDays20[which(Pred$PredDays20>16)]/2

Pred2 <- Pred %>% filter(!is.na(PredDays15))


ggplot(Pred, aes(x=PredDays15, y=Depredated))+
  geom_point()+
  geom_smooth(method="glm", method.args= list(family=binomial(link="cauchit")))+
  facet_grid(~TimePeriod)

ggplot(Pred, aes(x=PredDays20, y=Depredated))+
  geom_point()+
  geom_smooth(method="glm", method.args= list(family=binomial(link="cauchit")))+
  facet_grid(~TimePeriod)

#####Does days above 15 degrees during the predation periods make nestlings at risk of predation?
mod_preddays1 <-glm(Depredated ~ PredDays15*TimePeriod, family=binomial(link="log"), data=Pred2)
mod_preddays2 <-glm(Depredated ~ PredDays15*TimePeriod, family=binomial(link="logit"), data=Pred2)
mod_preddays3 <-glm(Depredated ~ PredDays15*TimePeriod, family=binomial(link="cauchit"), data=Pred2)
mod_preddays4 <-glm(Depredated ~ PredDays15*TimePeriod, family=binomial(link="cloglog"), data=Pred2)
mod_preddays5 <-glm(Depredated ~ PredDays15*TimePeriod, family=binomial(link="probit"), data=Pred2)
AICc(mod_preddays1, mod_preddays2, mod_preddays3, mod_preddays4, mod_preddays5)
#Cauchit is much better. 
library(DescTools)
PseudoR2(mod_preddays1) 
PseudoR2(mod_preddays2) 
PseudoR2(mod_preddays3) 
PseudoR2(mod_preddays4) 
PseudoR2(mod_preddays5) 
#Wow these are all terrible r^2 but I guess it's what we go for. 

mod_pred15 <-glm(Depredated ~ PredDays15*TimePeriod, family=binomial(link="cauchit"), data=Pred2)
plot(mod_pred15) #There are a couple of points with leverage
plot(resid(mod_pred15)~Pred2$TimePeriod) #Not great but not really worse than before.  
plot(resid(mod_pred15)~Pred2$PredDays15)  #Looks ok


options(na.action="na.fail")
dredge(mod_pred15) 
car::Anova(mod_pred15)

#Looks like Days above 15 degrees is a significant predictor but doesn't explain everything for time periods. 
#More days with warm temperatures=more predation. 
mam_pred15 <- glm(Depredated ~ PredDays15+TimePeriod, family=binomial(link="cauchit"), data=Pred2)

#Here are our beta values and OR
summary(mam_pred15)
oddsRat_days <- exp(coef(mam_pred15))
oddsRatSE_days <- get.or.se(mam_pred15)

######Does days above 20 degrees during the predation periods make nestlings at risk of predation?
mod_preddays1 <-glm(Depredated ~ PredDays20*TimePeriod, family=binomial(link="log"), data=Pred2)
mod_preddays2 <-glm(Depredated ~ PredDays20*TimePeriod, family=binomial(link="logit"), data=Pred2)
mod_preddays3 <-glm(Depredated ~ PredDays20*TimePeriod, family=binomial(link="cauchit"), data=Pred2)
mod_preddays4 <-glm(Depredated ~ PredDays20*TimePeriod, family=binomial(link="cloglog"), data=Pred2)
mod_preddays5 <-glm(Depredated ~ PredDays20*TimePeriod, family=binomial(link="probit"), data=Pred2)
AICc(mod_preddays1, mod_preddays2, mod_preddays3, mod_preddays4, mod_preddays5)
#Cauchit is  better. 
library(DescTools)
PseudoR2(mod_preddays1) 
PseudoR2(mod_preddays2) 
PseudoR2(mod_preddays3) 
PseudoR2(mod_preddays4) 
PseudoR2(mod_preddays5) 
#Wow these are all terrible r^2 but I guess it's what we go for. They're even lower than days above 15

mod_pred20 <-glm(Depredated ~ PredDays20*TimePeriod, family=binomial(link="cauchit"), data=Pred2)
plot(mod_pred20) #There are a couple of points with leverage
plot(resid(mod_pred20)~Pred2$TimePeriod) #Not great but not really worse than before.  
plot(resid(mod_pred20)~Pred2$PredDays20)  #Looks ok


options(na.action="na.fail")
dredge(mod_pred20) #Can't really tell if we need the interaction or not
car::Anova(mod_pred20)

#Looks like Days above 15 degrees is a significant predictor but doesn't explain everything for time periods. 
#More days with warm temperatures=more predation. 
mam_pred20 <- glm(Depredated ~ PredDays20+TimePeriod, family=binomial(link="cauchit"), data=Pred2)

#Here are our beta values and OR
summary(mam_pred20)
oddsRat_days <- exp(coef(mam_pred20))
oddsRatSE_days <- get.or.se(mam_pred20)


#How does our best model for 20 vs 15 degrees compare?

AICc(mam_pred15, mam_pred20)
#They are almost exactly the same. I will go with 15 degrees since I have a
#citation that I can use to justify that cutoff.


#Predation rates were much higher when the population was declining than when
#the population wasn't growing. #Cosewic designates gray ratsnakes as threatened
#in april 1998-- which is right after we see the population start declining.



