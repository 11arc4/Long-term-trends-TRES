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
Pred <- dat %>% filter(!is.na(Daysabove18) & (Fledge2==1 | (FailureCause2=="PREDATION" & Fledge2==0))) #Want to use the same dataset here as later

#Make a column for whether they were depredated or not (yes=1). This is the
#opposite of fledge2, but is a better more intuitive way of plotting that I'm
#sure.
Pred$Depredated <- NA
Pred$Depredated[Pred$Fledge2==0]<- 1
Pred$Depredated[Pred$Fledge2==1]<- 0



########################################
#Question: Are there more predated nests in years during the decline? 

mod_pred <- glm(Depredated ~ TimePeriod*Year2, family=binomial, data=Pred)
plot(mod_pred)
plot(resid(mod_pred)~Pred$TimePeriod) #A little wonky but I don't think I'm worried
plot(resid(mod_pred)~Pred$Year2) #Think it's not so bad. Not perfect but not so bad

options(na.action="na.fail")
dredge(mod_pred)
Anova(mod_pred)
summary(mod_pred)
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
                      TimePeriod=c(rep("Growing", 22), rep("Declining", 17), rep("PostDecline", 4)),
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
  ylim(0, 1)+
  labs(y="Predation Rate", x="Year")+
  geom_vline(xintercept = c(1996.5, 2013.5 ))+
  ggthemes::theme_few(base_size = 16)




#Question 2: Is predation somehow related to weather? I'm not really expecting
#it to be at the binary level, but perhaps.
mod_preddays <- glm(Depredated ~ TimePeriod*Daysabove18, family=binomial, data=Pred)
plot(mod_pred)
plot(resid(mod_pred)~Pred$TimePeriod) 
plot(resid(mod_pred)~Pred$Daysabove18) 

options(na.action="na.fail")
dredge(mod_preddays)
Anova(mod_pred)
summary(mod_preddays)

oddsRat_days <- exp(coef(mod_preddays))
oddsRatSE_days <- get.or.se(mod_preddays)

newdata_days <- data.frame(Daysabove18=rep(seq(0, 9, 1),3),
                           TimePeriod=c(rep("Growing", 10), rep("Declining", 10), rep("PostDecline", 10)),
                           predicted_logit=NA,
                           predicted=NA, 
                           se_logit=NA,
                           use=NA, 
                           lse=NA)


newdata_days$predicted_logit <- predict(mod_preddays, newdata_days, se.fit = T)$fit
newdata_days$se_logit <- predict(mod_preddays, newdata_days, se.fit = T)$se.fit

newdata_days$predicted <- arm::invlogit(newdata_days$predicted_logit)
newdata_days$use <- arm::invlogit(newdata_days$predicted_logit+ (newdata_days$se_logit))
newdata_days$lse <- arm::invlogit(newdata_days$predicted_logit- (newdata_days$se_logit))

ggplot(newdata_days, aes(x=Daysabove18, y=predicted, fill=TimePeriod))+
  geom_line(aes(color=TimePeriod), size=1)+
  geom_ribbon(aes(ymin=use, ymax=lse), alpha=0.4)+
  labs(x="Days of good weather", y="Predation rate", fill="Population \nStatus", color="Population \nStatus")+
  ggthemes::theme_few(base_size = 16)

#WOW when the population was declining, in warmer weather the predation risk was
#way higher.

#Cosewic designates gray ratsnakes as threatened in april 1998-- which is right after we see the population start declining. 
