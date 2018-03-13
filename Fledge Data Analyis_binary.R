#Are birds fledging less often due to local weather conditions?
#you could say
#that death due to predation isn't driven by weather. HOWEVER, it's possible
#that visitation rates are influenced by weahter (e.g. snakes aren't out on cold
#days, parental visitation rates are higher on warm days=better shot that
#predators are going to get you)
#therefore, we will do both including and excluding predated nests. 


library(tidyverse)



#Load in the individual data.
dat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/TRES Data Analysis/Extracted Statistics/Fledge Rate Analysis Data.csv", as.is=T, na.strings=c(""))
#Let's set female age to either SY or ASY. 
dat$FAge <- as.factor(dat$FAge)
levels(dat$FAge)

dat$FAge[which(dat$FAge=="HY")] <- NA #this has to be wrong!
dat$FAge2<- dat$FAge
dat$FAge2[which(dat$FAge!="SY" & !is.na(dat$FAge))]<- "ASY"

#What has happened over time? Are birds dying for different reasons
dat$FailureCause2 <- NA
dat$FailureCause2[dat$FailureCause=="PREDATION" |
                    dat$FailureCause=="PREDATION" |
                    dat$FailureCause=="PREDATION?" |
                    dat$FailureCause=="EGGS DEPREDATED"] <- "PREDATION"

dat$FailureCause2[dat$FailureCause=="NESTLINGS DIED" |
                    dat$FailureCause=="NESTLINGS DEAD" |
                    dat$FailureCause=="DEAD IN NEST" |
                    dat$FailureCause==" NESTLINGS DIED" ] <- "NESTLINGS DIED"

dat$FailureCause2[dat$FailureCause=="COMPETITION, FEMALE DIED" |
                    dat$FailureCause==" INFANTICIDE" |
                    dat$FailureCause=="COMPETITOR" |
                    dat$FailureCause=="INTRASPECIFIC COMPETITOR INTERFERENCE"|
                    dat$FailureCause=="COMPETITION"|
                    dat$FailureCause=="COMPETITOR INTERFERENCE?"|
                    dat$FailureCause=="INFANTICIDE"] <- "COMPETITOR"

dat$FailureCause2[which(!is.na(dat$FailureCause) & is.na(dat$FailureCause2))]<- "OTHER"

hist(dat$FledgeRate) #HIGHLY bimodal

dat$Fledge2 <- NA
dat$Fledge2[which(dat$Fledge>0)]<- 1 
dat$Fledge2[which(dat$Fledge==0)]<- 0


#Let's remove the super invasive experiments
dat$Experiment<- as.factor(dat$Experiment)
levels(dat$Experiment)
#There are a lot of different names for experiments, but anything where we
#transfered, remov[ed], add, female [remove], male remov, kill needs to be taken
#out

dat2 <- dat [which(!grepl("trans", dat$Experiment, fixed=T) &
                     !grepl("TRANS", dat$Experiment, fixed=T) &
                     !grepl("TREANS", dat$Experiment, fixed=T) &
                     !grepl("REMOV", dat$Experiment, fixed=T) &
                     !grepl("remov", dat$Experiment, fixed=T) &
                     !grepl("ADD", dat$Experiment, fixed=T) &
                     !grepl("add", dat$Experiment, fixed=T) &
                     !grepl("FEMALE", dat$Experiment, fixed=T) &
                     !grepl("female", dat$Experiment, fixed=T) &
                     !grepl("male remo", dat$Experiment, fixed=T) &
                     !grepl("kill", dat$Experiment, fixed=T) &
                     !grepl("reduction of nestlings", dat$Experiment, fixed=T) &
                     !grepl("brood", dat$Experiment, fixed=T) 
                   
), ] %>% filter (Fledge<=Hatch)

summary(dat2$Experiment)


#OK so we've now got some data that is clearly better than our predivious
#data!!! It removes all the nests that were heavily manipulated and therefore
#not really worth our time







weather_pre <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Hartington IHD Weather Station Daily Data 1975 to  2017.csv", as.is=T)
weather <- weather_pre[26:nrow(weather_pre), c(1,2,6,8,10,12,14,16,18)]
rm(weather_pre)
colnames(weather) <- c("Date", "Year",  
                        "MaxTemp", "MinTemp", "MeanTemp", "HeatDegDays", "CoolDegDays", "TotRain", "TotPrecip") 

weather$JDate <- lubridate::yday(as.Date(weather$Date, format="%m/%d/%Y"))
weather$MeanTemp <- as.numeric(weather$MeanTemp)
weather$MaxTemp <- as.numeric(weather$MaxTemp)



#I'm going to calculate how many days were above 18 degrees during the
#poikilothermic & intermediate time (ie for the first 6 days of life) since we know that's when
#most of the death occurs



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
    return(length(which(weatherPoik$MaxTemp>18 & weatherPoik$TotRain==0)))
  }
}



for (i in 1:nrow(dat2)){
  dat2$Daysabove18[i]<- calculateDaysabove18(HatchDate = dat2$HatchDate[i], Year=dat2$Year[i])
}

dat2$Year2 <- dat2$Year/10-197.5

#Now we have some weather data that we could use to do a binary glm

#Question 1: Are nestlings fledging less often in later years ? 
ggplot(dat2 %>% filter(Fledge/Hatch<=1), aes(x=Year, y=Fledge/Hatch))+
  geom_point(alpha=0.2)+
  stat_smooth()+
  geom_vline(xintercept=c(1996, 2015))+
  labs(y="Fledge Rate")


ggplot(dat %>% filter(Fledge/Hatch<=1), aes(x=Year, y=Fledge/Hatch))+
  geom_point(alpha=0.2)+
  stat_smooth()+
  geom_vline(xintercept=c(1996, 2015))+
  labs(y="Fledge Rate")

ggplot(Pred2, aes(x=Year, y=Fledge2))+
  geom_point(alpha=0.2)+
  stat_smooth()+
  geom_vline(xintercept=c(1996, 2014))+
  labs(y="Fledge Rate")
#I guess you see the same thing, It's just slighly less dramatic. It does look
#like it might be staged though, with different time periods of growth and
#decline, or at least a 3rd order polynomial

#Let me set up time periods. 

dat2$TimePeriod <- NA
dat2$TimePeriod[which(dat2$Year<1997)]<- "Growing"
dat2$TimePeriod[which(dat2$Year>1996)]<- "Declining"
dat2$TimePeriod[which(dat2$Year>2013)]<- "PostDecline"
dat2$TimePeriod <- factor(dat2$TimePeriod, levels=c("Growing", "Declining", "PostDecline"))

ggplot(dat2, aes(x=Year, y=Fledge2, group=TimePeriod))+
  geom_point(alpha=0.2)+
  stat_smooth(method="lm")+
  geom_vline(xintercept=c(1996, 2014))+
  labs(y="Fledge Rate")



mod <- glm(Fledge2 ~ Year2*TimePeriod, family=binomial, data=dat2)
plot(mod)
hist(resid(mod))
plot(resid(mod)~dat2$Year2)
plot(resid(mod)~dat2$Hatch) 
#Model fits better if we don't put hatch size into it.... Plus side is that
#there are no trends with hatch size when it's not included so probably not
#problematic.
plot(resid(mod)~dat2$TimePeriod)
#Model only fits if Hatch size isn't included. That's ok with me!

car::Anova(mod)

options(na.action="na.fail")
dredge(mod) #need all terms. 

summary(mod)

#Yes! This is fantastic. It finally looks like our data is 

newdata <- data.frame(Year2=rep(seq(0, 4.2, 0.1)),
                      TimePeriod=c(rep("Growing", 22), rep("Declining", 17), rep("PostDecline", 4)),
                      predicted_logit=NA,
                      predicted=NA, 
                      se_logit=NA,
                      use=NA, 
                      lse=NA)


newdata$predicted_logit <- predict(mod, newdata, se.fit = T)$fit
newdata$se_logit <- predict(mod, newdata, se.fit = T)$se.fit

newdata$predicted <- arm::invlogit(newdata$predicted_logit)
newdata$use <- arm::invlogit(newdata$predicted_logit+ (newdata$se_logit))
newdata$lse <- arm::invlogit(newdata$predicted_logit- (newdata$se_logit))

ggplot(newdata, aes(y=predicted, x=Year2*10+1975, group=TimePeriod))+
  geom_line()+
  geom_ribbon(aes(ymin=lcl, ymax=ucl),alpha=0.2)+
  ylim(0, 1)+
  labs(y="Fledging Success", x="Year")+
  geom_vline(xintercept = c(1996.5, 2013.5 ))+
  ggthemes::theme_few(base_size = 16)





#Question: Is nest failure (excluding death due to predation) more common in
#bad weather, AND, are they surviving worse over the years?
NoPred2 <- NoPred %>% filter(Hatch>0 & !is.na(Daysabove18))
Pred2 <- dat2 %>% filter(!is.na(Daysabove18))

mod_pred <- glm(Fledge2 ~ TimePeriod*Year2, family=binomial, data=Pred2)
dredge(mod_pred)

mod_pred <- glm(Fledge2 ~ TimePeriod*Daysabove18, family=binomial, data=Pred2)


daysMod <- glm(Fledge2 ~ TimePeriod*Daysabove18 + TimePeriod*Year2 , family=binomial, data=Pred2)
plot(daysMod)
plot(resid(daysMod)~predict(daysMod))
plot(resid(daysMod)~Pred2$Year2) #This isn't ideal
plot(resid(daysMod)~Pred2$TimePeriod)
plot(resid(daysMod)~Pred2$Daysabove18)
#This looks like it fits ok. I'm pleased. 

car::Anova(daysMod)
options(na.action = "na.fail")
dredge(daysMod)



mamPred_Days <- daysMod
summary(mamPred_Days)

AICc(mod_pred)
AICc(mamPred_Days)

#OK so using year is a lot better-- Year is clearly still accounting for
#something that days above 18 can't. Not sure what that is. Still, Days above 18
#is well supported

YearSummary <- Pred2 %>% 
  group_by(Year2) %>% 
  summarise(Daysabove18 = mean(Daysabove18),
            predicted_logit = NA, 
            predicted =NA,
            TimePeriod=NA,
            ucl=NA, 
            lcl=NA, 
            se_logit=NA, 
            NPpredicted_logit = NA, 
            NPpredicted =NA,
            TimePeriod=NA,
            NPucl=NA, 
            NPlcl=NA, 
            NPse_logit=NA) 
YearSummary$TimePeriod <- as.factor(c(rep("Growing", 22), rep("Declining", 16), rep("PostDecline", 4)))


YearSummary$predicted_logit <- predict(daysMod, YearSummary, se.fit = T)$fit
YearSummary$se_logit <- predict(daysMod, YearSummary, se.fit = T)$se.fit

YearSummary$predicted <- arm::invlogit(YearSummary$predicted_logit)
YearSummary$use <- arm::invlogit(YearSummary$predicted_logit+ (YearSummary$se_logit))
YearSummary$lse <- arm::invlogit(YearSummary$predicted_logit- (YearSummary$se_logit))



ggplot(YearSummary, aes(y=predicted, x=Year2))+
  geom_point()+
  geom_segment(aes(y=lse, yend=use, x=Year2, xend=Year2),alpha=0.2)+
  geom_smooth()+
  labs(y="Fledging Success", x="Year")


ggplot(YearSummary, aes(y=Daysabove18, x=Year2, color=TimePeriod))+
  geom_point()+
  stat_smooth(method="lm")+
  labs(y="Mean days above 18.5", x="Year")





#######Are nest failures NOT due to predadtion increasing at different rates?

#Let's remove the predation caused failures. 
NoPred <- dat2 %>% filter( !is.na(Fledge2) & (FailureCause2!="PREDATION" |is.na(FailureCause2)) & !is.na(Daysabove18) )



nopredMod <- glm(Fledge2 ~ Year2*TimePeriod, family=binomial, data=NoPred)
plot(nopredMod)
plot(resid(nopredMod)~NoPred$Year2)
plot(resid(nopredMod)~NoPred$TimePeriod) #Better at growin but I think this is a amount of data thing. 
summary(nopredMod)

newdata2 <- data.frame(Year2=rep(seq(0, 4.2, 0.1)),
                      TimePeriod=c(rep("Growing", 22), rep("Declining", 17), rep("PostDecline", 4)),
                      predicted_logit=NA,
                      predicted=NA, 
                      se_logit=NA,
                      use=NA, 
                      lse=NA)


newdata2$predicted_logit <- predict(nopredMod, newdata2, se.fit = T)$fit
newdata2$se_logit <- predict(nopredMod, newdata2, se.fit = T)$se.fit

newdata2$predicted <- arm::invlogit(newdata2$predicted_logit)
newdata2$use <- arm::invlogit(newdata2$predicted_logit+ (newdata2$se_logit))
newdata2$lse <- arm::invlogit(newdata2$predicted_logit- (newdata2$se_logit))

ggplot(newdata2, aes(y=predicted, x=Year2*10+1975, group=TimePeriod))+
  geom_line()+
  geom_ribbon(aes(ymin=lse, ymax=use),alpha=0.2)+
  ylim(0, 1)+
  labs(y="Fledging Success", x="Year")+
  geom_vline(xintercept = c(1996.5, 2013.5 ))+
  ggthemes::theme_few(base_size = 16)


daysMod_nopred <- glm(Fledge2 ~ Year2*TimePeriod + TimePeriod*Daysabove18, family=binomial, data=NoPred)

plot(daysMod_nopred)
plot(resid(daysMod_nopred)~predict(daysMod_nopred))
plot(resid(daysMod_nopred)~NoPred$Year2)
plot(resid(daysMod_nopred)~NoPred$TimePeriod)
plot(resid(daysMod_nopred)~NoPred$Daysabove18)
#Looks ok

dredge(daysMod_nopred)
car::Anova(daysMod_nopred)
mamNoPred_days <-  glm(Fledge2 ~ Year2*TimePeriod + Daysabove18, family=binomial, data=NoPred)
summary(mamNoPred_days)



YearSummary$NPpredicted_logit <- predict(mamNoPred_days, YearSummary, se.fit = T)$fit
YearSummary$NPse_logit <- predict(mamNoPred_days, YearSummary, se.fit = T)$se.fit

YearSummary$NPpredicted <- arm::invlogit(YearSummary$NPpredicted_logit)
YearSummary$NPucl <- arm::invlogit(YearSummary$NPpredicted_logit+ 1.96*(YearSummary$se_logit))
YearSummary$NPlcl <- arm::invlogit(YearSummary$NPpredicted_logit- 1.96*(YearSummary$se_logit))


ggplot(YearSummary, aes( x=Year2*10+1975), group=TimePeriod)+
  geom_point(aes(y=NPpredicted), color="red")+
  geom_point(aes(y=predicted), color="blue")+
  geom_ribbon(aes(ymin=NPlcl, ymax=NPucl),alpha=0.2, fill="red")+
  geom_ribbon(aes(ymin=lcl, ymax=ucl),alpha=0.2, fill="blue")+
  ylim(0, 1)+
  labs(y="Fledging Rate", x="Year")
#Predation vs. no predation makes little differentce. 
