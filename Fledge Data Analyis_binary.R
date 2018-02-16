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



hist(dat2$FledgeRate) #HIGHLY bimodal

dat2$Fledge2 <- NA
dat2$Fledge2[which(dat2$Fledge>0)]<- 1 
dat2$Fledge2[which(dat2$Fledge==0)]<- 0

#Let's remove the predation caused failures. 
dat3 <- dat2 %>% filter( !is.na(Fledge2) & !is.na(Hatch) & (FailureCause2!="PREDATION" |is.na(FailureCause2)) )
#let's rescale year so that we are actualy looking at decades sice 1975
dat3$Year2 <- dat3$Year/10-197.5



weather_pre <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Hartington IHD Weather Station Daily Data 1975 to  2017.csv", as.is=T)
weather <- weather_pre[26:nrow(weather_pre), c(1,2,6,8,10,12,14,16,18)]
rm(weather_pre)
colnames(weather) <- c("Date", "Year",  
                        "MaxTemp", "MinTemp", "MeanTemp", "HeatDegDays", "CoolDegDays", "TotRain", "TotPrecip") 

weather$JDate <- lubridate::yday(as.Date(weather$Date, format="%m/%d/%Y"))
weather$MeanTemp <- as.numeric(weather$MeanTemp)
weather$MaxTemp <- as.numeric(weather$MaxTemp)



#I'm going to calculate how many days were above 18 degrees during the
#poikilothermic time (ie for the first 6 days of life) since we know that's when
#most of the death occurs



calculateDaysabove18 <- function(HatchDate, Year){
  if(is.na(HatchDate)){
    return(NA)
  }
  weatherPoik <- weather[which(Year==weather$Year & weather$JDate>=HatchDate & weather$JDate<(HatchDate+7)),c(3, 8)]
  if (anyNA(weatherPoik)){
    #if we are missing data for any of the days, then we'll just return NA
    return(NA)
  } else {
    #how many of those days were warm enough for insects, and not raining (more than 1mm)
    return(length(which(weatherPoik$MaxTemp>18 & weatherPoik$TotRain==0)))
  }
}



for (i in 1:nrow(dat3)){
  dat3$Daysabove18[i]<- calculateDaysabove18(HatchDate = dat3$HatchDate[i], Year=dat3$Year[i])
}


#Now we have some weather data that we could use to do a binary glm


#Question: Is nestling death (excluding death due to predation) more common in
#bad weather, AND, are they surviving worse over the years?
dat4 <- dat3 %>% filter(Hatch>0 & !is.na(Daysabove18))

mod <- glm(Fledge2 ~ Year2*Hatch*Daysabove18, family=binomial, data=dat4)
plot(mod)
plot(resid(mod)~predict(mod))
plot(resid(mod)~dat4$Year2)
plot(resid(mod)~dat4$Hatch)
plot(resid(mod)~dat4$Daysabove18)
#This looks like it fits ok. I'm pleased. 

car::Anova(mod)

summary(mod)

dat4$predicted <- predict(mod)


newdata <- data.frame(Year2=rep(seq(0, 4.2, 0.1),4), 
                      Hatch=c(rep(3,43), rep(6, 43)), 
                      Daysabove18=c(rep(2, 86), rep(5, 86)), 
                      predicted_logit=NA,
                      predicted=NA, 
                      se_logit=NA,
                      ucl=NA, 
                      lcl=NA)


newdata$predicted_logit <- predict(mod, newdata, se.fit = T)$fit
newdata$se_logit <- predict(mod, newdata, se.fit = T)$se.fit

newdata$predicted <- arm::invlogit(newdata$predicted_logit)
newdata$ucl <- arm::invlogit(newdata$predicted_logit+ 1.96*(newdata$se_logit))
newdata$lcl <- arm::invlogit(newdata$predicted_logit- 1.96*(newdata$se_logit))

newdata$Daysabove18 <- factor(newdata$Daysabove18, labels=c("Poor weather", "Good weather"))
ggplot(newdata, aes(x=Year2*10+1975, y=predicted, fill=factor(Hatch)))+
  geom_line(aes(color=factor(Hatch)))+
  geom_ribbon(aes(ymin=ucl, ymax=lcl), alpha=0.3)+
  facet_grid(~factor(Daysabove18))+
  labs(x="Year", y="Likelihood of fledging", fill="Hatch Size", color="Hatch Size")+
  ggthemes::theme_few()




