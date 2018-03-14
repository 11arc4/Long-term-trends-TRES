#Predation analysis 
#it's possible that visitation rates are influenced by
#weahter (e.g. snakes aren't out on cold days, parental visitation rates are
#higher on warm days=better shot that predators are going to get you) Do you get
#more predation for nests when the nestlings are older (ie have higher
#visitation rates by parents), or when the weather THAT day is warmer?

#Read in the binary fledging data
dat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/Binary Fledge Success wo experimental nests.csv", as.is=T, na.strings = "" ) %>% filter(Daysabove18 <10)
dat$TimePeriod<-factor(dat$TimePeriod, levels=c("Growing", "Declining", "PostDecline"))

Pred <- dat %>% filter(!is.na(Daysabove18)) #Want to use the same dataset here as later


########################################
#Question: Is there less fledging success in years during the decline when you include predation? 

mod_pred <- glm(Fledge2 ~ TimePeriod*Year2, family=binomial, data=Pred)
plot(mod_pred)
plot(resid(mod_pred)~Pred$TimePeriod)
plot(resid(mod_pred)~Pred$Year2)
#It seems ok. Not perfect, but ok. 

options(na.action("na.fail"))
dredge(mod_pred)

#mod_pred is the best model. Let's make the plot

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
  labs(y="Fledging Success", x="Year")+
  geom_vline(xintercept = c(1996.5, 2013.5 ))+
  ggthemes::theme_few(base_size = 16)



summary(mod_pred)
#Same sort of results as when predated nests were excluded. 


########################################
#Question 2: Is there less fledging success in when there are less good days?  
mod_preddays <- glm(Fledge2 ~ TimePeriod*Daysabove18, family=binomial, data=Pred)
plot(mod_preddays)
plot(resid(mod_preddays)~Pred$Year2) #This isn't ideal
plot(resid(mod_preddays)~Pred$TimePeriod)
plot(resid(mod_preddays)~Pred$Daysabove18)
#This looks like it fits ok. I'm pleased. 


dredge(mod_preddays)
summary(mod_preddays)
#Here mod_preddays is also the best model. That's because The declining time
#period is less affected by days above 0 than the others.

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
  labs(x="Days of good weather", y="Fledging Success", fill="Population Status", color="Population Status")+
  ggthemes::theme_few(base_size = 16)
#They have such lower fledging success when decling-- looks to me that they are
#just getting hammared by predation during the time they were declining.



#####################################
#Question 3: Are there less good days on average experienced by all the nests (predated or not)?
YearSummary <- Pred %>% 
  group_by(Year2, TimePeriod) %>% 
  summarise(MeanHatchDate=mean(HatchDate), 
            MeanDaysabove18= mean(Daysabove18, na.rm=T), 
            Daysabove18_2 = NA,
            Year=NA) 

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
    return(length(which(weatherPoik$MaxTemp>18 & weatherPoik$TotRain==0)))
  }
}





YearSummary$TimePeriod <- factor(YearSummary$TimePeriod, levels=c("Growing", "Declining", "PostDecline"))

mod2 <- lm(MeanDaysabove18~Year2*TimePeriod, data=YearSummary)
plot(mod2)

dredge(mod2)
#Yes there are more good days, but predation is also increased it looks like. 

ggplot(YearSummary, aes(y=MeanDaysabove18, x=Year))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(y="Mean days of good weather", y="Year")+
  ggthemes::theme_few(base_size = 16)






##############################
#Are nests more likely to be predated now? 
Pred$Predated <- 0
Pred$Predated[which(Pred$FailureCause2=="PREDATION")]<- 1

Pred2 <- Pred %>% group_by(Year, TimePeriod) %>% summarise(PredatedNests = sum(Predated), 
                                               TotalNests = length(unique(NestID) ), 
                                               RatioPredated = NA, 
                                               FailedNests = sum(Fledge2==0), 
                                               RatioPredatedofFails = NA
                                               )
Pred2$RatioPredated <- Pred2$PredatedNests/Pred2$TotalNests
Pred2$RatioPredatedofFails <- Pred2$PredatedNests/Pred2$FailedNests

# number predated nests ~ time


# predated/total ~time


ggplot(Pred2, aes(x=Year, y=PredatedNests))+
  geom_point()

ggplot(Pred2 %>% filter(TotalNests>25), aes(x=Year, y=RatioPredated))+
  geom_point()
ggplot(Pred2 %>% filter(TotalNests>25), aes(x=Year, y=RatioPredatedofFails))+
  geom_point()

