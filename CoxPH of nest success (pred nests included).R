#Are nests surviving less than they used to when we include predated nests? 
#a cox-proportional hazards model
library(survival)

library(tidyverse)
library(MuMIn)



#Load in the nest summarized data
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

#Let's remove only 
dat3 <- dat2 %>% filter( !is.na(Fledge2) & !is.na(Hatch) & Hatch>0 & FledgeDate >HatchDate & !is.na(FledgeDate) & !is.na(HatchDate)  )
#let's rescale year so that we are actualy looking at decades sice 1975
dat3$Year2 <- dat3$Year/10-197.5




####NOW WE NEED TO CHANGE THE FORMAT OF THIS DATA SO THAT EACH DAY HAS A ENTRY FOR THE COXPH
survdat <-  as.data.frame(matrix(ncol=11, nrow=4000, NA)) 
colnames(survdat) <- c("NestID", "Year", "Time2", "Time1", "Age","PreStatus" ,"Status", "MaxTemp", "MeanTemp", "MinTemp", "TotRain" )  

a=0
for (i in 1:nrow(dat3)){
  
  temp <- data.frame(NestID=rep(dat3$NestID[i]), 
                     Year=rep(dat3$Year[i]), 
                     Time2=seq(dat3$HatchDate[i]+1, dat3$FledgeDate[i],1), 
                     Time1=seq(dat3$HatchDate[i], dat3$FledgeDate[i]-1,1),
                     Age=seq(1, dat3$FledgeDate[i]-dat3$HatchDate[i],1),
                     PreStatus=c(rep(x=1, times=dat3$FledgeDate[i]-dat3$HatchDate[i]-1), dat3$Fledge2[i])
  )
  if(i==1){
    a=1
  } 
  b <- nrow(temp)+a-1
  survdat[a:b, 1:6]<- temp
  a <- b+1
}
#They want a true  for dead or false not dead
survdat$Status[which(survdat$PreStatus==1)] <- F
survdat$Status[which(survdat$PreStatus==0)] <- T

survdat$Year2 <- survdat$Year/10-197.5


weather_pre <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Hartington IHD Weather Station Daily Data 1975 to  2017.csv", as.is=T)
weather <- weather_pre[26:nrow(weather_pre), c(1,2,6,8,10,12,14,16,18)]
rm(weather_pre)
colnames(weather) <- c("Date", "Year",  
                       "MaxTemp", "MinTemp", "MeanTemp", "HeatDegDays", "CoolDegDays", "TotRain", "TotPrecip") 

weather$JDate <- lubridate::yday(as.Date(weather$Date, format="%m/%d/%Y"))
weather$MeanTemp <- as.numeric(weather$MeanTemp)
weather$MaxTemp <- as.numeric(weather$MaxTemp)
weather$MinTemp <- as.numeric(weather$MinTemp)
weather$TotRain <- as.numeric(weather$TotRain)

weather2 <- weather %>% filter(JDate>139 & JDate<216) 

for(i in 1:nrow(weather2)){
  c <- which(survdat$Year==weather2$Year[i] & survdat$Time1==weather2$JDate[i])
  if(length(c)!=0){
    survdat$MaxTemp[c] <- weather2$MaxTemp[i]
    survdat$MeanTemp[c] <- weather2$MeanTemp[i]
    survdat$MinTemp[c] <- weather2$MinTemp[i]
    survdat$TotRain[c] <- weather2$TotRain[i]
  }
}



#YAY I made the data. It's ready for a coxPH analysis now.
#I wonder if we should use Age2 (similar to Kennedy's growth and survival
#analysis,  poikilothermic, intermediate, and endothermic)
survdat$Age2 <- NA
survdat$Age2[survdat$Age<7]<- "Poikilotherm"
survdat$Age2[survdat$Age>6 & survdat$Age<9]<- "Intermediate"
survdat$Age2[survdat$Age>8]<- "Endotherm"
survdat$Age2 <- as.factor(survdat$Age2)


library(stats)
weatherVar <- weather2[,c(3:5, 8)]
#we are going to use all the weather variables because that's what we did in the
#other analyses and it's important to be consistant, even thoguh the results are
#no different either way

weather.pca <- prcomp(na.omit(weatherVar), 
                      center=T, 
                      scale=T)

plot(weather.pca, type="lines")
summary(weather.pca)

survdat$PC1 <- predict(weather.pca, survdat[,7:10])[,1]
survdat$PC2 <- predict(weather.pca, survdat[,7:10])[,2]


weather2$PC1 <- predict(weather.pca, weather2[,c(3:5, 8)])[,1]
weather2$PC2 <- predict(weather.pca, weather2[,c(3:5, 8)])[,2]



survdat$TimePeriod <- NA
survdat$TimePeriod[which(survdat$Year<1997)]<- "Growing"
survdat$TimePeriod[which(survdat$Year>1996)]<- "Declining"
survdat$TimePeriod[which(survdat$Year>2013)]<- "PostDecline"
survdat$TimePeriod <- factor(survdat$TimePeriod, levels=c("Growing", "Declining", "PostDecline"))

survdat2 <- survdat %>% filter(!is.na(TotRain) & !is.na(MeanTemp))
survdat2$TotRain2 <- 0
survdat2$TotRain2[survdat2$TotRain>0]<- 1 #Need to code it like this to let the model converge. 




###########################
#Are hazard ratios increasing for anyone based on year? 
mod <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*Year2*TimePeriod, data=survdat)
test.ph <- cox.zph(mod) #fantastic! We are meeting all the assumptions
plot(resid(mod)~survdat$Age2)
plot(resid(mod)~survdat$Year2)
plot(resid(mod)~survdat$TimePeriod)
plot(predict(mod)~resid(mod))


car::Anova(mod)
options(na.action="na.fail")
dredge(mod)
mam <- mod


newdata <- data.frame(Year2=rep(seq(0, 4.2, 0.1), 3),
                      TimePeriod=rep(c(rep("Growing", 22), rep("Declining", 17), rep("PostDecline", 4)),3),
                      Age2 = c(rep("Poikilotherm", 43), rep("Intermediate", 43), rep("Endotherm", 43)), 
                      predicted=NA, 
                      se=NA,
                      ucl=NA, 
                      lcl=NA)

newdata$predicted<- predict(mam, newdata=newdata, se.fit = T,  type="risk")[[1]]
newdata$se<- predict(mam, newdata=newdata, se.fit = T,  type="risk")[[2]]

ggplot(newdata, aes(x=Year2*10+1975, y=predicted))+
  geom_line(aes(color=Age2))+
  geom_ribbon(aes(ymin=predicted-se, ymax=predicted+se, fill=Age2), alpha=0.2 )+
  labs(x="Year", y="Mortality Risk", color="Nestling Age", fill="Nestling Age" )
#OK this make much more sense. It's shitty that this makes so much more sense but it's also good. 




