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
survdat <-  as.data.frame(matrix(ncol=10, nrow=4000, NA)) 
colnames(survdat) <- c("NestID", "Year", "Time2", "Time1", "Age", "Status", "MaxTemp", "MeanTemp", "MinTemp", "TotRain" )  

a=0
for (i in 1:nrow(dat3)){
  
  temp <- data.frame(NestID=rep(dat3$NestID[i]), 
                     Year=rep(dat3$Year[i]), 
                     Time2=seq(dat3$HatchDate[i]+1, dat3$FledgeDate[i],1), 
                     Time1=seq(dat3$HatchDate[i], dat3$FledgeDate[i]-1,1),
                     Age=seq(1, dat3$FledgeDate[i]-dat3$HatchDate[i],1),
                     Status=c(rep(x=1, times=dat3$FledgeDate[i]-dat3$HatchDate[i]-1), dat3$Fledge2[i])
  )
  if(i==1){
    a=1
  } 
  b <- nrow(temp)+a-1
  survdat[a:b, 1:6]<- temp
  a <- b+1
}
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


survdat2 <- survdat %>% filter(!is.na(TotRain) & !is.na(MeanTemp))
survdat2$TotRain2 <- 0
survdat2$TotRain2[survdat2$TotRain>0]<- 1 #Need to code it like this to let the model converge. 

#################################################################
##### Analysis of MaxTemp's predictive ability for survival 
mod1 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*Year2*MaxTemp, data=survdat2)
test.ph1 <- cox.zph(mod1) #fantastic! We are meeting all the assumptions

#DO we need a 3 way interaction
mod1_2 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*Year2+ Year2*MaxTemp + Age2*MaxTemp, data=survdat2)
anova(mod1, mod1_2)
#Nope. 
#How about a Year:Age interaction?

mod1_3 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Year2*MaxTemp + Age2*MaxTemp, data=survdat2)
anova(mod1_3, mod1_2)
#nope
#How about a Year:Max Temp?

mod1_4 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Year2 + Age2*MaxTemp, data=survdat2)
anova(mod1_3, mod1_4)
#nope. 
#How about age:Max temp


mod1_5 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Year2 + Age2 + MaxTemp, data=survdat2)
anova(mod1_5, mod1_4)
#Nope
#How about year?
mod1_6 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Age2 + MaxTemp, data=survdat2)
anova(mod1_5, mod1_6) 
AICc(mod1_6)
AICc(mod1_5)
#Nope but we are getting closer

#How about Age?
mod1_7 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ MaxTemp, data=survdat2)
anova(mod1_6, mod1_7) #yes we need Age

#Do we need max temp?
mod1_8 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Age2, data=survdat2)

anova(mod1_6, mod1_8) #Yup
mam1 <- mod1_6




################################################
###############Does Min temperature predict survival
mod2 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*Year2*MinTemp, data=survdat2)
test.ph2 <- cox.zph(mod2) #fantastic! We are meeting all the assumptions
#Do we need the 3 way interaction? 
mod2_2 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*Year2+ Year2*MinTemp+ MinTemp*Age2, data=survdat2)
anova(mod2, mod2_2)
#Nope
#Do we need age:year?
mod2_3 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Year2*MinTemp+ MinTemp*Age2, data=survdat2)
anova(mod2_2, mod2_3)
#Nope 
#Do we need Year:MinTemp?
mod2_4 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Year2+ MinTemp*Age2, data=survdat2)
anova(mod2_4, mod2_3)
#Nope
#Do we need Mintemp:Age?
mod2_5 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Year2+ MinTemp + Age2, data=survdat2)
anova(mod2_4, mod2_5)
#Nope
#How about Age? 
mod2_6 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Year2+ MinTemp , data=survdat2)
anova(mod2_5, mod2_6)
#yes. 
#HOw about Year?
mod2_7 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~  MinTemp + Age2, data=survdat2)
anova(mod2_5, mod2_7) #Yes

#How about MinTemp?
mod2_8 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Year2+  Age2, data=survdat2)
anova(mod2_5, mod2_8) #Yes

mam2 <- mod2_5


#############################
######Does Mean temperature influence survival? 
mod3 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*Year2*MeanTemp, data=survdat2)
test.ph3 <- cox.zph(mod3) #fantastic! We are meeting all the assumptions

#Do we need the 3 way interaction?
mod3_2 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*Year2 + Age2*MeanTemp + MeanTemp*Year2, data=survdat2)
anova(mod3, mod3_2)
#nope 
#Do we need Year:Age?
mod3_3 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Age2*MeanTemp + MeanTemp*Year2, data=survdat2)
anova(mod3_2, mod3_3)
#Not a bit
#Do we need Year: MeanTemp?
mod3_4 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Age2*MeanTemp +Year2, data=survdat2)
anova(mod3_3, mod3_4)
#nope
#Do we need Age:MeanTemp?
mod3_5 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Age2 + MeanTemp +Year2, data=survdat2)
anova(mod3_4, mod3_5) 
#Nope but we are getting closer to something different. 

#Do we need Year?
mod3_6 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Age2 + MeanTemp , data=survdat2)
anova(mod3_5, mod3_6) 
#Yes we do need year
#DO we need Age?
mod3_7 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~  MeanTemp +Year2, data=survdat2)
anova(mod3_5, mod3_7) #Yup
#DO we need MeanTemp
mod3_8 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~  Age2 +Year2, data=survdat2)
anova(mod3_8, mod3_5) #Yes

mam3 <- mod3_5



######DOes whether it rains or not infuence survival? 
mod4 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*Year2*TotRain2, data=survdat2)
test.ph4 <- cox.zph(mod4) #fantastic! We are meeting all the assumptions
#Do we need a 3 way interaction?
mod4_2 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*Year2+ Year2*TotRain2 + TotRain2*Age2, data=survdat2)
anova(mod4, mod4_2) #nope not at all

#Do we need year:age?
mod4_3 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Year2*TotRain2 + TotRain2*Age2, data=survdat2)
anova(mod4_2, mod4_3) 
#Nope

#Do we need TotRain:Age?
mod4_4 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Year2*TotRain2 + Age2, data=survdat2)
anova(mod4_3, mod4_4)
#Nope

#Do we need Year:TotRain?
mod4_5 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Year2 + TotRain2 + Age2, data=survdat2)
anova(mod4_4, mod4_5)
#Nope

#DO we need TotRain?
mod4_6 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Year2 + Age2, data=survdat2)
anova(mod4_6, mod4_5)
#Nope

#Do we need Year?

mod4_7 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~  Age2, data=survdat2)
anova(mod4_6, mod4_7) #Yes

#Do we need Age?
mod4_8 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Year2, data=survdat2)
anova(mod4_6, mod4_8)
#Yes

mam4 <- mod4_6

##########################################
############# Does the combination of weather variables influence us mroe. 
mod5 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*Year2*PC1 + Age2*Year2*PC2, data=survdat2)
test.ph5 <- cox.zph(mod5) #fantastic! We are meeting all the assumptions

#Do we need the 3way interaction with Age:Year:PC2?
mod5_2 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*Year2*PC1 + Age2*PC2 + Year2*PC2, data=survdat2)
anova(mod5_2, mod5)
#not at all

#Do we need the other 3 way?
mod5_3 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*Year2 + Year2*PC1 + Age2*PC1 + Age2*PC2 + Year2*PC2, data=survdat2)
anova(mod5_2, mod5_3)
#Nope

#DO I need Age:Year?
mod5_4 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Year2*PC1 + Age2*PC1 + Age2*PC2 + Year2*PC2, data=survdat2)
anova(mod5_4, mod5_3)
#Nope


#Do we need Age:PC2?
mod5_5 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Year2*PC1 + Age2*PC1 +  Year2*PC2, data=survdat2)
anova(mod5_5, mod5_4)
#Nope



#DO we need Year:PC2?
mod5_6 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Year2*PC1 + Age2*PC1 +  PC2, data=survdat2)
anova(mod5_5, mod5_6)
#nope

#Do we need Year:PC1?
mod5_7 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Year2 + Age2*PC1 +  PC2, data=survdat2)
anova(mod5_6, mod5_7)
#Nope

#DO we need Age:PC1?
mod5_8 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Year2 + Age2 + PC1 +  PC2, data=survdat2)
anova(mod5_7, mod5_8)
#Nope


#Do we need PC2?
mod5_9 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Year2 + Age2 + PC1 , data=survdat2)
anova(mod5_8, mod5_9)
#Nope

#Do we need Age?
mod5_10 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Year2  + PC1 , data=survdat2)
anova(mod5_10, mod5_9)
#Yes

#DO we ned Year?
mod5_11 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Age2 + PC1 , data=survdat2)
anova(mod5_11, mod5_9)
#Yes

#DO we need PC1?
mod5_12 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Age2 + Year2 , data=survdat2)
anova(mod5_12, mod5_9)
#Very much so. 

mam5 <- mod5_9



AICc(mam1, mam2, mam3, mam4, mam5) 
#By far the best model is mam1 which uses maximum temperature and Age to predict survival
mam1
