#Does risk of predation increase during the period of decline in a way that can
#explain the confusing results we were seeing earlier?

library(survival)
library(tidyverse)
library(MuMIn)


# # #Load in the nest level fledge data.
# dat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/Binary Fledge Success wo experimental nests.csv", as.is=T, na.strings = "" ) %>% filter(Daysabove18 <10)
# #
# # #Let's remove nests without dated that we need.
# #make a dataset containing only nests that weren't predated and nests that were
# #predated. Remove all other causes of death because they obscure whether the
# #nest would ultimately have been depredated
# Pred <- dat %>% filter( FledgeDate >HatchDate & !is.na(FledgeDate) & !is.na(HatchDate) &  (Fledge2==1 | (FailureCause2=="PREDATION" & Fledge2==0)))
# #
# #
# # ####NOW WE NEED TO CHANGE THE FORMAT OF THIS DATA SO THAT EACH DAY HAS A ENTRY FOR THE COXPH
# depreddat <-  as.data.frame(matrix(ncol=11, nrow=4000, NA))
# colnames(depreddat) <- c("NestID", "Year", "Time2", "Time1", "Age","PreStatus" ,"Status", "MaxTemp", "MeanTemp", "MinTemp", "TotRain" )
# 
# a=0
# for (i in 1:nrow(Pred)){
# 
#   temp <- data.frame(NestID=rep(Pred$NestID[i]),
#                      Year=rep(Pred$Year[i]),
#                      Time2=seq(Pred$HatchDate[i]+1, Pred$FledgeDate[i],1),
#                      Time1=seq(Pred$HatchDate[i], Pred$FledgeDate[i]-1,1),
#                      Age=seq(1, Pred$FledgeDate[i]-Pred$HatchDate[i],1),
#                      PreStatus=c(rep(x=1, times=Pred$FledgeDate[i]-Pred$HatchDate[i]-1), Pred$Fledge2[i])
#   )
#   if(i==1){
#     a=1
#   }
#   b <- nrow(temp)+a-1
#   depreddat[a:b, 1:6]<- temp
#   a <- b+1
# }
# # #They want a true  for Predated or false not alive (not predated)
# depreddat$Status[which(depreddat$PreStatus==1)] <- F
# depreddat$Status[which(depreddat$PreStatus==0)] <- T
# 
# depreddat$Year2 <- depreddat$Year/10-197.5
# 
# 
# weather_pre <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Hartington IHD Weather Station Daily Data 1975 to  2017.csv", as.is=T)
# weather <- weather_pre[26:nrow(weather_pre), c(1,2,6,8,10,12,14,16,18)]
# rm(weather_pre)
# colnames(weather) <- c("Date", "Year",
#                        "MaxTemp", "MinTemp", "MeanTemp", "HeatDegDays", "CoolDegDays", "TotRain", "TotPrecip")
# 
# weather$JDate <- lubridate::yday(as.Date(weather$Date, format="%m/%d/%Y"))
# weather$MeanTemp <- as.numeric(weather$MeanTemp)
# weather$MaxTemp <- as.numeric(weather$MaxTemp)
# weather$MinTemp <- as.numeric(weather$MinTemp)
# weather$TotRain <- as.numeric(weather$TotRain)
# 
# weather2 <- weather %>% filter(JDate>139 & JDate<216)
# 
# for(i in 1:nrow(weather2)){
#   c <- which(depreddat$Year==weather2$Year[i] & depreddat$Time1==weather2$JDate[i])
#   if(length(c)!=0){
#     depreddat$MaxTemp[c] <- weather2$MaxTemp[i]
#     depreddat$MeanTemp[c] <- weather2$MeanTemp[i]
#     depreddat$MinTemp[c] <- weather2$MinTemp[i]
#     depreddat$TotRain[c] <- weather2$TotRain[i]
#   }
# }
# 
# 
# 
# #YAY I made the data. It's ready for a coxPH analysis now.
# #I wonder if we should use Age2 (similar to Kennedy's growth and survival
# #analysis,  poikilothermic, intermediate, and endothermic)
# depreddat$Age2 <- NA
# depreddat$Age2[depreddat$Age<7]<- "Poikilotherm"
# depreddat$Age2[depreddat$Age>6 & depreddat$Age<9]<- "Intermediate"
# depreddat$Age2[depreddat$Age>8]<- "Endotherm"
# depreddat$Age2 <- factor(depreddat$Age2, levels=c("Poikilotherm", "Intermediate", "Endotherm"))
# 
# 
# library(stats)
# weatherVar <- weather2[,c(3:5, 8)]
# #we are going to use all the weather variables because that's what we did in the
# #other analyses and it's important to be consistant, even thoguh the results are
# #no different either way
# 
# weather.pca <- prcomp(na.omit(weatherVar),
#                       center=T,
#                       scale=T)
# 
# plot(weather.pca, type="lines")
# summary(weather.pca)
# 
# depreddat$PC1 <- predict(weather.pca, depreddat[,8:11])[,1]
# depreddat$PC2 <- predict(weather.pca, depreddat[,8:11])[,2]
# 
# 
# weather2$PC1 <- predict(weather.pca, weather2[,c(3:5, 8)])[,1]
# weather2$PC2 <- predict(weather.pca, weather2[,c(3:5, 8)])[,2]
# 
# 
# 
# depreddat$TimePeriod <- NA
# depreddat$TimePeriod[which(depreddat$Year<1992)]<- "Growing"
# depreddat$TimePeriod[which(depreddat$Year>1991)]<- "Declining"
# depreddat$TimePeriod[which(depreddat$Year>2013)]<- "PostDecline"
# depreddat$TimePeriod <- factor(depreddat$TimePeriod, levels=c("Growing", "Declining", "PostDecline"))
# 
# 
# 
# write.csv(depreddat, file= "file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/CoxPH depredation data.csv", na="", row.names = F)



depreddat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/CoxPH depredation data.csv", na.strings="", as.is = T)
depreddat$Age2 <- factor(depreddat$Age2, levels=c("Poikilotherm", "Intermediate", "Endotherm"))
depreddat$TimePeriod <- factor(depreddat$TimePeriod, levels=c("Growing", "Declining", "PostDecline"))

depreddat2 <- depreddat %>% filter(!is.na(MaxTemp) & !is.na(PC1))
depreddat2$TotRain2 <- 0
depreddat2$TotRain2[depreddat2$TotRain>0]<- 1 #Need to code it like this to let the model converge.







###########################Question 1: Has nest predation risk  increased over time?

#I dropped the 3 way interaction
#SE is WILD. I think this analysis really isn't going to work. We have too many groups, and too few depredations. 
#We think that risk of depredation is higher for intermediates with time but it's a bit unclear still. I will 
mod <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*TimePeriod + Year2*TimePeriod, data=depreddat2)
test.ph <- cox.zph(mod) #not perfect
plot(test.ph) #I think this is workable
plot(resid(mod)~depreddat2$Age2)
plot(resid(mod)~depreddat2$Year2)
plot(resid(mod)~depreddat2$TimePeriod)
plot(predict(mod)~resid(mod))
#This looks like it fits ok. Not perfect, but fits ok


dredge(mod)
#Best mod is the full mod

mam <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2 + Year2*TimePeriod, data=depreddat2)
summary(mam)


newdata <- data.frame(Year2=rep(seq(0, 4.2, 0.1), 3),
                      TimePeriod=rep(c(rep("Growing", 22), rep("Declining", 17), rep("PostDecline", 4)),3),
                      Age2 = c(rep("Poikilotherm", 43), rep("Intermediate", 43), rep("Endotherm", 43)), 
                      predicted=NA, 
                      se=NA,
                      use=NA, 
                      lse=NA)

newdata$predicted<- predict(mam, newdata=newdata, se.fit = T,  type="risk")[[1]]
newdata$se<- predict(mam, newdata=newdata, se.fit = T,  type="risk")[[2]]

ggplot(newdata, aes(x=Year2*10+1975, y=predicted))+
  geom_line(aes(color=Age2), data=newdata %>% filter(TimePeriod=="Declining"))+
  geom_ribbon(aes(ymin=predicted-se, ymax=predicted+se, fill=Age2), alpha=0.2, data=newdata %>% filter(TimePeriod=="Declining") )+
  geom_line(aes(color=Age2), data=newdata %>% filter(TimePeriod=="Growing"))+
  geom_ribbon(aes(ymin=predicted-se, ymax=predicted+se, fill=Age2), alpha=0.2, data=newdata %>% filter(TimePeriod=="Growing") )+
  geom_line(aes(color=Age2), data=newdata %>% filter(TimePeriod=="PostDecline"))+
  geom_ribbon(aes(ymin=predicted-se, ymax=predicted+se, fill=Age2), alpha=0.2, data=newdata %>% filter(TimePeriod=="PostDecline") )+
  geom_vline(xintercept = c(1996.5, 2013.5 ))+
  labs(x="Year", y="Nest Predation Risk", color="Nestling Age", fill="Nestling Age" )+
  ggthemes::theme_few(base_size = 16)



###########################Question 2: Does nest predation risk differ across environmental conditions?
#Max Temp
mod_max <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*TimePeriod*MaxTemp, data=depreddat2)

test.ph <- cox.zph(mod_max) #fantastic! We are meeting all the assumptions
plot(test.ph)
plot(resid(mod_max) ~ depreddat2$Age2)
plot(resid(mod_max) ~ depreddat2$TimePeriod)
plot(resid(mod_max) ~ depreddat2$MaxTemp)

car::Anova(mod_max)
dredge(mod_max)
mam_max <-coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2 +TimePeriod*MaxTemp, data=depreddat2)



#Mean Temp
mod_mean <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*TimePeriod*MeanTemp, data=depreddat2)

test.ph <- cox.zph(mod_mean) #fantastic! We are meeting all the assumptions
plot(test.ph)
plot(resid(mod_mean) ~ depreddat2$Age2)
plot(resid(mod_mean) ~ depreddat2$TimePeriod)
plot(resid(mod_mean) ~ depreddat2$MeanTemp)

dredge(mod_mean)

mam_mean <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2+TimePeriod+MeanTemp, data=depreddat2)
summary(mam_mean)
#Higher risk of predation in warmer mean temps, higher risk during decline


#MinTemp 
mod_min <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*TimePeriod*MinTemp, data=depreddat2)
test.ph <- cox.zph(mod_min) #fantastic! We are meeting all the assumptions
plot(test.ph)
plot(resid(mod_min) ~ depreddat2$Age2)
plot(resid(mod_min) ~ depreddat2$TimePeriod)
plot(resid(mod_min) ~ depreddat2$MeanTemp)

dredge(mod_min)


mam_min <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2+TimePeriod+MinTemp, data=depreddat2)
summary(mam_min)
#decline has higher risk, but weather is a lousy predictor



#Raining or not raining? 
depreddat2$Rain <- 0
depreddat2$Rain[depreddat2$TotRain>0] <- 1
depreddat2$Rain <- factor(depreddat2$Rain)

mod_rain <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*TimePeriod*Rain, data=depreddat2)
test.ph <- cox.zph(mod_rain) #fantastic! We are meeting all the assumptions
plot(test.ph)
plot(resid(mod_rain) ~ depreddat2$Age2)
plot(resid(mod_rain) ~ depreddat2$TimePeriod)
plot(resid(mod_rain) ~ depreddat2$Rain)
#Looks good. 
Anova(mod_rain)
dredge(mod_rain)

mam_rain <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2+TimePeriod*Rain, data=depreddat2)


#PCs?
mod_PC <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*PC1*TimePeriod + Age2*PC2*TimePeriod, data=depreddat2)
test.ph <- cox.zph(mod6) #mostly OK
plot(test.ph) #overall looks ok
plot(resid(mod_PC) ~ depreddat2$Age2)
plot(resid(mod_PC) ~ depreddat2$TimePeriod)
plot(resid(mod_PC) ~ depreddat2$PC1)
plot(resid(mod_PC) ~ depreddat2$PC2)


dredge(mod_PC)
mam_PC <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2+ PC1*TimePeriod + PC2*TimePeriod, data=depreddat2)

AICTable <- AICc(mam_max, mam_mean, mam_min, mam_rain, mam_PC)
AICTable$Delta <- AICTable$AICc - min(AICTable$AICc)
mam_max




newdata2 <- data.frame(MaxTemp=rep(seq(10.5, 35, length=40), 9),
                       Age2 = rep(c(rep("Poikilotherm", 40), rep("Intermediate", 40), rep("Endotherm", 40)), 3),
                       TimePeriod= c(rep("Growing", 120), rep("Declining", 120), rep("PostDecline", 120)),
                       predicted=NA, 
                       se=NA,
                       ucl=NA, 
                       lcl=NA)

newdata2$predicted<- predict(mam_max, newdata=newdata2, se.fit = T,  type="risk")[[1]]
newdata2$se<- predict(mam_max, newdata=newdata2, se.fit = T,  type="risk")[[2]]
newdata2$TimePeriod <- factor(newdata2$TimePeriod, levels=c("Growing", "Declining", "PostDecline"))

ggplot(newdata2, aes(x=MaxTemp, y=predicted))+
  geom_line(aes(color=Age2))+
  geom_ribbon(aes(ymin=predicted-se, ymax=predicted+se, fill=Age2), alpha=0.4)+
  labs(y="Nest Predation Risk", x="Max Temperature", fill="Nestling Age", color="Nestling Age")+
  ggthemes::theme_few(base_size=16)+
  facet_grid(~TimePeriod)

