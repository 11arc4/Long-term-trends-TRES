#Are nests surviving less than they used to when we include predated nests? 
#a cox-proportional hazards model
library(survival)

library(tidyverse)
library(MuMIn)


# # #Load in the nest level fledge data.
# dat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/Binary Fledge Success wo experimental nests.csv", as.is=T, na.strings = "" ) %>% filter(Daysabove18 <10)
# #
# # #Let's remove nests without dated that we need, and the predated nests
# Pred <- dat %>% filter( FledgeDate >HatchDate & !is.na(FledgeDate) & !is.na(HatchDate) )
# #
# #
# # ####NOW WE NEED TO CHANGE THE FORMAT OF THIS DATA SO THAT EACH DAY HAS A ENTRY FOR THE COXPH
# survdatP <-  as.data.frame(matrix(ncol=11, nrow=4000, NA))
# colnames(survdatP) <- c("NestID", "Year", "Time2", "Time1", "Age","PreStatus" ,"Status", "MaxTemp", "MeanTemp", "MinTemp", "TotRain" )
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
#   survdatP[a:b, 1:6]<- temp
#   a <- b+1
# }
# # #They want a true  for dead or false not dead
# survdatP$Status[which(survdatP$PreStatus==1)] <- F
# survdatP$Status[which(survdatP$PreStatus==0)] <- T
# 
# survdatP$Year2 <- survdatP$Year/10-197.5
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
#   c <- which(survdatP$Year==weather2$Year[i] & survdatP$Time1==weather2$JDate[i])
#   if(length(c)!=0){
#     survdatP$MaxTemp[c] <- weather2$MaxTemp[i]
#     survdatP$MeanTemp[c] <- weather2$MeanTemp[i]
#     survdatP$MinTemp[c] <- weather2$MinTemp[i]
#     survdatP$TotRain[c] <- weather2$TotRain[i]
#   }
# }
# 
# 
# 
# #YAY I made the data. It's ready for a coxPH analysis now.
# #I wonder if we should use Age2 (similar to Kennedy's growth and survival
# #analysis,  poikilothermic, intermediate, and endothermic)
# survdatP$Age2 <- NA
# survdatP$Age2[survdatP$Age<7]<- "Poikilotherm"
# survdatP$Age2[survdatP$Age>6 & survdatP$Age<9]<- "Intermediate"
# survdatP$Age2[survdatP$Age>8]<- "Endotherm"
# survdatP$Age2 <- factor(survdatP$Age2, levels=c("Poikilotherm", "Intermediate", "Endotherm"))
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
# survdatP$PC1 <- predict(weather.pca, survdatP[,8:11])[,1]
# survdatP$PC2 <- predict(weather.pca, survdatP[,8:11])[,2]
# 
# 
# weather2$PC1 <- predict(weather.pca, weather2[,c(3:5, 8)])[,1]
# weather2$PC2 <- predict(weather.pca, weather2[,c(3:5, 8)])[,2]
# 
# 
# 
# survdatP$TimePeriod <- NA
# survdatP$TimePeriod[which(survdatP$Year<1997)]<- "Growing"
# survdatP$TimePeriod[which(survdatP$Year>1996)]<- "Declining"
# survdatP$TimePeriod[which(survdatP$Year>2013)]<- "PostDecline"
# survdatP$TimePeriod <- factor(survdatP$TimePeriod, levels=c("Growing", "Declining", "PostDecline"))
# 
# survdatP2 <- survdatP %>% filter(!is.na(TotRain) & !is.na(MeanTemp))
# survdatP2$TotRain2 <- 0
# survdatP2$TotRain2[survdatP2$TotRain>0]<- 1 #Need to code it like this to let the model converge. 
# 
# 
# write.csv(survdatP, file= "file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/CoxPH survival datas WITH PREDATION.csv", na="", row.names = F)

survdat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/CoxPH survival datas WITH PREDATION.csv", na.strings="", as.is = T)
survdat$Age2 <- factor(survdat$Age2, levels=c("Poikilotherm", "Intermediate", "Endotherm"))
survdat$TimePeriod <- factor(survdat$TimePeriod, levels=c("Growing", "Declining", "PostDecline"))

survdat2 <- survdat %>% filter(!is.na(MaxTemp) & !is.na(PC1))


###########################
#Question 1: Has nest failure risk including risk from predation increased over time?
mod <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*Year2*TimePeriod, data=survdat2)
test.ph <- cox.zph(mod) #fantastic! We are meeting all the assumptions
plot(test.ph) #Couple aren't great but not soooooo bad
plot(resid(mod)~survdat2$Age2)
plot(resid(mod)~survdat2$Year2)
plot(resid(mod)~survdat2$TimePeriod)
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
  labs(x="Year", y="Nest Failure Risk", color="Nestling Age", fill="Nestling Age" )+ 
  ggthemes::theme_few(base_size = 16)

#Really this looks very similar to the graph not including predated nests. This
#is good and the consistancy matches with the binary analysis
mam


#####################################
#Question 2: Do any particulat weather condiditons predict whether nest failure is likely? 

#Mean temp?
mod2 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~MeanTemp*Age2*TimePeriod, data=survdat2)
test.ph <- cox.zph(mod) #fantastic! We are meeting all the assumptions
plot(test.ph)
plot(resid(mod2) ~ survdat2$Age2)
plot(resid(mod2) ~ survdat2$TimePeriod)
plot(resid(mod2) ~ survdat2$MeanTemp)

car::Anova(mod2)
dredge(mod2)

mam_meanTemp <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*MeanTemp + TimePeriod , data=survdat2)



###Max temp
mod3 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*MaxTemp*TimePeriod, data=survdat2)
test.ph <- cox.zph(mod3) #eh. not perfect but ok
plot(test.ph)
plot(resid(mod3) ~ survdat2$Age2)
plot(resid(mod3) ~ survdat2$TimePeriod)
plot(resid(mod3) ~ survdat2$MaxTemp)
dredge(mod3)
car::Anova(mod3)
mam_maxtemp <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2+TimePeriod*MaxTemp, data=survdat2)


#Does differences in min temp make the difference? 
mod4 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*TimePeriod*MinTemp, data=survdat2)
test.ph <- cox.zph(mod4) #good although one questional point for endotherms
plot(test.ph)
plot(resid(mod4) ~ survdat2$Age2)
plot(resid(mod4) ~ survdat2$TimePeriod)
plot(resid(mod4) ~ survdat2$MinTemp)
dredge(mod4)
mam_mintemp <- coxph(Surv(time=Time1, time2=Time2, event=Status)~TimePeriod + MinTemp*Age2, data=survdat2)




#Does total predicitation make the difference? I've coded it as raining or not
#raining to be consistant with the no pred analysis whih requires it. 
survdat2$Rain <- 0
survdat2$Rain[survdat2$TotRain>0]<- 1

mod5 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~TimePeriod*Rain*Age2, data=survdat2)
test.ph <- cox.zph(mod5) #endotherms aren't great but should be ok
plot(test.ph)
plot(resid(mod5) ~ survdat2$Age2)
plot(resid(mod5) ~ survdat2$TimePeriod)
plot(resid(mod5) ~ survdat2$Rain)
car::Anova(mod5)
dredge(mod5)

mam_rain <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2 + Rain, data=survdat2)



#Do all the weather parameters matter more than a single one? 
mod6 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*PC1*TimePeriod + Age2*PC2*TimePeriod, data=survdat2)
test.ph <- cox.zph(mod6) #mostly OK
plot(test.ph) #overall looks ok
plot(resid(mod6) ~ survdat2$Age2)
plot(resid(mod6) ~ survdat2$TimePeriod)
plot(resid(mod6) ~ survdat2$PC1)
plot(resid(mod6) ~ survdat2$PC2)
car::Anova(mod6)
dredge(mod6)

mam_PC <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*PC1 + PC2 + TimePeriod, data=survdat2)

mam


AICc(mam_maxtemp, mam_meanTemp, mam_mintemp, mam_PC, mam_rain)
#Max temp is easily the best predictor of whether nests fail or not
mam_maxtemp




newdata2 <- data.frame(MaxTemp=rep(seq(10.5, 35, length=40), 9),
                       Age2 = rep(c(rep("Poikilotherm", 40), rep("Intermediate", 40), rep("Endotherm", 40)), 3),
                       TimePeriod= c(rep("Growing", 120), rep("Declining", 120), rep("PostDecline", 120)),
                       predicted=NA, 
                       se=NA,
                       ucl=NA, 
                       lcl=NA)

newdata2$predicted<- predict(mam_maxtemp, newdata=newdata2, se.fit = T,  type="risk")[[1]]
newdata2$se<- predict(mam_maxtemp, newdata=newdata2, se.fit = T,  type="risk")[[2]]
newdata2$TimePeriod <- factor(newdata2$TimePeriod, levels=c("Growing", "Declining", "PostDecline"))

ggplot(newdata2, aes(x=MaxTemp, y=predicted))+
  geom_line(aes(color=Age2))+
  geom_ribbon(aes(ymin=predicted-se, ymax=predicted+se, fill=Age2), alpha=0.4)+
  labs(y="Nest Failure Risk", x="Max Temperature", fill="Nestling Age", color="Nestling Age")+
  ggthemes::theme_few(base_size=16)+
  facet_grid(~TimePeriod)










