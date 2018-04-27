#Are nests surviving less than they used to when we exclude predated nests? 
#a cox-proportional hazards model

library(survival)
library(MuMIn)
library(tidyverse)
# #Load in the nest level fledge data.
# dat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/Binary Fledge Success wo experimental nests.csv", as.is=T, na.strings = "" ) %>% filter(Daysabove18 <10)
# #
# # #Let's remove nests without dated that we need, and the predated nests
# NoPred <- dat %>% filter( FledgeDate >HatchDate & !is.na(FledgeDate) & !is.na(HatchDate) & (FailureCause2!="PREDATION"|is.na(FailureCause2) ) )
# #
# #
# # ####NOW WE NEED TO CHANGE THE FORMAT OF THIS DATA SO THAT EACH DAY HAS A ENTRY FOR THE COXPH
# survdatNP <-  as.data.frame(matrix(ncol=11, nrow=4000, NA))
# colnames(survdatNP) <- c("NestID", "Year", "Time2", "Time1", "Age","PreStatus" ,"Status", "MaxTemp", "MeanTemp", "MinTemp", "TotRain" )
# 
# a=0
# for (i in 1:nrow(NoPred)){
# 
#   temp <- data.frame(NestID=rep(NoPred$NestID[i]),
#                      Year=rep(NoPred$Year[i]),
#                      Time2=seq(NoPred$HatchDate[i]+1, NoPred$FledgeDate[i],1),
#                      Time1=seq(NoPred$HatchDate[i], NoPred$FledgeDate[i]-1,1),
#                      Age=seq(1, NoPred$FledgeDate[i]-NoPred$HatchDate[i],1),
#                      PreStatus=c(rep(x=1, times=NoPred$FledgeDate[i]-NoPred$HatchDate[i]-1), NoPred$Fledge2[i])
#   )
#   if(i==1){
#     a=1
#   }
#   b <- nrow(temp)+a-1
#   survdatNP[a:b, 1:6]<- temp
#   a <- b+1
# }
# # #They want a true  for dead or false not dead
# survdatNP$Status[which(survdatNP$PreStatus==1)] <- F
# survdatNP$Status[which(survdatNP$PreStatus==0)] <- T
# 
# survdatNP$Year2 <- survdatNP$Year/10-197.5
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
#  for(i in 1:nrow(weather2)){
#    c <- which(survdatNP$Year==weather2$Year[i] & survdatNP$Time1==weather2$JDate[i])
#    if(length(c)!=0){
#      survdatNP$MaxTemp[c] <- weather2$MaxTemp[i]
#      survdatNP$MeanTemp[c] <- weather2$MeanTemp[i]
#      survdatNP$MinTemp[c] <- weather2$MinTemp[i]
#      survdatNP$TotRain[c] <- weather2$TotRain[i]
#    }
#  }
# 
# 
# 
# #YAY I made the data. It's ready for a coxPH analysis now.
# #I wonder if we should use Age2 (similar to Kennedy's growth and survival
# #analysis,  poikilothermic, intermediate, and endothermic)
# survdatNP$Age2 <- NA
# survdatNP$Age2[survdatNP$Age<7]<- "Poikilotherm"
# survdatNP$Age2[survdatNP$Age>6 & survdatNP$Age<9]<- "Intermediate"
# survdatNP$Age2[survdatNP$Age>8]<- "Endotherm"
# survdatNP$Age2 <- factor(survdatNP$Age2, levels=c("Poikilotherm", "Intermediate", "Endotherm"))
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
# survdatNP$PC1 <- predict(weather.pca, survdatNP[,8:11])[,1]
# survdatNP$PC2 <- predict(weather.pca, survdatNP[,8:11])[,2]
# 
# 
# weather2$PC1 <- predict(weather.pca, weather2[,c(3:5, 8)])[,1]
# weather2$PC2 <- predict(weather.pca, weather2[,c(3:5, 8)])[,2]
# 
# 
# 
# survdatNP$TimePeriod <- NA
# survdatNP$TimePeriod[which(survdatNP$Year<1997)]<- "Growing"
# survdatNP$TimePeriod[which(survdatNP$Year>1996)]<- "Declining"
# survdatNP$TimePeriod[which(survdatNP$Year>2013)]<- "PostDecline"
# survdatNP$TimePeriod <- factor(survdatNP$TimePeriod, levels=c("Growing", "Declining", "PostDecline"))
# 
# survdatNP2 <- survdatNP %>% filter(!is.na(TotRain) & !is.na(MeanTemp))
# survdatNP2$TotRain2 <- 0
# survdatNP2$TotRain2[survdatNP2$TotRain>0]<- 1 #Need to code it like this to let the model converge.
# 
# 
# write.csv(survdatNP, file= "file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/CoxPH survival datas NO PREDATION.csv", na="", row.names = F)


survdat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/CoxPH survival datas NO PREDATION.csv", na.strings="", as.is = T)
survdat$Age2 <- factor(survdat$Age2, levels=c("Poikilotherm", "Intermediate", "Endotherm"))
survdat$TimePeriod <- factor(survdat$TimePeriod, levels=c("Growing", "Declining", "PostDecline"))

survdat2 <- survdat %>% filter(!is.na(MaxTemp) & !is.na(PC1))


###########################################
#Question 1: Has mortality risk (with predated nests excluded) increased over time?

mod <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*Year2*TimePeriod, data=survdat2)
test.ph <- cox.zph(mod) #Endotherms might be a problem.....
plot(test.ph) #This looks good overall
plot(resid(mod)~survdat2$Age2)
plot(resid(mod)~survdat2$Year2)
plot(resid(mod)~survdat2$TimePeriod)
plot(predict(mod)~resid(mod))


anova(mod, type="Chisq")
options(na.action="na.fail")
dredge(mod)

survdat2$Age2 <- factor(survdat2$Age2, levels=c( "Endotherm", "Intermediate","Poikilotherm" ))
survdat2$TimePeriod <- factor(survdat2$TimePeriod, levels=c( "Declining","Growing", "PostDecline"))
mam <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*Year2*TimePeriod, data=survdat2)
summary(mam)

4.070e+00*2.646e-01
2.588e+00*2.816e-01
2.816e+00*1.828e-01
7.599e+00*4.337e-01
newdata <- data.frame(Year2=rep(seq(0, 4.2, 0.1), 3),
                      TimePeriod=rep(c(rep("Growing", 22), rep("Declining", 17), rep("PostDecline", 4)),3),
                      Age2 = c(rep("Poikilotherm", 43), rep("Intermediate", 43), rep("Endotherm", 43)), 
                      predicted=NA, 
                      se=NA,
                      ucl=NA, 
                      lcl=NA)


newdata$predicted<- predict(mam, newdata=newdata, se.fit = T,  type="risk")[[1]]
newdata$se<- predict(mam, newdata=newdata, se.fit = T,  type="risk")[[2]]

PanelA <- ggplot(newdata, aes(x=Year2*10+1975, y=predicted, group=Age2))+
  geom_line( data=newdata %>% filter(TimePeriod=="Declining"))+
  geom_ribbon(aes(ymin=predicted-se, ymax=predicted+se, fill=Age2), alpha=0.6, data=newdata %>% filter(TimePeriod=="Declining") )+
  geom_line( data=newdata %>% filter(TimePeriod=="Growing"))+
  geom_ribbon(aes(ymin=predicted-se, ymax=predicted+se, fill=Age2), alpha=0.6, data=newdata %>% filter(TimePeriod=="Growing") )+
  geom_line( data=newdata %>% filter(TimePeriod=="PostDecline"))+
  geom_ribbon(aes(ymin=predicted-se, ymax=predicted+se, fill=Age2), alpha=0.6, data=newdata %>% filter(TimePeriod=="PostDecline") )+
  geom_vline(xintercept = c(1996.5, 2013.5 ))+
  labs(x="Year", y="Nest Failure Risk", fill="" )+ 
  ggthemes::theme_few(base_size = 16, base_family = "serif")+
  theme(legend.position = c(0.25, 0.85), legend.background = element_rect(fill=alpha('white', 0)), legend.text = element_text(size=10))+
  scale_fill_grey(labels=c("Poikilothermic (0-6 days)", "Intermediate (7-8 days)", "Homeothermic (9+ days)"), start=0.2, end=0.8)
  



#Based on our graph and the t values of the summary of mam
# Mortality differences between endotherms across time aren’t statistically
# significant (betas all aren’t different than 0). Intermediates really are
# increasingly at risk of death during the decline. Poikilotherms became more
# and more risky during the growth time period, but were more or less stable
# during the decline (although higher).



############################################
#Question 2: Do any weather variables predict mortality risk? What are the best weather variables for predicting mortality risk?

#############Max temp? 
survdat2$Age2 <- factor(survdat2$Age2, levels=c("Endotherm", "Intermediate", "Poikilotherm"))


mod_maxtemp <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*TimePeriod*MaxTemp, data=survdat2)
test.ph <- cox.zph(mod_maxtemp) #all looks good
plot(test.ph) #This looks good overall
plot(resid(mod_maxtemp)~survdat2$Age2)
plot(resid(mod_maxtemp)~survdat2$MaxTemp)
plot(resid(mod_maxtemp)~survdat2$TimePeriod)
plot(predict(mod_maxtemp)~resid(mod_maxtemp))

anova(mod_maxtemp)
dredge(mod_maxtemp)
mam_maxtemp <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2+ MaxTemp, data=survdat2)
#higher max temp= less mortality risk, Intermediates and endotherms are more affected by temperature though. 
#THis agrees well with our binary results!
summary(mam_maxtemp)

0.81129*0.14150
0.51179*0.12849
0.86820*0.01041
############Min temp?
mod_mintemp <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*TimePeriod*MinTemp, data=survdat2)
test.ph <- cox.zph(mod_mintemp) #all looks good
plot(test.ph) #This looks good overall
plot(resid(mod_mintemp)~survdat2$Age2)
plot(resid(mod_mintemp)~survdat2$MaxTemp)
plot(resid(mod_mintemp)~survdat2$TimePeriod)
plot(predict(mod_mintemp)~resid(mod_mintemp))
#all good

anova(mod_mintemp)
dredge(mod_mintemp)
mam_mintemp <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2* MinTemp, data=survdat2)
#higher min temp= less mortality risk

#############Mean temp?
mod_meantemp <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*TimePeriod*MeanTemp, data=survdat2)
test.ph <- cox.zph(mod_meantemp) #all looks good
plot(test.ph) #This looks good overall
plot(resid(mod_meantemp)~survdat2$Age2)
plot(resid(mod_meantemp)~survdat2$MeanTemp)
plot(resid(mod_meantemp)~survdat2$TimePeriod)
plot(predict(mod_meantemp)~resid(mod_meantemp))

anova(mod_meantemp)
dredge(mod_meantemp)
mam_meantemp <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*MeanTemp, data=survdat2)
#Higher mean temp = less mortality, especially for poikilotherms


##############Tot Rain?
mod_totrain <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*TimePeriod*TotRain, data=survdat2)
test.ph <- cox.zph(mod_totrain) #maybe iffy for endotherms
plot(test.ph) #This looks OK but not great
plot(resid(mod_totrain)~survdat2$Age2)
plot(resid(mod_totrain)~survdat2$TotRain)
plot(resid(mod_totrain)~survdat2$TimePeriod)
plot(predict(mod_totrain)~resid(mod_totrain))
#Maybe not tip top. 

#what if we do Rain/NoRain
survdat2$Rain <- 0
survdat2$Rain[survdat2$TotRain>0]<- 1

mod_rain <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*TimePeriod*Rain, data=survdat2)
test.ph <- cox.zph(mod_rain) #I think this is slightly better
plot(test.ph) #much better
plot(resid(mod_rain)~survdat2$Age2)
plot(resid(mod_rain)~survdat2$Rain)
plot(resid(mod_rain)~survdat2$TimePeriod)
plot(predict(mod_rain)~resid(mod_rain))
#I think this model fits better. 

anova(mod_rain)
dredge(mod_rain)
mam_rain <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2+ Rain, data=survdat2)
#More likely to fail if it rained the previous day


##############a combination via PCs? 
mod_PC <- coxph(Surv(time=Time1, time2=Time2, event=Status)~Age2*TimePeriod*PC1 + Age2*TimePeriod*PC2, data=survdat2)
test.ph <- cox.zph(mod_PC) #Looks good
plot(test.ph) 
plot(resid(mod_PC)~survdat2$Age2)
plot(resid(mod_PC)~survdat2$PC1)
plot(resid(mod_PC)~survdat2$PC2)
plot(resid(mod_PC)~survdat2$TimePeriod)
plot(predict(mod_PC)~resid(mod_PC))
#looks like it fits

anova(mod_PC)
dredge(mod_PC)

mam_PC <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Age2+ PC1 + PC2, data=survdat2)


#So which of our weather variables is the best predictor? 

AICc(mam_maxtemp, mam_mintemp, mam_meantemp, mam_rain, mam_PC)
# Oh wow. Max temperature is actually better
#than year and all that at predicting nest failure. This is fantastic and in
#complete agreemment with the binary results.



newdata2 <- data.frame(MaxTemp=rep(seq(10.5, 35, length=40), 3),
                      Age2 = c(rep("Poikilotherm", 40), rep("Intermediate", 40), rep("Endotherm", 40)), 
                      predicted=NA, 
                      se=NA,
                      ucl=NA, 
                      lcl=NA)

newdata2$predicted<- predict(mam_maxtemp, newdata=newdata2, se.fit = T,  type="risk")[[1]]
newdata2$se<- predict(mam_maxtemp, newdata=newdata2, se.fit = T,  type="risk")[[2]]

PanelB <- ggplot(newdata2, aes(x=MaxTemp, y=predicted, group=Age2))+
  geom_line()+
  geom_ribbon(aes(ymin=predicted-se, ymax=predicted+se, fill=Age2), alpha=0.6)+
  labs(y="Nest Failure Risk", x=expression('Max Temperature ('*degree*C*')'), fill="", color="")+
  ggthemes::theme_few(base_size = 16, base_family = "serif")+
  theme(legend.position = c(0.75, 0.85), legend.background = element_rect(fill=alpha('white', 0)), legend.text = element_text(size=10))+
  scale_fill_grey(labels=c("Poikilothermic (0-6 days)", "Intermediate (7-8 days)", "Homeothermic (9+ days)"), start=0.2, end=0.8)

library(cowplot)
plot_grid(PanelA, PanelB,nrow=2, ncol=1, labels=c("a", "b"), label_size = 20, label_fontfamily = "serif")
ggsave(filename='~/Masters Thesis Project/Long term trends paper/Plots for paper/Supplemental Fledging plot.jpeg', width=5, height=6, units="in", device="jpeg")


############################
#Question 3: Has the mean temperature experienced by nestlings during these time periods gone down? 

YearSummary <- survdat2 %>% 
  group_by(Year2, TimePeriod, Age2) %>% 
  summarise(MeanMaxTemp=mean(MaxTemp),
            Daysabove18_2 = NA,
            Year=NA) 



ggplot(YearSummary, aes(x=Year2*10+1975, y=MeanMaxTemp))+
  geom_point()+
  facet_grid(~Age2)+
  geom_smooth(method="lm")+
  labs(x="Year", y="Mean Maximum \nDaily Temperature")+
  ggthemes::theme_few(base_size = 16)

mod_weather <- lm(MeanMaxTemp~Year2*TimePeriod*Age2, data=YearSummary)
hist(resid(mod_weather))
plot(mod_weather) #fits
plot(resid(mod_weather)~YearSummary$Year2)
plot(resid(mod_weather)~YearSummary$TimePeriod)
plot(resid(mod_weather)~YearSummary$Age2)
#looks OK

anova(mod_weather, test="F")
dredge(mod_weather)
YearSummary$Year <- YearSummary$Year2*10+1975

mam_weather <- lm(MeanMaxTemp~Year2+Age2, data=YearSummary)
summary(mam_weather)
