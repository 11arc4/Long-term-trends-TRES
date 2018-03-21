#Does local weather the month after fledging predict whether fledglings recruit successfully? 


library(dplyr)
library(RMark)
library(ggplot2)
#Fledglings are trying to feed themselves, and mothers are trying to feed them
#as well for the first 2 weeks at least. Thats energetically demanding and might
#reduce survival

#Load in the data (already filtering out the males, and half of the nestlings for each year that we guess were male)
datMark <- readRDS("~/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis/Female Fledgling MARK Data.rda")


#Load in average fledge dates. These fledge dates are based on the average
#fledging date of successful nests (NOT including failure dates) because nest
#failures might renest, and definitely wouldnt affect the weather juvenile birds
#are experiencing as they try to learn to feed themselves. 

fledgeDat <- as.data.frame(matrix(nrow=5506, ncol=4))
colnames(fledgeDat) <- c("Year", "NestID", "FledgeSize", "FledgeDate")

i=0
for(nest in as.list(globalData$nests)){
  i=i+1
  fledgeDat$Year[i] <- nest$year
  fledgeDat$NestID[i] <- nest$siteID
  fledgeDat$FledgeSize[i] <- nest$fledgeSize
  fledgeDat$FledgeDate[i] <- nest$fledgeDate
}

saveRDS(fledgeDat, "~/Masters Thesis Project/Long term trends paper/Data Files_long term trends/Fledge Success.rda")


fledgeDat<- readRDS("~/Masters Thesis Project/Long term trends paper/Data Files_long term trends/Fledge Success.rda")

#Manipulate this data to get yearly estimates
YearlyFledge <- fledgeDat %>% 
  filter(!is.na(FledgeDate) & FledgeSize>0) %>% 
  group_by(Year) %>% 
  summarise(meanFledgeDate= mean(FledgeDate))

#We have NO fledge dates for 1999, 2006, or 2007. I will put in the mean fledge date for that year
YearlyFledge <- rbind(YearlyFledge, 
                      c(1999, mean(YearlyFledge$meanFledgeDate)), 
                      c(2006, mean(YearlyFledge$meanFledgeDate)), 
                      c(2007, mean(YearlyFledge$meanFledgeDate)))


ggplot(YearlyFledge, aes(x=Year, y=meanFledgeDate))+
  geom_point()+
  geom_smooth(method="lm")
#Fledge date is getting earlier.  


weather <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Hartington IHD Weather Station Daily Data 1975 to  2017.csv", as.is=T)
weather2 <- weather[26:nrow(weather), c(1,2,6,8,10,12,14,16,18)]

colnames(weather2)<- weather[25,c(1,2,6,8,10,12,14,16,18)]
  
colnames(weather2) <- c("Date", "Year",  
                       "MaxTemp", "MinTemp", "MeanTemp", "HeatDegDays", "CoolDegDays", "TotRain", "TotPrecip") 

weather2$JDate <- lubridate::yday(as.Date(weather2$Date, format="%m/%d/%Y"))
weather2$MeanTemp <- as.numeric(weather2$MeanTemp)
weather2$MaxTemp <- as.numeric(weather2$MaxTemp)
weather2$TotRain <- as.numeric(weather2$TotRain)


YearlyFledge$MeanTemp <- c(NA)
YearlyFledge$MinTemp <- c(NA)
YearlyFledge$DaysBelow18_mean <- c(NA)
YearlyFledge$DaysBelow18_max <- c(NA)


for(i in 1:nrow(YearlyFledge)){
  rows <- which(YearlyFledge$Year[i]==weather2$Year &
                  YearlyFledge$meanFledgeDate[i]<=weather2$JDate & 
                  YearlyFledge$meanFledgeDate[i]+ 28 >= weather2$JDate)[1:28]
  YearlyFledge$MeanTemp[i] <- mean(weather2$MeanTemp[rows], na.rm=T)
  YearlyFledge$MinTemp[i] <- min(weather2$MinTemp[rows], na.rm=T)
  YearlyFledge$DaysBelow18_mean[i] <- sum(weather2$MeanTemp[rows]<18 | weather2$TotRain[rows]>0)
  YearlyFledge$DaysBelow18_max[i] <- sum(weather2$MaxTemp[rows]<18 | weather2$TotRain[rows]>0) 
}


#2011 is problematic. There is no data for that year because apparently all the
#weather stations were out. I was able to find some weather data from the
#Kingston Climate weather station. It's hourly, so slightly more detailed that
#Hartington court but we'll go for it anyway

#Because of this weather I am going to have to use only local temperature. That
#was the best predictor for nestlings so is probably good, but of course might
#not be the best.
June2011 <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/June 2011 Kingston Climate hourly data.csv", as.is=T)
June2011 <- June2011[17:nrow(June2011),c(1,2,3,4,7) ]
colnames(June2011) <- c("Date", "Year", "Month", "Day", "Temp")

July2011 <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/July 2011 Kingston Climate hourly data.csv", as.is=T)
July2011 <- July2011[17:nrow(July2011),c(1,2,3,4,7) ]
colnames(July2011) <- c("Date", "Year", "Month", "Day", "Temp")


Summer2011 <- rbind(June2011, July2011)

Summer2011$Date <- paste(Summer2011$Month, Summer2011$Day, Summer2011$Year, sep="/")
Summer2011$JDate <- lubridate::yday(as.Date(Summer2011$Date, format="%m/%d/%Y"))


Summer2011$Temp <- as.numeric(Summer2011$Temp)

Summer2011_2 <- Summer2011 %>% group_by(JDate, Year) %>% summarise(MeanTemp= mean(Temp, na.rm=T), 
                                                             MinTemp=min(Temp, na.rm=T), 
                                                             MaxTemp= max(Temp, na.rm=T))  



#Fill in 2011 with the Kingston data.
YearlyFledge$MeanTemp[34] <- mean(Summer2011_2$MeanTemp[which(2011==Summer2011_2$Year &
                                                           YearlyFledge$meanFledgeDate[34]<=Summer2011_2$JDate & 
                                                           YearlyFledge$meanFledgeDate[34]+ 28 >=Summer2011_2$JDate)], na.rm=T)

YearlyFledge$MinTemp[34] <- min(Summer2011_2$MinTemp[which(2011==Summer2011_2$Year &
                                                        YearlyFledge$meanFledgeDate[34]<=Summer2011_2$JDate & 
                                                        YearlyFledge$meanFledgeDate[34]+ 28 >= Summer2011_2$JDate)], na.rm=T)
YearlyFledge$DaysBelow18_mean[34] <- sum(Summer2011_2$MeanTemp[which(2011==Summer2011_2$Year &
                                                             YearlyFledge$meanFledgeDate[34]<=Summer2011_2$JDate & 
                                                             YearlyFledge$meanFledgeDate[34]+ 28 >= Summer2011_2$JDate)]<18 |
                                      Summer2011_2$TotRain[which(2011==Summer2011_2$Year &
                                                                    YearlyFledge$meanFledgeDate[34]<=Summer2011_2$JDate & 
                                                                    YearlyFledge$meanFledgeDate[34]+ 28 >= Summer2011_2$JDate)]>0)
YearlyFledge$DaysBelow18_mean[34] <- NA


#Clean up environment
rm(list=(ls()[ls()!=c("datMark", "YearlyFledge")]))


saveRDS(YearlyFledge, "~/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis/LocalWeather")

###Begining the analysis part, where we see if temperature post fledging predicts survival. 
setwd("~/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis")
#I've created multiple groups based on sex and at what age we first saw the
#bird. Also set the initial year to 1975.

# We're going to want a CJS #model because our data is recapture data, and we
# can't tell whether you've actually died. 
tsprocess <-process.data(datMark,model="CJS",
                         begin.time=1975, 
                         groups= ("age"),
                         initial.ages =c(0,1, 2))
ts.ddl<- make.design.data(tsprocess, parameters=list(Phi=list(age.bins=c(0, 0.8, 1.8, 44)),
                                                     p=list(age.bins=c(0, 0.8, 1.8, 44))))
#set up the population status variable
ts.ddl$Phi$popStatus<- rep(NA)
ts.ddl$Phi$popStatus[which(ts.ddl$Phi$Time<=21)]<- 0  #1975-1996 increased
ts.ddl$Phi$popStatus[which(ts.ddl$Phi$Time>21)]<- 1 #1997-2014 decreased

#Add in Mean temperature data for the year
ts.ddl$Phi$MeanTemp<- rep(NA)
ts.ddl$Phi$DaysBelow18 <- rep(NA)
for (i in 1:nrow(ts.ddl$p)){
  ts.ddl$Phi$MeanTemp[i] <- as.numeric (YearlyFledge$MeanTemp[which(YearlyFledge$Year==ts.ddl$Phi$time[i])])
  ts.ddl$Phi$DaysBelow18[i] <- as.numeric (YearlyFledge$DaysBelow18[which(YearlyFledge$Year==ts.ddl$Phi$time[i])])
  
}


#Take the best model from pure female recruitment over time, and add the
#environmental covariate of interest to that we are interested in how local
#temperatures are affecting post fledging survival. We could be affected by mean
#temp post fledging or perhaps how much cold days there are?

p.timeage <- list(formula= ~time + age)
Phi.1<- list(formula=~popStatus*age)
Phi.2<- list(formula=~popStatus*age + MeanTemp)
Phi.3<- list(formula=~popStatus*age + MeanTemp*age)
Phi.4<- list(formula=~popStatus*age + MeanTemp*popStatus)
Phi.5<- list(formula=~popStatus*age*MeanTemp)
Phi.6<- list(formula=~age*MeanTemp)




cml <- create.model.list("CJS")
results <- mark.wrapper(cml, data=tsprocess, ddl=ts.ddl, output=F, adjust=F)

export.MARK(tsprocess, "FemaleNestlings_meantemp", results, replace=T)
#Median Chat on the global is 1.17

adjustedresults <- adjust.chat(results, chat=1.17)






#What about if what's really important isn't mean temp but rather how many cold lousy days there are?
Phi.7<- list(formula=~popStatus*age)
Phi.8<- list(formula=~popStatus*age + DaysBelow18)
Phi.9<- list(formula=~popStatus*age + DaysBelow18*age)
Phi.10<- list(formula=~popStatus*age + DaysBelow18*popStatus)
Phi.11<- list(formula=~popStatus*age*DaysBelow18)
Phi.12<- list(formula=~age*DaysBelow18)
results


export.MARK(tsprocess, "FemaleNestlings_DaysBelow18", results, replace=T)
#median chat= 1.17 which is the same as Mean Temp, so I think I'm goldent to
#adjust chat together for all of them. Let's put all of these things together
#and compare them.


cml <- create.model.list("CJS")
results <- mark.wrapper(cml, data=tsprocess, ddl=ts.ddl, output=F, adjust=F)
adjustedresults <- adjust.chat(results, chat=1.17)

#There is literally no differences between DaysBelow18 and MeanTemp models.
#Daysbelow18 looks slightly better, but they're ultimately indistinguishable,
#probably because mean temp and days below 18 are tightly correlated.




#The clear winner is model 4, where the effect of MeanTemp is not age dependent
#but does depend on popStatus


bestmod <- adjustedresults[1]$Phi.10

min1 <- min(YearlyFledge$DaysBelow18[which(YearlyFledge$Year>1974 & YearlyFledge$Year<1997)])
max1 <- max(YearlyFledge$DaysBelow18[which(YearlyFledge$Year>1974 & YearlyFledge$Year<1997)])

min2 <- min(YearlyFledge$DaysBelow18[which(YearlyFledge$Year>1996)], na.rm=T)
max2 <- max(YearlyFledge$DaysBelow18[which(YearlyFledge$Year>1996)], na.rm=T)


newdata <- data.frame(age=c(rep(0, 25), rep(1, 25), rep(2, 25)), 
                      popStatus=c(rep(0, 75), rep(1, 75)), 
                      DaysBelow18=c(rep(seq(min1, max1, length.out = 25), 3),rep(seq(min2, max2, length.out = 25), 3)), 
                      estimate=NA, 
                      se=NA, 
                      lcl=NA, 
                      ucl=NA)

#recruitment predictions in increase
newdata[1:25, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[1,,drop=FALSE],"Phi", replicate=TRUE,data=data.frame(DaysBelow18=seq(min1, max1, length.out = 25)))[,15:18] 
#SY return predictions in increase
newdata[26:50, 4:7 ] <-predict_real(bestmod,ts.ddl$Phi[2,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(DaysBelow18=seq(min1, max1, length.out = 25)))[,15:18]  
#ASY Return predictions in increase
newdata[51:75, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[3,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(DaysBelow18=seq(min1, max1, length.out = 25)))[,15:18] 
#recruitment predictions in decline
newdata[76:100, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[694,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(DaysBelow18=seq(min1, max1, length.out = 25))) [,15:18]
#SY Return predictions in decline
newdata[101:125, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[1804,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(DaysBelow18=seq(min1, max1, length.out = 25)))[,15:18] 
#ASY Return predictions in decline
newdata[126:150, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[23,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(DaysBelow18=seq(min1, max1, length.out = 25)))[,15:18] 

TimePeriodNames <- c("0"="Growing", 
                     "1"="Declining")

newdata$popStatus <- factor(newdata$popStatus)
newdata$age <- factor(newdata$age)

ggplot(newdata, aes(x=DaysBelow18, y=estimate, fill=age))+
  geom_line(aes(color=age))+
  facet_grid(~popStatus, labeller = as_labeller(TimePeriodNames))+
  
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.2)+
  labs(x=expression("Days below "*18~degree*C), y="Survival overwinter", fill="Age", color="Age")+
  scale_fill_manual(labels=c("Recruitment", "1-year-old return", "Older female return"), 
                    values=c("azure4", "burlywood4", "dodgerblue4"))+
  scale_color_manual(labels=c("Recruitment", "1-year-old return", "Older female return"), 
                     values=c("azure4","burlywood4", "dodgerblue4"))+
  ggthemes::theme_few(base_size = 16)

ggplot(YearlyFledge %>% filter(Year != 1999 & Year != 2006 & Year != 2007), aes(x=Year, y=DaysBelow18))+
  geom_point()+
  geom_vline(xintercept=1996.5)+
  labs(x="Year", y=expression("Days below "*18~degree*C))+
  ggthemes::theme_few(base_size = 16)





newdata2 <- data.frame(age=c(rep(0, 42), rep(1, 42), rep(2, 42)),
                       year=rep(seq(1975, 2016,1),3),
                       DaysBelow18=rep(YearlyFledge$DaysBelow18[1:42],3), 
                       estimate=NA, 
                       se=NA, 
                       lcl=NA, 
                       ucl=NA)

#recruitment predictions in increase
newdata2[1:22, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[1,,drop=FALSE],"Phi",
                                     replicate=TRUE,
                                     data=data.frame(DaysBelow18= newdata2$DaysBelow18[1:22]))[,15:18] 
#SY return predictions in increase
newdata2[43:64, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[2,,drop=FALSE],"Phi",
                                      replicate=TRUE,
                                      data=data.frame(DaysBelow18= newdata2$DaysBelow18[1:22]))[,15:18] 
#ASY Return predictions in increase
newdata2[85:106, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[3,,drop=FALSE],"Phi",
                                       replicate=TRUE,
                                       data=data.frame(DaysBelow18= newdata2$DaysBelow18[1:22]))[,15:18] 
#recruitment predictions in decline
newdata2[23:42, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[694,,drop=FALSE],"Phi",
                                      replicate=TRUE,
                                      data=data.frame(DaysBelow18= newdata2$DaysBelow18[23:42]))[,15:18] 

#SY Return predictions in decline
newdata2[65:84, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[1804,,drop=FALSE],"Phi",
                                      replicate=TRUE,
                                      data=data.frame(DaysBelow18= newdata2$DaysBelow18[23:42]))[,15:18] 
#ASY Return predictions in decline
newdata2[107:126, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[23,,drop=FALSE],"Phi",
                                        replicate=TRUE,
                                        data=data.frame(DaysBelow18= newdata2$DaysBelow18[23:42]))[,15:18] 


newdata2$age<- factor(newdata2$age, levels=c(0,1,2))

ggplot(newdata2 , aes(x=year, y=estimate, color=age))+
  geom_segment(aes(x=year, xend=year, y=ucl, yend=lcl))+
  geom_point(size=3)+
  labs(x="Year", y="Survival", color="Age")+
  scale_color_manual(labels=c("Recruitment", "1-year-old return", "Older female return"), 
                     values=c("azure4","burlywood4", "dodgerblue4"))+
  ggthemes::theme_few(base_size = 16)


#Wow that looks really good actually!! It could potentially explain stuff!!





cleanup(ask=F)
