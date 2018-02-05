#Investigating fledging rate
library(ggplot2)
library(dplyr)
library(reshape2)
library(car)

#Let's just look first at the yearly data. 
PopData<- read.csv(file= "file:///C:/Users/11arc/Documents/Masters Thesis Project/TRES Data Analysis/Matrix Pop Estimates/Yearly Vital Rates.csv", na.strings=c(""), as.is=T)


FledgeRatePlot <- ggplot(PopData, aes(x=year, y=fledgeRate))+
  geom_point()+
  geom_smooth(method="lm")+
  xlab("Year")+
  ylab("Annual \n Fledge \n Rate")+
  theme_classic(base_size = 20)+
  theme(axis.title.y=element_text(angle=0, vjust=0.5))

#looks like there could be something there. 

png(filename = "~/Masters Thesis Project/Committee Meetings/Dec 15 2017/Annual Fledge Rate.png",
    width =600, height = 400)
FledgeRatePlot
dev.off()


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
                   
), ] %>% filter (FledgeRate<=1 & Hatch >0)

summary(dat2$Experiment)



ggplot(dat2 %>% filter(!is.na(FAge2)), aes(x=Year, y=FledgeRate))+
  geom_count()+
  geom_smooth(method="lm")+
  facet_grid(FAge2~.)







dat3 <- dat2 %>% filter (!is.na(FailureCause) & Fledge==0)%>% group_by(Year) %>% summarise(length(which(FailureCause2=="NESTLINGS DIED")), 
                                                                                     length(which(FailureCause2=="PREDATION")), 
                                                                                     length(which(FailureCause2=="COMPETITOR")), 
                                                                                     length(which(FailureCause2=="OTHER")))

colnames(dat3) <- c("Year", "Died", "Pred", "Comp", "Other")
dat4 <- melt(dat3, id = c("Year"))


ggplot(dat4, aes(x = Year, y = value, fill = variable)) + 
  geom_bar(position ="fill", stat = "identity", show.legend = TRUE) + 
  xlab("Year")+
  ylab("Proportion Failure")+
  theme_bw(base_size = 20)+
  theme(axis.text.x=element_text( size=12, angle=90), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggplot(dat4, aes(x = Year, y = value, fill = variable)) + 
  geom_bar( stat = "identity", show.legend = TRUE) + 
  xlab("Year")+
  ylab("Failed Nests")+
  theme_bw(base_size = 20)+
  theme(axis.text.x=element_text( size=12, angle=90), panel.grid.major = element_blank(), panel.grid.minor = element_blank())



#Let's start doing some preliminary modelling


hist(dat2$FledgeRate)
#This is SO strongly bimodal that I wonder if treating it like we either fledge
#nestlings or we dont would be a better method

dat2$Fledge2[which(dat2$Fledge>0)]<- "Fledge"

dat2$Fledge2[which(dat2$Fledge==0)]<- "Fail"

dat2$Fledge2 <- as.factor(dat2$Fledge2)

modDat <- dat2 %>% filter(!is.na(FAge) )

mod <- glm(as.factor(Fledge2) ~ Year*FAge2, data=modDat, family = binomial)
sum <- summary.glm(mod)
Anova(mod)


plot(mod)
plot(resid(mod)~fitted(mod)) 

plot(resid(mod)~modDat$FAge2) #That looks good at least

plot(cooks.distance(mod))

#I think that this model fits OK! Fantastic. Dispersion is  

modDat$Fitted <- fitted(mod)


modDat$Fledge3[which(modDat$Fledge>0)]<- 1

modDat$Fledge3[which(modDat$Fledge==0)]<- 0

ggplot(data=modDat, aes(x=Year))+
  geom_line(aes(y=Fitted))+
  geom_count(aes(y=Fledge3))+
  facet_grid(~FAge2)


#OK let's start trying to add in some of the variation due to local weather and broadscale climate!





#Pull in the local weather data
weather <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Harington Weather Station Daily Data 1975 to  2017.csv", na.strings = c(""), as.is=T)
colnames(weather) <- weather[25,]
weather <- weather[26:nrow(weather),]

weather$JDate <- lubridate::yday(as.Date(weather$`Date/Time`, format="%Y-%m-%d"))

weather$`Total Precip (mm)` <- as.numeric(weather$`Total Precip (mm)`)

for(i in 1:nrow(dat2)){
  dat2$TotRain[i] <- sum(weather$`Total Precip (mm)`[which(weather$Year==dat2$Year[i] & weather$JDate>dat2$HatchDate[i] & weather$JDate< dat2$FledgeDate[i])],na.rm=T)

  dat2$DaysBelow18[i] <- length (which(weather$`Mean Temp (Â°C)` [which(weather$Year==dat2$Year[i] & weather$JDate>dat2$HatchDate[i] & weather$JDate< dat2$FledgeDate[i])]<18))
  
}


modDat <- dat2 %>% filter(!is.na(FAge) )

mod2 <- glm(as.factor(Fledge2) ~ Year*FAge2 + FAge2*TotRain, data=modDat, family = binomial)
Anova(mod)
summary(mod) #Total rain is significant but this causes an increase in fledging!. This makes no sense



plot(resid(mod)~fitted(mod)) #not sure whether that's good or not really. I think it's passable? 

plot(resid(mod)~modDat$FAge2) #That looks good at least

plot(cooks.distance(mod)) #that seems ok



mod3 <- glm(Fledge2 ~ Year*FAge2 + FAge2*DaysBelow18, data=modDat, family = binomial)
Anova(mod3)
summary(mod3)




###How does average nestling mass at fledging reflect whether they fledge succcessfully or not? 




