#Regression analysis of environmental causes of recruitment decline. 

#this is based on my "Regression analysis of overwinter survival" where I did
#all 3 age classes. I thought that the first graph shows that really only
#recruitment has crashed, so perhaps interpretation would be easier if I only
#looked at the recruits in environmental models. They might well be affected
#differently.
library(MuMIn)
library(tidyverse)
library(betareg)
############MAKE THE DATA
Survival <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/Yearly Survival Estimates.csv", na.strings="", as.is=T)

Survival$TimePeriod <- NA
Survival$TimePeriod[which(Survival$Year<1997)]<- "Growing"
Survival$TimePeriod[which(Survival$Year>1996)]<- "Declining"
#Survival$TimePeriod[which(Survival$Year>2013)]<- "PostDecline"
Survival$TimePeriod <- factor(Survival$TimePeriod, levels=c("Growing", "Declining"))
Survival$Age <- factor(Survival$Age)

#Remove the years and specific estimates that aren't any good (not enough birds
#went into them, or we are at a boundary)
Survival2 <- Survival %>% filter(Year !=1975 & Year!=1976 & Year!=2016 & Estimate<.9 & SE<0.2 & Year !=2011 & Age=="Recruit")

Survival2$Year2 <- (Survival2$Year-1975 )/10

#Do any of the environmental varaibles that we have data for predict survival well? 
#Load in all the environmental data
#weather on the breeding ground
YearlyFledge <- readRDS("~/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis/LocalWeather")
YearlyFledge <- YearlyFledge %>% arrange(Year)
#acreage sugar cane
presugar <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Sugar Cane Acreage Available USDA survey.csv", as.is=T, na.strings =c(""))
sugar <- presugar[,c(2, 19, 21, 23,25)]
colnames(sugar) <- c("year", "acreCaneSeed", "tonsCaneSeed", "acreCaneSeedSugar", "acreCaneSugar")
rm(presugar)
sugar$acreCaneSeed[which(sugar$year>1974 & sugar$year<1997)]/10000
sugar$acreCaneSeed[which(sugar$year>1996)]/10000
sugar$acreCaneSeed <- as.numeric(gsub(',', '', sugar$acreCaneSeed))
  #ENSO
ENSOdat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Monthly El Nino Southern Oscillation index.csv", as.is=T)[,1:13]
colnames(ENSOdat) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
ENSOdat<- ENSOdat %>% arrange(Year)
for(i in 2:nrow(ENSOdat)){
  ENSOdat$ENSOWinter[i] <-  mean(c(ENSOdat$Mar[i-1], ENSOdat$Dec[i-1], ENSOdat$Jan[i], ENSOdat$Feb[i]), na.rm=T)
}
#Hurricanes
hurricane <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Hurricanes.csv")

Survival2$WinterENSO <- NA
Survival2$DaysBelow18_mean <- NA
Survival2$DaysBelow18_max <- NA
Survival2$Hurricanes <- NA
Survival2$SugarAcreage <- NA

#Match all of these parameters of interest up with the survival estimates. 
for (i in 1:nrow(Survival2)){
  Year<- Survival2$Year[i]
  Survival2$SugarAcreage[i] <- sugar$acreCaneSeed[Year==sugar$year]
  Survival2$DaysBelow18_mean[i] <- YearlyFledge$DaysBelow18_mean [Year==YearlyFledge$Year]
  Survival2$DaysBelow18_max[i] <- YearlyFledge$DaysBelow18_max [Year==YearlyFledge$Year]
  Survival2$Hurricanes[i] <- hurricane$Hurricanes[Year==hurricane$Year]
  Survival2$WinterENSO[i] <- ENSOdat$ENSOWinter[Year==ENSOdat$Year]
}

Survival2$SugarAcreage2 <- Survival2$SugarAcreage/ 10000

#############Does winter ENSO score predict recruitment rates?
bmod_ENSO <- betareg(Estimate ~ WinterENSO*TimePeriod, data=Survival2, link="loglog")
plot(bmod_ENSO) #There are a couple of outliers. 
plot(resid(bmod_ENSO)~Survival2$WinterENSO)
plot(resid(bmod_ENSO)~Survival2$TimePeriod) # We are overestimating declining. 
which(resid(bmod_ENSO) <(-3))

#Try removing leverage points
Survival3 <- Survival2[-c(33, 35),]
bmod_ENSO <- betareg(Estimate ~ WinterENSO*TimePeriod, data=Survival3, link="loglog")
plot(bmod_ENSO) #Fixed that.  
plot(resid(bmod_ENSO)~Survival3$WinterENSO)
plot(resid(bmod_ENSO)~Survival3$TimePeriod) #That's all better now. 

options(na.action = "na.fail")
dredge(bmod_ENSO)
car::Anova(bmod_ENSO)

bmam_ENSO <- betareg(Estimate ~ WinterENSO+TimePeriod, data=Survival3, link="loglog")





################Do hurricanes predict reccruitment?
bmod_hurricane <- betareg(Estimate ~ Hurricanes*TimePeriod, data=Survival2, link="loglog")
plot(bmod_hurricane) #There's definitely leverage
plot(resid(bmod_hurricane)~Survival2$Hurricanes)
plot(resid(bmod_hurricane)~Survival2$TimePeriod) #not tip top again
which(resid(bmod_hurricane) <(-3))

bmod_hurricane <- betareg(Estimate ~ Hurricanes*TimePeriod, data=Survival3, link="loglog")
plot(resid(bmod_hurricane)~Survival3$Hurricanes)
plot(resid(bmod_hurricane)~Survival3$TimePeriod)

dredge(bmod_hurricane) 
car::Anova(bmod_hurricane)

bmam_hurricane <- betareg(Estimate ~ Hurricanes+TimePeriod, data=Survival3, link="loglog")

##############Does sugar cane acreage predict recruitment? 
bmod_sugar <- betareg(Estimate ~ SugarAcreage2*TimePeriod, data=Survival2, link="loglog")
plot(bmod_sugar) #def has outliers
plot(resid(bmod_sugar)~Survival2$TimePeriod) #not great
plot(resid(bmod_sugar)~Survival2$SugarAcreage2)
which(resid(bmod_sugar)<(-3))
#lets try removing the outliers
bmod_sugar <- betareg(Estimate ~ SugarAcreage2*TimePeriod, data=Survival3, link="loglog")
plot(resid(bmod_sugar)~Survival3$SugarAcreage2)
plot(resid(bmod_sugar)~Survival3$TimePeriod)
#Looks a lot better, althogh still not perfect. 

dredge(bmod_sugar)
car::Anova(bmod_sugar)
bmam_sugar <- betareg(Estimate ~ SugarAcreage2*TimePeriod, data=Survival3, link="loglog")

################Does the number of good days during the post-fledging period predict recruitment?
#based on mean temp
bmod_mean <- betareg(Estimate ~ DaysBelow18_mean*TimePeriod, data=Survival2, link="loglog")
plot(bmod_mean)
plot(resid(bmod_mean)~Survival2$TimePeriod) #not great
plot(resid(bmod_mean)~Survival2$DaysBelow18_mean)
which(resid(bmod_mean)<(-3))
#removing residuals
bmod_mean <- betareg(Estimate ~ DaysBelow18_mean*TimePeriod, data=Survival3, link="loglog")
plot(resid(bmod_mean)~Survival3$TimePeriod) #not great
plot(resid(bmod_mean)~Survival3$DaysBelow18_mean)

dredge(bmod_mean)
car::Anova(bmod_mean)

bmam_mean <- betareg(Estimate ~ DaysBelow18_mean*TimePeriod, data=Survival3, link="loglog")

#based on max temp
bmod_max <- betareg(Estimate ~ DaysBelow18_max*TimePeriod, data=Survival2, link="loglog")
plot(bmod_max)
plot(resid(bmod_max)~Survival2$TimePeriod) #not great
plot(resid(bmod_max)~Survival2$DaysBelow18_max) #very bad
which(resid(bmod_max)<(-3))
#remove leverage points (at least 2 of them)
bmod_max <- betareg(Estimate ~ DaysBelow18_max*TimePeriod, data=Survival3, link="loglog")
plot(resid(bmod_max)~Survival3$TimePeriod) #not great
plot(resid(bmod_max)~Survival3$DaysBelow18_max) #much better

dredge(bmod_max)
car::Anova(bmod_max)

bmam_max <- bmod_max


AICc(bmam_ENSO, bmam_hurricane, bmam_sugar, bmam_max, bmam_mean)
#Sugar and days (based on mean) are about equal. 

#Lets put them all in together. 
bmod_all <- betareg(Estimate ~ DaysBelow18_max*TimePeriod + SugarAcreage2*TimePeriod+ ENSO+Hurricane, data=Survival3, link="loglog")
plot(bmod_sugarandDays)
plot(resid(bmod_sugarandDays)~Survival3$DaysBelow18_max)
plot(resid(bmod_sugarandDays)~Survival3$TimePeriod)
plot(resid(bmod_sugarandDays)~Survival3$SugarAcreage2)
#Looks good

dredge(bmod_sugarandDays)
car::Anova(bmod_sugarandDays)
#Huh. When you put them both together, sugar acreage take it all away

summary(bmam_sugar)



newdata_sugar <- data.frame(SugarAcreage2=rep(seq(3.43, 6.78, length.out = 20 ), 2), 
                            TimePeriod= c(rep("Growing", 20), rep("Declining", 20)), 
                            Predicted=NA, 
                            Variance=NA)

newdata_sugar$Predicted <- predict(bmam_sugar, newdata_sugar)
newdata_sugar$TimePeriod <- factor(newdata_sugar$TimePeriod, levels=c("Growing", "Declining"))

ggplot()+
  geom_line(data=newdata_sugar, aes(x=SugarAcreage2*10000, y=Predicted))+
  geom_point(data=Survival2, aes(x=SugarAcreage, y=Estimate))+
  facet_grid(~TimePeriod)+
  labs(x="Acres of sugar cane", y="Apparent Recruitment")+
  ggthemes::theme_few(base_size = 16)

  



#Perhaps the juveniles were overwintering in natural swamps and habitat during
#the time that the population was growing and had other options so weren't as
#dependent on sugar cane fields for overwinter habitat.






###########Has sugar cane declined? 
ggplot(sugar, aes(x=year, y=acreCaneSeed))+
  geom_point()
#Yes it has since about the time we are interested in BUT historically we have
#been increasing since about the 1900s. 1800s were way higher

Sugar2 <- sugar %>% filter(year>1975 & year<2017)

ggplot(Sugar2, aes(x=year, y=acreCaneSeed))+
  geom_point()+
  geom_vline(xintercept = 1996)


Sugar2$TimePeriod <- NA
Sugar2$TimePeriod[which(Sugar2$year<1997)]<- "Growing"
Sugar2$TimePeriod[which(Sugar2$year>1996)]<- "Declining"
Sugar2$TimePeriod <- factor(Sugar2$TimePeriod, levels=c("Growing", "Declining"))
Sugar2$Year2 <- Sugar2$year/10-197.5

Sugar2$AcreageSugar <- Sugar2$acreCaneSeed/10000

mod <- lm(AcreageSugar~TimePeriod*Year2, data=Sugar2)
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod)) #passes but just barely. I will go with it. 
plot(resid(mod)~Sugar2$Year2)
plot(resid(mod)~Sugar2$TimePeriod)
#This looks great

dredge(mod) #full model is easily the best
car::Anova(mod)

summary(mod)

ggplot(Sugar2, aes(y=AcreageSugar*10000, x=Year2*10+1975, group=TimePeriod))+
  geom_point()+
  geom_smooth(method="lm", formula= y~x)+
  labs(x="Year", y="Acreage of Sugar" )+
  ggthemes::theme_few(base_size = 16)






#plots for presentation
ggplot()+
  geom_line(data=newdata_sugar, aes(x=SugarAcreage2*10000/247, y=Predicted), size=1)+
  geom_point(data=Survival2, aes(x=SugarAcreage/247, y=Estimate))+
  facet_grid(~TimePeriod)+
  labs(x="Square km of sugar cane", y="Juvenile\nSurvival")+
  ggthemes::theme_few(base_size = 16)+
  theme(text = element_text(size=20), axis.title.y = element_text(angle=0, vjust=0.5))+
  scale_x_continuous(breaks=c(150, 200, 250))


ggplot(Sugar2, aes(y=AcreageSugar*10000/247, x=Year2*10+1975, group=TimePeriod))+
  geom_point()+
  geom_smooth(method="lm", formula= y~x, color="black", se=F, size=1)+
  labs(x="Year", y="Square km \nsugar cane" )+
  ggthemes::theme_few(base_size = 16)+
  scale_y_continuous(breaks=c(150, 200, 250))+
  theme(text = element_text(size=20), axis.title.y = element_text(angle=0, vjust=0.5))+
  geom_vline(xintercept = 1996.5)



