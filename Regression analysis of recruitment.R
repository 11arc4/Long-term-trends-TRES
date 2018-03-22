#Regression analysis of environmental causes of recruitment decline. 

#this is based on my "Regression analysis of overwinter survival" where I did
#all 3 age classes. I thought that the first graph shows that really only
#recruitment has crashed, so perhaps interpretation would be easier if I only
#looked at the recruits in environmental models. They might well be affected
#differently.


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



#############Does winter ENSO score predict recruitment rates?
bmod_ENSO <- betareg(Estimate ~ WinterENSO*TimePeriod, data=Survival2, link="loglog")
plot(bmod_ENSO) #There are a couple of outliers. 
plot(resid(bmod_ENSO)~Survival2$WinterENSO)
plot(resid(bmod_ENSO)~Survival2$TimePeriod) # We are overestimating declining. 
which(resid(bmod_ENSO) <(-3))

#Try removing leverage points
Survival3 <- Survival2[-c(33, 34, 35),]
bmod_ENSO <- betareg(Estimate ~ WinterENSO*TimePeriod, data=Survival3, link="loglog")
plot(bmod_ENSO) #Fixed that.  
plot(resid(bmod_ENSO)~Survival3$WinterENSO)
plot(resid(bmod_ENSO)~Survival3$TimePeriod) #That's all better now. 


dredge(bmod_ENSO)
car::Anova(bmod_ENSO)

