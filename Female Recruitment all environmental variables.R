#Which of the different environmental covariates predicts survival the best? 
#If we include multiple, can we remove popStatus?
setwd("~/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis")

library(dplyr)
library(RMark)
library(ggplot2)
#Fledglings are trying to feed themselves, and mothers are trying to feed them
#as well for the first 2 weeks at least. Thats energetically demanding and might
#reduce survival

#Load in the data (already filtering out the males, and half of the nestlings for each year that we guess were male)
datMark <- readRDS("~/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis/Female Fledgling MARK Data.rda")

#Load in all the environmental data
YearlyFledge <- readRDS("LocalWeather")
presugar <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Sugar Cane Acreage Available USDA survey.csv", as.is=T, na.strings =c(""))
sugar <- presugar[,c(2, 19, 21, 23,25)]
colnames(sugar) <- c("year", "acreCaneSeed", "tonsCaneSeed", "acreCaneSeedSugar", "acreCaneSugar")

sugar$acreCaneSeed[which(sugar$year>1974 & sugar$year<1997)]/10000
sugar$acreCaneSeed[which(sugar$year>1996)]/10000


sugar$acreCaneSeed <- as.numeric(gsub(',', '', sugar$acreCaneSeed))

ENSOdat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Monthly El Nino Southern Oscillation index.csv", as.is=T)[,1:13]
colnames(ENSOdat) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
ENSOdat<- ENSOdat %>% arrange(Year)


for(i in 2:nrow(ENSOdat)){
  
  ENSOdat$ENSOWinter[i] <-  mean(c(ENSOdat$Mar[i-1], ENSOdat$Dec[i-1], ENSOdat$Jan[i], ENSOdat$Feb[i]), na.rm=T)
  
}
hurricane <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Hurricanes.csv")


#process the data and make a design data table.

tsprocess <-process.data(datMark,model="CJS",
                         begin.time=1975, 
                         groups= ("age"),
                         initial.ages =c(0,1, 2))
ts.ddl<- make.design.data(tsprocess, parameters=list(Phi=list(age.bins=c(0, 0.8, 1.8, 44)),
                                                     p=list(age.bins=c(0, 0.8, 1.8, 44))))


#set up the population status variable
ts.ddl$Phi$popStatus<- rep(NA)
ts.ddl$Phi$popStatus[which(ts.ddl$Phi$Time<=21)]<- 0  #1975-1996 increased
ts.ddl$Phi$popStatus[which(ts.ddl$Phi$Time>21)]<- 1 #1997-2017 decreased

ts.ddl$Phi$Hurricanes<- rep(NA)
ts.ddl$Phi$ENSOWinter<- rep(NA)
ts.ddl$Phi$SugarAcres<- rep(NA)
ts.ddl$Phi$DaysBelow18<- rep(NA)


for (i in 1:nrow(ts.ddl$p)){
  ts.ddl$Phi$Hurricanes[i] <- as.numeric (hurricane$Hurricanes[which(hurricane$Year+1==ts.ddl$Phi$time[i])])
  ts.ddl$Phi$ENSOWinter[i] <- as.numeric (ENSOdat$ENSOWinter[which(ENSOdat$Year==ts.ddl$Phi$time[i])])
  ts.ddl$Phi$SugarAcres[i] <- as.numeric (sugar$acreCaneSeed[which(sugar$year+1==ts.ddl$Phi$time[i])])/10000
  ts.ddl$Phi$DaysBelow18[i] <- as.numeric (YearlyFledge$DaysBelow18[which(YearlyFledge$Year==ts.ddl$Phi$time[i])])
}


#Set up all the possible combos that you're interested in testing. 
p.timeage <- list(formula= ~time + age)

Phi.1 <- list(formula= ~ popStatus * age)

Phi.2 <- list(formula= ~ SugarAcres*age + Hurricanes*age + ENSOWinter*age + DaysBelow18*age) #this has very low support
Phi.3 <- list(formula= ~ SugarAcres*age*popStatus + Hurricanes*age*popStatus + ENSOWinter + DaysBelow18*age*popStatus)
Phi.4 <- list(formula= ~ SugarAcres*age*popStatus + Hurricanes*age*popStatus  + DaysBelow18*age*popStatus)
Phi.5 <- list(formula= ~ SugarAcres*age*popStatus +  ENSOWinter + DaysBelow18*age*popStatus)
Phi.5 <- list(formula= ~ SugarAcres*age*popStatus +  ENSOWinter )
Phi.6 <- list(formula= ~ SugarAcres*age*popStatus + DaysBelow18*age*popStatus)
Phi.7 <- list(formula= ~ ENSOWinter + DaysBelow18*age*popStatus)


Phi.8 <- list(formula= ~ SugarAcres*age*popStatus + Hurricanes*age*popStatus + ENSOWinter )
Phi.9 <- list(formula= ~ SugarAcres*age*popStatus + Hurricanes*age*popStatus )
Phi.10 <- list(formula= ~ Hurricanes*age*popStatus + ENSOWinter )

Phi.11 <- list(formula= ~ Hurricanes*age*popStatus + ENSOWinter + DaysBelow18*age*popStatus)
Phi.12 <- list(formula= ~ Hurricanes*age*popStatus  + DaysBelow18*age*popStatus)
Phi.13 <- list(formula= ~ SugarAcres*age*popStatus)
Phi.14 <- list(formula= ~ Hurricanes*age*popStatus )
Phi.15 <- list(formula= ~  age*popStatus + ENSOWinter)
Phi.16 <- list(formula= ~  DaysBelow18*age*popStatus)


cml<- create.model.list("CJS")
results <- mark.wrapper(cml, data=tsprocess, ddl=ts.ddl, output=F, adjust=F)





