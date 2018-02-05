library(dplyr)
library(ggplot2)
library(RMark)
library(ggthemes)



#Lets explore how broad scale climate indices affect TRES survival
boxOcc <- read.csv("~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/Box Occupancy using nestdata renest function.csv")



AMOdat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Monthly Atlantic Multidecadal Oscillation (AMO).csv", as.is=T)[1:43,]
colnames(AMOdat) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

ENSOdat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Monthly El Nino Southern Oscillation index.csv", as.is=T)
colnames(ENSOdat) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


ENSOdat <- ENSOdat[order(ENSOdat$Year, decreasing=F),]
ENSOdat <- ENSOdat[1:43,]
NAOdat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/North American Oscillation Index monthly.csv", as.is=T)
colnames(NAOdat) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
#FOr our region this one should be the most active

#High values mean that there should be less storms down south, and it should be dry up north that year
#Often people appear to use a winter index (ie the average for Dec-Mar)
#Sillett et al. 2000 used yearly means
Climate <- boxOcc

for(i in 1:nrow(Climate)){
  Climate$NAOWinter[i] <-  mean(c(NAOdat$Mar[i-1], NAOdat$Dec[i-1], NAOdat$Jan[i], NAOdat$Feb[i]), na.rm=T)
  Climate$NAOYearly[i] <-  sum(NAOdat[i,2:13], na.rm=T)/sum(!is.na(NAOdat[i,2:13]))
  
  Climate$ENSOWinter[i] <-  mean(c(ENSOdat$Mar[i-1], ENSOdat$Dec[i-1], ENSOdat$Jan[i], ENSOdat$Feb[i]), na.rm=T)
  Climate$ENSOYearly[i] <-  sum(ENSOdat[i,2:13], na.rm=T)/sum(!is.na(ENSOdat[i,2:13]))
  
  Climate$AMOWinter[i] <-  mean(c(AMOdat$Mar[i-1], AMOdat$Dec[i-1], AMOdat$Jan[i], AMOdat$Feb[i]), na.rm=T)
  Climate$AMOYearly[i] <-  sum(AMOdat[i,2:13], na.rm=T)/sum(!is.na(AMOdat[i,2:13]))
  
}

library(Hmisc)
rcorr(as.matrix(Climate[,c(23, 37:42)]))

WinterNAO <- ggplot(Climate, aes(x=NAOWinter, y=BoxOccTotal))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_few(base_size = 15)+
  ylab("")+
  xlab("Winter average NAO")

mod <- lm(BoxOccTotal ~ NAOWinter, data=Climate)
Anova(mod)

YearlyNAO <- ggplot(Climate, aes(x=NAOYearly, y=BoxOccTotal))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_few(base_size = 15)+
  ylab("")+
  xlab("Yearly average NAO")



WinterENSO <- ggplot(Climate, aes(x=ENSOWinter, y=BoxOccTotal))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_few(base_size = 15)+
  ylab("")+
  xlab("Winter average ENSO")

#Looks like there is a stronger slope for the winter than the yearly-- maybe
#that means it's doing stuff during the winter

YearlyENSO <- ggplot(Climate, aes(x=ENSOYearly, y=BoxOccTotal))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_few(base_size = 15)+
  ylab("")+
  xlab("Yearly average ENSO")


ggplot(Climate, aes(x=Year, y=ENSOWinter))+
  geom_point()+
  geom_smooth()+
  theme_few(base_size = 15)+
  ylab("Winter ENSO")+
  xlab("Year")





WinterAMO <- ggplot(Climate, aes(x=AMOWinter, y=BoxOccTotal))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_few(base_size = 15)+
  ylab("Box Occupancy")+

  xlab("Winter average AMO")



YearlyAMO <- ggplot(Climate, aes(x=AMOYearly, y=BoxOccTotal))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_few(base_size = 15)+
  xlab("Yearly average AMO")+
  ylab("Box Occupancy")
  
  #ylab("")
  




library(cowplot)
ClimateIndices <- plot_grid(plotlist=list(WinterAMO, YearlyAMO, WinterENSO, YearlyENSO,  WinterNAO, YearlyNAO ), 
                            nrow=3,
                            ncol=2, 
                            align="h",
                            labels=c("A", "B", "C", "D", "E", "F"))

ClimateIndices2 <- plot_grid(plotlist=list(WinterAMO,  WinterENSO,  WinterNAO, YearlyAMO, YearlyENSO,  YearlyNAO ), 
                            nrow=2,
                            ncol=3, 
                            align="h",
                            labels=c("A", "B", "C", "D", "E", "F"))
png(filename = "~/Masters Thesis Project/Committee Meetings/Dec 15 2017/Climate Indices and Box Occupancy Correlation.png",
    width =600, height = 400)
ClimateIndices2
dev.off()














##########################################
#DOES SURVIVAL CHANGE BASED ON NAO INDEX? 
# we can build on our global model from sugar cane

setwd("~/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis")

theme_few(base_size = 20)
datMark_F <- readRDS("Adult Female MARK Data.rda") 


tsprocess <-process.data(datMark_F,model="CJS",
                         begin.time=1975, 
                         groups= ("age"),
                         initial.ages =c(1, 2))

ts.ddl<- make.design.data(tsprocess, parameters=list(Phi=list(age.bins=c(1, 2, 43)),
                                                     p=list(age.bins=c(1,2, 43)))) 

for (i in 1:nrow(ts.ddl$Phi)){
  ts.ddl$p$BoxOccupancy[i] <- boxOcc$BoxOccTotal[which(boxOcc$Year==ts.ddl$p$time[i])]
  ts.ddl$Phi$WinterNAO[i] <- Climate$NAOWinter[which(Climate$Year==ts.ddl$Phi$time[i])]
  ts.ddl$Phi$WinterENSO[i] <- Climate$ENSOWinter[which(Climate$Year==ts.ddl$Phi$time[i])]
  ts.ddl$Phi$WinterAMO[i] <- Climate$AMOWinter[which(Climate$Year==ts.ddl$Phi$time[i])]
  ts.ddl$Phi$YearlyNAO[i] <- Climate$NAOYearly[which(Climate$Year==ts.ddl$Phi$time[i])]
  ts.ddl$Phi$YearlyENSO[i] <- Climate$ENSOYearly[which(Climate$Year==ts.ddl$Phi$time[i])]
  ts.ddl$Phi$YearlyAMO[i] <- Climate$AMOYearly[which(Climate$Year==ts.ddl$Phi$time[i])]
}

#Then we can start adding environmental covariants to see if we can do better
#than the global

weather<- c(WinterNAO, WinterENSO, WinterAMO, YearlyNAO, YearlyENSO, YearlyAMO)

for (weatherIndex in weather){
  #Make the capture formulas
  p.dot <- list(formula= ~1)
  p.boxOcc <- list(formula= ~ BoxOccupancy)
  
  #Make the survival formulas
  Phi.agebytime <- list(formula= ~ Time*age )
  Phi.timebyageWinterNAO <- list(formula= ~ Time*age +weatherIndex)
  Phi.timebyagebyWinterNAO <- list(formula= ~age*weatherIndex + Time*age )
  
  Phi.agetime <- list(formula= ~ Time+age)
  Phi.timeageWinterNAO <- list(formula= ~ Time+age +weatherIndex )
  Phi.timeagebyWinterNAO <- list(formula= ~age*weatherIndex + Time+age)
  
  
  cml <- create.model.list("CJS")
  female.results <- mark.wrapper(cml, data=tsprocess, ddl=ts.ddl, output=F, adjust=F)
  
  mod <- female.results[[2]]
  mod$results$deviance/mod$results$deviance.df
  
  
  
  #Let's export our models to MARK so that I can check their fit properly. 
  
  export.MARK(tsprocess, "FemaleNAOProject", female.results, replace=T)
  chat<- 
    female.results.adjusted <- adjust.chat(chat, female.results)
  
}






#Let's to NAO first
p.dot <- list(formula= ~1)
p.boxOcc <- list(formula= ~ BoxOccupancy)


Phi.agebytime <- list(formula= ~ Time*age )
Phi.timebyageWinterNAO <- list(formula= ~ Time*age +WinterNAO)
Phi.timebyagebyWinterNAO <- list(formula= ~age*WinterNAO + Time*age )

Phi.agetime <- list(formula= ~ Time+age)
Phi.timeageWinterNAO <- list(formula= ~ Time+age +WinterNAO )
Phi.timeagebyWinterNAO <- list(formula= ~age*WinterNAO + Time+age)




cml <- create.model.list("CJS")
female.results <- mark.wrapper(cml, data=tsprocess, ddl=ts.ddl, output=F, adjust=F)


global <- female.results[[9]]
global$results$deviance/global$results$deviance.df
#Chat= 4.38 which is over 1 but less than 10 so it's overdispersed but not uncorrectably so


#Let's export our models to MARK so that I can check their fit properly. 

export.MARK(tsprocess, "FemaleNAOProject", female.results, replace=T)
chat<- global$results$deviance/global$results$deviance.df
female.results.adjusted <- adjust.chat(chat, female.results)

