#Female Recruitment Overtime (RMark) looking at how sugar acreage influences survival

setwd("~/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis")#Make sure you set the working directory to something seperarate otherwise MARK
nstgMark <- readRDS("Female Fledgling MARK Data.rda")
#This has only female nestlings that fledged, PLUS all the females. Unknown
#fledglings (ie those that didn't recruit) were set to male or female randomly
#(50-50) and males were removed.

library(RMark)
#I've created multiple groups based on sex and at what age we first saw the
#bird. Also set the initial year to 1975.

# We're going to want a CJS #model because our data is recapture data, and we
# can't tell whether you've #actually died. 
tsprocess <-process.data(nstgMark,model="CJS",
                         begin.time=1975, 
                         groups= ("age"),
                         initial.ages =c(0,1, 2))
ts.ddl<- make.design.data(tsprocess, parameters=list(Phi=list(age.bins=c(0, 0.8, 1.8, 44)),
                                                     p=list(age.bins=c(0, 0.8, 1.8, 44))))


#set up the population status variable
ts.ddl$Phi$popStatus<- rep(NA)
ts.ddl$Phi$popStatus[which(ts.ddl$Phi$Time<=21)]<- 0  #1975-1996 increased
ts.ddl$Phi$popStatus[which(ts.ddl$Phi$Time>21)]<- 1 #1997-2014 decreased
#ts.ddl$Phi$popStatus[which(ts.ddl$Phi$Time>41)]<- 2 #2015-2017 have been increasing again
ts.ddl$Phi$popStatus<- as.factor(ts.ddl$Phi$popStatus)

#set up the sugar acreage variable. here make sure you are using the best one
#that predicts population size most closely. (acrecaneseed)
presugar <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Sugar Cane Acreage Available USDA survey.csv", as.is=T, na.strings =c(""))
sugar <- presugar[,c(2, 19, 21, 23,25)]
colnames(sugar) <- c("year", "acreCaneSeed", "tonsCaneSeed", "acreCaneSeedSugar", "acreCaneSugar")

sugar$acreCaneSeed[which(sugar$year>1974 & sugar$year<1997)]/10000
sugar$acreCaneSeed[which(sugar$year>1996)]/10000


sugar$acreCaneSeed <- as.numeric(gsub(',', '', sugar$acreCaneSeed))

for (i in 1:nrow(ts.ddl$p)){
  ts.ddl$Phi$SugarAcres[i] <- as.numeric (sugar$acreCaneSeed[which(sugar$year+1==ts.ddl$Phi$time[i])])/10000
}



#Take the best model from pure female recruitment over time, and add the
#environmental covariate of interest to that
p.timeage <- list(formula= ~time + age)
Phi.popstatusbyage<- list(formula=~popStatus*age)
Phi.popstatusbyagebysugar<- list(formula=~popStatus*age*SugarAcres)
Phi.popstatusbyagesugar<- list(formula=~popStatus*age + SugarAcres)
Phi.popstatusbyageagebysugar<- list(formula=~popStatus*age + age*SugarAcres)
Phi.popstatusbyagepopstatusbysugar<- list(formula=~popStatus*age+ popStatus*SugarAcres)

cml <- create.model.list("CJS")
results <- mark.wrapper(cml, data=tsprocess, ddl=ts.ddl, output=F, adjust=F)

#ooo looks like the three way interaction is super important. 
#Lets see how the global model fits
export.MARK(tsprocess, "FemaleNestlings_sugar", results, replace=T)
#median chat=1.16 for the global model (results[[3]])


adjustedresults <- adjust.chat(1.16, results)

#Nice. There is still a really obvious winner 
#There is a 3 way interaction between popStatus, age and sugar

bestmod <- adjustedresults[[3]]

c <- coef(bestmod)[1:12,]
summary(bestmod)

min1 <- min(sugar$acreCaneSeed[which(sugar$year>1974 & sugar$year<1997)]/10000)
max1 <- max(sugar$acreCaneSeed[which(sugar$year>1974 & sugar$year<1997)]/10000)

min2 <- min(sugar$acreCaneSeed[which(sugar$year>1996)]/10000, na.rm=T)
max2 <- max(sugar$acreCaneSeed[which(sugar$year>1996)]/10000, na.rm=T)


newdata <- data.frame(age=c(rep(0, 150), rep(1, 150), rep(2, 150)), 
                      popStatus=c(rep(0, 450), rep(1, 450)), 
                      SugarAcres=c(rep(seq(min1, max1, length.out = 150), 3),rep(seq(min2, max2, length.out = 150), 3)), 
                      estimate=NA, 
                      se=NA, 
                      lcl=NA, 
                      ucl=NA)




#recruitment predictions in increase
newdata[1:150, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[1,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(SugarAcres=seq(min1, max1, length.out = 150)))[,14:17] 
#SY return predictions in increase
newdata[151:300, 4:7 ] <-predict_real(bestmod,ts.ddl$Phi[2,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(SugarAcres=seq(min1, max1, length.out = 150)))[,14:17]  
#ASY Return predictions in increase
newdata[301:450, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[3,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(SugarAcres=seq(min1, max1, length.out = 150)))[,14:17] 
#recruitment predictions in decline
newdata[451:600, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[694,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(SugarAcres=seq(min1, max1, length.out = 150))) [,14:17]
#SY Return predictions in decline
newdata[601:750, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[1804,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(SugarAcres=seq(min1, max1, length.out = 150)))[,14:17] 
#ASY Return predictions in decline
newdata[751:900, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[23,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(SugarAcres=seq(min1, max1, length.out = 150)))[,14:17] 


newdata$popStatus <- factor(newdata$popStatus)
newdata$age <- factor(newdata$age)

TimePeriodNames <- c("0"="Growing", 
                     "1"="Declining")


library(ggplot2)
library(dplyr)

ggplot(newdata, aes(x=SugarAcres*10000, y=estimate, fill=age))+
  geom_line(aes(color=age))+
  facet_grid(~popStatus, labeller = as_labeller(TimePeriodNames))+

  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.2)+
  labs(x="Acreage of sugarcane", y="Survival overwinter", fill="Age", color="Age")+
  scale_fill_manual(labels=c("Recruitment", "1-year-old return", "Older female return"), 
                    values=c("azure4", "burlywood4", "dodgerblue4"))+
    scale_color_manual(labels=c("Recruitment", "1-year-old return", "Older female return"), 
                     values=c("azure4","burlywood4", "dodgerblue4"))+
  ggthemes::theme_few(base_size = 16)



ggplot(sugar %>% filter(year>1974), aes(x=year, y=acreCaneSeed))+
  geom_point()+
  geom_vline(xintercept=1996.5)+
  labs(x="Year", y="Acres of sugar cane \nand seed planted")+
  ggthemes::theme_few(base_size = 16)



#Calculate out estimates for yearly survival based on this model!
sugar2 <- sugar[sugar$year>1973 & sugar$year<2016, ]
sugar2 <- sugar %>% arrange(year) %>% filter(year>1973 & year<2016)
newdata2 <- data.frame(age=c(rep(0, 42), rep(1, 42), rep(2, 42)),
                      year=rep(seq(1975, 2016,1),3),
                      SugarAcres=rep(sugar2$acreCaneSeed,3), 
                      estimate=NA, 
                      se=NA, 
                      lcl=NA, 
                      ucl=NA)




#recruitment predictions in increase
newdata2[1:22, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[1,,drop=FALSE],"Phi",
                                     replicate=TRUE,
                                     data=data.frame(SugarAcres= newdata2$SugarAcres[1:22]/10000))[,14:17] 
#SY return predictions in increase
newdata2[43:64, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[2,,drop=FALSE],"Phi",
                                     replicate=TRUE,
                                     data=data.frame(SugarAcres= newdata2$SugarAcres[1:22]/10000))[,14:17] 
#ASY Return predictions in increase
newdata2[85:106, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[3,,drop=FALSE],"Phi",
                                      replicate=TRUE,
                                      data=data.frame(SugarAcres= newdata2$SugarAcres[1:22]/10000))[,14:17] 
#recruitment predictions in decline
newdata2[23:42, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[694,,drop=FALSE],"Phi",
                                     replicate=TRUE,
                                     data=data.frame(SugarAcres= newdata2$SugarAcres[23:42]/10000))[,14:17] 

#SY Return predictions in decline
newdata2[65:84, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[1804,,drop=FALSE],"Phi",
                                     replicate=TRUE,
                                     data=data.frame(SugarAcres= newdata2$SugarAcres[23:42]/10000))[,14:17]
#ASY Return predictions in decline
newdata2[107:126, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[23,,drop=FALSE],"Phi",
                                       replicate=TRUE,
                                       data=data.frame(SugarAcres= newdata2$SugarAcres[23:42]/10000))[,14:17]


newdata2$age<- factor(newdata2$age, levels=c(0,1,2))

ggplot(newdata2 , aes(x=year, y=estimate, color=age))+
  geom_segment(aes(x=year, xend=year, y=lcl, yend=ucl), color="black")+
  geom_point(size=3)+
  labs(x="Year", y="Survival", color="Age")+
  scale_color_manual(labels=c("Recruitment", "1-year-old return", "Older female return"), 
                     values=c("azure4","burlywood4", "dodgerblue4"))+
  ggthemes::theme_few(base_size = 16)
#yeah that's pretty similar to our discretely estimates estimates too. Damn
#thats interesting. It's not capturing all the variation in what's going on but
#it sure gets a lot, particularly when it's declining. It misses some of the
#variation when the population isn't declining yet



cleanup(ask=F)



