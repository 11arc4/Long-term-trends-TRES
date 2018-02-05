#Lets bring in sugar cane production as a measure of how much good habitat is
#available for TRES

presugar <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Sugar Cane Acreage Available USDA survey.csv", as.is=T, na.strings =c(""))
sugar <- presugar[,c(2, 19, 21, 23,25)]
colnames(sugar) <- c("year", "acreCaneSeed", "tonsCaneSeed", "acreCaneSeedSugar", "acreCaneSugar")

sugar$acreCaneSeed <- as.numeric(gsub(',', '', sugar$acreCaneSeed))
sugar$acreCaneSeedSugar <- as.numeric(gsub(',', '', sugar$acreCaneSeedSugar))
sugar$acreCaneSugar <- as.numeric(gsub(',', '', sugar$acreCaneSugar))

#i wonder if sugar cane acreage predicts box occupancy with any sort of accuracy?

boxOcc <- read.csv("~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/Box Occupancy using nestdata renest function.csv")
colnames(boxOcc)[1]<- "year"
sugar2 <- boxOcc

for(i in 1:nrow(sugar2)){
  sugar2$acreCaneSeed[i] <- sugar$acreCaneSeed[which(sugar$year+1==sugar2$year[i])]
}


library(ggplot2)
library(ggthemes)
library(dplyr)
ggplot(sugar %>% filter(year>1974), aes(x=year))+
  geom_point(aes(y=acreCaneSeed))
#Holy shit. that looks like it maps perfectly with my box occupancy. Or at least
#pretty damn close-- cane is what's important too since they roost in sugar cane fields. 

ggplot(sugar %>% filter(year>1974), aes(x=year))+
  geom_point(aes(y=acreCaneSeedSugar))

ggplot(sugar %>% filter(year>1974), aes(x=year))+
  geom_point(aes(y=acreCaneSugar))





SugarBoxOccPlot <- ggplot(sugar2, aes(x=acreCaneSeed, y=BoxOccTotal))+
  geom_point()+
  geom_smooth(method="lm")+
  ylab("Box Occupancy")+
  xlab("Acerage of Sugar Cane and Seed (US)")+
  theme_few(base_size = 20)

rcorr(as.matrix(sugar2[,c(23,37)]))
#Not a significant correlation
mod <- lm(BoxOccTotal~ acreCaneSeed, data=sugar2)
summary(mod) 
png(filename = "~/Masters Thesis Project/Committee Meetings/Dec 15 2017/Sugar and Box Occupancy Correlation.png",
    width =450, height = 300)
SugarBoxOccPlot
dev.off()


ggplot(sugar2, aes(x=acreCaneSeed, y=BirdTotal))+
  geom_point()+
  geom_smooth(method="lm")+
  ylab("Total Birds")+
  xlab("Acerage of Sugar Cane and Seed (US)")
#Huh I wonder if maybe this is worth exploring more!!! That's pretty exciting. They match up pretty well. 

#Let's see if sugar cane affects female survival
library(RMark)
setwd("~/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis")
datMark_F <- readRDS("Adult Female MARK Data.rda") 


tsprocess <-process.data(datMark_F,model="CJS",
                         begin.time=1975, 
                         groups= ("age"),
                         initial.ages =c(1, 2))

ts.ddl<- make.design.data(tsprocess, parameters=list(Phi=list(age.bins=c(1, 2, 43)),
                                                     p=list(age.bins=c(1,2, 43)))) 
BirdsSeen <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/TRES Data Analysis/Extracted Statistics/CaptureHistory.csv",
                      na.strings=c(""), as.is=T)

for (i in 1:nrow(ts.ddl$p)){
  ts.ddl$Phi$SugarAcres[i] <- as.numeric (sugar$acreCaneSeed[which(sugar$year+1==ts.ddl$Phi$time[i])])/10000
  ts.ddl$p$BoxOccupancy[i] <- boxOcc$BoxOccTotal[which(boxOcc$year==ts.ddl$p$time[i])]
  ts.ddl$p$FCaptureRateOnGrid[i] <- BirdsSeen$FCaptureRate[which(BirdsSeen$years==ts.ddl$p$time[i])]
}

#OK I think now we have a nice sugar acres option and we are ready to start playing around with that. 


#Capture probability probably varies by year, but there should be no trend so we use time (discrete)
#Box Occupancy could change their capture by changing how many of them return to their surroundings. 

p.dot <- list(formula= ~1)
p.boxOcc <- list(formula = ~ BoxOccupancy)
p.capture <- list(formula = ~ FCaptureRateOnGrid)
p.boxOccCapture <- list(formula= ~ BoxOccupancy + FCaptureRateOnGrid)


#Survival probably varies by age, and we want to see if something has happened over the course of time (continuous)
#Sugar cane crops could change their actual survival by providing habitat
#Use these to find the best global model
Phi.dot <- list(formula=~1)
Phi.age <- list(formula=~age)
Phi.agetime <- list(formula= ~age + Time)
Phi.agebytime <- list(formula= ~age * Time)
#Build up all the possible base models and collect them based on AIC

cml <- create.model.list("CJS")
female.results <- mark.wrapper(cml, data=tsprocess, ddl=ts.ddl, output=F, adjust=F)

RELEASEresults <- release.gof(tsprocess, invisible = F)
#Test 2 isn't violated-- this means that all marked individuals are equally
#likely to be captured. 

#Test 3 is violated quite badly-- this means that all
#marked individuals are NOT eqully likely to survive. That's quite an issue
#acctually but again this isn't including all the structure we're interested in....
#chat <- RELEASEresults$Chi.square[3]/RELEASEresults$df[3]


#Let's export this to MARK to make sure the residuals look OK!
#Let's export our models to MARK so that I can check their fit properly. 

export.MARK(tsprocess, "FemaleBaseProject", female.results, replace=T)

#Nice I've checked the models residuals and it all looks pretty decent. I feel
#ok with using these models. I like the more complex model best--- it's
#residuals are best so I will use it as MAM I will just adjust c-hat and carry on with the analysis


#I'm going to adjust everyone by the deviance/df of the global model to take
#care of the overdispersion we see. I think this is better than adjusting using
#RELEASE because RELEASE doesn't include box occupancy since it's a yearly covariant!

global <- female.results[[6]]
chat <- global$results$deviance/global$results$deviance.df


female.results.adjusted <- adjust.chat(chat, female.results)



####################################################################################
#Then we can start adding environmental covariants to see if we can do better
#than the global by adding in sugar cane
p.capture <- list(formula = ~ FCaptureRateOnGrid)
p.boxOccCapture <- list(formula= ~ BoxOccupancy + FCaptureRateOnGrid)


Phi.agebytime <- list(formula= ~ Time + age )
Phi.timebyagesugar <- list(formula= ~ Time+ age +SugarAcres)
Phi.timebyagebysugarBoxOcc <- list(formula= ~age*SugarAcres + Time )


#Build up all the possible environmental and collect them based on AIC

cml2 <- create.model.list("CJS")
female.results2 <- mark.wrapper(cml2, data=tsprocess, ddl=ts.ddl, output=F, adjust=F)

export.MARK(tsprocess, "FemaleSugarProject", female.results2, replace=T)




#I'm going to adjust everyone by the deviance/df of the global model to take
#care of the overdispersion we see. I think this is better than adjusting using
#RELEASE because RELEASE doesn't include box occupancy since it's a yearly covariant!

global2 <- female.results2[[3]]
chat2 <- global2$results$deviance/global2$results$deviance.df

female.results.adjusted2 <- adjust.chat(chat2, female.results2)



#SO this has the parameters listed by PIM. The PIMs correspond to the pims from the global model
#By that token I only care about the survival PIMs (1:2709)

SYPIM <- rep(NA, 42)
for( i in 1:42){
  SYPIM[i] <- global2$pims$Phi[[2]][[1]][i,i]
  
}

model.average(female.results.adjusted2)[c(906, 947, 987),] #this makes sense
model.average(female.results.adjusted2)[c(907, 948, 988, 1027),]
model.average(female.results.adjusted2)[c(3, 44, 84),]  #Same numbers but backwards and this makes little sense to me. That is problematic
model.average(female.results.adjusted2)[c(4,45,85,124),]  #yeah this doesn't do anything I understand. 
 #I suspect there is something I don't understand here and this makes me nervous
 #BUT I THINK the second set of Phi PIMS are the ones I'm interested in



avMod <- model.average(female.results.adjusted2)









#SO this is a bit tricky because there are so many damn parameters estimates and
#we are definitely going to need to model average

bestMod <- female.results.adjusted2[[2]]

x<- bestMod$results$beta$estimate[4]

SugarSlope <- exp(x)/(1+exp(x))
#for every 10,000 acres of sugar cane survival increases 0.45

#THIS WHOLE BIT WITH GETTING PREDICTED RESULTS CURRENTLY ISN'T WORKING I DON'T KNOW WHY. FIGURE IT OUT TOMORROW
#Let's get some predicted values based on our model

minAcresSugar <- min(Results$AcresSugar, na.rm=T)/10000
maxAcresSugar <- max(Results$AcresSugar, na.rm=T)/10000

meanBoxOcc <- mean(boxOcc$BoxOccTotal, na.rm=T)

Predict<- data.frame(Time=1995, age=c(rep(1, 34), rep(2, 34)), SugarAcres=rep(seq(minAcresSugar, maxAcresSugar, 0.1),2), BoxOccupancy=meanBoxOcc)

Predict$Survival<- covariate.predictions(female.results.adjusted2, parameter="Phi" )




AvResults <- model.average(female.results.adjusted2, parameter = "Phi", drop=T, simplified=T)

Results <-as.data.frame(matrix(nrow=42, ncol=8, NA ))
colnames(Results) <- c("Year", "AcresSugar", "SYSurvival", "SYse", "ASYSurvival", "ASYse", "BoxOccupancy", "FCaptureRate")
Results$Year<- seq(1975,2016,1)
for(i in 1:nrow(Results)){
  Results$AcresSugar[i]<- sugar$acreCaneSeed[which(Results$Year[i]+1==sugar$year)]
}

Results$FCaptureRate <- BirdsSeen$FCaptureRate[1:42]

Results$BoxOccupancy<- boxOcc$BoxOccTotal [1:42]

Results$SYSurvival <- AvResults$estimate[SYPIM] 
Results$SYse <- AvResults$se[SYPIM] 

Results$ASYSurvival <- AvResults$estimate[1:42]
Results$ASYse <- AvResults$se[1:42]





ggplot(data=Results)+
  geom_point(aes(x=AcresSugar, y=SYSurvival), color="brown")+
  geom_segment(aes(x=AcresSugar, xend=AcresSugar, y=SYSurvival-SYse, yend=SYSurvival+SYse), color="brown")+
  geom_point(aes(x=AcresSugar, y=ASYSurvival), color="blue")+
  geom_segment(aes(x=AcresSugar, xend=AcresSugar, y=ASYSurvival-ASYse, yend=ASYSurvival+ASYse), color="blue")+
  geom_smooth(method="lm", aes(x=AcresSugar, y=SYSurvival), color="brown")+
  geom_smooth(method="lm", aes(x=AcresSugar, y=ASYSurvival), color="blue")+
  xlab("Acres of Sugarcane and Sugar Seed")+
  ylab("Apparent Survival")+
  theme_few(base_size = 15)
#Can't see shit here because the box occupancy affect is obscuring it... damn



ggplot(data=Results)+
  geom_point(aes(x=BoxOccupancy, y=SYSurvival), color="brown")+
  geom_segment(aes(x=BoxOccupancy, xend=BoxOccupancy, y=SYSurvival-SYse, yend=SYSurvival+SYse), color="brown")+
  geom_point(aes(x=BoxOccupancy, y=ASYSurvival), color="blue")+
  geom_segment(aes(x=BoxOccupancy, xend=BoxOccupancy, y=ASYSurvival-ASYse, yend=ASYSurvival+ASYse), color="blue")+
  geom_smooth(method="lm", aes(x=BoxOccupancy, y=SYSurvival), color="brown")+
  geom_smooth(method="lm", aes(x=BoxOccupancy, y=ASYSurvival), color="blue")+
  xlab("Box Occupancy")+
  ylab("Apparent Survival")+
  theme_few(base_size = 15)


ggplot(data=Results)+
  geom_point(aes(x=Year, y=SYSurvival), color="brown")+
  geom_segment(aes(x=Year, xend=Year, y=SYSurvival-SYse, yend=SYSurvival+SYse), color="brown")+
  geom_point(aes(x=Year, y=ASYSurvival), color="blue")+
  geom_segment(aes(x=Year, xend=Year, y=ASYSurvival-ASYse, yend=ASYSurvival+ASYse), color="blue")+
  theme_few(base_size = 15)+
  xlab("Year")+
  ylab("Apparent Survival")


ggplot(data=Results )+
  geom_point(aes(x=FCaptureRate, y=SYSurvival), color="brown")+
  geom_segment(aes(x=FCaptureRate, xend=FCaptureRate, y=SYSurvival-SYse, yend=SYSurvival+SYse), color="brown")+
  geom_point(aes(x=FCaptureRate, y=ASYSurvival), color="blue")+
  geom_segment(aes(x=FCaptureRate, xend=FCaptureRate, y=ASYSurvival-ASYse, yend=ASYSurvival+ASYse), color="blue")+
  geom_smooth(method="lm", aes(x=FCaptureRate, y=SYSurvival), color="brown")+
  geom_smooth(method="lm", aes(x=FCaptureRate, y=ASYSurvival), color="blue")+
  xlab("Female Capture Rate")+
  ylab("Apparent Survival")+
  theme_few(base_size = 15)
#Hmmm. I wonder if I am not taking this into account appropriately



#Actually Survival is really APPARENT survival. I wonder if we just see more of
#the birds coming back in later years because box occupancy is lower? I wonder
#if it's actually returning to the boxes that's changed, not survival



