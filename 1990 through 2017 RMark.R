#I"m going to redo the analysis but dropping the years when box occupancy was saturated


boxOcc <- read.csv("~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data/Box Occupancy using nestdata renest function.csv")
colnames(boxOcc)[1]<- "year"


library(RMark)
library(dplyr)
setwd("~/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis")
datMark_F <- readRDS("Adult Female MARK Data.rda") 

#remove all records before 1990 (ie now it's only 28 capture records) to make datMark_F2
datMark_F2<- datMark_F
datMark_F2$ch <- substring(datMark_F$ch, 16, 43)
datMark_F2 <- datMark_F2 %>% filter (ch != "0000000000000000000000000000")

tsprocess <-process.data(datMark_F2,model="CJS",
                         begin.time=1990, 
                         groups= ("age"),
                         initial.ages =c(1, 2))

ts.ddl <- make.design.data(tsprocess, parameters=list(Phi=list(age.bins=c(1, 1.5, 29)),
                                                     p=list(age.bins=c(1,1.5, 29)))) 
levels(ts.ddl$Phi$age)<- c("SY", "ASY")
levels(ts.ddl$p$age)<- c("SY", "ASY")

BirdsSeen <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/TRES Data Analysis/Extracted Statistics/CaptureHistory.csv",
                      na.strings=c(""), as.is=T)

for (i in 1:nrow(ts.ddl$p)){
  ts.ddl$p$BoxOccupancy[i] <- boxOcc$BoxOccTotal[which(boxOcc$year==ts.ddl$p$time[i])]
  ts.ddl$p$FCaptureRate_centered[i] <- BirdsSeen$FCaptureRate[which(BirdsSeen$years==ts.ddl$p$time[i])]- mean(BirdsSeen$FCaptureRate, na.rm=T)
}

#OK I think now we have a nice sugar acres option and we are ready to start playing around with that. 


#Capture probability probably varies by year, but there should be no trend so we
#use time (discrete) Box Occupancy could change their capture by changing how
#many of them return to their surroundings. There is a strong correlation
#between Boxy Occupancy and F cpture rate, so we shouldn't include both in the
#model. I will use F Capture Rate as that's a more direct measure. I will also
#center that value because it's colinear with time
p.dot <- list(formula= ~1)
#p.capture <- list(formula = ~ FCaptureRate_centered)
p.time <- list(formula= ~time)
p.timeplusage <- list(formula= ~time+age) 
p.age <- list(formula= ~age)


#p.capture.age <- list(formula = ~ FCaptureRate_centered*age)

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
female.results[[8]]

#Compare the best model with p(time+age) to p(time). If the recapture rates for
#SY are very high, then we know that it is likely assuming that a bird caught
#first as an ASY was a SY last year
female.results[[7]] #just time
female.results[[8]] #includes age


RELEASEresults <- release.gof(tsprocess, invisible = F)
#Test 3 is violated still-- this means that all
#marked individuals are NOT eqully likely to survive. 
global <- female.results[[6]]
chat <- global$results$deviance/global$results$deviance.df
female.results.adjusted <- adjust.chat(chat, female.results)



AvResults <- model.average(female.results, parameter = "Phi", drop=T, simplified=T)
Results <-as.data.frame(matrix(nrow=28, ncol=7, NA ))
colnames(Results) <- c("Year", "SYSurvival", "SYse", "ASYSurvival", "ASYse", "BoxOccupancy", "FCaptureRate")
Results$Year<- seq(1990,2017,1)


Results$FCaptureRate <- BirdsSeen$FCaptureRate[15:42]

Results$BoxOccupancy<- boxOcc$BoxOccTotal [15:42]
SYPIM <- rep(NA, 28)
for( i in 1:27){
  SYPIM[i] <- global$pims$Phi[[1]][[1]][i,i]
  
}
Results$SYSurvival <- AvResults$estimate[SYPIM] 
Results$SYse <- AvResults$se[SYPIM] 
Results$ASYSurvival[1:27] <- AvResults$estimate[379:405]
Results$ASYse[1:27] <- AvResults$se[379:405]




ggplot(data=Results)+
  geom_point(aes(x=BoxOccupancy, y=SYSurvival), color="brown")+
  geom_segment(aes(x=BoxOccupancy, xend=BoxOccupancy, y=SYSurvival-SYse, yend=SYSurvival+SYse), color="brown")+
  geom_point(aes(x=BoxOccupancy, y=ASYSurvival), color="blue")+
  geom_segment(aes(x=BoxOccupancy, xend=BoxOccupancy, y=ASYSurvival-ASYse, yend=ASYSurvival+ASYse), color="blue")+
  geom_smooth(method="lm", aes(x=BoxOccupancy, y=SYSurvival), color="brown")+
  geom_smooth(method="lm", aes(x=BoxOccupancy, y=ASYSurvival), color="blue")+
  xlab("Box Occupancy")+
  ylab("Apparent Survival")+
  ggthemes::theme_few(base_size = 15)


ggplot(data=Results)+
  geom_line(aes(x=Year, y=SYSurvival), color="brown")+
  geom_segment(aes(x=Year, xend=Year, y=SYSurvival-SYse, yend=SYSurvival+SYse), color="brown")+
  geom_line(aes(x=Year, y=ASYSurvival), color="blue")+
  geom_segment(aes(x=Year, xend=Year, y=ASYSurvival-ASYse, yend=ASYSurvival+ASYse), color="blue")+
  ggthemes::theme_few(base_size = 15)+
  xlab("Year")+
  ylab("Apparent Survival")
  ylim(0.4,0.5) #ASY birds are doing what would make sense. It's the SY birds that make no sense. 
#Hmmm could that be due to the fact that as box occupancy declines, SY birds
#have a better shot at getting a box than before?


ggplot(data=Results )+
  geom_point(aes(x=FCaptureRate, y=SYSurvival), color="brown")+
  geom_segment(aes(x=FCaptureRate, xend=FCaptureRate, y=SYSurvival-SYse, yend=SYSurvival+SYse), color="brown")+
  geom_point(aes(x=FCaptureRate, y=ASYSurvival), color="blue")+
  geom_segment(aes(x=FCaptureRate, xend=FCaptureRate, y=ASYSurvival-ASYse, yend=ASYSurvival+ASYse), color="blue")+
  geom_smooth(method="lm", aes(x=FCaptureRate, y=SYSurvival), color="brown")+
  geom_smooth(method="lm", aes(x=FCaptureRate, y=ASYSurvival), color="blue")+
  xlab("Female Capture Rate")+
  ylab("Apparent Survival")+
  ggthemes::theme_few(base_size = 15)

plot(FCaptureRate~Year,  data=Results) #Capture rate has gone up over the years very clearly and is probably driving my whole thing
plot(BoxOccupancy~Year,  data=Results) #Capture rate has gone up over the years very clearly and is probably driving my whole thing

cleanup(ask=F)



plot(FCaptureRate~years, data=BirdsSeen %>%filter(years>1989))
plot(FCaptureRate~FEstimatePop, data=BirdsSeen %>%filter(years>1989))







png(filename = "~/Masters Thesis Project/Committee Meetings/Dec 15 2017/Capture by pop size.png",
    width =600, height = 300)
ggplot(data=BirdsSeen %>%filter(years>1989), aes(y=FCaptureRate, x=FEstimatePop))+
  geom_point()+
  ylab("Female \n Capture \n Rate")+
  xlab("Female Population Size")+
  theme_classic(base_size = 20)+
  theme(axis.title.y=element_text(angle=0, vjust=0.5))+
  geom_smooth(method="lm")
dev.off()


png(filename = "~/Masters Thesis Project/Committee Meetings/Dec 15 2017/Capture by year.png",
    width =600, height = 300)
ggplot(data=BirdsSeen %>%filter(years>1989), aes(x=years, y=FCaptureRate))+
  geom_point()+
  ylab("Female \n Capture \n Rate")+
  xlab("Year")+
  theme_classic(base_size = 20)+
  theme(axis.title.y=element_text(angle=0, vjust=0.5))+
  geom_smooth(method="lm")
dev.off()

png(filename = "~/Masters Thesis Project/Committee Meetings/Dec 15 2017/Apparent Survival with Continuous year.png",
    width =600, height = 300)
ggplot(data=Results)+
  geom_ribbon(aes(x=Year, xend=Year, ymin=SYSurvival-SYse, ymax=SYSurvival+SYse, alpha=0.2), fill="chocolate4", show.legend = F)+
  geom_line(aes(x=Year, y=SYSurvival), color="chocolate4")+
  geom_ribbon(aes(x=Year, xend=Year, ymin=ASYSurvival-ASYse, ymax=ASYSurvival+ASYse, alpha=0.2), fill="deepskyblue4", show.legend = F)+
  geom_line(aes(x=Year, y=ASYSurvival), color="deepskyblue4")+
  xlab("Year")+
  ylab("Apparent \n Survival")+
  ylim(0,1)+
  ggthemes::theme_few(base_size = 20)+
  theme(axis.title.y=element_text(angle=0, vjust=0.5))
dev.off()

