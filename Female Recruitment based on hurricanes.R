#Does female recruitment and age dependent survival depend on how many
#hurricanes are in the gulf the previous year when the birds are down south?

setwd("~/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis")
nstgMark <- readRDS("Female Fledgling MARK Data.rda")
#This has only female nestlings that fledged, PLUS all the females. Unknown
#fledglings (ie those that didn't recruit) were set to male or female randomly
#(50-50) and males were removed.

library(RMark)
#I've created multiple groups based on sex and at what age we first saw the
#bird. Also set the initial year to 1975.

# We're going to want a CJS #model because our data is recapture data, and we
# can't tell whether you've actually died. 
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

#Add in the hurricane data
hurricane <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Hurricanes.csv")

plot(Hurricanes~Year, data=hurricane)

for (i in 1:nrow(ts.ddl$p)){
  ts.ddl$Phi$Hurricanes[i] <- as.numeric (hurricane$Hurricanes[which(hurricane$Year+1==ts.ddl$Phi$time[i])])
}



#Take the best model from pure female recruitment over time, and add the
#environmental covariate of interest to that
p.timeage <- list(formula= ~time + age)
Phi.1<- list(formula=~popStatus*age)
Phi.2<- list(formula=~popStatus*age + Hurricanes)
Phi.3<- list(formula=~popStatus*age + Hurricanes*age)
Phi.4<- list(formula=~popStatus*age + Hurricanes*popStatus)
Phi.5<- list(formula=~popStatus*age*Hurricanes)


cml <- create.model.list("CJS")
results <- mark.wrapper(cml, data=tsprocess, ddl=ts.ddl, output=F, adjust=F)

export.MARK(tsprocess, "FemaleNestlings_hurricanes", results, replace=T)
#Chat on the global is 1.29

adjustedresults <- adjust.chat(1.29, results)


bestmod <- adjustedresults[5]$Phi.5.p.timeage




min1 <- min(hurricane$Hurricanes[which(hurricane$Year>1974 & hurricane$Year<1997)])
max1 <- max(hurricane$Hurricanes[which(hurricane$Year>1974 & hurricane$Year<1997)])

min2 <- min(hurricane$Hurricanes[which(hurricane$Year>1996)], na.rm=T)
max2 <- max(hurricane$Hurricanes[which(hurricane$Year>1996)], na.rm=T)


newdata <- data.frame(age=c(rep(0, 10), rep(1, 10), rep(2, 10), rep(0, 14), rep(1, 14), rep(2, 14)), 
                      popStatus=c(rep(0, 30), rep(1, 42)), 
                      Hurricanes=c(rep(seq(min1, max1, 1), 3),rep(seq(min2, max2, 1), 3)), 
                      estimate=NA, 
                      se=NA, 
                      lcl=NA, 
                      ucl=NA)



#recruitment predictions in increase
newdata[1:10, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[1,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(Hurricanes=2:11))[,14:17] 
#SY return predictions in increase
newdata[11:20, 4:7 ] <-predict_real(bestmod,ts.ddl$Phi[2,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(Hurricanes=2:11))[,14:17]  
#ASY Return predictions in increase
newdata[21:30, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[3,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(Hurricanes=2:11))[,14:17] 
#recruitment predictions in decline
newdata[31:44, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[694,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(Hurricanes=2:15)) [,14:17]
newdata[45:58, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[1804,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(Hurricanes=2:15))[,14:17] #SY Return predictions in decline
newdata[59:72, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[23,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(Hurricanes=2:15))[,14:17] #ASY Return predictions in decline

newdata$age<- factor(newdata$age, levels=c(0,1,2))

TimePeriodNames <- c("0"="Growing", 
                        "1"="Declining")


library(ggplot2)
ggplot(newdata, aes(x=Hurricanes, y=estimate, fill=age))+
  geom_line(aes(color=age))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.2)+
  facet_grid(~popStatus, labeller = as_labeller(TimePeriodNames))+
  labs(x="Yearly hurricanes", y="Survival overwinter", fill="Age", color="Age")+
  scale_fill_manual(labels=c("Recruitment", "1-year-old return", "Older female return"), 
                    values=c("azure4", "burlywood4", "dodgerblue4"))+
  scale_color_manual(labels=c("Recruitment", "1-year-old return", "Older female return"), 
                     values=c("azure4","burlywood4", "dodgerblue4"))+
  ggthemes::theme_few(base_size = 16)




ggplot(hurricane, aes(x=Year, y=Hurricanes))+
  geom_point()+
  ggthemes::theme_few(base_size = 16)+
  labs(x="Year", y="North Atlantic basin \nhurricane counts")




#Calculate out estimates for yearly survival based on this model!
newdata2 <- data.frame(age=c(rep(0, 42), rep(1, 42), rep(2, 42)),
                       year=rep(seq(1975, 2016,1),3),
                       Hurricanes=rep(hurricane$Hurricanes[1:42],3), 
                       estimate=NA, 
                       se=NA, 
                       lcl=NA, 
                       ucl=NA)




#recruitment predictions in increase
newdata2[1:22, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[1,,drop=FALSE],"Phi",
                                     replicate=TRUE,
                                     data=data.frame(Hurricanes= newdata2$Hurricanes[1:22]))[,14:17] 
#SY return predictions in increase
newdata2[43:64, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[2,,drop=FALSE],"Phi",
                                      replicate=TRUE,
                                      data=data.frame(Hurricanes= newdata2$Hurricanes[1:22]))[,14:17]  
#ASY Return predictions in increase
newdata2[85:106, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[3,,drop=FALSE],"Phi",
                                       replicate=TRUE,
                                       data=data.frame(Hurricanes= newdata2$Hurricanes[1:22]))[,14:17]  
#recruitment predictions in decline
newdata2[23:42, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[694,,drop=FALSE],"Phi",
                                      replicate=TRUE,
                                      data=data.frame(Hurricanes= newdata2$Hurricanes[23:42]))[,14:17]  

#SY Return predictions in decline
newdata2[65:84, 4:7 ] <- predict_real(bestmod, ts.ddl$Phi[1804,,drop=FALSE],"Phi",
                                      replicate=TRUE,
                                      data=data.frame(Hurricanes= newdata2$Hurricanes[23:42]))[,14:17]  
#ASY Return predictions in decline
newdata2[107:126, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[23,,drop=FALSE],"Phi",
                                        replicate=TRUE,
                                        data=data.frame(Hurricanes= newdata2$Hurricanes[23:42]))[,14:17]  


newdata2$age<- factor(newdata2$age, levels=c(0,1,2))



ggplot(newdata2, aes(x=year, y=estimate, color=age))+
  geom_segment(aes(x=year, xend=year, y=lcl, yend=ucl), color="black")+
  geom_point(size=3)+
  geom_vline(xintercept=1996.5)+
  labs(x="Year", y="Survival", color="Age", title="Survival predictions based on yearly hurricanes")+
  scale_color_manual(labels=c("Recruitment", "1-year-old return", "Older female return"), 
                     values=c("azure4","burlywood4", "dodgerblue4"))+
  ggthemes::theme_few(base_size = 16)





