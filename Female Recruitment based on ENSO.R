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


#Add winter ENSO data
ENSOdat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Monthly El Nino Southern Oscillation index.csv", as.is=T)[,1:13]
colnames(ENSOdat) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
ENSOdat<- ENSOdat %>% arrange(Year)


for(i in 2:nrow(ENSOdat)){

  ENSOdat$ENSOWinter[i] <-  mean(c(ENSOdat$Mar[i-1], ENSOdat$Dec[i-1], ENSOdat$Jan[i], ENSOdat$Feb[i]), na.rm=T)

}
for (i in 1:nrow(ts.ddl$Phi)){
  ts.ddl$Phi$ENSOWinter[i] <- as.numeric (ENSOdat$ENSOWinter[which(ENSOdat$Year==ts.ddl$Phi$time[i])])
}



#Take the best model from pure female recruitment over time, and add the
#environmental covariate of interest to that
p.timeage <- list(formula= ~time + age)
Phi.1<- list(formula=~popStatus*age)
Phi.2<- list(formula=~ENSOWinter*age )
Phi.3<- list(formula=~popStatus*age +ENSOWinter)
Phi.4<- list(formula=~popStatus*age +ENSOWinter*age)
Phi.4<- list(formula=~popStatus*age +ENSOWinter*popStatus)
Phi.5<- list(formula=~popStatus*age +ENSOWinter*popStatus + ENSOWinter*age)
Phi.6<- list(formula=~popStatus*age*ENSOWinter)



cml <- create.model.list("CJS")
results <- mark.wrapper(cml, data=tsprocess, ddl=ts.ddl, output=F, adjust=F)

export.MARK(tsprocess, "FemaleNestlings_ENSOwinter", results, replace=T)
#gloabl model chat = 1.17
adjustedresults <- adjust.chat(1.17, results)
#There's no one clear answer. ENSO may or may not be important.

bestmod <- adjustedresults[[3]]

min1 <- min(ENSOdat$ENSOWinter[which(ENSOdat$Year>1974 & ENSOdat$Year<1997)])
max1 <- max(ENSOdat$ENSOWinter[which(ENSOdat$Year>1974 & ENSOdat$Year<1997)])

min2 <- min(ENSOdat$ENSOWinter[which(ENSOdat$Year>1996)], na.rm=T)
max2 <- max(ENSOdat$ENSOWinter[which(ENSOdat$Year>1996)], na.rm=T)



newdata <- data.frame(age=c(rep(0, 150), rep(1, 150), rep(2, 150)), 
                      popStatus=c(rep(0, 450), rep(1, 450)), 
                      ENSOWinter=c(rep(seq(min1, max1, length.out = 150), 3),rep(seq(min2, max2, length.out = 150), 3)), 
                      estimate=NA, 
                      se=NA, 
                      lcl=NA, 
                      ucl=NA)




#recruitment predictions in increase
newdata[1:150, 4:7 ] <- predict_real(bestmod,
                                     ts.ddl$Phi[1,,drop=FALSE],
                                     "Phi",
                                     replicate=TRUE,
                                     data=data.frame(ENSOWinter=seq(min1, max1, length.out = 150)))[,14:17] 
#SY return predictions in increase
newdata[151:300, 4:7 ] <-predict_real(bestmod,ts.ddl$Phi[2,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(ENSOWinter=seq(min1, max1, length.out = 150)))[,14:17]  
#ASY Return predictions in increase
newdata[301:450, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[3,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(ENSOWinter=seq(min1, max1, length.out = 150)))[,14:17] 
#recruitment predictions in decline
newdata[451:600, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[694,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(ENSOWinter=seq(min1, max1, length.out = 150))) [,14:17]
#SY Return predictions in decline
newdata[601:750, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[1804,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(ENSOWinter=seq(min1, max1, length.out = 150)))[,14:17] 
#ASY Return predictions in decline
newdata[751:900, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[23,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(ENSOWinter=seq(min1, max1, length.out = 150)))[,14:17] 
newdata$age<- factor(newdata$age, levels=c(0,1,2))

TimePeriodNames <- c("0"="Growing", 
                     "1"="Declining")



library(ggplot2)
ggplot(newdata, aes(x=ENSOWinter, y=estimate, fill=age))+
  geom_line(aes(color=age))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), alpha=0.2)+
  facet_grid(~popStatus, labeller = as_labeller(TimePeriodNames))+
  labs(x="Mean ENSO (Dec-Mar)", y="Survival overwinter", fill="Age", color="Age")+
  scale_fill_manual(labels=c("Recruitment", "1-year-old return", "Older female return"), 
                    values=c("azure4", "burlywood4", "dodgerblue4"))+
  scale_color_manual(labels=c("Recruitment", "1-year-old return", "Older female return"), 
                     values=c("azure4","burlywood4", "dodgerblue4"))+
  ggthemes::theme_few(base_size = 16)




#Calculate out estimates for yearly survival based on this model!
newdata2 <- data.frame(age=c(rep(0, 42), rep(1, 42), rep(2, 42)),
                       year=rep(seq(1975, 2016,1),3),
                       ENSOWinter=rep(ENSOdat$ENSOWinter[2:43],3), 
                       estimate=NA, 
                       se=NA, 
                       lcl=NA, 
                       ucl=NA)




#recruitment predictions in increase
newdata2[1:22, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[1,,drop=FALSE],"Phi",
                                     replicate=TRUE,
                                     data=data.frame(ENSOWinter= newdata2$ENSOWinter[1:22]))[,14:17] 
#SY return predictions in increase
newdata2[43:64, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[2,,drop=FALSE],"Phi",
                                      replicate=TRUE,
                                      data=data.frame(ENSOWinter= newdata2$ENSOWinter[1:22]))[,14:17]  
#ASY Return predictions in increase
newdata2[85:106, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[3,,drop=FALSE],"Phi",
                                       replicate=TRUE,
                                       data=data.frame(ENSOWinter= newdata2$ENSOWinter[1:22]))[,14:17]  
#recruitment predictions in decline
newdata2[23:42, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[694,,drop=FALSE],"Phi",
                                      replicate=TRUE,
                                      data=data.frame(ENSOWinter= ENSOdat$ENSOWinter[23:42]))[,14:17]  

#SY Return predictions in decline
newdata2[65:84, 4:7 ] <- predict_real(bestmod, ts.ddl$Phi[1804,,drop=FALSE],"Phi",
                                      replicate=TRUE,
                                      data=data.frame(ENSOWinter= ENSOdat$ENSOWinter[23:42]))[,14:17]  
#ASY Return predictions in decline
newdata2[107:126, 4:7 ] <- predict_real(bestmod,ts.ddl$Phi[23,,drop=FALSE],"Phi",
                                        replicate=TRUE,
                                        data=data.frame(ENSOWinter= ENSOdat$ENSOWinter[23:42]))[,14:17]  


newdata2$age<- factor(newdata2$age, levels=c(0,1,2))



ggplot(newdata2, aes(x=year, y=estimate, color=age))+
  geom_segment(aes(x=year, xend=year, y=lcl, yend=ucl), color="black")+
  geom_point(size=3)+
  geom_vline(xintercept=1996.5)+
  labs(x="Year", y="Survival", color="Age", title="Survival predictions based on mean winter ENSO")+
  scale_color_manual(labels=c("Recruitment", "1-year-old return", "Older female return"), 
                     values=c("azure4","burlywood4", "dodgerblue4"))+
  ggthemes::theme_few(base_size = 16)



ggplot(ENSOdat, aes(x=Year, y=ENSOWinter))+
  geom_point()+
  ggthemes::theme_few(base_size = 16)+
  labs(x="Year", y="Mean winter ENSO (Dec-Mar) ")





cleanup(ask=F)
