
survdat2$CenteredMaxTemp2 <- survdat2$CenteredMaxTemp ^2

mod1 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Age2*Year2+ Age2*CenteredMaxTemp2 + Age2*CenteredMaxTemp, data=survdat2)
#FUCK YES. I can force it to run by manually calculating the temp^2
car::Anova(mod1)




#Do we need the Age:Year interaction? 
mod1_2 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Year2+ Age2*CenteredMaxTemp2 + Age2*CenteredMaxTemp, data=survdat2)
anova(mod1, mod1_2)
#nope

#DO we need the age:Temp^2 interaction?
mod1_3 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ Year2+ CenteredMaxTemp2 + Age2*CenteredMaxTemp, data=survdat2)
anova(mod1_2, mod1_3)
#Yes. Max Temp affects age groups differently. 

#DO we need Year?
mod1_4 <- coxph(Surv(time=Time1, time2=Time2, event=Status)~ CenteredMaxTemp2 + Age2*CenteredMaxTemp, data=survdat2)
anova(mod1_4, mod1_2)
#Yes


mam1 <- mod1_2

dredge(mod1)



newdata <- data.frame(Year2 =  (as.numeric(weather2$Year)-1975)/10, 
                      Date = as.Date(weather2$Date, format="%m/%d/%Y"), 
                      Age2 = c(rep("Poikilotherm", nrow(weather2)), rep("Intermediate", nrow(weather2)), rep( "Endotherm", nrow(weather2))), 
                      CenteredMaxTemp = weather2$MaxTemp-mean(weather2$MaxTemp, na.rm=T), 
                      CenteredMaxTemp2 = weather2$MaxTemp-mean(weather2$MaxTemp, na.rm=T)^2, 
                      Predicted = NA, 
                      SE=NA)
newdata$Predicted <- predict(mam1, newdata=newdata, se.fit=T, type="risk", reference="sample")$fit
newdata$SE <- predict(mam1, newdata=newdata, se.fit=T, type="risk", reference="sample")$se.fit


ggplot(newdata, aes(x=CenteredMaxTemp+ mean(weather2$MaxTemp, na.rm = T), y=Predicted, color=Year2, group=Year2))+
  geom_line()+
  #geom_ribbon()+
  facet_grid(~Age2)+
  ylab()
  
  
YearMaxTempSummary <- survdat2 %>% 
    group_by(Year2, Time1, Age2) %>% 
    summarise(Nests=length(Status),
              CenteredMaxTemp=first(CenteredMaxTemp)) %>%
    group_by(Year2 , Age2) %>%
    summarise(CenteredMaxTemp = weighted.mean(CenteredMaxTemp),
              CenteredMaxTemp2 = weighted.mean(CenteredMaxTemp)^2, 
              Predicted = NA, 
              SE = NA) %>%
    arrange(Age2)

YearMaxTempSummary$Predicted <- predict(mam1, newdata=YearMaxTempSummary, se.fit=T, type="risk", reference="sample")$fit
YearMaxTempSummary$SE <- predict(mam1, newdata=YearMaxTempSummary, se.fit=T, type="risk", reference="sample")$se.fit



ggplot(YearMaxTempSummary, aes(x=Year2, y=CenteredMaxTemp))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(~Age2)

ggplot(YearMaxTempSummary, aes(x=Year2, y=Predicted))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(~Age2)

