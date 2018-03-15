#Regression analysis of survival estimates

#Matt Guzzo suggested pulling the MARK estimates of survival out and using those as data points in a regression analysis. 
#Then we can follow a similar analysis as the fledging analysis. 

Survival <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/Yearly Survival Estimates.csv", na.strings="", as.is=T)

Survival$TimePeriod <- NA
Survival$TimePeriod[which(Survival$Year<1997)]<- "Growing"
Survival$TimePeriod[which(Survival$Year>1996)]<- "Declining"
#Survival$TimePeriod[which(Survival$Year>2013)]<- "PostDecline"
Survival$TimePeriod <- factor(Survival$TimePeriod, levels=c("Growing", "Declining"))

Survival$Age <- factor(Survival$Age)

#Remove the years and specific estimates that aren't any good (not enough birds
#went into them, or we are at a boundary)
Survival2 <- Survival %>% filter(Year !=1975 & Year!=1976 & Year!=2016 & Estimate<.9 & SE<0.2)

ggplot(Survival2, aes(x=Year, y=Estimate, color=Age))+
  #geom_segment(aes(x=Year, xend=Year, y=Estimate-SE, yend=Estimate+SE, color=Age), alpha=0.6)+
  geom_point( )+
  labs(x="Year", y="Apparent Survival" )+
  ggthemes::theme_few()+
  #geom_smooth()+
  geom_smooth(data=Survival2 %>% filter(TimePeriod=="Growing"), aes(x=Year, y=Estimate, color=Age),method="lm")+
  geom_smooth(data=Survival2 %>% filter(TimePeriod=="Declining"), aes(x=Year, y=Estimate, color=Age),method="lm")+
  geom_smooth(data=Survival2 %>% filter(TimePeriod=="PostDecline"), aes(x=Year, y=Estimate, color=Age),method="lm")+
  facet_grid(~Age)


Survival2$Year2 <- (Survival2$Year-1975 )/10

#############################
#Has survival declined? 
mod <- lm(Estimate ~ Year2*TimePeriod*Age, data=Survival2)
plot(mod)#Normality might be a bit of an issue......
hist(resid(mod)) 
shapiro.test(resid(mod))
plot(resid(mod)~Survival2$TimePeriod) #Good
plot(resid(mod)~Survival2$Age) #Much lower variation for recruit than for ASY and SY return BUT we aren't overestimating or underestimating anyone
plot(resid(mod)~Survival2$Year2) #Fine

#Lack of normality is an issue

ggplot(Survival2, aes(Estimate))+
  geom_histogram()+
  facet_grid(Age~.)
#Probably beta distributed. 


fitdistrplus::descdist(Survival2$Estimate) #Maybe it's a beta distribution

1/(Survival2$Estimate)

mod <- glm(Estimate ~ Year2*TimePeriod*Age, data=Survival2, family=binomial)

library(betareg)

bmod <- betareg(Estimate ~ Year2*Age*TimePeriod, data=Survival2, link="loglog") #We will keep the precision parameter the same for all-- no reason to expect otherwise
plot(bmod) #We have 3 poits with high leverage. Might want to run with and without
plot(bmod, which = 5, type = "deviance", sub.caption = "")
plot(bmod, which = 1, type = "deviance", sub.caption = "")
plot(resid(bmod)~Survival2$Year2)
plot(resid(bmod)~Survival2$Age)
plot(resid(bmod)~Survival2$TimePeriod)
#Holy shit this looks SOOOOOO much better. I think this actually fits. Amazing
#what using the proper assumptions does!



#Which link function should we use?
bmod1 <- betareg(Estimate ~ Year2*Age*TimePeriod, data=Survival2, link="logit")
bmod2 <- betareg(Estimate ~ Year2*Age*TimePeriod, data=Survival2, link="loglog")
bmod3 <- betareg(Estimate ~ Year2*Age*TimePeriod, data=Survival2, link="probit")
bmod4 <- betareg(Estimate ~ Year2*Age*TimePeriod, data=Survival2, link="cloglog")
#bmod5 <- betareg(Estimate ~ Year2*Age*TimePeriod, data=Survival2, link="cauchit")
bmod6 <- betareg(Estimate ~ Year2*Age*TimePeriod, data=Survival2, link="log")


AICc(bmod1, bmod2, bmod3, bmod4, bmod6)
summary(bmod1)$pseudo.r.squared
summary(bmod2)$pseudo.r.squared #LIKE WAYYYYYYYY better. We will use this one. 
summary(bmod3)$pseudo.r.squared
summary(bmod4)$pseudo.r.squared
summary(bmod6)$pseudo.r.squared



dredge(bmod)

summary(bmod)

newdata <- data.frame(Year2=rep(seq(0, 4.2, 0.1), 3),
                      Year=rep(seq(1975, 2017, 1), 3),
                      TimePeriod=rep(c(rep("Growing", 22), rep("Declining", 21)),3),
                      Age = c(rep("Recruit", 43), rep("SYReturn", 43), rep("ASYReturn", 43)), 
                      predicted=NA, 
                      se=NA,
                      ucl=NA, 
                      lcl=NA)

newdata$predicted <- predict(bmod, newdata, type="response")
newdata$variance <- predict(bmod, newdata, type="variance")

ggplot(newdata, aes(x=Year, y=predicted, color=Age))+
  geom_line( )+
  geom_ribbon(aes(ymin=predicted-variance, ymax=predicted+variance, fill=Age), alpha=0.4)
