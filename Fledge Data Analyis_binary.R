#Are birds fledging less often due to local weather conditions?
#Here we will exclude predated nests and look only at failure due to nestling death. 

library(tidyverse)
library(MuMIn)


#Read in the binary fledging data
dat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/Binary Fledge Success wo experimental nests.csv", as.is=T, na.strings = "" ) %>% filter(Daysabove18 <10)



#Now we have some weather data that we could use to do a binary glm
#############################################################
#Question 1: Are nestlings fledging less often in later years ? 
ggplot(dat %>% filter(Fledge/Hatch<=1), aes(x=Year, y=Fledge/Hatch))+
  geom_point(alpha=0.2)+
  stat_smooth()+
  geom_vline(xintercept=c(1996, 2014))+
  labs(y="Fledge Rate")


ggplot(dat, aes(x=Year, y=Fledge2))+
  geom_point(alpha=0.2)+
  stat_smooth(method="lm", aes(group=TimePeriod))+
  geom_vline(xintercept=c(1991, 2014))+
  labs(y="Fledge Rate")+
  geom_smooth()
#this all looks OK



dat$TimePeriod <- factor(dat$TimePeriod, levels= c("Growing", "Declining", "PostDecline"))



mod <- glm(Fledge2 ~ Year2*TimePeriod, family=binomial, data=dat)
plot(mod)
hist(resid(mod))
plot(resid(mod)~dat$Year2)
plot(resid(mod)~dat$TimePeriod)


options(na.action="na.fail")
dredge(mod) #need all terms. 

summary(aov(mod)) #not good for binomial
#Better options
car::Anova(mod)
anova(mod, test="Chisq")
#beta values
summary(mod)


#Yes! This is fantastic. It finally looks like our data is 


#################################################################
#Question: Is nest failure (excluding death due to predation) more common in
#bad weather, AND, are they surviving worse over the years?
#######Are nest failures NOT due to predadtion increasing at different rates?

#Let's remove the predation caused failures. 
NoPred <- dat %>% filter( !is.na(Fledge2) & (FailureCause2!="PREDATION" |is.na(FailureCause2)) & !is.na(Daysabove18) )

#write.csv(NoPred, "file:///C:/Users/11arc/Documents/Binary fledge success (no pred).csv", na="", row.names = F)


nopredMod1 <- glm(Fledge2 ~ Year2*TimePeriod, family=binomial(link="logit"), data=NoPred)
nopredMod2 <- glm(Fledge2 ~ Year2*TimePeriod, family=binomial(link="probit"), data=NoPred)
nopredMod3 <- glm(Fledge2 ~ Year2*TimePeriod, family=binomial(link="cauchit"), data=NoPred)
nopredMod4 <- glm(Fledge2 ~ Year2*TimePeriod, family=binomial(link="log"), data=NoPred)
nopredMod5 <- glm(Fledge2 ~ Year2*TimePeriod, family=binomial(link="cloglog"), data=NoPred)
#Quick check to pick the best link function
AICc(nopredMod1, nopredMod2, nopredMod3, nopredMod4, nopredMod5) #Cauchit link is the best by a lot

NoPred$TimePeriod<- factor(NoPred$TimePeriod, levels=c("Growing", "Declining", "PostDecline"))
NoPred$TimePeriod<- factor(NoPred$TimePeriod, levels=c("Declining", "Growing", "PostDecline"))
NoPred$TimePeriod<- factor(NoPred$TimePeriod, levels=c("PostDecline", "Declining", "Growing"))


plot(nopredMod3)
plot(resid(nopredMod3)~NoPred$Year2)
plot(resid(nopredMod3)~NoPred$TimePeriod) #Looks pretty good to me overall

#Not very good to use F stats for a binomial GLM because probably doesn't follow
#F distribution
summary(aov(nopredMod3))
#Very likely DOES follow a chisq distribution though so we will report those stats
car::Anova(nopredMod3)
anova(nopredMod3, test="Chisq")


dredge(nopredMod3)
summary(nopredMod3)
#definitely need the full model


oddsRat <- exp(coef(nopredMod3))
get.or.se <- function(model) {
  broom::tidy(model) %>% 
    mutate(or = exp(estimate),
           var.diag = diag(vcov(model)),
           or.se = sqrt(or^2 * var.diag)) %>%
    select(or.se) %>% unlist %>% unname
}

oddsRatSE <- get.or.se(nopredMod3)

ggplot(NoPred, aes(y=Fledge2, x=Year2*10+1975, group=TimePeriod))+
  ylim(0, 1)+
  stat_smooth(method="glm", method.args = list(family=binomial(link="cauchit")))+
  labs(y="Probability of \nFledging Success", x="Year")+
  geom_vline(xintercept = c(1991, 2013 ))+
  ggthemes::theme_few(base_size = 16)+
  scale_x_continuous(breaks=c(1980, 1991, 2002, 2013))+
  theme(text = element_text(size=20), axis.title.y = element_text(angle=0, vjust=0.5))
ggsave(filename='~/Masters Thesis Project/BGRS symposium presentation/Fledging success through time Plot.jpeg', width=9, height=5, units="in", device="jpeg")



PanelA <- ggplot(NoPred, aes(y=Fledge2, x=Year2*10+1975, group=TimePeriod))+
  ylim(0, 1)+
  stat_smooth(method="glm", method.args = list(family=binomial(link="cauchit")), color="black")+
  labs(y="Probability of \nFledging Success", x="Year")+
  geom_vline(xintercept = c(1991, 2013 ), linetype="dashed")+
  theme_classic(base_size = 16, base_family = "serif")+
  scale_x_continuous(breaks=c(1980, 1991, 2002, 2013))


PanelA_2 <- ggplot(NoPred, aes(y=Fledge2, x=Year2*10+1975, group=TimePeriod))+
  ylim(0, 1)+
  stat_smooth(method="glm", method.args = list(family=binomial(link="cauchit")), color="black")+
  labs(y="Probability of \nFledging Success", x="Year")+
  geom_vline(xintercept = c(1991, 2013 ), linetype="dashed")+
  geom_count(shape=1)+
  theme_classic(base_size = 16, base_family = "serif")+
  scale_x_continuous(breaks=c(1980, 1991, 2002, 2013))+
  theme(legend.position = c(0.1, 0.3), legend.background = element_rect(fill=alpha('white', 0)), legend.text = element_text(size=12), legend.key.size = unit(3,"mm"))
  
#Yes During the decline, fledge success was lower and declined

#################################################################
#Question: Is more days above 18 when the nestlings are vulnerable to weather (day 0-8 inclusive) predictive of better nest success? 
daysMod_nopred <- glm(Fledge2 ~  TimePeriod*Daysabove18, family=binomial(link="cauchit"), data=NoPred)

plot(daysMod_nopred)
plot(resid(daysMod_nopred)~predict(daysMod_nopred))
plot(resid(daysMod_nopred)~NoPred$Year2)
plot(resid(daysMod_nopred)~NoPred$TimePeriod)
plot(resid(daysMod_nopred)~NoPred$Daysabove18)
#Looks ok

dredge(daysMod_nopred)

#Don't use the F stat
summary(aov(daysMod_nopred))  

#Use the chisq instead
car::Anova(daysMod_nopred)
anova(daysMod_nopred, test="Chisq")

summary(mamdaysMod_nopred)
summary(daysMod_nopred)

mamdaysMod_nopred <- glm(Fledge2 ~  TimePeriod+Daysabove18, family=binomial(link="cauchit"), data=NoPred)
summary(aov(mamdaysMod_nopred))
summary(mamdaysMod_nopred)
#Yes. More days above 18 mean that you are more likely to fledge.  

oddsRateDays <- exp(coef(mamdaysMod_nopred))
oddsRatSEDays <- get.or.se(mamdaysMod_nopred)


newdata_days <- data.frame(Daysabove18=rep(seq(0, 9, 1),3),
                       TimePeriod=c(rep("Growing", 10), rep("Declining", 10), rep("PostDecline", 10)),
                       predicted_logit=NA,
                       predicted=NA, 
                       se_logit=NA,
                       use=NA, 
                       lse=NA)


std <- qnorm(0.95 / 2 + 0.5)
newdata_days$predicted_logit <- predict(mamdaysMod_nopred, newdata_days, type="link", se=T)$fit
newdata_days$se_logit <- predict(mamdaysMod_nopred, newdata_days, type="link", se=T)$se
newdata_days$lcl <- mamdaysMod_nopred$family$linkinv(newdata_days$predicted_logit - std * newdata_days$se_logit)
newdata_days$ucl <- mamdaysMod_nopred$family$linkinv(newdata_days$predicted_logit + std * newdata_days$se_logit)
newdata_days$predicted <- mamdaysMod_nopred$family$linkinv(newdata_days$predicted_logit)  # Rescale to 0-1


newdata_days$TimePeriod <- factor(newdata_days$TimePeriod, levels=c("Growing", "Declining", "PostDecline"))

ggplot(newdata_days, aes(x=Daysabove18, y=predicted))+
  geom_line(size=1, aes(group=TimePeriod))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=TimePeriod), alpha=0.2)+
  labs(x="Days of good weather \nduring early development", y="Fledging\nSuccess", fill="Population \nStatus", color="Population \nStatus")+
  ggthemes::theme_few(base_size = 20)+
  theme(text = element_text(size=20), axis.title.y = element_text(angle=0, vjust=0.5))+
  scale_x_continuous(breaks=c(1, 3, 5, 7, 9))+
  scale_fill_manual(values=c("forestgreen", "red2", "gold2"), labels=c("Growing", "Declining", "Post Decline"))
ggsave(filename='~/Masters Thesis Project/BGRS symposium presentation/Fledging Success and weather Plot.jpeg', width=9, height=5, units="in", device="jpeg")


PanelB <- ggplot(newdata_days, aes(x=Daysabove18, y=predicted))+
  geom_line(size=1, aes(group=TimePeriod, linetype=TimePeriod))+
  geom_ribbon(aes(ymin=ucl, ymax=lcl, fill=TimePeriod), alpha=0.5)+
  labs(x="Days of good weather", y="Probability of \nFledging Success", fill="",linetype="", color="Population \nStatus")+
  theme_classic(base_size = 16, base_family = "serif")+
  theme(legend.position = c(0.85, 0.3), legend.background = element_rect(fill=alpha('white', 0)))+
  scale_x_continuous(breaks=c(1, 3, 5, 7, 9))+
  scale_fill_manual(values=c("gray15", "gray40", "grey80"), labels=c("Growing", "Declining", "Post Decline"))+
  scale_linetype_manual(values=c("solid", "dashed", "dotted"), labels=c("Growing", "Declining", "Post Decline"))

PanelB

PanelB_2 <- ggplot(newdata_days, aes(x=Daysabove18, y=predicted))+
  geom_line(size=1)+
  geom_ribbon(aes(ymin=ucl, ymax=lcl), alpha=0.5)+
  labs(x="Days of good weather", y="Probability of \nFledging Success",  shape="Population \nStatus")+
  theme_classic(base_size = 16, base_family = "serif")+
  geom_count(data=NoPred, aes(x=Daysabove18, y=Fledge2), shape=1)+
  theme(legend.position = c(0.85, 0.3), legend.background = element_rect(fill=alpha('white', 0)), legend.text = element_text(size=12), legend.key.size = unit(3,"mm"))+
  scale_x_continuous(breaks=c(1, 3, 5, 7, 9))+
  facet_grid(~TimePeriod)

PanelB_2

#################################################################
#Are there fewer days above 18 during vulnerable periods for non-predated nests? 
#Calculate the mean days above 18 for nests in each year. 

YearSummary <- dat %>% 
  group_by(Year2, TimePeriod) %>% 
  summarise(MeanHatchDate=mean(HatchDate), 
            MeanDaysabove18= mean(Daysabove18, na.rm=T), 
            Daysabove18_2 = NA,
            RatioFledgeFail= length(Fledge2>0)/length(Fledge2),
            Year=NA) 

YearSummary$Year <- YearSummary$Year2*10+1975


YearSummary$TimePeriod <- factor(YearSummary$TimePeriod, levels=c("Growing", "Declining", "PostDecline"))
write.csv(YearSummary, "file:///C:/Users/11arc/Documents/Annual Summary of Days above 18.csv", na="", row.names = F)


mod2 <- lm(MeanDaysabove18~Year2*TimePeriod, data=YearSummary)
plot(mod2)
plot(resid(mod2)~YearSummary$Year2)
plot(resid(mod2)~YearSummary$TimePeriod)
#Looks good

dredge(mod2)
summary(aov(mod2))
#Yup definitely need year but don't need time period

mam2 <- lm(MeanDaysabove18~Year2, data=YearSummary)
summary(mam2)
anova(mam2, test="F")
#We are seeing that MeanDaysabove 18 is declining across time, consistantly not differnt through time periods

ggplot(YearSummary, aes(y=MeanDaysabove18, x=Year))+
  geom_point()+
  geom_smooth(method="lm", color="black")+
  labs(y="Mean days of\ngood weather", x="Year")+
  ggthemes::theme_few(base_size = 20)+
  theme(text = element_text(size=20), axis.title.y = element_text(angle=0, vjust=0.5))

ggsave(filename='~/Masters Thesis Project/BGRS symposium presentation/Weather through time plot.jpeg', width=7, height=5, units="in", device="jpeg")

  

PanelC <- ggplot(YearSummary, aes(y=MeanDaysabove18, x=Year))+
  geom_point()+
  geom_smooth(method="lm", color="black")+
  labs(y="Mean days of \ngood weather", x="Year")+
  theme_classic(base_size = 16, base_family = "serif")



library(cowplot)


plot_grid(PanelA, PanelB,PanelC, nrow=3, ncol=1, labels=c("a", "b", "c"), label_size = 20, label_fontfamily = "serif")
ggsave(filename='~/Masters Thesis Project/Long term trends paper/Plots for paper/PDF Figures/Figure 2.pdf', width=5, height=9, units="in", device="pdf")



ggsave(filename='~/Masters Thesis Project/Long term trends paper/Plots for paper/Fledging plot.jpeg', width=3.5, height=8, units="in", device="jpeg")






plot_grid(PanelA_2, PanelB_2, nrow=2, ncol=1, labels=c("a", "b"), label_size = 20, label_fontfamily = "serif")



ggsave(filename='~/Masters Thesis Project/Long term trends paper/Plots for paper/Fledging plot with points.jpeg', width=4, height=6, units="in", device="jpeg")




