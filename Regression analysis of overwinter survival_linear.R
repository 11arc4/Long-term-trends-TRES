#3 linear regressions one for each age class's survival estimates
library(tidyverse)
library(betareg)
library(MuMIn)
#Matt Guzzo suggested pulling the MARK estimates of survival out and using those
#as data points in a regression analysis. Then we can follow a similar analysis
#as the fledging analysis.

Survival <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/Yearly Survival Estimates.csv", na.strings="", as.is=T)


Survival$TimePeriod <- NA
Survival$TimePeriod[which(Survival$Year<1992)]<- "Growing"
Survival$TimePeriod[which(Survival$Year>1991)]<- "Declining"
#Only have 1 year post decline so let's definitel not group it off seperately!
#Survival$TimePeriod[which(Survival$Year>2014)]<- "PostDecline"
Survival$TimePeriod <- factor(Survival$TimePeriod, levels=c("Growing", "Declining"))
Survival$Age <- factor(Survival$Age)


#Remove the years and specific estimates that aren't any good (not enough birds
#went into them, or we are at a boundary), or there is no environmental data
Survival2 <- Survival %>% filter( Estimate<.9 & ((SE<0.2 & Age!="Recruit" ) |(SE<0.1 & Age=="Recruit")) & Year!=2016 & Year !=2011 )


ggplot(Survival2, aes(x=Year, y=Estimate, color=Age))+
  geom_segment(aes(x=Year, xend=Year, y=Estimate-SE, yend=Estimate+SE, color=Age), alpha=0.6)+
  geom_point( )+
  labs(x="Year", y="Apparent Survival" )+
  ggthemes::theme_few()+
  geom_smooth(method="lm", formula=y~x, aes(group=TimePeriod))+
  facet_grid(~Age)
#Damn that recruitment looks a hell of a lot like the curve the population is showing......


Survival2$Year2 <- (Survival2$Year-1975 )/10



#A linear regression didn't fit when we tried to do all three age classes
#together. I wonder if we could do them each seperately and use a linear
#regression successfully there?



################Does ASY survival decline? 
ASYSurvival2 <- Survival2 %>% filter(Age=="ASYReturn")
modASY <- lm(Estimate ~ Year2*TimePeriod, data=ASYSurvival2)
plot(modASY)
hist(resid(modASY))
shapiro.test(resid(modASY))
plot(resid(modASY)~ASYSurvival2$Year2)
plot(resid(modASY)~ASYSurvival2$TimePeriod)

options(na.action="na.fail")
dredge(modASY)
#Null model is the best. No changes over time, even just time period
anova(modASY, test="F")
summary(aov(modASY))

summary(modASY)

################Does SY survival decline?
SYSurvival2 <- Survival2 %>% filter(Age=="SYReturn")
plot(SYSurvival2$Estimate~SYSurvival2$Year2)
modSY <- lm(log(Estimate) ~ Year2*TimePeriod, data=SYSurvival2)
plot(modSY)
hist(resid(modSY))
shapiro.test(resid(modSY))
plot(resid(modSY)~SYSurvival2$Year2)
plot(resid(modSY)~SYSurvival2$TimePeriod)


#Hmmmm SY doesn't fit great. Resids aren't normal. They're definitely the
#problem. Does logging help? Yes. WHen we log it seems to be acceptable.
#although still not exactly perfect.

options(na.action="na.fail")
dredge(modSY)
summary(aov(modSY))
anova(modSY, test="F")
#Similar to ASY, there isn't really any strong evidence for changing SY return
#rates, and if there was, it looks like SYs are coming back slightly more


################Does recruitment decline?
RecruitSurvival2 <- Survival2 %>% filter(Age=="Recruit")
modRecruit <- lm(Estimate ~ Year2*TimePeriod, data=RecruitSurvival2)
plot(modRecruit)
hist(resid(modRecruit))
shapiro.test(resid(modRecruit))
plot(resid(modRecruit)~RecruitSurvival2$Year)
plot(resid(modRecruit)~RecruitSurvival2$TimePeriod)
#normality of residuals isn't perfect, but shapiro test doesn't identify as
#being super off, and Q-Q plot is probably acceptable. The thing that is worst
#is the hist. I think we are OK to go with it. 

options(na.action="na.fail")
dredge(modRecruit)
summary(aov(modRecruit))
anova(modRecruit, test="F")
#Recruitment is changing across time

summary(modRecruit)

RecruitSurvival2$TimePeriod <- factor(RecruitSurvival2$TimePeriod, levels=c("Declining", "Growing"))
summary(modRecruit)
#Recruitment grew 6.9% per decade while the population was growing, and has
#declined 1.7% per decade while the population was declining.




Survival2$Age <- factor(Survival2$Age, levels=c("Recruit", "SYReturn", "ASYReturn"))
Age_names <- c(`Recruit`="Juvenile", 
                `ASYReturn`="Older Female", 
                `SYReturn`="1-yr-old Female")

ggplot(Survival2, aes(x=Year, y=Estimate))+
  geom_point(aes(color=Age), show.legend=F, size=2)+
  stat_smooth(data=Survival2%>% filter(Age=="Recruit"), method="lm", formula=y~x, aes(group=TimePeriod), color="black")+
  facet_grid(.~Age, labeller=labeller(Age=as_labeller(Age_names)))+
  labs(x="Year", y="Survival \nOverwinter")+
  ggthemes::theme_few(base_size = 22)+
  theme(axis.title.y=element_text(angle=0, vjust=0.5))+
  scale_color_manual(values=c("azure4", "burlywood4", "deepskyblue4"))
ggsave(filename='~/Masters Thesis Project/BGRS symposium presentation/Overwinter Survival Plot.jpeg', width=12, height=6, units="in", device="jpeg")






