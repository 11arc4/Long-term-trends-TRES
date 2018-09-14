#Regression analysis of environmental causes of recruitment decline using simple linear regression

#this is based on my "Regression analysis of overwinter survival" where I did
#all 3 age classes. I thought that the first graph shows that really only
#recruitment has crashed, so perhaps interpretation would be easier if I only
#looked at the recruits in environmental models. They might well be affected
#differently.
library(MuMIn)
library(tidyverse)

############MAKE THE DATA
Survival <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/Yearly Survival Estimates.csv", na.strings="", as.is=T)

Survival$TimePeriod <- NA
Survival$TimePeriod[which(Survival$Year<1992)]<- "Growing"
Survival$TimePeriod[which(Survival$Year>1991)]<- "Declining"
#Survival$TimePeriod[which(Survival$Year>2014)]<- "PostDecline"
#We don't have enough post decline data to really use this. 
Survival$TimePeriod <- factor(Survival$TimePeriod, levels=c("Growing", "Declining"))
Survival$Age <- factor(Survival$Age)

#Remove the years and specific estimates that aren't any good (not enough birds
#went into them, or we are at a boundary)
Survival2 <- Survival %>% filter( Estimate<.9 & Age=="Recruit" & SE<0.1 & Year!=2016 & Year !=2011 )

Survival2$Year2 <- (Survival2$Year-1975 )/10

#Do any of the environmental varaibles that we have data for predict survival well? 
#Load in all the environmental data
#weather on the breeding ground
YearlyFledge <- readRDS("~/Masters Thesis Project/TRES Data Analysis/Vital Rates Models/RMark Preliminary Survival Analysis/LocalWeather")
YearlyFledge <- YearlyFledge %>% arrange(Year)
#acreage sugar cane
presugar <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Sugar Cane Acreage Available USDA survey.csv", as.is=T, na.strings =c(""))
sugar <- presugar[,c(2, 19, 21, 23,25)]
colnames(sugar) <- c("year", "acreCaneSeed", "tonsCaneSeed", "acreCaneSeedSugar", "acreCaneSugar")
rm(presugar)
sugar$acreCaneSeed[which(sugar$year>1974 & sugar$year<1997)]/10000
sugar$acreCaneSeed[which(sugar$year>1996)]/10000
sugar$acreCaneSeed <- as.numeric(gsub(',', '', sugar$acreCaneSeed))
#ENSO
ENSOdat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Monthly El Nino Southern Oscillation index.csv", as.is=T)[,1:13]
colnames(ENSOdat) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
ENSOdat<- ENSOdat %>% arrange(Year)
for(i in 2:nrow(ENSOdat)){
  ENSOdat$ENSOWinter[i] <-  mean(c(ENSOdat$Mar[i-1], ENSOdat$Dec[i-1], ENSOdat$Jan[i], ENSOdat$Feb[i]), na.rm=T)
}
#Hurricanes
hurricane <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Hurricanes.csv")

Survival2$WinterENSO <- NA
Survival2$DaysBelow18_mean <- NA
Survival2$DaysBelow18_max <- NA
Survival2$Hurricanes <- NA
Survival2$SugarAcreage <- NA

#Match all of these parameters of interest up with the survival estimates. 
for (i in 1:nrow(Survival2)){
  Year<- Survival2$Year[i]
  Survival2$SugarAcreage[i] <- sugar$acreCaneSeed[Year==sugar$year]
  Survival2$DaysBelow18_mean[i] <- YearlyFledge$DaysBelow18_mean [Year==YearlyFledge$Year]
  Survival2$DaysBelow18_max[i] <- YearlyFledge$DaysBelow18_max [Year==YearlyFledge$Year]
  Survival2$Hurricanes[i] <- hurricane$Hurricanes[Year==hurricane$Year]
  Survival2$WinterENSO[i] <- ENSOdat$ENSOWinter[Year==ENSOdat$Year]
}

Survival2$SugarAcreage2 <- Survival2$SugarAcreage/ 10000
Survival2$DaysAbove18_max <- 28-Survival2$DaysBelow18_max


ggplot(Survival2, aes(x=Year, y=Estimate))+
  geom_point()+
  geom_point(data=Survival2 %>%filter(SE>0.1),aes(x=Year, y=Estimate), color="red" )+
  geom_vline(xintercept = 1991)+
  stat_smooth(method="lm", aes(group=TimePeriod))


#WIll simple linear regression fit our data here?

#Let's swap estimates into percentages.
Survival2$Recruitment<- Survival2$Estimate*100



#Does winter ENSO predict recruitment?
ggplot(Survival2, aes(x=WinterENSO, y=Recruitment, color=TimePeriod))+
  geom_point()+
  #geom_smooth(se=F)+
  stat_smooth(method="lm", aes(group=TimePeriod), se=F)
  #facet_grid(~TimePeriod)

mod_ENSO <- lm(Recruitment ~ WinterENSO*TimePeriod, data=Survival2)
plot(mod_ENSO)
hist(resid(mod_ENSO))
shapiro.test(resid(mod_ENSO))
plot(resid(mod_ENSO)~Survival2$WinterENSO)
plot(resid(mod_ENSO)~Survival2$TimePeriod)
#THis really looks like maybe it fits good! Residuals appear normal and there are no leverage points. 

summary(aov(mod_ENSO))
anova(mod_ENSO, test="F")
dredge(mod_ENSO)

mam_ENSO <- lm(Recruitment ~ WinterENSO+TimePeriod, data=Survival2)
summary(mam_ENSO)



ggplot(Survival2, aes(x=WinterENSO, y=Recruitment))+
  geom_point()+
  geom_line(aes(x=WinterENSO, y=predict(mam_ENSO), color=TimePeriod))+
  #geom_smooth(se=F)+
  facet_grid(~TimePeriod)


#Does sugar acreage predict recruitment?
ggplot(Survival2, aes(x=SugarAcreage2, y=Recruitment))+
  geom_point()+
  stat_smooth(method="lm", se=F)
#This looks like it probably won't show anything but we will see. 

mod_sugar <- lm(Recruitment ~ SugarAcreage2*TimePeriod, data=Survival2)
plot(mod_sugar)
hist(resid(mod_sugar))
shapiro.test(resid(mod_sugar))
plot(resid(mod_sugar)~Survival2$TimePeriod)
plot(resid(mod_sugar)~Survival2$SugarAcreage2)
#This all looks ok. we aren't violating assumptions and appear to predict things pretty well. 

dredge(mod_sugar)
anova(mod_sugar, test="F")
summary(mod_sugar)

#sugar acreage isn't a good predictor, 
mam_sugar <- lm(Recruitment ~ TimePeriod, data=Survival2)
summary(mam_sugar)


#Do hurricanes predict recruitment?
ggplot(Survival2, aes(x=Hurricanes, y=Recruitment))+
  geom_point()+
  stat_smooth(method="lm", se=F)+
  facet_grid(~TimePeriod)

mod_hurricane<- lm(Recruitment ~ TimePeriod*Hurricanes, data=Survival2)
plot(mod_hurricane)
hist(resid(mod_hurricane))
shapiro.test(resid(mod_hurricane))
plot(resid(mod_hurricane)~Survival2$TimePeriod)
plot(resid(mod_hurricane)~Survival2$Hurricanes)

dredge(mod_hurricane)
summary(aov(mod_hurricane))
anova(mod_hurricane, test="F")
#hurricanes are shitty predictors. Time period alone is better

#Does days where the max temp stays below 18 or it rains post fledging predict recruitment?
ggplot(Survival2, aes(x=DaysAbove18_max, y=Recruitment))+
  geom_point()+
  geom_line(aes(x=DaysAbove18_max, y=predict(mam_days)))+
  #stat_smooth(method="lm", se=T)+
  facet_grid(~TimePeriod)

#Probably not going to really predict much. 

Survival3 <- Survival2[-c(2,14),]
mod_days <- lm(Recruitment~DaysAbove18_max*TimePeriod, data=Survival2)
plot(mod_days)
hist(resid(mod_days))
shapiro.test(resid(mod_days))
plot(resid(mod_days)~Survival2$DaysAbove18_max)
plot(resid(mod_days)~Survival2$TimePeriod)
#All looks good

dredge(mod_days)
summary(aov(mod_days))
anova(mod_days, test="F")

summary(mod_days)
Survival2$TimePeriod <- factor(Survival2$TimePeriod, levels=c("Declining", "Growing"))

mam_days_nolev <- lm(Recruitment~DaysAbove18_max+TimePeriod, data=Survival3)
mam_days <- lm(Recruitment~DaysAbove18_max*TimePeriod, data=Survival2)
summary(mam_days)
summary(mam_days_nolev)

#The leverage really does NOTHING to the line. 

#

mam_null <- lm(Recruitment~TimePeriod, data=Survival2)


AICcTable <- AICc(mam_ENSO, mam_sugar, mam_days, mam_null)
AICcTable$Delta <- AICcTable$AICc-AICcTable$AICc[1]

#What about if we combined all the best bits? These things change substantially
#based on what we count as important bits to include. I don't really feel super
#comfortable doing this.
mod_all <- lm(Recruitment~WinterENSO+SugarAcreage2+DaysBelow18_max+TimePeriod, data=Survival2)
plot(mod_all)
hist(resid(mod_all))
shapiro.test(resid(mod_all))
plot(resid(mod_all)~Survival2$TimePeriod)
plot(resid(mod_all)~Survival2$SugarAcreage2)
plot(resid(mod_all)~Survival2$Hurricanes)
plot(resid(mod_all)~Survival2$WinterENSO)
plot(resid(mod_all)~Survival2$DaysBelow18_max)

dredge(mod_all)
anova(mod_all, test="F")
#These don't agree at all. I suspect this is because they 

summary(mod_all)

mam_all <- lm(Recruitment~TimePeriod+SugarAcreage2+Hurricanes, data=Survival2)
summary(mam_all)

AICc(mam_all)


ggplot(Survival2, aes(x=SugarAcreage2, y=Recruitment, color=Hurricanes))+
  geom_point()+
  stat_smooth(method="lm", se=F)+
  facet_grid(~TimePeriod)+
  ggthemes::theme_few()



library(car)
scatter3d(formula= Recruitment ~SugarAcreage2 + Hurricanes, data=Survival2 %>% filter(TimePeriod=="Declining") )

scatter3d(formula= Recruitment ~SugarAcreage2 + Hurricanes|TimePeriod, data=Survival2  )


scatter3d(formula= Recruitment ~SugarAcreage2 + Hurricanes, data=Survival2 %>% filter(TimePeriod=="Growing") )



#Have the important weather variables changed overtime? 

#WinterENSO
ggplot(ENSOdat, aes(x=Year, y=ENSOWinter))+
  geom_point()

ENSOdat$TimePeriod <- "Growing"
ENSOdat$TimePeriod[ENSOdat$Year>1991] <- "Declining"
ENSOdat$TimePeriod <- factor(ENSOdat$TimePeriod)
ENSOdat2 <- ENSOdat %>% filter(Year>=1975)
modENSO <- lm(ENSOWinter ~Year*TimePeriod, data=ENSOdat2)
plot(modENSO)
hist(resid(modENSO)) #Not ideal but really not so bad
shapiro.test(resid(modENSO))
plot(resid(modENSO)~ENSOdat2$Year)
plot(resid(modENSO)~ENSOdat2$TimePeriod) #There's more variance when the population is declinng. 

dredge(modENSO)
anova(modENSO, test="F")
#There really have been no changes in ENSO over the time we've been looking. 


YearlyFledge$DaysAbove18 <- 28-YearlyFledge$DaysBelow18_max
YearlyFledge$TimePeriod <- "Growing"
YearlyFledge$TimePeriod[YearlyFledge$Year>1991]<- "Declining"
YearlyFledge$TimePeriod<- factor(YearlyFledge$TimePeriod)
YearlyFledge$Year2 <- (YearlyFledge$Year-1975)/10

YearlyFledge2 <- YearlyFledge%>% filter(!is.na(DaysAbove18))
ggplot(YearlyFledge2, aes(x=Year, y=DaysAbove18))+
  geom_point()+
  stat_smooth(method="lm")

modGooddays <- lm(DaysAbove18~TimePeriod*Year2, data=YearlyFledge2)
plot(modGooddays)
hist(resid(modGooddays))
shapiro.test(resid(modGooddays))
plot(resid(modGooddays)~YearlyFledge2$Year)
plot(resid(modGooddays)~YearlyFledge2$TimePeriod)
dredge(modGooddays)
anova(modGooddays, test="F")

mamgooddays <- lm(DaysAbove18~Year2, data=YearlyFledge2)
summary(mamgooddays)
anova(mamgooddays, test="F")








#################
#Let's make the publication quality composite plot. 
Survival3 <- Survival %>% filter( Estimate<.9 & ((SE<0.2 & Age!="Recruit" ) |(SE<0.1 & Age=="Recruit")) & Year!=2016 & Year !=2011 )
Survival3$Age <- factor(Survival3$Age, levels = c("Recruit", "SYReturn", "ASYReturn"))

PanelA <- ggplot(Survival3 %>% filter(Age=="Recruit"), aes(x=Year, y=Estimate*100, group=TimePeriod))+
  geom_point()+
  stat_smooth(method="lm", color="black")+
  labs(x="Year", y="Juvenile survival \noverwinter (%)")+
 theme_classic(base_size = 16, base_family = "serif")
PanelA


PanelB <- ggplot(Survival2, aes(x=WinterENSO, y=Recruitment))+
  geom_point()+
  labs(y="Juvenile survival \noverwinter (%)", x="Mean ENSO (Dec-Mar)")+
  theme_classic(base_size = 16, base_family = "serif")+
  geom_line(aes(x=WinterENSO, y=predict(mam_ENSO)))+
  geom_ribbon(aes(x=WinterENSO, ymin=predict(mam_ENSO)-1.96*predict(mam_ENSO,se=T)$se.fit, ymax=predict(mam_ENSO)+1.96*predict(mam_ENSO,se=T)$se.fit), alpha=0.2)+
  facet_grid(~TimePeriod)
PanelB

PanelC <- ggplot(Survival2, aes(x=DaysAbove18_max, y=Recruitment))+
  geom_point()+
  labs(y="Juvenile survival \noverwinter (%)", x="Mean days of good \nweather post-fledging")+
  theme_classic(base_size = 16, base_family = "serif")+
  stat_smooth(method="lm", color="black")+
  facet_grid(~TimePeriod)
PanelC




PanelD <- ggplot(YearlyFledge, aes(x=Year, y=28-DaysBelow18_max))+
  geom_point()+
  labs(x="Year", y="Mean days of good \nweather post-fledging")+
  stat_smooth(method="lm", formula=y~x, color="black")+
  theme_classic(base_size = 16, base_family = "serif")
PanelD


library(cowplot)
plot_grid(PanelA, PanelC, PanelB, PanelD, nrow=2, ncol=2, labels=c("a", "c", "b",  "d"), label_size = 20, label_fontfamily = "serif")
ggsave(filename='~/Masters Thesis Project/Long term trends paper/Plots for paper/Juvenile survival plot.jpeg', width=8, height=6, units="in", device="jpeg")
ggsave(filename='~/Masters Thesis Project/Long term trends paper/Plots for paper/PDF Figures/Figure 4.pdf', width=8, height=6, units="in", device="pdf")




ggdraw() +
  draw_plot(PanelA, x = 0, y = .6, width = 1, height = .4) +
  draw_plot(PanelB, x = 0, y = 0.3, width = .5, height = .3) +
  draw_plot(PanelC, x = .5, y = 0.3, width = .5, height = .3) +
  draw_plot(PanelD, x = 0, y = 0, width = .5, height = .3) +
  draw_plot(PanelE, x = .5, y = 0, width = .5, height = .3) +
  draw_plot_label(label = c("a", "b", "d", "c", "e"), size = 20,
                  x = c(0, 0, 0.5, 0, 0.5), y = c(1, 0.6, 0.6, 0.3, 0.3), family="serif")







ggplot(ENSOdat, aes(x=Year, y=ENSOWinter))+
  geom_point()+
  labs(x="Year", y="Mean ENSO \n(Dec-Mar)")+
  ggthemes::theme_few(base_size = 16, base_family = "serif")
ggsave(filename='~/Masters Thesis Project/Long term trends paper/Plots for paper/Supplemental ENSO through time plot.jpeg', width=4, height=3, units="in", device="jpeg")




PanelA <- ggplot(Survival %>% filter( Estimate<.9 & Age!="Recruit" & SE<0.2 & Year!=2016 ), aes(x=Year, y=Estimate*100))+
  geom_point()+
  facet_grid(~Age, labeller=as_labeller(c(`Recruit`="Juvenile", `SYReturn`="1yr-year-old female", `ASYReturn`= "Older female")))+
  labs(x="Year", y="Overwinter survival (%)")+
  ggthemes::theme_few(base_size = 16, base_family = "serif")

#Other panels are from the regression analysese of SY and ASY
ggdraw() +
  draw_plot(PanelA, x = 0, y = .5, width = 1, height = .5) +
  draw_plot(PanelB, x = 0, y = 0, width = .5, height = .5) +
  draw_plot(PanelC, x = .5, y = 0, width = .5, height = .5) +
  draw_plot_label(label = c("a", "b", "c"), size = 20,
                  x = c(0, 0, 0.5), y = c(1, 0.5, 0.5), family="serif")
ggsave(filename='~/Masters Thesis Project/Long term trends paper/Plots for paper/Supplemental Adult Overwinter Survival plot.jpeg', width=8, height=6, units="in", device="jpeg")




















#Plots for exit seminar!
ggplot(Survival2, aes(x=DaysAbove18_max, y=Recruitment))+
  geom_point()+
  labs(y="Juvenile \nsurvival \noverwinter \n(%)", x="Mean days of good \nweather post-fledging")+
  theme_classic(base_size = 16)+
  theme(axis.title.y=element_text(angle=0, vjust=0.5))+
  stat_smooth(method="lm", color="black")+
  facet_grid(~TimePeriod)
ggsave("~/Masters Thesis Project/Committee Meetings/Sept 4 2018 Defense/Juv survival with weather.jpeg", units="in", width=6.5, height=4)



ggplot(YearlyFledge, aes(x=Year, y=28-DaysBelow18_max))+
  geom_point()+
  labs(x="Year", y="Mean days \nof good \nweather \npost-fledging")+
  stat_smooth(method="lm", formula=y~x, color="black")+
  theme_classic(base_size = 16, base_family = "serif")+
  theme(axis.title.y=element_text(angle=0, vjust=0.5))
ggsave("~/Masters Thesis Project/Committee Meetings/Sept 4 2018 Defense/PostFledging weather.jpeg", units="in", width=6.5, height=4)






