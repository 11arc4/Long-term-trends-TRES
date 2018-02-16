#Are nestlings fledging at a lower conditions than they did in the past? 

#I'm interested in knowing this because it could help explain why we see
#differences in recruitment across years. It could also explain why local
#weather conditions only affect survival when the population is declining. If
#the juveniles are poor quality they don't have much buffer and both they and
#their parents may have to work harder to feed themselves in poor weather. 
library(tidyverse)
library(lme4)
library(MuMIn)


dat <- read.csv("~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Extracted Data for Analysis/Nestling Measurements for Analysis 1975-2017.csv", as.is=T)

dat2 <- dat %>% filter(mass<30, age>0, age<24, mass>10)


dat2$popStatus <- "Declining"
dat2$popStatus[dat2$year<=1996]<- "Increasing"
dat2$popStatus <- factor(dat2$popStatus, levels=c("Increasing", "Declining"))




dat3 <- dat2 %>% filter(!is.na(year) & !is.na(age) & age>9 & !is.na(tarsus) & tarsus<20, tarsus>8)
length(unique(dat3$nestlingID))
#Data set has 4916 rows, and 1036 unique nests, 4696 unique nestlings. I wonder
#if I should specifically take only the later measurement (since most of the
#earlier year have only measurements of older birds this is better). We only
#have one measurement for MOST nestlings so it doesn't really make sense to
#include a random effect for nestling.
dat3 <- dat3 %>% arrange(nestlingID, age)
dat4 <- dat3[!duplicated(dat3$nestlingID),]



plot(mass ~ tarsus, data=dat4)

mod_pre <- lm(mass~tarsus, data=dat4)
plot(mod)
dat4$condition <- resid(mod_pre)
#calculate condition based on residuals of mass and tarsus



#Scale year to look at it in terms of decades since 1975. This is still
#interpretable easily, but avoids issues of scale in the modelling
dat4$year2 <- (dat4$year-1975)/10


mod <- lmer(condition ~ year2*age + (1|nestID), data=dat4)

plot(mod) #this looks ok but not perfect
hist(resid(mod))
plot(resid(mod)~dat4$year2)
plot(resid(mod)~dat4$age)
plot(resid(mod)~factor(dat4$nestID))

#this looks ok I think this all fits well enough
summary(mod)
#I won't remove nestID. It's clearly explaining a lot of variation. And if we
#compare AIC to the lm, we find that the random effect is important (as
#expected)

AICc(mod)
AIC(mod)
mod1 <- lm(condition ~ year2*age, data=dat4)
plot(mod1) #hah this is even better
hist(resid(mod1))
plot(resid(mod1)~dat3$year2)
plot(resid(mod1)~dat3$age)
plot(resid(mod1)~factor(dat3$nestID))

AICc(mod1)
AIC(mod1)

#Do we need the interacction?
car::Anova(mod) # No we just need year and age

mod2 <- lmer(condition ~ year2 + age + (1|nestID), data=dat4)
anova(mod, mod2)
summary(mod2)

mod3 <- lmer(condition ~ year2 + (1|nestID), data=dat4)
anova(mod2, mod3) #yes they are different, need to keep the age

mod4 <- lmer(condition ~ age + (1|nestID), data=dat4)
anova(mod2, mod4)
#also need year. 

mam <- mod2

options(na.action = "na.fail") 
MuMIn ::dredge(mod) #Dredge also supports a difference between years, but is not as sure about age effects. 





newdata <- data.frame(age = c(rep(12,27), rep(16, 27)),
                      year2 = rep(seq(1.6,4.2, .1),2), 
                      predicted=NA,
                      lcl=NA, 
                      ucl=NA
)

#calculate predicted values. 
newdata$predicted <- predict(mam, newdata, re.form=NA)
#age= 16 for the ready to fledge birds and 12 for the not

#bootstrap confidence intervales based on https://github.com/lme4/lme4/issues/388 
## param only (not including individual variation or anything like that)
b3 <- bootMer(mam,FUN=function(x) predict(x,newdata=newdata,re.form=~0),
              ## re.form=~0 is equivalent to use.u=FALSE
              nsim=100,seed=101)

bootsum <- function(x,ext="_1") {
  d <- t(data.frame(apply(x$t,2,
                          function(x) c(mean(x),quantile(x,c(0.025,0.975))))))
  colnames(d) <- c("bpred","lwr","upr")
  return(d)
}

newdata[3:5] <- bootsum(b3,"_3")



newdata$age <- as.factor(newdata$age)

Conditionplot <- ggplot(newdata, aes(x=year2*10+1975))+
  geom_line(aes(y=predicted, color=age))+
  geom_ribbon(aes( ymin=lcl, ymax=ucl, fill=age), alpha=0.3)+
  labs(y="Condition", x="Year", color="Age (days)", fill="Age (days)")+
  scale_fill_manual(values=c("steelblue","orchid3"))+
  scale_color_manual(values=c("steelblue","orchid3"))+
  xlim(1975,2017)+
  theme_classic(base_size = 16)
#Birds should be in better condition by the time they fledge (they're putting on
#mass for it probably), but we are seeing that birds have consistantly declining
#condition.





cowplot::plot_grid(Massplot, Conditionplot, nrow=2, labels=c("a", "b"), label_size = 18)

