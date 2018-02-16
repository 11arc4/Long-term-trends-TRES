#Are nestlings fledging at a lower weight than they did in the past? 

#I'm interested in knowing this because it could help explain why we see
#differences in recruitment across years. It could also explain why local
#weather conditions only affect survival when the population is declining. If
#the juveniles are poor quality they don't have much buffer and both they and
#their parents may have to work harder to feed themselves in poor weather. 
library(tidyverse)


dat <- read.csv("~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Extracted Data for Analysis/Nestling Measurements for Analysis 1975-2017.csv", as.is=T)


#There look to be some points in there that must be typos or misrecordings because there's no way the bird was that size. 
ggplot(dat %>% filter(mass<30, age>0, age<24), aes(x=age, y=mass))+
  geom_point()+
  facet_grid(~popStatus)


dat2 <- dat %>% filter(mass<30, age>0, age<24)


dat2$popStatus <- "Declining"
dat2$popStatus[dat2$year<=1996]<- "Increasing"
dat2$popStatus <- factor(dat2$popStatus, levels=c("Increasing", "Declining"))

ggplot(dat2 %>% filter(age>=10), aes(x=age, y=mass))+
  geom_point()+
  facet_grid(~popStatus)+
  geom_smooth(method="lm")+
  ggthemes::theme_few()

#looks like even at the very coarse population status level, we may be seeing
#differences in mass for the older birds.



#OK let's try modelling changes in nestling mass through time. If we model all
#the ages we probably need to include something to deal with the polynomial
#nature of the data. Instead, perhaps it's better to only look at the last
#measurements (day 10 and on), when they are closer to fledging
dat3 <- dat2 %>% filter(!is.na(year) & !is.na(age) & age>9)
length(unique(dat3$nestID))
#Data set has 28, 577 rows, and 2,915 unique nests, 13,842 unique nestlings. 


#Scale year to look at it in terms of decades since 1975. This is still
#interpretable easily, but avoids issues of scale in the modelling
dat3$year2 <- (dat3$year-1975)/10

library(lme4)
mod <- lm(mass ~ year*age, data=dat3)
plot(mod)
plot(resid(mod)~dat3$year)
plot(resid(mod)~as.factor(dat3$age))

#this doesn't look perfectly good. Perhaps if we include the randome effects things will be better. 

#mod1 <- lmer(mass ~ year*age + (1|nestID/nestlingID), data=dat3)
#is be equivalent to
mod <- lmer(mass ~ year2*age + (1|nestID) + (1|nestlingID), data=dat3)
#since my nestlingIDs are unique so the same nestling only ever falls within one
#nest (according to
#https://stats.stackexchange.com/questions/228800/crossed-vs-nested-random-effects-how-do-they-differ-and-how-are-they-specified),
#but mod runs literally just SOOOO much faster so I will keep that one instead. :)

plot(mod)
plot(resid(mod)~dat3$year2) #Looks pretty decent
plot(resid(mod)~as.factor(dat3$age)) #this is better 

summary(mod) #f the SD of the random effect is smaller than the variance then we should keep the random effect

#Do we need to keep the nestling random effect?

mod2 <- lmer(mass ~ year2*age + (1|nestID), data=dat3)

anova(mod, mod2) #Yes we do. The models are different and with the random effect is a better estimate



options(na.action = "na.fail") 
MuMIn ::dredge(mod)
#There's only one top model-- the full model (mod)

mam2 <- mod

summary(mam)
#Looks like older nestlings, in later years have lower mass. This matches our predictions. Feeling good about this. 

#Let's make a plot. 

#let's group the nestling ages as younger (10-14), and older (15 and up) based
#on whether they should be capable of fledging now or not
dat3$ageClass <- "Ready to fledge"
dat3$ageClass[dat3$age<15]<- "Not ready to fledge"

ggplot(dat3, aes(x=year, y=mass))+
  geom_point(alpha=0.3)+
  facet_grid(~ageClass)
  

newdata2 <- data.frame(age = c(rep(12,43), rep(16, 43)),
                      year2 = rep(seq(0,4.2, .1),2), 
                      predicted=NA,
                      lcl=NA, 
                      ucl=NA
                        )


#calculate predicted values. 
newdata2$predicted <- predict(mam2, newdata2, re.form=NA)
#age= 16 for the ready to fledge birds and 12 for the not

#bootstrap confidence intervales based on https://github.com/lme4/lme4/issues/388 
## param only (not including individual variation or anything like that)
b3 <- bootMer(mam2,FUN=function(x) predict(x,newdata=newdata2,re.form=~0),
              ## re.form=~0 is equivalent to use.u=FALSE
              nsim=100,seed=101)

bootsum <- function(x,ext="_1") {
  d <- t(data.frame(apply(x$t,2,
                        function(x) c(mean(x),quantile(x,c(0.025,0.975))))))
  colnames(d) <- c("bpred","lwr","upr")
  return(d)
}

newdata2[3:5] <- bootsum(b3,"_3")

newdata2$age <- as.factor(newdata2$age)

Massplot <- ggplot(newdata2, aes(x=year2*10+1975))+
  geom_line(aes(y=predicted, color=age))+
  geom_ribbon(aes( ymin=lcl, ymax=ucl, fill=age), alpha=0.3)+
  labs(y="Mass", x="Year", color="Age (days)", fill="Age (days)")+ 
  xlim(1975,2017)+
  scale_fill_manual(values=c("steelblue","orchid3"))+
  scale_color_manual(values=c("steelblue","orchid3"))+
  theme_classic(base_size = 16)


