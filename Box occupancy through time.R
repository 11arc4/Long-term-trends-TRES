#Changes in box occupancy. 

#load in the box occupancy data (created in the TRES Data Analysis folder "Box occupancy based on database")
dir <- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data"
BoxOccupancy <- read.csv(paste(dir, "Box Occupancy using database.csv", sep="/"))
rm(dir)



#Let's see if we can identify some breakpoints
library(strucchange)

breakpoints <- breakpoints(formula= BoxOccupancy$BoxOccTotal ~BoxOccupancy$Year)
#There should be 2 breakpoints
BoxOccupancy$Year[breakpoints$breakpoints]
ggplot(BoxOccupancy, aes(x=Year, y=BoxOccTotal))+
  geom_vline(xintercept=c(1991.5, 2011.5), linetype="dashed")+
  geom_smooth(method="lm", formula=y~x, aes(group=TimePeriod2), fill="lightgrey", color="black")+
  geom_point()+
  labs(x="Year", y="Box Occupancy (%)")+
  ggthemes::theme_few(base_size = 16)

BoxOccupancy$TimePeriod2 <- NA
BoxOccupancy$TimePeriod2[BoxOccupancy$Year<=1991] <- "Growing"
BoxOccupancy$TimePeriod2[BoxOccupancy$Year>1991] <- "Declining"
BoxOccupancy$TimePeriod2[BoxOccupancy$Year>2011] <- "PostDecline"


#Very interesting. I think I should use these breakpoints instead. They look
#good and then I'd have a better justification for the time periods

BoxOccupancy$TimePeriod2 <- factor(BoxOccupancy$TimePeriod2, levels=c("Growing", "Declining", "PostDecline"))




BoxOccupancy$TimePeriod <- NA
BoxOccupancy$TimePeriod[BoxOccupancy$Year<=1996] <- "Growing"
BoxOccupancy$TimePeriod[BoxOccupancy$Year>1996] <- "Declining"
BoxOccupancy$TimePeriod <- factor(BoxOccupancy$TimePeriod, levels=c("Growing", "Declining"))
BoxOccupancy$TimePeriod <- factor(BoxOccupancy$TimePeriod, levels=c("Declining", "Growing"))


mod <- lm(BoxOccTotal ~TimePeriod*Year, data=BoxOccupancy)
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod)) #few we just barely make it
plot(resid(mod)~BoxOccupancy$Year)
plot(resid(mod)~BoxOccupancy$TimePeriod)
#This maybe isn't perfect but also isn't too bad. I think it's probably the best
#we will get and I will go with it. A beta regression, what is probably the
#better option won't work because of those couple of years with box occupancy
#above 1. I could also try a gamma


# gmod <- glm(BoxOccTotal ~TimePeriod*Year, data=BoxOccupancy, family=Gamma)
# plot(gmod)
# plot(resid(gmod)~BoxOccupancy$Year)
# plot(resid(gmod)~BoxOccupancy$TimePeriod)
# #This is really no better. I will stick to normal thanks. 

car::Anova(mod)
options(na.action="na.fail")
MuMIn::dredge(mod)
#Full mod is best obviously


summary(mod)

coef(mod)[3]+coef(mod)[4]

library(ggplot2)
ggplot(BoxOccupancy, aes(x=Year, y=100*BoxOccTotal))+
  geom_smooth(method="lm", formula = y~x, aes(group=TimePeriod), color="black", fill="lightgrey")+
  geom_point()+
  labs(y="Box Occupancy (%)", x="Year")+
  geom_vline(xintercept = c(1996.5, 2014.5), linetype="dashed")+
  #geom_smooth(method="loess", se=F)+
  ggthemes::theme_few(base_size = 12)



setwd("~/Masters Thesis Project/Long term trends paper/Plots for paper")
ggsave(filename='Box Occupancy Plot.jpeg', width=4.5, height=3, units="in", device="jpeg")
