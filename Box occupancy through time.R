#Changes in box occupancy. 
setwd("~/Masters Thesis Project/Long term trends paper/Plots for paper")

#load in the box occupancy data (created in the TRES Data Analysis folder "Box occupancy based on database")
dir <- "~/Masters Thesis Project/Tree Swallow Data/Amelia TRES data 1975-2016/Improved and Cleaned Data"
BoxOccupancy <- read.csv(paste(dir, "Box Occupancy using database.csv", sep="/"))
rm(dir)

library(tidyverse)


#Let's see if we can identify some breakpoints using the struccchange package
library(strucchange)
breakpoints <- breakpoints(formula= BoxOccupancy$BoxOccTotal ~BoxOccupancy$Year)
#Strucchange::breakpoints is sensitive to how much data is required in each time
#period. By default it wasn't 15% of all data in each group. I will relax that a
#bit and say that 10% is fine since I have a lot of data. Lets try using the
#change points package to see if that improves analyses of breakpoints.

breakpoints <- breakpoints(formula= BoxOccupancy$BoxOccTotal ~BoxOccupancy$Year, h=0.1)
confint(breakpoints)
#There should be 2 breakpoints
BoxOccupancy$Year[breakpoints$breakpoints]
ggplot(BoxOccupancy, aes(x=Year, y=100*BoxOccTotal))+
  geom_vline(xintercept=c(1991, 2013), linetype="dashed")+
  geom_smooth(method="lm", formula=y~x, aes(group=TimePeriod), fill="lightgrey", color="black")+
  geom_point()+
  labs(x="Year", y="Box Occupancy (%)")+
  ggthemes::theme_few(base_size = 16)+
  scale_x_continuous(breaks=c(1980, 1991, 2002, 2013))
ggsave(filename='Box Occupancy Plot with breakpoints.jpeg', width=4.5, height=3, units="in", device="jpeg")



library(changepoint)
#actually this won't work. Changepoints identifies periods wehre the mean was
#different, or where the variance was different. We expect that the population
#grew and then declined, therefore there shouldn't be distinct levels like this.




BoxOccupancy$TimePeriod <- NA
BoxOccupancy$TimePeriod[BoxOccupancy$Year<=1991] <- "Growing"
BoxOccupancy$TimePeriod[BoxOccupancy$Year>1991] <- "Declining"
BoxOccupancy$TimePeriod[BoxOccupancy$Year>2013] <- "PostDecline"


#Very interesting. I think I should use these breakpoints instead. They look
#good and then I'd have a better justification for the time periods

BoxOccupancy$TimePeriod <- factor(BoxOccupancy$TimePeriod, levels=c("Growing", "Declining", "PostDecline"))
BoxOccupancy$TimePeriod <- factor(BoxOccupancy$TimePeriod, levels=c( "Declining", "Growing" , "PostDecline"))
BoxOccupancy$TimePeriod <- factor(BoxOccupancy$TimePeriod, levels=c( "PostDecline", "Growing" , "Declining"))




mod <- lm(BoxOccTotal ~TimePeriod*Year, data=BoxOccupancy)
hist(resid(mod))
shapiro.test(resid(mod)) #that looks good now
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


