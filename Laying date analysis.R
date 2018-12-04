#First Egg date mini analysis

#Extract the date of first egg

# data <- as.data.frame(matrix(nrow= 12000, ncol=4))
# colnames(data) <- c("NestID", "Year", "LayDate", "FemaleAge")
# 
# i=0
# for (nest in as.list(globalData$nests)){
#   i=i+1
#   data$NestID[i] <- paste(nest$siteID, nest$year, nest$renestStatus, sep="-")
#   data$Year[i]  <- nest$year
#   data$LayDate[i] <- nest$firstEggDate
# 
#   #data$FemaleAge[i] <- nest$fAge
#   
#   
# 
# }
# i
# 
# data<- data[1:i, ]
# write.csv(data, col.names = T, row.names = F, file="file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/Lay date data.csv")

dat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Long term trends paper/Data Files_long term trends/Lay date data.csv")


data2 <- dat %>% group_by(Year) %>% summarise(MLayDate = mean(LayDate, na.rm=T))

data2$TimePeriod <- "Growing"
data2$TimePeriod[data2$Year>1991] <- "Declining"
data2$TimePeriod[data2$Year>2013] <- "PostDecline"

data2$TimePeriod <- factor(data2$TimePeriod)
data3 <- data2 %>% filter(!is.na(MLayDate))


mod <- lm(MLayDate ~TimePeriod*Year, data=data3)
plot(mod)
hist(resid(mod))
shapiro.test(resid(mod))
plot(resid(mod)~data3$Year)
plot(resid(mod)~data3$TimePeriod)
#Looks good
options(na.action="na.fail")
dredge(mod)
anova(mod, test="F")
car::Anova(mod)

mam <- lm(MLayDate ~Year, data=data3)

summary(mam)

ggplot(data3, aes(x=Year, y=MLayDate))+
  geom_smooth(method="lm", color="black")+
  geom_point()+
  labs(y= "Mean laying date (Julian)", x="Year")+
  ggthemes::theme_few(base_size = 16, base_family = "serif")+
  scale_y_continuous(breaks=c(133, 137, 141, 145))
  
ggsave(filename='~/Masters Thesis Project/Long term trends paper/Plots for paper/Supplementary laying date plot.jpeg', width=4, height=3, units="in", device="jpeg")




