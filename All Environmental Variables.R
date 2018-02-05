hurricane <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Hurricanes.csv")
ENSOdat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Monthly El Nino Southern Oscillation index.csv", as.is=T)[,1:13]
colnames(ENSOdat) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
ENSOdat<- ENSOdat %>% arrange(Year)
for(i in 2:nrow(ENSOdat)){
  
  ENSOdat$ENSOWinter[i] <-  mean(c(ENSOdat$Mar[i-1], ENSOdat$Dec[i-1], ENSOdat$Jan[i], ENSOdat$Feb[i]), na.rm=T)
  
}
presugar <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/Environmental Datasets/Sugar Cane Acreage Available USDA survey.csv", as.is=T, na.strings =c(""))
sugar <- presugar[,c(2, 19, 21, 23,25)]
colnames(sugar) <- c("Year", "acreCaneSeed", "tonsCaneSeed", "acreCaneSeedSugar", "acreCaneSugar")

sugar <- sugar %>% arrange(year)


library(dplyr)


Environmental <- merge(sugar[,1:2], ENSOdat[,c(1,14)], by="Year")
Environmental <- merge(Environmental, hurricane[,1:2], by="Year")

M <- cor(Environmental, use="pairwise")

corrplot::corrplot(M)
#Strong correlations between acres seed and hurricanes. 

plot(acreCaneSeed ~Hurricanes, data=Environmental)
plot(acreCaneSeed ~ENSOWinter, data=Environmental)
plot(Hurricanes ~ENSOWinter, data=Environmental)

