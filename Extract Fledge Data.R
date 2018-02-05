#Extracting all the relevent fledging data




dat <- as.data.frame(matrix(nrow=20000, ncol=16, NA))
colnames(dat)<- c("NestID", "Year", "Experiment", "FledgeRate", "Fledge", "FledgeDate",  "Hatch", "HatchDate", "Clutch", "RenestStatus", "FailureCause",  "FAge", "MAge", "AvNestlingGrowthRate", "NestlingAge", "AverageMass")



i=0
for(nest in as.list(globalData$nests)){
  i=i+1
  dat$NestID[i]<- paste( nest$year, nest$siteID, sep="-")
  dat$Year[i]<- nest$year
  dat$Experiment[i] <- nest$experiment
  dat$Fledge[i] <- nest$fledgeSize
  dat$FledgeDate[i] <- nest$fledgeDate
  dat$Hatch[i] <- nest$hatchSize
  dat$HatchDate[i] <- nest$hatchDate
  dat$FledgeRate[i] <- dat$Fledge[i]/dat$Hatch[i]
  dat$Clutch[i] <- nest$clutchSize
  dat$RenestStatus[i] <- nest$renestStatus
  dat$FailureCause[i] <- nest$reasonforFailure
  #Parent age
  if(!is.na(nest$femaleID$m_key)){
    bird <- get(nest$femaleID$m_key, globalData$birds)
    for (year in bird$yearsSeen$as.list()){
      if (nest$year==year$year){
        dat$FAge[i] <- year$age
      }
    }
  }
  if(!is.na(nest$maleID$m_key)){
    bird <- get(nest$maleID$m_key, globalData$birds)
    for (year in bird$yearsSeen$as.list()){
      if (nest$year==year$year){
        dat$MAge[i] <- year$age
      }
    }
  }
  #Extract out the important nestling information
  growthrates <- rep(NA, 8)
  nestlingAges<- rep(NA, 8)
  Mass <- rep(NA, 8)
  a<- 0
  if(nest$nestlings$length>0){
    for ( nestlingID in nest$nestlings$as.list()){
      a<- a+1
      nestling <- get(nestlingID$m_key, globalData$nestlings)
      growthrates[a] <- nestling$growthRateMass
      if(nestling$measurements$length>0){
        oldestMeasure<- nestling$measurements$buffer[[nestling$measurements$length]]
        nestlingAges[a]<- oldestMeasure$age
        Mass[a]<- oldestMeasure$mass
      }
      
    }
    if(any(!is.na(growthrates))){
      dat$AvNestlingGrowthRate[i] <- mean(growthrates, na.rm=T)
    }
    if(any(!is.na(Mass))){
      dat$NestlingAge[i]<- max(nestlingAges, na.rm=T)
      dat$AverageMass[i] <- mean (Mass[which(nestlingAges==max(nestlingAges, na.rm=T))])
      
    }
    
  }
  
}

 #Info about nestling growth
 
dat<- dat[1:i,]
 


write.csv(dat, "file:///C:/Users/11arc/Documents/Masters Thesis Project/TRES Data Analysis/Extracted Statistics/Fledge Rate Analysis Data.csv", na="", row.names=F)
