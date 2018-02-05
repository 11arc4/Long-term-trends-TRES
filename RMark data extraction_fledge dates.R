#Make an RMark data file that incorporates fleding date for all the nests the bird was associated with (i.e.)

#There are a huge number of logistical issues with this. Namely which nestligns
#should we drop as male, what to do with years where we don't know how many
#nestlings fledged etc. Since we are taking such a long window for local weather
#conditions (1month) I've decided to use yearly averaged fledging dates instead.


setwd("~/Masters Thesis Project/TRES Data Analysis/Long Term Trends Models")
#Now all the stupid mark files will get put in their own folder away from
#everything important.
library(dplyr)
library(tidyr)
#Want to take out capture history, age at first capture and sex
years<- (seq(from= 1975, to = 2017, by=1))
dummy <- data.frame(matrix(ncol = 2*length(years)+2, nrow = length(as.list(globalData$birds))))
colnames(dummy) <- c(years, paste("FledgeDate", years, sep=""), "sex", "ageAtFirstSight")

#For each bird, we need to make a row of their capture history 
#This takes a while so make sure you're all set and ready to look at stuff
b <- 0
for (bird in as.list(globalData$birds)){
  b <- b+1
  y<- 0
  #We'll also set their sex because sex might change survival and I know it
  #changes capture probabilities (males were caught less)
  dummy$sex[b] <- bird$sex
  for (year in bird$yearsSeen$as.list()){
    y=y+1 #years are sorted so if y=1 then it's the first time we saw the bird
    #We will only count birds that have a nest associated with them
    if(!is.na(year$hatchNest$m_key) | year$nest$length>0){
      if(y==1){
        #Age when we first saw you might also change your survival
        dummy$ageAtFirstSight[b] <- year$age
      }
      
      #Put a 1 in every year where the bird was seen 
      dummy[b, which(years==year$year)] <- 1
      #Put in the fledging date for that year
      if(!is.na(year$hatchNest$m_key)){
        #If it's a nestling, the find it's hatch nest
        #check that it's hatch nest exists!
        if(exists(year$hatchNest$m_key, globalData$nests)){
          hatchnest <- get(year$hatchNest$m_key, globalData$nests)
          if(!is.na(hatchnest$fledgeDate){
            dummy[b, which(years==year$year)+43] <- "AVERAGE"
          }else {
            dummy[b, which(years==year$year)+43] <- hatchnest$fledgeDate
            
          }
        } else {
          dummy[b, which(years==year$year)+43] <- "AVERAGE"
        }
       
      } else {
        #if it was an adult, take the fledge date from the LAST nest it had that
        #year (ie when it's totally done nestling rearing)
        lastnestkey <- year$nest$as.list()[[year$nest$length]]
        lastnest <- get(lastnestkey$m_key, globalData$nests)
        if(is.na(lastnest$fledgeDate)){
          dummy[b, which(years==year$year)+43] <- "AVERAGE"
          
        } else {
          dummy[b, which(years==year$year)+43] <- lastnest$fledgeDate
          
        }
      
      }
    }
 
  }
  #Fill in all the empty spaces with 0s
  dummy[b, which(is.na(dummy[b,1:43]))] <- 0
}
