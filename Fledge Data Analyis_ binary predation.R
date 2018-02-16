#Predation analysis 
#it's possible that visitation rates are influenced by
#weahter (e.g. snakes aren't out on cold days, parental visitation rates are
#higher on warm days=better shot that predators are going to get you) Do you get
#more predation for nests when the nestlings are older (ie have higher
#visitation rates by parents), or when the weather THAT day is warmer?



library(tidyverse)



#Load in the individual data.
dat <- read.csv("file:///C:/Users/11arc/Documents/Masters Thesis Project/TRES Data Analysis/Extracted Statistics/Fledge Rate Analysis Data.csv", as.is=T, na.strings=c(""))
#Let's set female age to either SY or ASY. 
dat$FAge <- as.factor(dat$FAge)
levels(dat$FAge)

dat$FAge[which(dat$FAge=="HY")] <- NA #this has to be wrong!
dat$FAge2<- dat$FAge
dat$FAge2[which(dat$FAge!="SY" & !is.na(dat$FAge))]<- "ASY"






#What has happened over time? Are birds dying for different reasons
dat$FailureCause2 <- NA
dat$FailureCause2[dat$FailureCause=="PREDATION" |
                    dat$FailureCause=="PREDATION" |
                    dat$FailureCause=="PREDATION?" |
                    dat$FailureCause=="EGGS DEPREDATED"] <- "PREDATION"

dat$FailureCause2[dat$FailureCause=="NESTLINGS DIED" |
                    dat$FailureCause=="NESTLINGS DEAD" |
                    dat$FailureCause=="DEAD IN NEST" |
                    dat$FailureCause==" NESTLINGS DIED" ] <- "NESTLINGS DIED"

dat$FailureCause2[dat$FailureCause=="COMPETITION, FEMALE DIED" |
                    dat$FailureCause==" INFANTICIDE" |
                    dat$FailureCause=="COMPETITOR" |
                    dat$FailureCause=="INTRASPECIFIC COMPETITOR INTERFERENCE"|
                    dat$FailureCause=="COMPETITION"|
                    dat$FailureCause=="COMPETITOR INTERFERENCE?"|
                    dat$FailureCause=="INFANTICIDE"] <- "COMPETITOR"

dat$FailureCause2[which(!is.na(dat$FailureCause) & is.na(dat$FailureCause2))]<- "OTHER"




#Let's remove the super invasive experiments
dat$Experiment<- as.factor(dat$Experiment)
levels(dat$Experiment)
#There are a lot of different names for experiments, but anything where we
#transfered, remov[ed], add, female [remove], male remov, kill needs to be taken
#out

dat2 <- dat [which(!grepl("trans", dat$Experiment, fixed=T) &
                     !grepl("TRANS", dat$Experiment, fixed=T) &
                     !grepl("TREANS", dat$Experiment, fixed=T) &
                     !grepl("REMOV", dat$Experiment, fixed=T) &
                     !grepl("remov", dat$Experiment, fixed=T) &
                     !grepl("ADD", dat$Experiment, fixed=T) &
                     !grepl("add", dat$Experiment, fixed=T) &
                     !grepl("FEMALE", dat$Experiment, fixed=T) &
                     !grepl("female", dat$Experiment, fixed=T) &
                     !grepl("male remo", dat$Experiment, fixed=T) &
                     !grepl("kill", dat$Experiment, fixed=T) &
                     !grepl("reduction of nestlings", dat$Experiment, fixed=T) &
                     !grepl("brood", dat$Experiment, fixed=T) 
                   
), ] %>% filter (Fledge<=Hatch)

summary(dat2$Experiment)


#OK so we've now got some data that is clearly better than our predivious
#data!!! It removes all the nests that were heavily manipulated and therefore
#not really worth our time



hist(dat2$FledgeRate) #HIGHLY bimodal

dat2$Fledge2 <- NA
dat2$Fledge2[which(dat2$Fledge>0)]<- 1 
dat2$Fledge2[which(dat2$Fledge==0)]<- 0


#Lets make a dataset for all the birds whether we know when, and how many nestlings fledged. 

dat3 <- dat2 %>% filter(!is.na(Fledge) & !is.na(FledgeDate))
#Those are the only data points we can use for this analysis of how weather affects things-- we NEED those dates. 

dat3$Predation <- 0
dat3$Predation[dat3$FailureCause2=="PREDATION"] <- 1






