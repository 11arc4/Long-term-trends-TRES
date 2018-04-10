
#Breakpoint analysis following Paul Martin's code (MCMCglmm base)

plot(BoxOccTotal ~ Year, data=BoxOccupancy)

#Identify the first break point (pop growing to pop declining)
BoxOccupancy_bp1 <- BoxOccupancy %>% filter (Year<2010)
breaks <- BoxOccupancy_bp1$Year
mse <- numeric(length(breaks))
for(i in 1:length(breaks)){
  piecewise1 <- lm(BoxOccupancy_bp1$BoxOccTotal ~ BoxOccupancy_bp1$Year*(BoxOccupancy_bp1$Year < breaks[i]) + BoxOccupancy_bp1$Year*(BoxOccupancy_bp1$Year>=breaks[i]))
  mse[i] <- summary(piecewise1)[6]  #sigma which is residual SD for the model
}
mse <- as.numeric(mse)
plot(mse~breaks)
breaks[which(mse==min(mse))] #break point for threshold 
#This identified 1992 as a breakpoint. 



#Identify the second break point (pop declining to pop growing)
BoxOccupancy_bp2 <- BoxOccupancy %>% filter (Year>2000)
breaks <- BoxOccupancy_bp2$Year
mse <- numeric(length(breaks))
for(i in 1:length(breaks)){
  piecewise1 <- lm(BoxOccupancy_bp2$BoxOccTotal ~ BoxOccupancy_bp2$Year*(BoxOccupancy_bp2$Year < breaks[i]) + BoxOccupancy_bp2$Year*(BoxOccupancy_bp2$Year>=breaks[i]))
  mse[i] <- summary(piecewise1)[6]  #sigma which is residual SD for the model
}
mse <- as.numeric(mse)
plot(mse~breaks)
breaks[which(mse==min(mse))] #break point for threshold 
#This identifies the second breakpoint at 2014. 


# #PAUL MARTIN'S CODE

# # identify breakpoint in relationship between urban adaptation and breeding
# # occurrence for subordinate species in sympatry
# breaks <- symp.sub.dat$zbreed.propensity[which(symp.sub.dat$zbreed.propensity >= -0.5 & symp.sub.dat$zbreed.propensity <= 0.5)]
# 
# 
# mse <- numeric(length(breaks))
# for(i in 1:length(breaks)){
#   piecewise1 <- lm(symp.sub.dat$zbreed.occur ~ symp.sub.dat$zbreed.propensity*(symp.sub.dat$zbreed.propensity < breaks[i]) + symp.sub.dat$zbreed.propensity*(symp.sub.dat$zbreed.propensity>=breaks[i]))
#   mse[i] <- summary(piecewise1)[6]  #sigma which is residual SD for the model
# }
# mse <- as.numeric(mse)
# plot(mse~breaks)
# breaks[which(mse==min(mse))] #break point for threshold 
# # breakpoint occurs at urban.adaptation = 0.23802
# 
# 
# mod <- lm(BoxOccTotal ~TimePeriod*Year, data=BoxOccupancy)
# summary(mod)[6]
# 
# #incorporate subordinate breakpoint into model
# bp = 0.23802
# b1 <- function(x, bp) ifelse(x < bp, x, 0)
# b2 <- function(x, bp) ifelse(x < bp, 0, x)
# model.mc.breakpoint.s<-MCMCglmm(zbreed.occur~dominance*b1(zbreed.propensity,0.23802)*sympatry+dominance*b2(zbreed.propensity,0.23802)*sympatry, random=~spp.pair, family="gaussian", data=dat, nitt=50000, burnin=10000, thin=1, prior=prior.model1)




