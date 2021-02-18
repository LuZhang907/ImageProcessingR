library(TTR)
prices<-SPX$Close
logReturn<-rep(0,length(prices))
for (i in 1: length(prices)){
  logReturn[i]<-log(prices[i+1])-log(prices[i])
}
EMA<-EMA(prices)
RSI<-RSI(prices)
MA60<-SMA(prices)
ROCClose=ROC(prices)
library(minerva)

EMA<-as.matrix(EMA)
ROCClose<-as.matrix(ROCClose)
cstats(EMA, ROCClose)