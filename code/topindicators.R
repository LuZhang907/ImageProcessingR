##################################################################
###################   top 5 indicators    ########################

#calculate indicators-daily

# daily average prices 
n<-length(day_index)-1
avg_390<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_390[i]<-mean(spx_ts[start:end])
}

daily_prices<-avg_390
#index close based on daily closed prices
index_close<-seq(1,n,1)

# daily Exponential moving average(EMA)
# In the TTR package, we can use EMA():
#install.packages("TTR")
library(TTR)
EMA <-EMA(daily_prices) 

# Relative Strength Index(RSI)
RSI<-RSI(daily_prices)

# Moving average within period 60 minutes
MA60<-SMA(daily_prices)

# don't know how to calculate CORREL \0_0/

#transform the indicators in matrix form
y_label<-as.matrix(y_label)

#indicators
inds<-cbind(
  index_close,
  EMA,
  RSI,
  MA60,
  CMOclose=CMO(daily_prices),
  DPOclose=DPO(daily_prices),
  MACDClose=MACD(daily_prices)[,1],
  ROCClose=ROC(daily_prices),
  momentum=momentum(daily_prices),
  runPerRankClose=runPercentRank(daily_prices),
  TDI=TDI(daily_prices)[,1],
  TRIX=TRIX(daily_prices)[,1],
  VHF=VHF(daily_prices)           
)

#indicators
inds<-as.matrix(inds)
head(inds)

#calculate MIC
library(minerva)
mics<-cstats(inds,y_label)
#mics between indcators and y_lable are too small

#log_return<-as.matrix(logReturn)
#mics_log<-cstats(inds,log_return)
