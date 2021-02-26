##################################################################
##########    import financial data 2007-2020    #################

#read multiple text file 

#the path where text files are saved
mypath="/Users/luzhang/Desktop/SPX"
setwd(mypath)

#Create list of text files
SPX_ls=list.files(path=mypath, pattern = "*.txt")

#Read the files in, comma is the separator
SPX_df<-lapply(SPX_ls, function(x){read.table(file = x, 
                                              col.names = c("DateTime","Open", "High","Low","Close"),sep = ",")})

#Combine them
SPX <- do.call("rbind", lapply(SPX_df, as.data.frame))
#head(SPX)
#length(SPX$DataTime)

#DateTime: character->dateTime(double)
SPX$DateTime<-as.POSIXct(SPX$DateTime,taz="EST")

##################################################################
###################      data preparing     ######################

options(digits.secs=3)
Sys.setenv(TZ='EST')

# truncate data from 2009-01-01 to 2019-12-31, 9:30 AM to 16:00 PM

SPX$DateTime<-as.POSIXct(SPX$DateTime,format="%H:%M:%OS", taz="EST")
SPX<- subset(SPX, lubridate::hour(SPX$DateTime)*60
             +lubridate::minute(SPX$DateTime) >= 9*60+30)
SPX <- subset(SPX, lubridate::hour(SPX$DateTime)*60
              +lubridate::minute(SPX$DateTime) <= 16*60)
head(SPX)
tail(SPX)

#    Wavelet transform    #
library(WaveletComp)
library(remotes)
library(wmtsa)
library(astsa)
library(ggplot2)

Close<-SPX$Close
High<-SPX$High
Low<-SPX$Low
Open<-SPX$Open

#wavelet transofrm close, high, low and open
dwt_Close<-wavShrink(Close, wavelet="d4",
                    n.level=1, 
                    shrink.fun="soft", thresh.fun="adaptive")
dwt_High<-wavShrink(High, wavelet="d4",
                    n.level=1, 
                    shrink.fun="soft", thresh.fun="adaptive")
dwt_Low<-wavShrink(Low, wavelet="d4",
                    n.level=1, 
                    shrink.fun="soft", thresh.fun="adaptive")
dwt_Open<-wavShrink(Open, wavelet="d4",
                    n.level=1, 
                    shrink.fun="soft", thresh.fun="adaptive")



# convert Posixct to xts format
library(highfrequency)
library(xts)
dwt_HL<-as.matrix(cbind(dwt_High,dwt_Low))
spx_HL<-xts(dwt_HL,SPX$DateTime)
#head(spx_HL)
dwt_HLC<-as.matrix(cbind(dwt_High,dwt_Low,dwt_Close))
spx_HLC<-xts(dwt_HLC,SPX$DateTime)
dwt_HLCO<-as.matrix(cbind(dwt_High,dwt_Low,dwt_Close,dwt_Open))
spx_HLCO<-xts(dwt_HLCO,SPX$DateTime)
dwt_CHL<-as.matrix(cbind(dwt_Close,dwt_High,dwt_Low))
spx_CHL<-xts(dwt_CHL, SPX$DateTime)
#using average of C, H, L, O as general prices
prices<-rowMeans(dwt_HLCO)
prices<-xts(prices,SPX$DateTime)
names(prices)<-"prices"
#head(prices)

## calculating y_label based on average prices
day_index<-endpoints(prices, on = "days", k = 1)
#head(day_index)
#length(day_index)

# last min prices for each trading day
lmP<-prices[day_index,]
head(lmP)

# average prices from 1 to 360 mins
n<-length(day_index)-1
avg_360<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]-30
  avg_360[i]<-mean(prices[start:end])
}

#calculate the log returns
logReturn<-log(avg_360)-log(lmP)
#length(logRetrn)

#figure 5
hist(logReturn, 
     main="Retrun freqeuncy distribution y_mean_390", 
     xlab="Return", 
     col="red",
     xlim=c(-0.03,0.03),
     las=1, 
     breaks=200)

#create y label
n<-length(lmP)
y_label<-rep(0,n)
for (i in 1:n ){
  if(avg_360[i]<lmP[i]){
    y_label[i]=1
  }else{
    y_label[i]=0
  }
}

table(y_label)

##indicators calculation
library(TTR)
#ADX long time runing
n<-length(day_index)-1
avg_ADX<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_ADX[i]<-mean(ADX(dwt_HLC)[start:end])
}
#aroon
avg_aroon<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_aroon[i]<-mean(aroon(dwt_HL)[start:end])
}
#ATR
avg_ATR<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_ATR[i]<-mean(ATR(dwt_HLC)[start:end])
}
#BBands
avg_BBands<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_BBands[i]<-mean(BBands(dwt_HLC)[start:end])
}
#CCI
avg_CCI<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_CCI[i]<-mean(CCI(dwt_HLC)[start:end])
}
#chaikinVolatility
avg_chaikinVolatility<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_chaikinVolatility[i]<-mean(chaikinVolatility(dwt_HLC)[start:end])
}
#CLV
avg_BBands<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_BBands[i]<-mean(BBands(dwt_HLC)[start:end])
}
#CMO
avg_CMO<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_CMO[i]<-mean(CMO(prices)[start:end])
}
#CTI
avg_CTI<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_CTI[i]<-mean(CTI(dwt_HLC)[start:end])
}
#DonchianChannel
avg_DonchianChannel<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_DonchianChannel[i]<-mean(DonchianChannel(dwt_CHL)[start:end])
}
#DPO
avg_DPO<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_DPO[i]<-mean(DPO(prices)[start:end])
}
#DVI
avg_DVI<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_DVI[i]<-mean(DVI(prices)[start:end])
}
#GMMA
avg_GMMA<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_GMMA[i]<-mean(GMMA(prices)[start:end])
}
#KST
avg_KST<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_KST[i]<-mean(KST(prices)[start:end])
}
#lags
avg_lags<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_lags[i]<-mean(lags(dwt_HLC)[start:end])
}
#MACD
avg_MACD<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_MACD[i]<-mean(MACD(dwt_HLC)[start:end])
}
#PBands
avg_PBands<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_PBands[i]<-mean(PBands(prices)[start:end])
}
#ROC
avg_ROC<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_ROC[i]<-mean(ROC(prices)[start:end])
}
#momentum
avg_momentum<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_momentum[i]<-mean(momentum(prices)[start:end])
}
#RSI
avg_RSI<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_RSI[i]<-mean(RSI(prices)[start:end])
}
#runSum
avg_runSum<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_runSum[i]<-mean(runSum(prices)[start:end])
}
#runMin
avg_runMin<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_runMin[i]<-mean(runMin(prices)[start:end])
}
#runMax
avg_runMax<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_runMax[i]<-mean(runMax(prices)[start:end])
}
#runMedian
avg_runMedian<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_runMedian[i]<-mean(runMedian(prices)[start:end])
}
#SAR
avg_SAR<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_SAR[i]<-mean(SAR(dwt_HL)[start:end])
}
#SMA
avg_SMA<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_SMA[i]<-mean(SMA(prices)[start:end])
}
#EMA
avg_EMA<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_EMA[i]<-mean(EMA(prices)[start:end])
}
#DEMA
avg_DEMA<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_DEMA[i]<-mean(DEMA(prices)[start:end])
}
#WMA
avg_WMA<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_WMA[i]<-mean(WMA(prices)[start:end])
}
#EVWMA
avg_EVWMA<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_EVWMA[i]<-mean(EVWMA(prices)[start:end])
}
#ZLEMA
avg_ZLEMA<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_ZLEMA[i]<-mean(ZLEMA(prices)[start:end])
}
#HMA
avg_HMA<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_HMA[i]<-mean(HMA(prices)[start:end])
}
#ALMA
avg_ALMA<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_ALMA[i]<-mean(ALMA(prices)[start:end])
}
#SNR
avg_SNR<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_SNR[i]<-mean(SNR(dwt_HLC)[start:end])
}
#stoch
avg_stoch<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_stoch[i]<-mean(stoch(prices)[start:end])
}
#SMI
avg_SMI<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_SMI[i]<-mean(SMI(dwt_HLC)[start:end])
}
#TDI
avg_TDI<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_TDI[i]<-mean(DTI(prices)[start:end])
}
#TRIX
avg_TRIX<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_TRIX[i]<-mean(TRIX(prices)[start:end])
}
#ultimateOscillator
avg_ultimateOscillator<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_ultimateOscillator[i]<-mean(ultimateOscillator(dwt_HLC)[start:end])
}
#VHF
avg_VHF<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_VHF[i]<-mean(VHF(prices)[start:end])
}
#Volatility
avg_volatility<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_volatility[i]<-mean(volatility(dwt_HLCO)[start:end])
}
#williamsAD
avg_williamsAD<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_williamsAD[i]<-mean(williamsAD(dwt_HLC)[start:end])
}
#WPR
avg_WPR<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_WPR[i]<-mean(WPR(dwt_HLC)[start:end])
}
#ZigZag
avg_ZigZag<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_ZigZag[i]<-mean(ZigZag(dwt_HL)[start:end])
}
# daily average prices 
#n<-length(day_index)-1
#avg_390<-rep(0,n)
#for (i in 1:n){
#  start<-day_index[i]+1
#  end<-day_index[i+1]
#  avg_390[i]<-mean(spx_ts[start:end])
#}

#daily_avg_prices<-avg_390
length(daily_avg_prices)

# Indicators calculation





