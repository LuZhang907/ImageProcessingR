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




# convert Posixct to xts format
library(highfrequency)
library(xts)
HL<-as.matrix(cbind(High,Low))
xts_HL<-xts(HL,SPX$DateTime)
#head(spx_HL)
HLC<-as.matrix(cbind(High,Low,Close))
xts_HLC<-xts(HLC,SPX$DateTime)
HLCO<-as.matrix(cbind(High,Low,Close,Open))
xts_HLCO<-xts(HLCO,SPX$DateTime)
CHL<-as.matrix(cbind(Close,High,Low))
xts_CHL<-xts(CHL, SPX$DateTime)
xts_Close<-xts(Close,SPX$DateTime)


## calculating y_label based on average prices
day_index<-endpoints(xts_Close, on = "days", k = 1)
#head(day_index)
#length(day_index)

# last min prices for each trading day
lmP<-xts_Close[day_index,]
head(lmP)

# average prices from 1 to 360 mins
n<-length(day_index)-1
avg_360<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]-30
  avg_360[i]<-mean(xts_Close[start:end])
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

#0    1 
#1547 1900 


##indicators calculation
library(TTR)
#ADX long time runing, done
n<-length(day_index)-1
avg_ADX<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_ADX[i]<-mean(ADX(xts_HLC)[start:end])
}
#aroon, done
avg_aroon<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_aroon[i]<-mean(aroon(xts_HL)[start:end])
}

#ATR done
avg_ATR<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_ATR[i]<-mean(ATR(xts_HLC)[start:end])
}

#CCI 
avg_CCI<-rep(0,n)
for (i in 2000:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_CCI[i]<-mean(CCI(xts_HLC)[start:end])
}
raw_features["avg_CCI"]<-avg_CCI
write.csv(raw_features,"/Users/luzhang/Desktop/indicator/raw_features.csv")
#chaikinVolatility 
avg_chaikinVolatility<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_chaikinVolatility[i]<-mean(chaikinVolatility(xts_HLC)[start:end])
}
#CLV 
avg_CLV<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_CLV[i]<-mean(CLV(xts_HLC)[start:end])
}
#CMOClose done
avg_CMOClose<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_CMOClose[i]<-mean(CMO(xts_Close)[start:end])
}

#DonchianChannel
avg_DonchianChannel<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_DonchianChannel[i]<-mean(DonchianChannel(xts_HL)[start:end])
}
#DPOClose done
avg_DPOClose<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_DPOClose[i]<-mean(DPO(spx_Close)[start:end])
}
write.csv(avg_DPOClose, "/Users/luzhang/Desktop/indicator/attempt03.csv")

#DVI done
avg_DVIClose<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_DVIClose[i]<-mean(DVI(spx_Close)[start:end])
}
#GMMAClose done
avg_GMMAClose<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_GMMAClose[i]<-mean(GMMA(spx_Close)[start:end])
}

#write.csv(avg_GMMAClose, "/Users/luzhang/Desktop/indicator/attempt04.csv")
#KSTClose done
avg_KSTClose<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_KSTClose[i]<-mean(KST(spx_Close)[start:end])
}
#write.csv(avg_KSTClose, "/Users/luzhang/Desktop/indicator/attempt04.csv")
#lags done
avg_lags<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_lags[i]<-mean(lags(dwt_HLC)[start:end])
}
write.csv(avg_lags, "/Users/luzhang/Desktop/indicator/attempt04.csv")

#MACD done
avg_MACD<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_MACD[i]<-mean(MACD(dwt_Close)[start:end])
}
write.csv(avg_MACD, "/Users/luzhang/Desktop/indicator/attempt04.csv")
#PBands done
avg_PBandsClose<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_PBandsClose[i]<-mean(PBands(spx_Close)[start:end])
}
#write.csv(avg_PBandsClose, "/Users/luzhang/Desktop/indicator/attempt04.csv")

#ROCclose done
avg_ROCClose<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_ROCClose[i]<-mean(ROC(spx_Close)[start:end])
}
#write.csv(avg_ROCClose, "/Users/luzhang/Desktop/indicator/attempt04.csv")
#momentum done
avg_momentumClose<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_momentumClose[i]<-mean(momentum(spx_Close)[start:end])
}
#write.csv(avg_momentumClose, "/Users/luzhang/Desktop/indicator/attempt04.csv")
#RSI done
avg_RSIClose<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_RSIClose[i]<-mean(RSI(spx_Close)[start:end])
}
#write.csv(avg_RSIClose, "/Users/luzhang/Desktop/indicator/attempt04.csv")
#runSum done
avg_runSum<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_runSum[i]<-mean(runSum(spx_Close)[start:end])
}
#write.csv(avg_runSum, "/Users/luzhang/Desktop/indicator/attempt04.csv")
#runMin  done
avg_runMin<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_runMin[i]<-mean(runMin(spx_Close)[start:end])
}
write.csv(avg_runMin, "/Users/luzhang/Desktop/indicator/attempt04.csv")
#runMax done
avg_runMax<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_runMax[i]<-mean(runMax(spx_Close)[start:end])
}
write.csv(avg_runMax, "/Users/luzhang/Desktop/indicator/attempt04.csv")
#runMedian done
avg_runMedian<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_runMedian[i]<-mean(runMedian(spx_Close)[start:end])
}
write.csv(avg_runMedian, "/Users/luzhang/Desktop/indicator/attempt04.csv")
#SAR done
avg_SAR<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_SAR[i]<-mean(SAR(dwt_HL)[start:end])
}
write.csv(avg_SAR, "/Users/luzhang/Desktop/indicator/attempt04.csv")
#SMA done
avg_SMAClose<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_SMAClose[i]<-mean(SMA(spx_Close)[start:end])
}
write.csv(avg_SMAClose, "/Users/luzhang/Desktop/indicator/attempt04.csv")

#EMA done
avg_EMAClose<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_EMAClose[i]<-mean(EMA(spx_Close)[start:end])
}
write.csv(avg_EMAClose, "/Users/luzhang/Desktop/indicator/attempt04.csv")
#DEMA done
avg_DEMAClose<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_DEMAClose[i]<-mean(DEMA(spx_Close)[start:end])
}
write.csv(avg_DEMAClose, "/Users/luzhang/Desktop/indicator/attempt04.csv")

#SNR done
avg_SNR<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_SNR[i]<-mean(SNR(dwt_HLC, n=30)[start:end])
}
write.csv(avg_SNR, "/Users/luzhang/Desktop/indicator/attempt04.csv")
#stoch error non-leading NAs
avg_stoch<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_stoch[i]<-mean(stoch(spx_Close)[start:end])
}
#SMI done
avg_SMI<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_SMI[i]<-mean(SMI(dwt_HLC)[start:end])
}
write.csv(avg_SMI, "/Users/luzhang/Desktop/indicator/attempt04.csv")
#TDI done
avg_TDI<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_TDI[i]<-mean(TDI(spx_Close)[start:end])
}
write.csv(avg_TDI, "/Users/luzhang/Desktop/indicator/attempt04.csv")
#TRIX done
avg_TRIX<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_TRIX[i]<-mean(TRIX(spx_Close)[start:end])
}
write.csv(avg_TRIX, "/Users/luzhang/Desktop/indicator/attempt04.csv")
#ultimateOscillator done
avg_ultimateOscillator<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_ultimateOscillator[i]<-mean(ultimateOscillator(dwt_HLC)[start:end])
}
write.csv(avg_ultimateOscillator, "/Users/luzhang/Desktop/indicator/attempt04.csv")
#VHF done
avg_VHF<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_VHF[i]<-mean(VHF(spx_Close)[start:end])
}
write.csv(avg_VHF, "/Users/luzhang/Desktop/indicator/attempt04.csv")

#Volatility done
avg_volatility<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_volatility[i]<-mean(volatility(dwt_HLCO)[start:end])
}
write.csv(avg_volatility, "/Users/luzhang/Desktop/indicator/attempt04.csv")
#williamsAD done
avg_williamsAD<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_williamsAD[i]<-mean(williamsAD(dwt_HLC)[start:end])
}
write.csv(avg_williamsAD, "/Users/luzhang/Desktop/indicator/attempt04.csv")
#WPR done
avg_WPR<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_WPR[i]<-mean(WPR(dwt_HLC)[start:end])
}
#ZigZag done
avg_ZigZag<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  avg_ZigZag[i]<-mean(ZigZag(dwt_HL)[start:end])
}
write.csv(avg_ZigZag, "/Users/luzhang/Desktop/indicator/attempt04.csv")
# daily average prices 
#n<-length(day_index)-1
#avg_390<-rep(0,n)
#for (i in 1:n){
#  start<-day_index[i]+1
#  end<-day_index[i+1]
#  avg_390[i]<-mean(spx_ts[start:end])
#}

#daily_avg_prices<-avg_390
#length(daily_avg_prices)

# random forest

rm(list = setdiff(ls(), lsf.str()))
library(fmlr)
library(quantmod)
library(TTR) # for various indicators
library(randomForestFML)
library(ROCR)

features <- read.csv("/Users/luzhang/Desktop/indicator/features.csv", header = T)
head(features)
dim(features)

allSet<-data.frame(Y=as.factor(y_label),features)
head(allSet)

#exclude NA at the begining of the indicators
idx_NA <- apply(allSet,1,function(x){sum(is.na(x))>0})
allSet <- subset(allSet, !idx_NA)
dim(allSet)
nx <- nrow(allSet)
trainSet <- allSet[1:floor(nx*2/3),]
testSet <- allSet[(floor(nx*2/3)+1):nx,]
dim(allSet); dim(trainSet); dim(testSet)

#smote
(tb <- table(trainSet$Y))
(ratio <- tb[names(tb)=="1"] / tb[names(tb)=="0"])

if(ratio > 1) perc <- list("0"=ratio, "1"=1) else perc <- list("0"=1, "1"= (1/ratio))

trainSet_balanced <- UBL::SmoteClassif(Y ~ . , dat = trainSet, C.perc = perc)
table(trainSet_balanced$Y)

mtry <- 1
set.seed(1) #no avg_ultimateOscillator
#bag <- randomForestFML(Y ~ avg_ADX+avg_aroon+avg_ATR+avg_DPOClose+avg_chaikinVolatility+avg_CLV+
#                         avg_CMOClose+avg_DPOClose+avg_DVIClose+avg_GMMAClose+avg_KSTClose
#                       +avg_PBandsClose+avg_ROCClose+avg_momentumClose+avg_RSIClose+
#                         run_Sum+run_Min+run_Max+run_Median+
#                        avg_SMAClose+avg_EMAClose+avg_DEMAClose+avg_TDI+
#                         avg_TRIX+avg_VHF+avg_lags+avg_MACD+
#                        avg_SAR+avg_SNR+avg_SMI+
#                         avg_volatility+avg_williamsAD+avg_WPR+avg_ZigZag, data = trainSet, mtry = mtry, importance = TRUE, ntree = 400, SB=0)
#plot(bag)


mtry <- 1
set.seed(03012021)
bag <- randomForestFML(Y ~ . -avg_ultimateOscillator, data = trainSet, mtry = mtry, importance = TRUE, ntree = 400, SB=0)
plot(bag)
legend("top", colnames(bag$err.rate),col=1:3,cex=0.8,lty=1:3)

varImpPlot(bag)

# evaluating auc based on the test set
prob_test <- predict(bag, newdata=testSet, type="prob")



# confusion matrix, note the order "0" and "1"
# here we associate TRUE with "1", note how to correctly read the confusion matrix
table(testSet$Y, prob_test[,2] >= 0.5)
#FALSE TRUE
#0   468   35
#1   331  303
pred <- prediction(prob_test[,2], testSet$Y) # the 2nd column is where the label "1" is
auc <- performance(pred, measure = "auc")@y.values[[1]]
auc
#[1] 0.8598159
acc_perf <- performance(pred, measure = "acc")
acc_vec <- acc_perf@y.values[[1]]
acc <- acc_vec[max(which(acc_perf@x.values[[1]] >= 0.5))]
acc
#[1] 0.6781003
tb_test <- table(testSet$Y)
lucky_score <- fmlr::acc_lucky(train_class = table(trainSet$Y),
                               test_class = tb_test,
                               my_acc = acc)
lucky_score
#$my_accuracy
#[1] 0.6781003

#$p_random_guess
#[1] 0

#$p_educated_guess
#[1] 0

#$mean_random_guess
#[1] 0.4998338

#$mean_educated_guess
#[1] 0.5065136

#$acc_majority_guess
#[1] 0.5576077


