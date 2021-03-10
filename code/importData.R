
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
##################       2014-04-11        #######################

#Extract the data on 2014-04-11
SPX140411 <- subset(SPX,DateTime >= "2014-04-11 09:30:00 " & DateTime <= "2014-04-11 16:00:00")

# calculate prices and returns

prices<-SPX140411$Close

# return=log(P_i)-log(P_i-1)
return<-rep(0,length(prices)-1)
for (i in 1:(length(prices)-1)){
  return[i]<-log(prices[i+1])-log(prices[i])
}

#plot prices and return
#install.packages("astsa")
library(astsa)
library(ggplot2)

#png(file='price.png',  width=600, height=320)
par(mar=c(5,5,1.5,1.5)+.5, mgp=c(1.2,.5,0))                    # trim the margins       
plot(prices, ylab='price', xlab="time",main="S&P500 price 2014-04-11",type='n')   # set up the plot
grid(lty=1, col=gray(.9))                                   # add a grid
lines(prices, type='l', col=4)                          # and now plot the line

dev.off() 

#png(file='return.png',  width=600, height=320)
par(mar=c(5,5,1.5,1.5)+.5, mgp=c(1.2,.5,0))                    # trim the margins       
plot(return, ylab='return', xlab="time",main="S&P500 return",type='n')   # set up the plot
grid(lty=1, col=gray(.9))                                   # add a grid
lines(return, type='l', col=4)   

dev.off()

##################################################################
##################   Wavelet Transform     #######################

#Discrete Wavelet transform

# mother wavelet function db4 (Daubechies wavelet of order 4)=d4
# decomposition level is 5
# soft thresholding
# threshold method: rigrsure->adpative

#install.packages("wmtsa") 
# new version have been archived

#load old version
#install.packages("remotes")
library(remotes)
#install_version("wmtsa", "2.0-3")
library(wmtsa)
DWTprice<-wavShrink(prices, wavelet="d4",
                    n.level=1, #level=5 is too smoothy #change n.levels will get huge difference on spectrum plot
                    shrink.fun="soft", thresh.fun="adaptive")

# original signal v.s. denoised signal
tsplot(prices, ylab="price", lwd=2, col=rgb(0.2,0.4,0.8))
lines(DWTprice, lwd=2, col=rgb( 0.8,0.5,0.3) )
legend('topright', col=c(rgb(0.2,0.4,0.8),  rgb( 0.8,0.5,0.3)), lwd=2, 
       legend=c("orginal signal", "denoised signal"), bg='white') 

# Continuous wavelet transform
library(WaveletComp)
logReturn<-rep(0,length(DWTprice)-1)
for (i in 1: (length(DWTprice)-1)){
  logReturn[i]<-log(DWTprice[i+1])-log(DWTprice[i])
}

# original return v.s. denoised return
tsplot(return, ylab="price", lwd=2, col=rgb(0.2,0.4,0.8))
lines(logReturn, lwd=2, col=rgb( 0.8,0.5,0.3) )
legend('topright', col=c(rgb(0.2,0.4,0.8),  rgb( 0.8,0.5,0.3)), lwd=2, 
       legend=c("orginal return", "denoised return"), bg='white') 

# mother wavelet function: Morlet wavelet
my.data<-data.frame(x=prices)
my.w<-analyze.wavelet(my.data, "x",
                      loess.span = 0,
                      dt=1,dj=1/250,
                      lowerPeriod = 2,
                      upperPeriod = 256,
                      make.pval = FALSE,n.sim = 10)

#plot the wavelet power spectrum-prices
wt.image(my.w, color.key = "interval",n.levels=250,
         legend.params = list(lab="wavelet power levels",mar=4.7))


# power spectrum-DWTprices
my.data<-data.frame(x=DWTprices)
my.w<-analyze.wavelet(my.data, "x",
                      loess.span = 0,
                      dt=1,dj=1/50,
                      lowerPeriod = 2,
                      upperPeriod = 256,
                      make.pval = FALSE,n.sim = 10)

#plot the wavelet power spectrum
wt.image(my.w, color.key = "interval",n.levels=250,
         legend.params = list(lab="wavelet power levels",mar=4.7, label.digits=2)
)

# power spectrum-logReturn
my.data<-data.frame(x=logReturn)
my.w<-analyze.wavelet(my.data, "x",
                      loess.span = 0,
                      dt=1,dj=1/50,
                      lowerPeriod = 2,
                      upperPeriod = 256,
                      make.pval = FALSE,n.sim = 10)

#plot the wavelet power spectrum
wt.image(my.w, color.key = "interval",n.levels=5,
         legend.params = list(lab="wavelet power levels",mar=4.7, label.digits=2)
)

##################################################################
##################       2009-04-08        #######################

#Extract the data on 2009-04-08
SPX090408 <- subset(SPX,DateTime >= "2009-04-08 09:30:00 " & DateTime <= "2009-04-08 16:00:00")

# calculate prices and returns

prices<-SPX090408$Close
DWTprice<-wavShrink(prices, wavelet="d4",
                    n.level=1, #level=5 is too smoothy #change n.levels will get huge difference on spectrum plot
                    shrink.fun="soft", thresh.fun="adaptive")

# original signal v.s. denoised signal
tsplot(prices, ylab="price", lwd=2, col=rgb(0.2,0.4,0.8))
lines(DWTprice, lwd=2, col=rgb( 0.8,0.5,0.3) )
legend('topright', col=c(rgb(0.2,0.4,0.8),  rgb( 0.8,0.5,0.3)), lwd=2, 
       legend=c("orginal signal", "denoised signal"), bg='white') 

#plot denoised power spectrum
logReturn<-rep(0,length(DWTprice)-1)
for (i in 1: (length(DWTprice)-1)){
  logReturn[i]<-log(DWTprice[i+1])-log(DWTprice[i])
}

my.data<-data.frame(x=logReturn)
my.w<-analyze.wavelet(my.data, "x",
                      loess.span = 0,
                      dt=1,dj=1/250,
                      lowerPeriod = 2,
                      upperPeriod = 256,
                      make.pval = FALSE,n.sim = 10)

#plot the wavelet power spectrum-prices
wt.image(my.w, color.key = "interval",n.levels=250,
         legend.params = list(lab="wavelet power levels",mar=4.7))

##################################################################
##################       2011-04-06        #######################

#Extract the data on 2011-04-06
SPX110406 <- subset(SPX,DateTime >= "2011-04-06 09:30:00 " & DateTime <= "2011-04-06 16:00:00")

# calculate prices and returns

prices<-SPX110406$Close
DWTprice<-wavShrink(prices, wavelet="d4",
                    n.level=1, #level=5 is too smoothy #change n.levels will get huge difference on spectrum plot
                    shrink.fun="soft", thresh.fun="adaptive")

# original signal v.s. denoised signal
tsplot(prices, ylab="price", lwd=2, col=rgb(0.2,0.4,0.8))
lines(DWTprice, lwd=2, col=rgb( 0.8,0.5,0.3) )
legend('topright', col=c(rgb(0.2,0.4,0.8),  rgb( 0.8,0.5,0.3)), lwd=2, 
       legend=c("orginal signal", "denoised signal"), bg='white') 

#plot denoised power spectrum
logReturn<-rep(0,length(DWTprice)-1)
for (i in 1: (length(DWTprice)-1)){
  logReturn[i]<-log(DWTprice[i+1])-log(DWTprice[i])
}

my.data<-data.frame(x=logReturn)
my.w<-analyze.wavelet(my.data, "x",
                      loess.span = 0,
                      dt=1,dj=1/250,
                      lowerPeriod = 2,
                      upperPeriod = 256,
                      make.pval = FALSE,n.sim = 10)

#plot the wavelet power spectrum-prices
wt.image(my.w, color.key = "interval",n.levels=250,
         legend.params = list(lab="wavelet power levels",mar=4.7))

##################################################################
##################       2013-04-10        #######################

#Extract the data on 2013-04-10
SPX130410 <- subset(SPX,DateTime >= "2013-04-10 09:30:00 " & DateTime <= "2013-04-10 16:00:00")

# calculate prices and returns

prices<-SPX130410$Close
DWTprice<-wavShrink(prices, wavelet="d4",
                    n.level=1, #level=5 is too smoothy #change n.levels will get huge difference on spectrum plot
                    shrink.fun="soft", thresh.fun="adaptive")

# original signal v.s. denoised signal
tsplot(prices, ylab="price", lwd=2, col=rgb(0.2,0.4,0.8))
lines(DWTprice, lwd=2, col=rgb( 0.8,0.5,0.3) )
legend('topright', col=c(rgb(0.2,0.4,0.8),  rgb( 0.8,0.5,0.3)), lwd=2, 
       legend=c("orginal signal", "denoised signal"), bg='white') 

#plot denoised power spectrum
logReturn<-rep(0,length(DWTprice)-1)
for (i in 1: (length(DWTprice)-1)){
  logReturn[i]<-log(DWTprice[i+1])-log(DWTprice[i])
}

my.data<-data.frame(x=logReturn)
my.w<-analyze.wavelet(my.data, "x",
                      loess.span = 0,
                      dt=1,dj=1/250,
                      lowerPeriod = 2,
                      upperPeriod = 256,
                      make.pval = FALSE,n.sim = 10)

#plot the wavelet power spectrum-prices
wt.image(my.w, color.key = "interval",n.levels=250,
         legend.params = list(lab="wavelet power levels",mar=4.7))

##################################################################
##################       2015-04-08        #######################

#Extract the data on 2015-04-08
SPX150408 <- subset(SPX,DateTime >= "2015-04-08 09:30:00 " & DateTime <= "2015-04-08 16:00:00")

# calculate prices and returns

prices<-SPX150408$Close
DWTprice<-wavShrink(prices, wavelet="d4",
                    n.level=1, #level=5 is too smoothy #change n.levels will get huge difference on spectrum plot
                    shrink.fun="soft", thresh.fun="adaptive")

# original signal v.s. denoised signal
tsplot(prices, ylab="price", lwd=2, col=rgb(0.2,0.4,0.8))
lines(DWTprice, lwd=2, col=rgb( 0.8,0.5,0.3) )
legend('topright', col=c(rgb(0.2,0.4,0.8),  rgb( 0.8,0.5,0.3)), lwd=2, 
       legend=c("orginal signal", "denoised signal"), bg='white') 

#plot denoised power spectrum
logReturn<-rep(0,length(DWTprice)-1)
for (i in 1: (length(DWTprice)-1)){
  logReturn[i]<-log(DWTprice[i+1])-log(DWTprice[i])
}

my.data<-data.frame(x=logReturn)
my.w<-analyze.wavelet(my.data, "x",
                      loess.span = 0,
                      dt=1,dj=1/250,
                      lowerPeriod = 2,
                      upperPeriod = 256,
                      make.pval = TRUE,n.sim = 10)

#plot the wavelet power spectrum-prices
wt.image(my.w, color.key = "interval",n.levels=250,
         legend.params = list(lab="wavelet power levels",mar=4.7))

##################################################################
##################       2017-04-12        #######################

#Extract the data on 2017-04-12
SPX170412 <- subset(SPX,DateTime >= "2017-04-12 09:30:00 " & DateTime <= "2017-04-12 16:00:00")

# calculate prices and returns

prices<-SPX170412$Close
DWTprice<-wavShrink(prices, wavelet="d4",
                    n.level=1, #level=5 is too smoothy #change n.levels will get huge difference on spectrum plot
                    shrink.fun="soft", thresh.fun="adaptive")

# original signal v.s. denoised signal
tsplot(prices, ylab="price", lwd=2, col=rgb(0.2,0.4,0.8))
lines(DWTprice, lwd=2, col=rgb( 0.8,0.5,0.3) )
legend('topright', col=c(rgb(0.2,0.4,0.8),  rgb( 0.8,0.5,0.3)), lwd=2, 
       legend=c("orginal signal", "denoised signal"), bg='white') 

#plot denoised power spectrum
logReturn<-rep(0,length(DWTprice)-1)
for (i in 1: (length(DWTprice)-1)){
  logReturn[i]<-log(DWTprice[i+1])-log(DWTprice[i])
}

my.data<-data.frame(x=logReturn)
my.w<-analyze.wavelet(my.data, "x",
                      loess.span = 0,
                      dt=1,dj=1/250,
                      lowerPeriod = 2,
                      upperPeriod = 256,
                      make.pval = FALSE,n.sim = 10)

#plot the wavelet power spectrum-prices
wt.image(my.w, color.key = "interval",n.levels=250,
         legend.params = list(lab="wavelet power levels",mar=4.7))


##################################################################
##################       2019-04-10        #######################

#Extract the data on 2019-04-10
SPX190410 <- subset(SPX,DateTime >= "2019-04-10 09:30:00 " & DateTime <= "2019-04-10 16:00:00")

# calculate prices and returns
prices<-SPX190410$Close
DWTprice<-wavShrink(prices, wavelet="d4",
                    n.level=1, #level=5 is too smoothy #change n.levels will get huge difference on spectrum plot
                    shrink.fun="soft", thresh.fun="adaptive")

# original signal v.s. denoised signal
tsplot(prices, ylab="price", lwd=2, col=rgb(0.2,0.4,0.8))
lines(DWTprice, lwd=2, col=rgb( 0.8,0.5,0.3) )
legend('topright', col=c(rgb(0.2,0.4,0.8),  rgb( 0.8,0.5,0.3)), lwd=2, 
       legend=c("orginal signal", "denoised signal"), bg='white') 

#plot denoised power spectrum
logReturn<-rep(0,length(DWTprice)-1)
for (i in 1: (length(DWTprice)-1)){
  logReturn[i]<-log(DWTprice[i+1])-log(DWTprice[i])
}

my.data<-data.frame(x=logReturn)
my.w<-analyze.wavelet(my.data, "x",
                      loess.span = 0,
                      dt=1,dj=1/250,
                      lowerPeriod = 2,
                      upperPeriod = 256,
                      make.pval = FALSE,n.sim = 10)

#plot the wavelet power spectrum-prices
wt.image(my.w, color.key = "interval",n.levels=250,
         legend.params = list(lab="wavelet power levels",mar=4.7))


##################################################################
##################       target y label     ######################

options(digits.secs=3)
Sys.setenv(TZ='EST')

# truncate data from 2009-01-01 to 2019-12-31, 9:30 AM to 16:00 PM

SPX$DateTime<-as.POSIXct(SPX$DateTime,format="%H:%M:%OS", taz="EST")
SPX<- subset(SPX, lubridate::hour(SPX$DateTime)*60
             +lubridate::minute(SPX$DateTime) >= 9*60+30)
SPX <- subset(SPX, lubridate::hour(SPX$DateTime)*60
              +lubridate::minute(SPX$DateTime) <= 16*60)
head(SPX)
SPX <- subset(SPX,DateTime >= " 2009-01-01" & DateTime <= "2019-12-31")
head(SPX)


#length(SPX$DateTime)
#calculate the log return between the average price from 1 to 360 minutes in the window and the price at the last minute of the day

# convert Posixct to xts format
library(highfrequency)
library(xts)
spx_ts<-xts(SPX$Close,SPX$DateTime)
names(spx_ts) <- "price"

day_index<-endpoints(spx_ts, on = "days", k = 1)
#head(day_index)
#length(day_index)

# last min prices for each trading day
lmP<-spx_ts[day_index,]
head(lmP)

# average prices from 1 to 360 mins
n<-length(day_index)-1
avg_360<-rep(0,n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]-30
  avg_360[i]<-mean(spx_ts[start:end])
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

##################################################################
####### creating daily 2D spectrum from 2009-2019     ############

# Don't run

mypath="//Users/luzhang/Desktop/spectrum_updown"
setwd(mypath)

#avoid overwriting what files already exist @RockScience
createNewFileName = function(path  = getwd(), pattern = "plot_of_something", extension=".png") {
  myExistingFiles = list.files(path = path, pattern = pattern)
  print(myExistingFiles)
  completePattern = paste0("^(",pattern,")([0-9]*)(",extension,")$")
  existingNumbers  = gsub(pattern = completePattern, replacement = "\\2", x = myExistingFiles)
  
  if (identical(existingNumbers, character(0)))
    existingNumbers = 0
  
  return(paste0(pattern,max(as.numeric(existingNumbers))+1,extension))
}

#plot and save daily spectrum from 2009-01-02 to 2019-12-31

library(WaveletComp)
library(wmtsa)

n<-length(day_index)-1
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  temp<-spx_ts[start:end,]
  prices<-as.vector(temp)
  DWTprice<-wavShrink(prices, wavelet="d4",
                      n.level=1, 
                      shrink.fun="soft", thresh.fun="adaptive")
  logReturn<-rep(0,length(DWTprice)-1)
  for (j in 1: (length(DWTprice)-1)){
    logReturn[j]<-log(DWTprice[j+1])-log(DWTprice[j])
  }
  
  my.data<-data.frame(x=logReturn)
  my.w<-analyze.wavelet(my.data, "x",
                        loess.span = 0,
                        dt=1,dj=1/250,
                        lowerPeriod = 2,
                        upperPeriod = 256,
                        make.pval = FALSE,n.sim = 10)
  
  #plot the wavelet power spectrum-prices
  
  if(y_label[i]==0){
    png(filename = createNewFileName(pattern="Downsepctrum"))
    wt.image(my.w, color.key = "interval",n.levels=250,
             legend.params = list(lab="wavelet power levels",mar=4.7))
    dev.off()  
  }else{
    png(filename = createNewFileName(pattern="Upsepctrum"))
    wt.image(my.w, color.key = "interval",n.levels=250,
             legend.params = list(lab="wavelet power levels",mar=4.7))
    dev.off()  
  }
  
  
}

# total will be 2766 figures


##################################################################
###################   MIC: top 5 indicators    ###################
 
# indicators vs y label#
# failed, mic value too small#

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

# close price index, don't know how to calculate it


# daily Exponential moving average(EMA)
# In the TTR package, we can use EMA():
#install.packages("TTR")
library(TTR)
EMA <-EMA(daily_prices) 

# Relative Strength Index(RSI)
RSI<-RSI(daily_prices)

# Moving average within period 60 minutes
MA60<-SMA(daily_prices)

#  CORREL =autocorrealtaion? NO, we need CORREL has the same length like other indicators
#CORREL=acf(daily_prices, lag=3, plot=F)

#transform the indicators in matrix form
y_label<-as.matrix(y_label)

#indicators
inds<-cbind(
  #index_close,
  EMA,
  RSI,
  MA60,
  CMOclose=CMO(daily_prices),
  DPOclose=DPO(daily_prices),
  MACDClose=MACD(daily_prices)[,1],
  momentum=momentum(daily_prices),
  runPerRankClose=runPercentRank(daily_prices),
  TDI=TDI(daily_prices)[,1],
  TRIX=TRIX(daily_prices)[,1],
  VHF=VHF(daily_prices)           
)

inds<-as.matrix(inds)
head(inds)

#calculate MIC
library(minerva)
ROCClose=ROC(daily_prices)
mics<-cstats(inds,y_label)

#mics between indicators and y_label are too small, around 0.1

# indicators based on minute close prices and prices change ratios (1 months data)
library(minerva)
library(TTR)
start<-day_index[1]+1
end<-day_index[30]
spx_test<-SPX[start:end,]
#head(spx_test)
#tail(spx_test)
prices<-spx_test$Close
ROCClose=ROC(prices)#prices change ratio

inds<-cbind(
  #index_close,
  EMA<-EMA(prices),
  RSI<-RSI(prices),
  MA60<-SMA(prices),
  CMOclose=CMO(prices),
  DPOclose=DPO(prices),
  MACDClose=MACD(prices)[,1],
  momentum=momentum(prices),
  runPerRankClose=runPercentRank(prices),
  TDI=TDI(prices)[,1],
  TRIX=TRIX(prices)[,1],
  VHF=VHF(prices)           
)

inds<-as.matrix(inds)
ROCClose<-as.matrix(ROCClose)
cstats(inds, ROCClose)

# indicators based on minute close prices and denoised log return

library(minerva)
library(TTR)
library(WaveletComp)
library(wmtsa)
start<-day_index[1]+1
end<-day_index[30]
spx_test<-SPX[start:end,]

#head(spx_test)
#tail(spx_test)
prices<-spx_test$Close

DWTprice<-wavShrink(prices, wavelet="d4",
                    n.level=1, 
                    shrink.fun="soft", thresh.fun="adaptive")
logReturn<-rep(0,length(DWTprice))
for (i in 1: (length(DWTprice))){
  logReturn[i]<-log(DWTprice[i+1])-log(DWTprice[i])
}


inds<-cbind(
  #index_close,
  EMA<-EMA(prices),
  RSI<-RSI(prices),
  MA60<-SMA(prices),
  CMOclose=CMO(prices),
  DPOclose=DPO(prices),
  MACDClose=MACD(prices)[,1],
  momentum=momentum(prices),
  runPerRankClose=runPercentRank(prices),
  TDI=TDI(prices)[,1],
  TRIX=TRIX(prices)[,1],
  VHF=VHF(prices)           
)

inds<-as.matrix(inds)
logReturn<-as.matrix(logReturn)
cstats(inds, logReturn)

# indicators based on denoised log return and prices change ratios

library(minerva)
library(TTR)
library(WaveletComp)
library(wmtsa)
start<-day_index[1]+1
end<-day_index[30]
spx_test<-SPX[start:end,]

prices<-spx_test$Close
ROCClose=ROC(prices)

DWTprice<-wavShrink(prices, wavelet="d4",
                    n.level=1, 
                    shrink.fun="soft", thresh.fun="adaptive")
logReturn<-rep(0,length(DWTprice))
for (i in 1: (length(DWTprice))){
  logReturn[i]<-log(DWTprice[i+1])-log(DWTprice[i])
}

# produce non-leading NAS
inds<-cbind(
  #index_close,
  EMA<-EMA(logReturn),
  RSI<-RSI(logReturn),
  MA60<-SMA(logReturn),
  CMOclose=CMO(logReturn),
  DPOclose=DPO(logReturn),
  MACDClose=MACD(logReturn)[,1],
  momentum=momentum(logReturn),
  runPerRankClose=runPercentRank(logReturn),
  TDI=TDI(logReturn)[,1],
  TRIX=TRIX(logReturn)[,1],
  VHF=VHF(logReturn)           
)

inds<-as.matrix(inds)
ROCClose<-as.matrix(ROCClose)
cstats(inds, ROCClose)

# indicators based on daily average log return and y label
n<-length(day_index)-1
avg_log<-rep(0, n)
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  temp<-spx_ts[start:end,]
  prices<-as.vector(temp)
  DWTprice<-wavShrink(prices, wavelet="d4",
                      n.level=1, 
                      shrink.fun="soft", thresh.fun="adaptive")
  logReturn<-rep(0,length(DWTprice)-1)
  for (j in 1: (length(DWTprice)-1)){
    logReturn[j]<-log(DWTprice[j+1])-log(DWTprice[j])
  }
  avg_log[i]<-mean(logReturn)
}
#length(avg_log)
#avg_log<-as.matrix(avg_log)
#y_label<-as.matrix(y_label)
#mic<-cstats(avg_log, y_label) #6.	daily average log return and y label

#transform the indicators in matrix form
y_label<-as.matrix(y_label)

#indicators
inds<-cbind(
  #index_close,
  EMA<-EMA(avg_log),
  RSI<-RSI(avg_log),
  MA60<-SMA(avg_log),
  CMOclose=CMO(avg_log),
  DPOclose=DPO(avg_log),
  MACDClose=MACD(avg_log)[,1],
  momentum=momentum(avg_log),
  runPerRankClose=runPercentRank(avg_log),
  TDI=TDI(avg_log)[,1],
  TRIX=TRIX(avg_log)[,1],
  VHF=VHF(avg_log)           
)


#calculate MIC
library(minerva)
inds<-as.matrix(inds)
head(inds)
mics<-cstats(inds,y_label)

##################################################################
############## indicators daily 2D spectrum    ###################

# EMA
mypath="//Users/luzhang/Desktop/EMAspectrum"
setwd(mypath)

#avoid overwriting what files already exist @RockScience
createNewFileName = function(path  = getwd(), pattern = "plot_of_something", extension=".png") {
  myExistingFiles = list.files(path = path, pattern = pattern)
  print(myExistingFiles)
  completePattern = paste0("^(",pattern,")([0-9]*)(",extension,")$")
  existingNumbers  = gsub(pattern = completePattern, replacement = "\\2", x = myExistingFiles)
  
  if (identical(existingNumbers, character(0)))
    existingNumbers = 0
  
  return(paste0(pattern,max(as.numeric(existingNumbers))+1,extension))
}

#plot and save daily spectrum from 2009-01-02 to 2019-12-31

library(WaveletComp)
library(wmtsa)
library(TTR)

n<-length(day_index)-1
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  temp<-spx_ts[start:end,]
  prices<-as.vector(temp)
  DWTprice<-wavShrink(prices, wavelet="d4",
                      n.level=1, 
                      shrink.fun="soft", thresh.fun="adaptive")
  logReturn<-rep(0,length(DWTprice)-1)
  for (i in 1: (length(DWTprice)-1)){
    logReturn[i]<-log(DWTprice[i+1])-log(DWTprice[i])
  }
  
  EMA<-EMA(logReturn)
  EMA<-EMA[!is.na(EMA)]
  my.data<-data.frame(x=EMA)
  my.w<-analyze.wavelet(my.data, "x",
                        loess.span = 0,
                        dt=1,dj=1/250,
                        lowerPeriod = 2,
                        upperPeriod = 256,
                        make.pval = FALSE,n.sim = 10)
  
  #plot the wavelet power spectrum-prices
  
  png(filename = createNewFileName(pattern="sepctrum"))
  wt.image(my.w, color.key = "interval",n.levels=250,
           legend.params = list(lab="wavelet power levels",mar=4.7))
  dev.off() 
}

# RSI
mypath="//Users/luzhang/Desktop/RSIspectrum"
setwd(mypath)

#avoid overwriting what files already exist @RockScience
createNewFileName = function(path  = getwd(), pattern = "plot_of_something", extension=".png") {
  myExistingFiles = list.files(path = path, pattern = pattern)
  print(myExistingFiles)
  completePattern = paste0("^(",pattern,")([0-9]*)(",extension,")$")
  existingNumbers  = gsub(pattern = completePattern, replacement = "\\2", x = myExistingFiles)
  
  if (identical(existingNumbers, character(0)))
    existingNumbers = 0
  
  return(paste0(pattern,max(as.numeric(existingNumbers))+1,extension))
}

#plot and save daily spectrum from 2009-01-02 to 2019-12-31

library(WaveletComp)
library(wmtsa)
library(TTR)

n<-length(day_index)-1
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  temp<-spx_ts[start:end,]
  prices<-as.vector(temp)
  DWTprice<-wavShrink(prices, wavelet="d4",
                      n.level=1, 
                      shrink.fun="soft", thresh.fun="adaptive")
  logReturn<-rep(0,length(DWTprice)-1)
  for (i in 1: (length(DWTprice)-1)){
    logReturn[i]<-log(DWTprice[i+1])-log(DWTprice[i])
  }
  
  RSI<-RSI(logReturn)
  RSI<-RSI[!is.na(RSI)]
  my.data<-data.frame(x=RSI)
  my.w<-analyze.wavelet(my.data, "x",
                        loess.span = 0,
                        dt=1,dj=1/250,
                        lowerPeriod = 2,
                        upperPeriod = 256,
                        make.pval = FALSE,n.sim = 10)
  
  #plot the wavelet power spectrum-prices
  
  png(filename = createNewFileName(pattern="sepctrum"))
  wt.image(my.w, color.key = "interval",n.levels=250,
           legend.params = list(lab="wavelet power levels",mar=4.7))
  dev.off() 
}

# MA60
mypath="//Users/luzhang/Desktop/MA60spectrum"
setwd(mypath)

#avoid overwriting what files already exist @RockScience
createNewFileName = function(path  = getwd(), pattern = "plot_of_something", extension=".png") {
  myExistingFiles = list.files(path = path, pattern = pattern)
  print(myExistingFiles)
  completePattern = paste0("^(",pattern,")([0-9]*)(",extension,")$")
  existingNumbers  = gsub(pattern = completePattern, replacement = "\\2", x = myExistingFiles)
  
  if (identical(existingNumbers, character(0)))
    existingNumbers = 0
  
  return(paste0(pattern,max(as.numeric(existingNumbers))+1,extension))
}

#plot and save daily spectrum from 2009-01-02 to 2019-12-31

library(WaveletComp)
library(wmtsa)
library(TTR)

n<-length(day_index)-1
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  temp<-spx_ts[start:end,]
  prices<-as.vector(temp)
  DWTprice<-wavShrink(prices, wavelet="d4",
                      n.level=1, 
                      shrink.fun="soft", thresh.fun="adaptive")
  logReturn<-rep(0,length(DWTprice)-1)
  for (i in 1: (length(DWTprice)-1)){
    logReturn[i]<-log(DWTprice[i+1])-log(DWTprice[i])
  }
  
  MA60<-SMA(logReturn)
  MA60<-MA60[!is.na(MA60)]
  my.data<-data.frame(x=MA60)
  my.w<-analyze.wavelet(my.data, "x",
                        loess.span = 0,
                        dt=1,dj=1/250,
                        lowerPeriod = 2,
                        upperPeriod = 256,
                        make.pval = FALSE,n.sim = 10)
  
  #plot the wavelet power spectrum-prices
  
  png(filename = createNewFileName(pattern="sepctrum"))
  wt.image(my.w, color.key = "interval",n.levels=250,
           legend.params = list(lab="wavelet power levels",mar=4.7))
  dev.off() 
}

##################################################################
#########################   CNN model   ##########################

# Copying spectrum images to training,validation, and test directories
Original_dataset_dir<-"//Users/luzhang/Desktop/indicator_spectrum_logRetrun/CMO"

base_dir<-"//Users/luzhang/Desktop/spectrum_CMO"
dir.create(base_dir)

train_dir<-file.path(base_dir,"train")
dir.create(train_dir)
test_dir<-file.path(base_dir,"test")
dir.create(test_dir)

train_up_dir<-file.path(train_dir,"upspectrum")
dir.create(train_up_dir)

train_down_dir<-file.path(train_dir,"downspectrum")
dir.create(train_down_dir)


test_up_dir<-file.path(test_dir,"upspectrum")
dir.create(test_up_dir)

test_down_dir<-file.path(test_dir,"downspectrum")
dir.create(test_down_dir)

fnames<-paste0("Upsepctrum", 1:1000, ".png")
file.copy(file.path(Original_dataset_dir, fnames),
          file.path(train_up_dir))

fnames<-paste0("Upsepctrum", 1001:1534, ".png")
file.copy(file.path(Original_dataset_dir, fnames),
          file.path(test_up_dir))

fnames<-paste0("Downsepctrum", 1:1000, ".png")
file.copy(file.path(Original_dataset_dir, fnames),
          file.path(train_down_dir))

fnames<-paste0("Downsepctrum", 1001:1534, ".png")
file.copy(file.path(Original_dataset_dir, fnames),
          file.path(test_down_dir))

#check how many pictures are in each training split(train/validation/test)
cat("total training upspectrum images:", length(list.files(train_up_dir)),"\n")
cat("total training downspectrum images:", length(list.files(train_down_dir)),"\n")

cat("total validation upspectrum images:", length(list.files(validation_up_dir)),"\n")
cat("total validation downspectrum images:", length(list.files(validation_down_dir)),"\n")


cat("total test upspectrum images:", length(list.files(test_up_dir)),"\n")
cat("total test downspectrum images:", length(list.files(test_down_dir)),"\n")

# Building networks
rm(list = setdiff(ls(), lsf.str()))
library(keras)

model<-keras_model_sequential()%>%
  layer_conv_2d(filters=32, kernel_size=c(3,3),activation="relu",
                input_shape=c(150,150,3))%>%
  layer_max_pooling_2d(pool_size=c(2,2))%>%
  layer_conv_2d(filters=64, kernel_size=c(3,3),activation="relu",
                input_shape=c(150,150,3))%>%
  layer_max_pooling_2d(pool_size=c(2,2))%>%
  layer_conv_2d(filters=128, kernel_size=c(3,3),activation="relu",
                input_shape=c(150,150,3))%>%
  layer_max_pooling_2d(pool_size=c(2,2))%>%
  layer_flatten() %>%
  layer_dense(units=512, activation="relu")%>%
  layer_dense(units=1,activation="sigmoid")
  
summary(model)

# Configuring the model for training
model %>% compile(
  loss="binary_crossentropy",
  optimizer=optimizer_rmsprop(lr=1e-4),
  metrics=c("acc")
)

# using image_data_generator to read images from directions
train_datagen<-image_data_generator(rescale = 1/255)
validation_datagen<-image_data_generator(rescale=1/255)
test_datagen<-image_data_generator(rescale = 1/255)

train_generator<-flow_images_from_directory(
  train_dir,
  train_datagen,
  target_size = c(150,150),
  batch_size = 20,
  class_mode = "binary"
)

validation_generator<-flow_images_from_directory(
  validation_dir,
  validation_datagen,
  target_size = c(150,150),
  batch_size = 20,
  class_mode = "binary"
)

test_generator<-flow_images_from_directory(
  test_dir,
  test_datagen,
  target_size = c(150,150),
  batch_size=20,
  class_mode = "binary"
)

  
batch<-generator_next(train_generator)
str(batch)

history<-model %>% fit_generator(
  train_generator,
  steps_per_epoch = 20,
  epochs=20,
  validation_data=validation_generator,
  validation_steps = 20
)

setwd("Users/luzhang/Desktop/acc_loss")
model %>% save_model_hdf5("acc_loss_attempt01")
plot(history)

model %>% evaluate_generator(test_generator,steps=20)











