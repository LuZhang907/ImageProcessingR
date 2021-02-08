
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
                      make.pval = FALSE,n.sim = 10)

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
############# prediction target y label     ######################

options(digits.secs=3)
Sys.setenv(TZ='EST')

# truncate data from 2009-01-01 to 2019-12-31, 9:30 AM to 16:00 PM

SPX$DateTime<-as.POSIXct(SPX$DateTime,format="%H:%M:%OS", taz="EST")
SPX<- subset(SPX, lubridate::hour(SPX$DateTime)*60
             +lubridate::minute(SPX$DateTime) >= 9*60+30)
SPX <- subset(SPX, lubridate::hour(SPX$DateTime)*60
              +lubridate::minute(SPX$DateTime) <= 16*60)
head(SPX)
SPX <- subset(SPX,DateTime >= " 2009-01-01 09:30:00" & DateTime <= "2019-12-31 16:00:00")
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

##################################################################
####### creating daily 2D spectrum from 2009-2019     ############

# Don't run

mypath="//Users/luzhang/Desktop/spectrum"
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
  
  png(filename = createNewFileName(pattern="sepctrum"))
  wt.image(my.w, color.key = "interval",n.levels=250,
           legend.params = list(lab="wavelet power levels",mar=4.7))
  dev.off() 
}

# total will be 2766 figures


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







