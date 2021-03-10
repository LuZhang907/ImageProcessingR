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
spx_Close<-xts(dwt_Close,SPX$DateTime)


#using average of C, H, L, O as general prices
prices<-spx_Close
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
#logReturn<-log(avg_360)-log(lmP)
#length(logRetrn)

#figure 5
#hist(logReturn, 
#     main="Retrun freqeuncy distribution y_mean_390", 
#     xlab="Return", 
#     col="red",
#    xlim=c(-0.03,0.03),
#     las=1, 
#     breaks=200)

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

#y_label
#0    1 
#1534 1913 

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

##indicators calculation
library(TTR)

#CMOClose done


mypath="/Users/luzhang/Desktop/indicator_spectrum/CMO"
setwd(mypath)

#plot and save daily spectrum

n<-length(day_index)-1
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  temp<-spx_ts[start:end,]
  prices<-as.vector(temp)
  DWTprice<-wavShrink(prices, wavelet="d4",
                      n.level=1, 
                      shrink.fun="soft", thresh.fun="adaptive")

  CMO<-CMO(DWTprice)
  CMO<-CMO[!is.na(CMO)]
  my.data<-data.frame(x=CMO)
  my.w<-analyze.wavelet(my.data, "x",
                        loess.span = 0,
                        dt=1,dj=1/250,
                        lowerPeriod = 2,
                        upperPeriod = 256,
                        make.pval = FALSE,n.sim = 10)
  
  #plot the wavelet power spectrum-prices
  if (y_label[i]==0) {
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

#DPOClose done

mypath="/Users/luzhang/Desktop/indicator_spectrum/DPO"
setwd(mypath)
#plot and save daily spectrum

n<-length(day_index)-1
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  temp<-spx_ts[start:end,]
  prices<-as.vector(temp)
  DWTprice<-wavShrink(prices, wavelet="d4",
                      n.level=1, 
                      shrink.fun="soft", thresh.fun="adaptive")
  
  DPO<-DPO(DWTprice)
  DPO<-DPO[!is.na(DPO)]
  my.data<-data.frame(x=DPO)
  my.w<-analyze.wavelet(my.data, "x",
                        loess.span = 0,
                        dt=1,dj=1/250,
                        lowerPeriod = 2,
                        upperPeriod = 256,
                        make.pval = FALSE,n.sim = 10)
  
  #plot the wavelet power spectrum-prices
  if (y_label[i]==0) {
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

#DVI fatel error!

mypath="/Users/luzhang/Desktop/indicator_spectrum/DVI"
setwd(mypath)
#plot and save daily spectrum

n<-length(day_index)-1
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  temp<-spx_ts[start:end,]
  prices<-as.vector(temp)
  DWTprice<-wavShrink(prices, wavelet="d4",
                      n.level=1, 
                      shrink.fun="soft", thresh.fun="adaptive")
# logReturn<-rep(0,length(DWTprice)-1)
# for (j in 1: (length(DWTprice)-1)){
#  logReturn[j]<-log(DWTprice[j+1])-log(DWTprice[j])
#}
  
  DVI<-DVI(DWTprice)
  DVI<-DVI[!is.na(DVI)]
  my.data<-data.frame(x=DVI)
  my.w<-analyze.wavelet(my.data, "x",
                        loess.span = 0,
                        dt=1,dj=1/250,
                        lowerPeriod = 2,
                        upperPeriod = 256,
                        make.pval = FALSE,n.sim = 10)
  
  #plot the wavelet power spectrum-prices
  if (y_label[i]==0) {
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

#RSI done
mypath="/Users/luzhang/Desktop/indicator_spectrum/RSI"
setwd(mypath)

#plot and save daily spectrum

n<-length(day_index)-1
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  temp<-spx_ts[start:end,]
  prices<-as.vector(temp)
  DWTprice<-wavShrink(prices, wavelet="d4",
                      n.level=1, 
                      shrink.fun="soft", thresh.fun="adaptive")
  
  RSI<-RSI(DWTprice)
  RSI<-RSI[!is.na(RSI)]
  my.data<-data.frame(x=RSI)
  my.w<-analyze.wavelet(my.data, "x",
                        loess.span = 0,
                        dt=1,dj=1/250,
                        lowerPeriod = 2,
                        upperPeriod = 256,
                        make.pval = FALSE,n.sim = 10)
  
  #plot the wavelet power spectrum-prices
  if (y_label[i]==0) {
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

#WPR 
mypath="/Users/luzhang/Desktop/indicator_spectrum/WPR"
setwd(mypath)

#plot and save daily spectrum

n<-length(day_index)-1
for (i in 1:n){
  start<-day_index[i]+1
  end<-day_index[i+1]
  temp<-spx_ts[start:end,]
  prices<-as.vector(temp)
  DWTprice<-wavShrink(prices, wavelet="d4",
                      n.level=1, 
                      shrink.fun="soft", thresh.fun="adaptive")
  WPR<-WPR(DWTprice)
  WPR<-WPR[!is.na(WPR)]
  my.data<-data.frame(x=WPR)
  my.w<-analyze.wavelet(my.data, "x",
                        loess.span = 0,
                        dt=1,dj=1/250,
                        lowerPeriod = 2,
                        upperPeriod = 256,
                        make.pval = FALSE,n.sim = 10)
  
  #plot the wavelet power spectrum-prices
  if (y_label[i]==0) {
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
