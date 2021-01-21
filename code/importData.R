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
SPX$DateTime<-as.POSIXct(SPX$DateTime,tz=Sys.timezone())

##################################################################
##################       2014-04-11        #######################

#Extract the data on 2014-04-11
SPX140411 <- subset(SPX,DateTime >= "2014-04-11 09:30:00 " & DateTime <= "2014-04-11 16:00:00")

# calculate prices and returns

# prices=avergae(O,H,L,C)
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

#Extract the data on 2017-04-12
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
############# prediction target y label     #######################

# from 2009-04-08 2019-04-10

