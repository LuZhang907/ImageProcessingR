##################################################################
##################       2014-04-11        #######################

#Extract the data on 2014-04-11
SPX140411 <- subset(SPX,DateTime >= "2014-04-11 09:30:00 " & DateTime <= "2014-04-11 16:00:00")

# calculate prices and returns

# prices=avergae(O,H,L,C)
x<-cbind(SPX140411$Open,SPX140411$High,SPX140411$Low, SPX140411$Close)
prices<-rowMeans(x)

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
