##################################################################
##################       2009-04-08        #######################

#Extract the data on 2009-04-08
SPX090408 <- subset(SPX,DateTime >= "2009-04-08 09:30:00 " & DateTime <= "2009-04-08 16:00:00")

# calculate prices and returns

# prices=avergae(O,H,L,C)
x<-cbind(SPX090408$Open,SPX090408$High,SPX090408$Low, SPX090408$Close)
prices<-rowMeans(x)
DWTprice<-wavShrink(prices, wavelet="d4",
                    n.level=1, #level=5 is too smoothy #change n.levels will get huge difference on spectrum plot
                    shrink.fun="soft", thresh.fun="adaptive")

# original signal v.s. denoised signal
tsplot(prices, ylab="price", lwd=2, col=rgb(0.2,0.4,0.8))
lines(DWTprice, lwd=2, col=rgb( 0.8,0.5,0.3) )
legend('topright', col=c(rgb(0.2,0.4,0.8),  rgb( 0.8,0.5,0.3)), lwd=2, 
       legend=c("orginal signal", "denoised signal"), bg='white') 

my.data<-data.frame(x=DWTprice)
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

# prices=avergae(O,H,L,C)
x<-cbind(SPX110406$Open,SPX110406$High,SPX110406$Low, SPX110406$Close)
prices<-rowMeans(x)
DWTprice<-wavShrink(prices, wavelet="d4",
                    n.level=1, #level=5 is too smoothy #change n.levels will get huge difference on spectrum plot
                    shrink.fun="soft", thresh.fun="adaptive")

# original signal v.s. denoised signal
tsplot(prices, ylab="price", lwd=2, col=rgb(0.2,0.4,0.8))
lines(DWTprice, lwd=2, col=rgb( 0.8,0.5,0.3) )
legend('topright', col=c(rgb(0.2,0.4,0.8),  rgb( 0.8,0.5,0.3)), lwd=2, 
       legend=c("orginal signal", "denoised signal"), bg='white') 

my.data<-data.frame(x=DWTprice)
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

# prices=avergae(O,H,L,C)
x<-cbind(SPX130410$Open,SPX130410$High,SPX130410$Low, SPX130410$Close)
prices<-rowMeans(x)
DWTprice<-wavShrink(prices, wavelet="d4",
                    n.level=1, #level=5 is too smoothy #change n.levels will get huge difference on spectrum plot
                    shrink.fun="soft", thresh.fun="adaptive")

# original signal v.s. denoised signal
tsplot(prices, ylab="price", lwd=2, col=rgb(0.2,0.4,0.8))
lines(DWTprice, lwd=2, col=rgb( 0.8,0.5,0.3) )
legend('topright', col=c(rgb(0.2,0.4,0.8),  rgb( 0.8,0.5,0.3)), lwd=2, 
       legend=c("orginal signal", "denoised signal"), bg='white') 

my.data<-data.frame(x=DWTprice)
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

# prices=avergae(O,H,L,C)
x<-cbind(SPX150408$Open,SPX150408$High,SPX150408$Low, SPX150408$Close)
prices<-rowMeans(x)
DWTprice<-wavShrink(prices, wavelet="d4",
                    n.level=1, #level=5 is too smoothy #change n.levels will get huge difference on spectrum plot
                    shrink.fun="soft", thresh.fun="adaptive")

# original signal v.s. denoised signal
tsplot(prices, ylab="price", lwd=2, col=rgb(0.2,0.4,0.8))
lines(DWTprice, lwd=2, col=rgb( 0.8,0.5,0.3) )
legend('topright', col=c(rgb(0.2,0.4,0.8),  rgb( 0.8,0.5,0.3)), lwd=2, 
       legend=c("orginal signal", "denoised signal"), bg='white') 

my.data<-data.frame(x=DWTprice)
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

# prices=avergae(O,H,L,C)
x<-cbind(SPX170412$Open,SPX170412$High,SPX170412$Low, SPX170412$Close)
prices<-rowMeans(x)
DWTprice<-wavShrink(prices, wavelet="d4",
                    n.level=1, #level=5 is too smoothy #change n.levels will get huge difference on spectrum plot
                    shrink.fun="soft", thresh.fun="adaptive")

# original signal v.s. denoised signal
tsplot(prices, ylab="price", lwd=2, col=rgb(0.2,0.4,0.8))
lines(DWTprice, lwd=2, col=rgb( 0.8,0.5,0.3) )
legend('topright', col=c(rgb(0.2,0.4,0.8),  rgb( 0.8,0.5,0.3)), lwd=2, 
       legend=c("orginal signal", "denoised signal"), bg='white') 

my.data<-data.frame(x=DWTprice)
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

# prices=avergae(O,H,L,C)
x<-cbind(SPX190410$Open,SPX190410$High,SPX190410$Low, SPX190410$Close)
prices<-rowMeans(x)
DWTprice<-wavShrink(prices, wavelet="d4",
                    n.level=1, #level=5 is too smoothy #change n.levels will get huge difference on spectrum plot
                    shrink.fun="soft", thresh.fun="adaptive")

# original signal v.s. denoised signal
tsplot(prices, ylab="price", lwd=2, col=rgb(0.2,0.4,0.8))
lines(DWTprice, lwd=2, col=rgb( 0.8,0.5,0.3) )
legend('topright', col=c(rgb(0.2,0.4,0.8),  rgb( 0.8,0.5,0.3)), lwd=2, 
       legend=c("orginal signal", "denoised signal"), bg='white') 

my.data<-data.frame(x=DWTprice)
my.w<-analyze.wavelet(my.data, "x",
                      loess.span = 0,
                      dt=1,dj=1/250,
                      lowerPeriod = 2,
                      upperPeriod = 256,
                      make.pval = FALSE,n.sim = 10)

#plot the wavelet power spectrum-prices
wt.image(my.w, color.key = "interval",n.levels=250,
         legend.params = list(lab="wavelet power levels",mar=4.7))