#install.packages("WaveletComp")
library(WaveletComp)

# Cite with WaveletComp1.1: A guided tour through the R package

###############################################################
######### Analysis of a univariate time series ################

# example 1: a series with constant period
x=periodic.series(start.period = 50, length=1000)
x=x+0.2*rnorm(1000) # add some noise

#wavelet transform
my.data<-data.frame(x=x)
my.w<-analyze.wavelet(my.data, "x",
                      loess.span = 0,
                      dt=1,dj=1/250,
                      lowerPeriod = 16,
                      upperPeriod = 128,
                      make.pval = TRUE,n.sim = 10)

#plot the wavelet power spectrum
wt.image(my.w, color.key = "quantile",n.levels=250,
         legend.params = list(lab="wavelet power levels",mar=4.7))

