#install.packages("WaveletComp")
library(WaveletComp)

# Cite with WaveletComp1.1: A guided tour through the R package @Angi

###############################################################
######### Analysis of a univariate time series ################

# example 1: a series with constant period
x=periodic.series(start.period = 50, length=1000)
x=x+0.2*rnorm(1000) # add some noise

#wavelet transform
my.data<-data.frame(x=x)
my.w<-analyze.wavelet(my.data, "x",
                      loess.span = 0, # no need to detrend this series
                      dt=1, # defines the time unit, one observation of x is made per time unit
                      dj=1/250, # 250 suboctaves
                      lowerPeriod = 16, #2^4 define the range of periods, expressed in time units, used in the wavelet transform
                      upperPeriod = 128, #2^7 
                      make.pval = TRUE,n.sim = 10) # The region of significant periods in x for each t is found using 10 simulations

#plot the wavelet power spectrum
wt.image(my.w, color.key = "quantile",
         n.levels=250,
         legend.params = list(lab="wavelet power levels",mar=4.7))

#reconstruct the signal
my.rec<-reconstruct(my.w)

# example 2: a series with variable priod
x=periodic.series(start.period = 20,end.period = 100,length=1000)
x=x+0.2*rnorm(1000)
my.data<-data.frame(x=x)
my.w<-analyze.wavelet(my.data, "x",
                      loess.span = 0, 
                      dt=1,
                      dj=1/250,
                      lowerPeriod = 16,
                      upperPeriod = 128,
                      make.pval = TRUE,n.sim = 10)
wt.image(my.w, color.key = "quantile",n.levels=250,
         legend.params = list(lab="wavelet power levels"))

my.rec<-reconstruct(my.w)
x.rec<-my.rec$series$x.r # x: name of original series

#plot(x.rec,type = "l")

# example 3: a series with two periods

x1<-periodic.series(start.period = 80, length=1000)
x2<-periodic.series(start.period = 30, length=1000)
x<-x1+x2+0.2*rnorm(1000)

my.data<-data.frame(x=x)
my.w<-analyze.wavelet(my.data, "x",
                      loess.span = 0, 
                      dt=1,
                      dj=1/250,
                      lowerPeriod = 16,
                      upperPeriod = 128,
                      make.pval = TRUE,n.sim = 10)
wt.image(my.w, color.key = "quantile",n.levels=250,
         legend.params = list(lab="wavelet power levels"))

# reconstruct only period 80
my.rec<-reconstruct(my.w, lwd=c(1,2),
                    legend.coords = "bottomleft") 

my.rec<-reconstruct(my.w, sel.period = 80, lwd=c(1,2),
                    legend.coords = "bottomleft")

# my.rec<-reconstruct(my.w, sel.period = 80, lwd=c(1,2), plot.waves = TRUE,
#                    legend.coords = "bottomleft")

my.w$Period[(my.w$Period>79)&(my.w$Period<81)]

# average power

x1<-periodic.series(start.period = 100,length=500)
x2<-1.2*periodic.series(start.period = 60,length=500)
x<-c(x1,x2)+0.3*rnorm(1000)

y1<-periodic.series(start.period = 100,length=1000)
y2<-1.2*periodic.series(start.period = 60,length=1000)
y<-(y1+y2)/2+0.3*rnorm(1000)

my.data<-data.frame(x=x, y=y)

my.wx<-analyze.wavelet(my.data, "x",
                      loess.span = 0, 
                      dt=1,
                      dj=1/20,
                      lowerPeriod = 16,
                      upperPeriod = 256,
                      make.pval = TRUE,n.sim = 10)

my.wy<-analyze.wavelet(my.data, "y",
                       loess.span = 0, 
                       dt=1,
                       dj=1/20,
                       lowerPeriod = 16,
                       upperPeriod = 256,
                       make.pval = TRUE,n.sim = 10)

#plot average power
maximum.level=1.001*max(my.wx$Power.avg,my.wy$Power.avg)

par(mfrow=c(2,1))
wt.avg(my.wx,maximum.level = maximum.level)
wt.avg(my.wy, maximum.level = maximum.level)

dev.off()

# Slecting the method of analysis

#construct series sarted with period 100, 
#then with the exception of the four inner cycles (period 50)

x1<-periodic.series(start.period = 100,length=400)
x2<-1.2*periodic.series(start.period = 50,length=200)
x<-c(x1,x2,x1)+0.2*rnorm(1000)

my.data<-data.frame(x=x)
my.w<-analyze.wavelet(my.data, "x",
                      method="white.noise",
                      loess.span=0,
                      dt=1,
                      dj=1/250,
                      lowerPeriod = 32,
                      upperPeriod = 256,
                      make.pval = TRUE, n.sim=10)

#default sig.level=0.1
wt.image(my.w, color.key = "interval",n.level=50,
         legend.params = list(lab="wavelet power levels"))

#set sig.level=0.05
wt.image(my.w, color.key = "interval",n.level=50,siglvl = 0.05,
         legend.params = list(lab="wavelet power levels"))

# method=Fourier.rand
my.w<-analyze.wavelet(my.data, "x",
                      method="Fourier.rand",
                      loess.span=0,
                      dt=1,
                      dj=1/250,
                      lowerPeriod = 32,
                      upperPeriod = 256,
                      make.pval = TRUE, n.sim=10)
wt.image(my.w, color.key = "interval",n.level=50,
         legend.params = list(lab="wavelet power levels"))

#plotting the power spectrum
my.w<-analyze.wavelet(my.data, "x",
                      method="white.noise",
                      loess.span=0,
                      dt=1,
                      dj=1/250,
                      lowerPeriod = 32,
                      upperPeriod = 256,
                      make.pval = TRUE, n.sim=10)

wt.image(my.w, color.key = "quantile",n.level=250,
         legend.params = list(lab="wavelet power levels",label.digits=2))

#black-and-white printout
wt.image(my.w, color.key = "quantile",n.level=250,
         legend.params = list(lab="wavelet power levels",label.digits=2),
         color.palette = "gray((n.levels):1/n.levels)",
         col.ridge = "blue")


#Time axis styles

#Simulate an hourly time series, with a 24-hour period, over the first 30 days of 2018
epoch.seq<-seq(from=as.POSIXct("2018-01-01 00:00:00"),
               to=as.POSIXct("2018-01-30 23:00:00"), by=3600) #why 3600? 1 hour has 60 mins, 1 min equal to 60 sec!
x<-periodic.series(start.period = 24, length=720)
x<-x+rnorm(720)

my.data<-data.frame(data=epoch.seq,x=x)
#plot(my.data, type="l")

my.w<-analyze.wavelet(my.data,"x", loess.span = 0, dt=1/24, dj=1/500,
                      lowerPeriod = 1/4, upperPeriod = 2, make.pval = FALSE)

#default axis

wt.image(my.w, periodlab = "periods (days)",
         legend.params = list(lab="wavelet power levels"),
         label.time.axis = TRUE, #default setting
         )


# time elapsed, measured in days

wt.image(my.w, periodlab = "periods (days)",
         legend.params = list(lab="wavelet power levels"),
         label.time.axis = TRUE, #default setting
         spec.time.axis = list(at=seq(1,720,by=48),
                               labels=seq(0,28,by=2))
         )

# time elapsed, measured in hours

wt.image(my.w, periodlab = "periods (days)",
         legend.params = list(lab="wavelet power levels"),
         label.time.axis = TRUE, #default setting
         spec.time.axis = list(at=seq(1,720,by=50),
                               labels=seq(0,719,by=50))
)

# automatic calendar, TEMPORARY IGNOR

wt.image(my.w, periodlab = "periods (days)",
         legend.params = list(lab="wavelet power levels"),
         label.time.axis = TRUE, #default setting
         show.date = TRUE, date.format = "%F %T" # not working, need to check  the time zone
)

#user-defined calendar, TEMPORARY IGNOR
ticks<-seq(as.POSIXct("2018-01-01 00:00:00", format="%F %T"),
           as.POSIXct("2018-01-31 23:00:00", format="%F %T"), by="week")
labels<-seq(as.Date("2018-01-01"), as.Date("2018-01-29"),by="week")
labels<-paste("Mon, ", labels)

wt.image(my.w, periodlab = "periods (days)",
         legend.params = list(lab="wavelet power levels"),
         label.time.axis = TRUE, #default setting
         show.date = TRUE, date.format = "%F %T", # not working, need to check  the time zone
         spec.time.axis=list(at=ticks, labels=labels,las=2)
)



















