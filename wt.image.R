
## The following example is adopted from Liu et al., 2007:

series.length <- 6*128*24
x1 <- periodic.series(start.period = 1*24, length = series.length)
x2 <- periodic.series(start.period = 8*24, length = series.length)
x3 <- periodic.series(start.period = 32*24, length = series.length)
x4 <- periodic.series(start.period = 128*24, length = series.length)

x <- x1 + x2 + x3 + x4

plot(x, type = "l", xlab = "index", ylab = "", xaxs = "i",
     main = "hourly series with periods of 1, 8, 32, 128 days")

## The following dates refer to the local time zone 
## (possibly allowing for daylight saving time):      
my.date <- seq(as.POSIXct("2014-10-14 00:00:00", format = "%F %T"), 
               by = "hour", 
               length.out = series.length)     
my.data <- data.frame(date = my.date, x = x)

## Computation of wavelet power:
## a natural choice of 'dt' in the case of hourly data is 'dt = 1/24', 
## resulting in one time unit equaling one day. 
## This is also the time unit in which periods are measured.
my.wt <- analyze.wavelet(my.data, "x", 
                         loess.span = 0, 
                         dt = 1/24, dj = 1/20, 
                         lowerPeriod = 1/4, 
                         make.pval = TRUE, n.sim = 10)

## Plot of wavelet power spectrum 
## with color breakpoints referring to quantiles:  
wt.image(my.wt, main = "wavelet power spectrum",
         legend.params = list(lab = "wavelet power levels (quantiles)", 
                              lab.line = 3.5, 
                              label.digits = 2),
         periodlab = "period (days)")
## Note:
## The default time axis shows an index of given points in time, 
## which is the count of hours in our example.

## The same plot, but with equidistant color breakpoints: 
wt.image(my.wt, color.key = "i", main = "wavelet power spectrum",
         legend.params = list(lab = "wavelet power levels (equidistant)"),
         periodlab = "period (days)")

## Alternative styles of the time axis:          

## The plot with time elapsed in days, starting from 0 and proceeding 
## in steps of 50 days (50*24 hours),
## instead of the (default) time index:
index.ticks  <- seq(1, series.length, by = 50*24)
index.labels <- (index.ticks-1)/24
## Insert your specification of the time axis: 
wt.image(my.wt, color.key = "i", main = "wavelet power spectrum",
         legend.params = list(lab = "wavelet power levels (equidistant)"),
         periodlab = "period (days)", timelab = "time elapsed (days)",
         spec.time.axis = list(at = index.ticks, labels = index.labels))

## The plot with (automatically produced) calendar axis:
wt.image(my.wt, color.key = "i", main = "wavelet power spectrum",
         legend.params = list(lab = "wavelet power levels (equidistant)"), 
         periodlab = "period (days)",
         show.date = TRUE, date.format = "%F %T")

## Individualizing your calendar axis (works with 'show.date = TRUE')...
## How to obtain, for example, monthly date ticks and labels:

## The sequence of tick positions:
monthly.ticks <- seq(as.POSIXct("2014-11-01 00:00:00", format = "%F %T"), 
                     as.POSIXct("2016-11-01 00:00:00", format = "%F %T"), 
                     by = "month")
## Observe that the following specification may produce an error:
## 'seq(as.Date("2014-11-01"), as.Date("2016-11-01"), by = "month")'
## Time of the day is missing here!

## The sequence of labels (e.g. information on month and year only):
monthly.labels <- strftime(monthly.ticks, format = "%b %Y")

## Insert your specification of the time axis: 
wt.image(my.wt, color.key = "i", main = "wavelet power spectrum",
         legend.params = list(lab = "wavelet power levels (equidistant)"), 
         periodlab = "period (days)", 
         show.date = TRUE, date.format = "%F %T", 
         spec.time.axis = list(at = monthly.ticks, labels = monthly.labels, 
                               las = 2))
## Note: 
## The monthly ticks specify the midpoints of the colored cells and match 
## the location of corresponding (default) time index ticks.

## Furthermore, the plot with an individualized period axis:
wt.image(my.wt, color.key = "i", main = "wavelet power spectrum",
         legend.params = list(lab = "wavelet power levels (equidistant)"), 
         periodlab = "period (days)", 
         show.date = TRUE, date.format = "%F %T",
         spec.time.axis = list(at = monthly.ticks, labels = monthly.labels, 
                               las = 2),
         spec.period.axis = list(at = c(1,8,32,128))) 

## Switching the time axis from index to time elapsed in hours 
## (starting from 0, and proceeding in steps of 500 hours), 
## and the period axis from days to hours:
index.ticks  <- seq(1, series.length, by = 500)
index.labels <- index.ticks - 1
wt.image(my.wt, color.key = "i", main = "wavelet power spectrum",
         legend.params = list(lab = "wavelet power levels (equidistant)"), 
         timelab = "time elapsed (hours)", periodlab = "period (hours)",
         spec.time.axis = list(at = index.ticks, labels = index.labels),
         spec.period.axis = list(at = c(1,8,32,128), labels = c(1,8,32,128)*24))            

## A plot with different colors:
wt.image(my.wt, main = "wavelet power spectrum",
         legend.params = list(lab = "wavelet power levels (quantiles)", 
                              lab.line = 3.5, label.digits = 2), 
         color.palette = "gray((1:n.levels)/n.levels)", col.ridge = "yellow",
         periodlab = "period (days)")

## In the case of monthly (or quarterly) data, the time axis should be 
## labeled at equally spaced time points. An example:

monthyear <- seq(as.Date("2014-01-01"), as.Date("2018-01-01"),
                 by = "month")
monthyear <- strftime(monthyear, format = "%b %Y")

xx <- periodic.series(start.period = 6, length = length(monthyear))
xx <- xx + 0.2*rnorm(length(monthyear))

plot(xx, type = "l", xlab = "index", ylab = "", xaxs = "i",
     main = "monthly series with period of 6 months")

monthly.data <- data.frame(date = monthyear, xx = xx)

my.wt <- analyze.wavelet(monthly.data, "xx", loess.span = 0, 
                         dt = 1, dj = 1/250, 
                         make.pval = TRUE, n.sim = 250)
## Note: 
## The natural choice of 'dt' in this example is 'dt = 1', 
## resulting in periods measured in months. 
## (Setting 'dt = 1/12' would result in periods measured in years.)

## The default wavelet power plot then shows the monthly:
wt.image(my.wt, main = "wavelet power spectrum", 
         periodlab = "period (months)")

## The following plot shows the elapsed time, measured in months: 
wt.image(my.wt, main = "wavelet power spectrum", 
         periodlab = "period (months)", timelab = "time elapsed (months)",
         spec.time.axis = list(at = 1:length(monthyear), 
                               labels = (1:length(monthyear))-1))

## In case you prefer the monthyear labels themselves: 
wt.image(my.wt,  main = "wavelet power spectrum", 
         periodlab = "period (months)", timelab = "month and year",
         spec.time.axis = list(at = 1:length(monthyear), labels = monthyear)) 

## You may sometimes wish to enhance your plot with additional information. 
## There is an option to add further objects to the image plot region, 
## by setting 'graphics.reset = FALSE' 
## (but recall previous par settings after plotting):

op <- par(no.readonly = TRUE)
wt.image(my.wt, main = "wavelet power spectrum", 
         periodlab = "period (months)", 
         spec.period.axis = list(at = c(2,4,6,8,12)), 
         spec.time.axis = list(at = 1:length(monthyear),
                               labels = substr(monthyear,1,3)),
         graphics.reset = FALSE)
abline(h = log2(6), lty = 3)         
abline(v = seq(1, length(monthyear), by = 12), lty = 3)
mtext(2014:2018, side = 1, 
      at = seq(1, length(monthyear), by = 12), line = 2)
par(op)

## For further axis plotting options:
## Please see the examples in our guide booklet,
## URL http://www.hs-stat.com/projects/WaveletComp/WaveletComp_guided_tour.pdf.


## End(Not run)