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
