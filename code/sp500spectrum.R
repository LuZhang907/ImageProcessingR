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

library(WaveletComp)
library(remotes)
library(wmtsa)
library(astsa)
library(ggplot2)
library(highfrequency)
library(xts)

Close<-SPX$Close

dwt_Close<-wavShrink(Close, wavelet="d4",
                     n.level=1, 
                     shrink.fun="soft", thresh.fun="adaptive")
prices<-dwt_Close
prices<-xts(prices,SPX$DateTime)
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
mypath="/Users/luzhang/Desktop/sp500spectrum"
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
  if (y_label[i]==0) {
    png(filename = createNewFileName(pattern="Downspectrum"))
    wt.image(my.w, color.key = "interval",n.levels=250,
             legend.params = list(lab="wavelet power levels",mar=4.7))
    dev.off() 
  }else{
    png(filename = createNewFileName(pattern="Upspectrum"))
    wt.image(my.w, color.key = "interval",n.levels=250,
             legend.params = list(lab="wavelet power levels",mar=4.7))
    dev.off() 
  }
}

# Copying spectrum images to training,validation, and test directories
Original_dataset_dir<-"//Users/luzhang/Desktop/sp500spectrum"

base_dir<-"//Users/luzhang/Desktop/sp500_spectrum"
dir.create(base_dir)

up_dir<-file.path(base_dir,"upspectrum")
dir.create(up_dir)

down_dir<-file.path(base_dir,"downspectrum")
dir.create(down_dir)

fnames<-paste0("Upspectrum", 1:1534, ".png") # attempt on balanced first
file.copy(file.path(Original_dataset_dir, fnames),
          file.path(up_dir))
fnames<-paste0("Downspectrum", 1:1534, ".png")
file.copy(file.path(Original_dataset_dir, fnames),
          file.path(down_dir))

#check how many pictures are in each training split(train/validation/test)
cat("total training upspectrum images:", length(list.files(up_dir)),"\n")
cat("total training downspectrum images:", length(list.files(down_dir)),"\n")


