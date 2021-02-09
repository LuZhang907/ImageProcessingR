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
