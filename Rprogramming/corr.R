corr <- function(directory, threshold = 0) {
   ## 'directory' is a character vector of length 1 indicating
   ## the location of the CSV files
   
   ## 'threshold' is a numeric vector of length 1 indicating the
   ## number of completely observed observations (on all
   ## variables) required to compute the correlation between
   ## nitrate and sulfate; the default is 0
   
   ## Return a numeric vector of correlations

   f <- function(fn) { paste(directory,"/",formatC(fn,width=3,format="d",flag="0"),".csv",sep="")}
   
   # apply cor function to correlate sulfate to nitrate
   m <- function(df) { cor(df$sulfate, df$nitrate)}
   
   data<-lapply(lapply(1:length(list.files(directory)), f), read.csv)
   
   # remove all NAs from data sets
   data<-lapply(data,na.omit)
   
   # calculate # of observations for each site
   nobs<-sapply(data,nrow)
   
   # determine sites that meet min threshold observations
   mt<-nobs>threshold
   
   #finally run the correlation between sulfate & nitrate for sites that meet the min   
   correlations<-sapply(lapply(data[mt],na.omit), m)
   if (length(correlations)== 0) { numeric(0) } 
   else { correlations }
   }