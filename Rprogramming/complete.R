complete <- function(directory, id = 1:332) {
   ## 'directory' is a character vector of length 1 indicating
   ## the location of the CSV files
   
   ## 'id' is an integer vector indicating the monitor ID numbers
   ## to be used
   
   ## Return a data frame of the form:
   ## id nobs
   ## 1  117
   ## 2  1041
   ## ...
   ## where 'id' is the monitor ID number and 'nobs' is the
   ## number of complete cases

   # function f takes an id list and builds a character vector with file names
   f <- function(fn) { paste(directory,"/",formatC(fn,width=3,format="d",flag="0"),".csv",sep="")}
   
   data<-lapply(lapply(id, f), read.csv)
   
   nobs<-sapply(lapply(data,na.omit),nrow)
   
   data.frame(id,nobs)

}