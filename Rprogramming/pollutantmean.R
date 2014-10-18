pollutantmean <- function(directory, pollutant, id = 1:332) {
   ## 'directory' is a character vector of length 1 indicating
   ## the location of the CSV files
   
   ## 'pollutant' is a character vector of length 1 indicating
   ## the name of the pollutant for which we will calculate the
   ## mean; either "sulfate" or "nitrate".
   
   ## 'id' is an integer vector indicating the monitor ID numbers
   ## to be used
   
   ## Return the mean of the pollutant across all monitors list
   ## in the 'id' vector (ignoring NA values)
   
   # open directory, lapply in-line function to get mean
   
   # function f takes an id list and builds a character vector with file names
   f <- function(fn) { paste(directory,"/",formatC(fn,width=3,format="d",flag="0"),".csv",sep="")}
   
   # function m extracts the pollutant from a dataset
   m <- function(x,p) { x[[p]] }
   
   # pull out the data based on file ids, and pollutant
   # data is a list of pollutant measurements at site ids
   data<-lapply(lapply(lapply(id, f), read.csv), m, p=pollutant)
   
   # final step is to combine all the values (unlist) and then take mean ignoring NAs
   round(mean(unlist(data), na.rm=T), digits = 3)
   
}