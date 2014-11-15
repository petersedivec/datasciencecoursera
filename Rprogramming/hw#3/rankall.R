rankall <- function(outcome, num = "best") {
    ## outcome - outcome from source data
    ## num - "best", "worst" or a numeric value indicating the rank hospital to 
    ## return where ties are broken by alphabetical order, NA if # > #hospitals

    ## read outcome data
    data<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## check validity of arguments (state, outcome, num)
    outcomes = list(heart_attack=11, heart_failure=17, pneumonia=23)
    if (is.null(outcomes[[sub(" ","_",outcome)]])) stop("invalid outcome")
    
    if (!(is.numeric(num) | num=="best" | num=="worst")) stop("invalid num")
    
    ## reduce the data frame to state specific outcome data for selected outcome
    d<-subset(data, select=c(2, 7, outcomes[[sub(" ","_",outcome)]]))
    d[,3]<-suppressWarnings(as.numeric(d[,3]))
    d<-d[complete.cases(d),]
    d<-d[order(d[,2], d[,3], d[,1]),] 
    
    ## create a function to use in tapply to return the num of hospital
    numhospital <- function(x, n) {
        if (n=="best") {
            x[1]
        } else if (n=="worst") {
            x[length(x)]    
        } else {
            x[n]    
        }
    }
    
    ## use tapply to select the num from the hospital
    hosrank<-tapply(d$Hospital.Name, d$State, numhospital, num)
    hospital<-hosrank
    state<-names(hosrank)
    
    ## return a data frame with the hospital name and state
    data.frame(hospital, state)
    
}