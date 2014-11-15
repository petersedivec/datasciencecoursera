rankhospital <- function(state, outcome, num = "best") {
    ## state - 2-character abbreviated name of sate
    ## outcome - outcome from source data
    ## num - "best", "worst" or a numeric value indicating the rank hospital to 
    ## return where ties are broken by alphabetical order, NA if # > #hospitals

    ## read outcome data
    data<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## check validity of arguments (state, outcome, num)
    outcomes = list(heart_attack=11, heart_failure=17, pneumonia=23)
    if (is.null(outcomes[[sub(" ","_",outcome)]])) stop("invalid outcome")
    
    if (!(state %in% state.abb)) stop("invalid state")
    
    if (!(is.numeric(num) | num=="best" | num=="worst")) stop("invalid num")
    
    ## reduce the data frame to state specific outcome data for selected outcome
    d<-subset(data, State==state, select=c(2, outcomes[[sub(" ","_",outcome)]]))
    d[,2]<-suppressWarnings(as.numeric(d[,2]))
    d<-d[order(d[,2], d[,1]),]  
    d<-d[complete.cases(d),]
        
    if (num=="best") {
        d[1,1] # return the first element
    } else if (num=="worst") {
        d[nrow(d),1] # return the last element
    } else {
        d[num, 1] # return the numth element, otherwise NA
    }
    
}