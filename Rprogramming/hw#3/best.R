best <- function(state, outcome) {
    ## read outcome data
    data<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## check that the state and outcome are valid
    outcomes = list(heart_attack=13, heart_failure=19, pneumonia=25)
    if (is.null(outcomes[[sub(" ","_",outcome)]])) stop("invalid outcome")
    
    if (!(state %in% state.abb)) stop("invalid state")
    
    ## reduce the data frame to state specific outcome data
    d<-subset(data, State==state, select=c(2, outcomes[[sub(" ","_",outcome)]]))
    d[,2]<-suppressWarnings(as.numeric(d[,2]))
    
    ## find and select the min values
    md<-min(d[,2],na.rm=T)
    hos<-subset(d,d[,2]==md, select=c(1))

    ## Return hospital name in that state with the lowest 30-day death rate
    shos<-sort(hos[[1]])
    shos[1]
}