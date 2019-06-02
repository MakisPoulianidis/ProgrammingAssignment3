test_state <- function(state) {
        
        outcome<-read.csv("outcome-of-care-measures.csv", colClasses="character")
        
        valid_state<-unique(outcome$State)       
        
        if (state %in% valid_state == FALSE)                
        {
                stop("invalid state")
        }         
}