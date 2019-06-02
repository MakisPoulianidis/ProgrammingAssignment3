test_input <- function(outcome) {
        
        
        valid_outcome<-c("heart attack", "pneumonia", "heart failure")       
        if (outcome %in% valid_outcome == FALSE)                
        {
                stop("invalid outcome")
        }         
}