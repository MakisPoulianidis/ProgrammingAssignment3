best <- function(state, outcome) {
        ##options(warn=-1)
        
        ## outcome can have three values
        ## - heart attack
        ## - heart failure
        ## - pneumonia   
        
        ## Read outcome data
        csv_input<-read.csv("outcome-of-care-measures.csv", colClasses="character")
        
        ## Check that state and outcome are valid
        
        ## CHECK VALUE OF STATE
        ## create vector with valid states by getting DISTINCT values from csv
        valid_state<-unique(csv_input$State)       
        
        ## CHECK VALUE OF OUTCOME
        ## create vector with valid values for outcome
        valid_outcome<-c("heart attack", "pneumonia", "heart failure")       
        
        ## IF OUTCOME = FALSE --> stop and throw error
        if (state %in% valid_state == FALSE)                
        {
                stop("invalid state")}
        
        
        else if (outcome %in% valid_outcome == FALSE ) {
                stop("invalid outcome")
                
        }        
        
        ## Only the following columns are needed
        ## $Hospital.Name (col 2)
        ## $State(col 7)
        ## $Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack (col 11)
        ## $Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure (col 17)
        ## $Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia (col 23)
        
        ## For convenience, put everything in one dataframe
        ## AND 
        ## Force the relevant columns into numeric columns
        ## Warnings are ignored
        
        x <- data.frame(
                ## - hospital
                hospital = csv_input$Hospital.Name,
                ## - state_hospital
                state_hospital = csv_input$State,
                ## - heart attack
                heart_attack = as.numeric(csv_input$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),
                ## - heart failure
                heart_failure = as.numeric(csv_input$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),
                ## - pneumonia   
                pneumonia = as.numeric(csv_input$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
        
        ## create a subset of values for one state only
        y <- subset(x, state_hospital==state, select =c(1:5))
        
        ## order the subset on
        if (outcome=="heart attack") {
                sort_output <- y[order(y$heart_attack, y$hospital),]
        }
        else if (outcome=="pneumonia") {
                sort_output <- y[order(y$pneumonia, y$hospital),]        
        }       
        else if (outcome=="heart failure") {
                sort_output <- y[order(y$heart_failure, y$hospital),]                        
        }
        else {stop()        }
        
        z <- sort_output[1,]
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        print(as.character(z$hospital))
        
        
        
        
        
        
        
}