rankall <- function(outcome, num="best") {
        options(warn=-1)
        
        ## outcome can have three values
        ## - heart attack
        ## - heart failure
        ## - pneumonia   
        
        ## Read outcome data
        csv_input<-read.csv("outcome-of-care-measures.csv", colClasses="character")
        
        ## Check that state and outcome are valid
        
        ## CHECK VALUE OF OUTCOME
        ## create vector with valid values for outcome
        valid_outcome<-c("heart attack", "pneumonia", "heart failure")       
        
        ## IF OUTCOME = FALSE --> stop and throw error
        if (outcome %in% valid_outcome == FALSE ) {
                stop("invalid outcome")
                
        }        
        
        ## CHECK VALUE OF NUM
        if (num=="best")
        {
                hospital.number = 1
        }
        else if (num=="worst")
        {
                hospital.number = 1
        }
        else {
                hospital.number = as.numeric(num)
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
        
        y<-x[order(x$state_hospital, x$hospital),]        
        
        ## create the-mother-of-all-dataframes with all relevant data
        
        moadf<-ddply(y, .(state_hospital), transform, 
                     rposha = rank(c(heart_attack, hospital), ties.method = 'first'),
                     rnegha = rank(c(-heart_attack, hospital),ties.method = 'first'),
                     rposhf = rank(c(heart_failure, hospital),ties.method = 'first'),
                     rneghf = rank(c(-heart_failure,hospital),ties.method = 'first'),
                     rpospn = rank(c(pneumonia, hospital),ties.method = 'first'),
                     rnegpn = rank(c(-pneumonia, hospital),ties.method = 'first'))
        
        
        
        
        rank_output <- subset(moadf, moadf$rposha==num,select =c(1:2))
        
        ## create subset from moadf
        if (outcome=="heart attack") {
                ## next rank the output on num="worst"
                if (num=='worst')
                {
                        rank_output <- subset(moadf, moadf$rnegha==hospital.number,select =c(1:2))
                        
                }
                else
                {
                        rank_output <- subset(moadf, moadf$rposha==hospital.number,select =c(1:2))
                }
        }
        else if (outcome=="pneumonia") {
                ## next rank the output on num="worst"
                if (num=='worst')
                {
                        rank_output <- subset(moadf, moadf$rnegpn==hospital.number,select =c(1:2))
                        
                }
                else
                {
                        rank_output <- subset(moadf, moadf$rpospn==hospital.number,select =c(1:2))
                }
        }       
        else if (outcome=="heart failure") {
                ## next rank the output on num="worst"
                if (num=='worst')
                {
                        rank_output <- subset(moadf, moadf$rneghf==hospital.number,select =c(1:2))
                        
                }
                else
                {
                        rank_output <- subset(moadf, moadf$rposhf==hospital.number,select =c(1:2))
                }
        }
        else {stop()        }
        
        ## create a dataframe with all valid states + ranks
        valid_state<-data.frame(state_hospital =unique(csv_input$State),state_rate=rank(unique(csv_input$State)) )
        
       ## Merge the dataframes 
       z<-merge(x=valid_state, y=rank_output, all.x=TRUE)
       
       ## create a dataframe for printing
       print_frame<-data.frame(hospital = z$hospital, state=z$state_hospital)
       print(print_frame)
}