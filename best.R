best <- function(state, outcome) {
        ## Read outcome data: COLS: 1HospitalName, 2State, 3HeartAttack, 4HeartFailure, 5Pneumonia
        data <- read.csv("C:/Data science/2 R programming/week 4/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")[,c(2,7,11,17,23)]
        
        ## Check that state and outcome are valid  
        ##State vector is encoded to factor to be ordered
        ##unique used to get unique values without duplicates
        if(! ( state %in% unique(factor(data$State)) ) ) {
                stop("invalid state")
        }
        
        if(! (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") ) {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death rate
        
        # Remove row by state and columns state , Row # 2 as defined above . 
        # COLS : 1HospitalName, 2HeartAttack, 3HeartFailure, 4Pneumonia
        data = data[data$State==state,]
        data = data[,c(1,3,4,5)]
        
        # Remove columns by outcome, only left 1HospitalName and 2HeartAttack(Deaths by outcome)
        if(outcome == "heart attack") {
                data = data[,c(1,2)]
        } else if(outcome == "heart failure") {
                data = data[,c(1,3)]
        } else if(outcome == "pneumonia") {
                data = data[,c(1,4)] 
        }
        #Setting name of outcome object (names)
        #Redefining (No. of Death) Col to numeric to calculate
        #suppressWarnings to Disregard warning about NA's being coerced
        names(data)[2] = "Deaths"
        data[, 2] = suppressWarnings(as.numeric(data[, 2]))

        # Remove rows with NA
        data = data[!is.na(data$Deaths),]
        
        # Order by Deaths and then HospitalName
        data = data[order(data$Deaths, data$Hospital.Name),]
        
        # Return
        return (data$Hospital.Name[1])
}

#Test data:sample output from the function.

##best("TX", "heart attack")
#"CYPRESS FAIRBANKS MEDICAL CENTER"
##best("TX", "heart failure")
#"FORT DUNCAN MEDICAL CENTER"
##best("MD", "heart attack")
#"JOHNS HOPKINS HOSPITAL, THE"
##best("MD", "pneumonia")
#"GREATER BALTIMORE MEDICAL CENTER"
##best("BB", "heart attack")
#Error in best("BB", "heart attack") : invalid state
##best("NY", "hert attack")
#Error in best("NY", "hert attack") : invalid outcome

        