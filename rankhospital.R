rankhospital <- function(state, outcome, num ) {
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
        
        
        if(class(num)=="character"){
                if(!(num =="best" || num == "worst")) {
                        stop("invalid number")
                }
        }
        ## Return hospital name in that state with lowest 30-day death rate
        
        # Remove row by state and columns state , Row # 2 as defined above . 
        # COLS : 1HospitalName, 2HeartAttack, 3HeartFailure, 4Pneumonia
        data = data[data$State==state,]
        data = data[,c(1,3,4,5)]
        
        # Remove columns by outcome, only left 1HospitalName and 2HeartAttack(Deaths by outcome)
        if(outcome == "heart attack") {
                data = data[,c(1,2)]
        }
        else if(outcome == "heart failure") {
                data = data[,c(1,3)]
        } 
        else if(outcome == "pneumonia") {
                data = data[,c(1,4)] 
        }
        #Setting name of outcome object (names)
        #Redefining (No. of Death) Col to numeric to calculate
        #suppressWarnings to Disregard warning about NA's being coerced
        names(data)[2] = "Deaths"
        data[, 2] = suppressWarnings(as.numeric(data[, 2]))
        
        # Remove rows with NA
        data = data[!is.na(data$Deaths),]
        
        #If the number given by num is larger than the number of hospitals in that
        #state, then the function should return NA
        
        if(class(num) =="numeric" && num > nrow(data)){
                return (NA)
        }
        
        
        # Order by Deaths and then HospitalName
        data = data[order(data$Deaths, data$Hospital.Name),]
        
        # Return
        if(class(num) == "character"){
                if(num == "best"){
                        return (data$Hospital.Name[1])
                }
                else if (num == "worst"){
                        return(data$Hospital.Name[nrow(data)])
                }
        }
                else {
                        return(data$Hospital.Name[num])
                }
        
        
}


#Test data:sample output from the function.

# rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"
# rankhospital("MD", "heart attack", "worst")

#[1] "HARFORD MEMORIAL HOSPITAL"
#> rankhospital("MN", "heart attack", 5000)
# [1] NA