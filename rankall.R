rankall <- function(outcome, num = "best") {
       
        ## Read outcome data: COLS: 1HospitalName, 2State, 3HeartAttack, 4HeartFailure, 5Pneumonia
        data <- read.csv("C:/Data science/2 R programming/week 4/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")[,c(2,7,11,17,23)]
        
        ## Check that state and outcome are valid
        if(! (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") ) {
                stop("invalid outcome")
        }
        
        
        if(class(num)=="character"){
                if(!(num =="best" || num == "worst")) {
                        stop("invalid number")
                }
        }
        ## For each state, find the hospital of the given rank
        # Remove columns by outcome, only left 1HospitalName ,2State and 2HeartAttack(Deaths by outcome)
        if(outcome == "heart attack") {
                data = data[,c(1,2,3)]
        }
        else if(outcome == "heart failure") {
                data = data[,c(1,2,4)]
        } 
        else if(outcome == "pneumonia") {
                data = data[,c(1,2,5)] 
        }
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        #Setting name of outcome object (names)
        #Redefining (No. of Death) Col to numeric to calculate
        #suppressWarnings to Disregard warning about NA's being coerced
        names(data)[3] = "Deaths"
        data[, 3] = suppressWarnings(as.numeric(data[, 3]))
        
        
        # Remove rows with NA
        data = data[!is.na(data$Deaths),]
        
        splited = split(data, data$State)
        ans = lapply(splited, function(x, num) {
        # Order by Deaths and then HospitalName
        x = x[order(x$Deaths, x$Hospital.Name),]
        
        # Return
        if(class(num) == "character"){
                if(num == "best"){
                        return (x$Hospital.Name[1])
                }
                else if (num == "worst"){
                        return(x$Hospital.Name[nrow(x)])
                }
        }
        else {
                return(x$Hospital.Name[num])
        }
        
        
},num)

        #Return data.frame with format
        return ( data.frame(hospital=unlist(ans), state=names(ans)) )
        
}

#Test data:sample output from the function.
#head(rankall("heart attack", 20), 10)
#tail(rankall("pneumonia", "worst"), 3)
#tail(rankall("heart failure"), 10)
