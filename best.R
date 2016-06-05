## Extract the csv files into your working directory before running this function
## Column numer return function
col_num <- function (k){
        i <- NULL
        if (k == 'heart attack') i <- 11 else
                if(k == 'heart failure') i <- 17 else 
                        i <- 23
                i
}

## NA fletching function
na <- function(x){
        vec <- NULL
        for(i in 1:length(x)){
                if(x[i] == 'Not Available')
                        x[i] <- NA
                else
                        x[i] <- x[i]
                vec <- c(vec, x[i])
        }
        res <- as.numeric(vec)
        res
}

best <- function(state, outcome){
        ## Read outcome data
        outcome.data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
        ## Check that State and outcome are valid
        state.list <- unique(outcome.data$State)
        outcome.list <- c('heart attack', 'heart failure', 'pneumonia')
        if (sum(grepl(state, state.list)) == 0)
                stop('invalid state')
        if (sum(grepl(outcome, outcome.list)) == 0)
                stop('invalid outcome')
        ## Return hospital name in that state with lowest 30-day death
        state.outcome <- subset(outcome.data, outcome.data$State == state)
        outcome.num <- na(state.outcome[, col_num(outcome)])
        .min <- min(outcome.num, na.rm = TRUE)
        hospital <- sort(subset(state.outcome$Hospital.Name, 
                                outcome.num == .min))
        return(hospital[1])
}