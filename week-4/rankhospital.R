## Extract the csv files into your working directory before running this function
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
## Column number return function
col_num <- function (k){
        i <- NULL
        if (k == 'heart attack') i <- 11 else
                if(k == 'heart failure') i <- 17 else 
                        i <- 23
                i
}
rankhospital <- function(state, outcome, num = 'best'){
        ## Read outcome data
        outcome.d <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
        ## Check that State and outcome are valid
        state.l <- unique(outcome.d$State)
        outcome.l <- c('heart attack', 'heart failure', 'pneumonia')
        if(sum(grepl(state, state.l)) == 0)
                stop('invalid state')
        if(sum(grepl(outcome, outcome.l)) == 0)
                stop('invalid outcome')
        ## Return hospital name in that state with the given rank
        ## Subset outcome data by state
        state.hos <- subset(outcome.d, outcome.d$State == state)
        ## Remove NA from outcome data
        clean.d <- state.hos[complete.cases(na(state.hos[ ,col_num(outcome)])), ]
        ## Ranking
        dd <- list(as.numeric(clean.d[, col_num(outcome)]), clean.d$Hospital.Name)
        ranking <- clean.d[do.call(order, dd), ]
        ifelse(num == 'best', rank <- ranking$Hospital.Name[1],
               ifelse(num == 'worst', rank <- ranking$Hospital.Name[nrow(ranking)],
                      rank <- ranking$Hospital.Name[num]))
        rank
}