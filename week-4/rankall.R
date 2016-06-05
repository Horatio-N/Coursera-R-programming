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
## Ranking function
ranking <- function(data, state_uniq, num_call){
        clean <- data[complete.cases(data[, 3]),]
        ds <- subset(clean, clean[, 2] == state_uniq)
        l <- list(ds[, 3], ds[, 1])
        ordered <- ds[do.call(order, l), ]
        if (num_call == 'best') result <- ordered[, 1][1] else
                if (num_call == 'worst') result <- ordered[, 1][nrow(ordered)] else
                        result <- ordered[, 1][num_call]
        result
}

rankall <- function(outcome, num = 'best'){
        ## Read outcome data
        d <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
        d.order <- d[order(d$State), ]
        outcome.valid <- c('heart attack', 'heart failure', 'pneumonia')
        d_filter <- data.frame(d.order$Hospital.Name, 
                               d.order$State, 
                               na(d.order[, col_num(outcome)]), 
                               stringsAsFactors = FALSE)
        ## Check that state and outcome are valid
        if (sum(grepl(outcome, outcome.valid)) == 0) stop ('invalid outcome')
        ## For each state, find the hospital of the given rank
        state <- unique(d_filter[, 2])
        hospital <- NULL
        for (i in 1:length(state)){
                hos.c <- ranking(d_filter, state[i], num)
                hospital <- c(hospital, hos.c)
        }
        ## Return a data frame with the hospital names and the
        result <- data.frame(hospital=hospital, state=state)
        result
}