
complete <- function(directory, id = 1:332) {
        ## Create nobs vector
        nobs <- NULL
        ## List out all file in specdata folder
        csv <- list.files(paste('./', directory, '/', sep = ''))
        ## Create path to csv files
        link <- paste(getwd(), '/', directory, '/', csv, sep = '')
        ## Add data to nobs
        for (i in id){
                ## Read file
                dat <- read.csv(link[i])
                ## Calculate complete cases
                cal <- sum(complete.cases(dat))
                ## Add result to nobs
                nobs <- c(nobs, cal)
        }
        ## Print the result data frame to console
        result <- data.frame(id, nobs, stringsAsFactors = FALSE)
        result
}
