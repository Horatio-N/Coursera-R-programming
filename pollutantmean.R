## Set working directory
setwd('/home/R/demo')
pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## Create container vector
        con.vec <- NULL
        ## List out all files in specdata folder
        files <- list.files(paste('./', directory, '/', sep = ''))
        ## Create path to files
        paths <- paste(getwd(), '/', directory, '/', files, sep = '')
        ## Add data to container vector
        for (i in id){
                ## Read file
                d <- read.csv(paths[i])
                ## Remove NA elements
                clean <- d[pollutant][!is.na(d[pollutant])]
                ## Add clean data to container vector
                con.vec <- c(con.vec, clean)
        }
        ## Calculate mean and print to console
        print(mean(con.vec))
}
