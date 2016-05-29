corr <- function(directory, threshold = 0) {
        ## Create container vector
        cor.vec <- NULL
        ## List out all files in specdata folder
        csv.files <- list.files(paste('./', directory, '/', sep = ''))
        ## Create path to csv files
        path <- paste(getwd(), '/', directory, '/', csv.files, sep = '')
        ## Calculate correlation
        for (i in 1:length(csv.files)){
                ## Read file
                d.cor <- read.csv(path[i])
                ## Checking criteria
                if(sum(complete.cases(d.cor)) <= threshold)next
                        ## Remove NA values
                        d.clean <- d.cor[complete.cases(d.cor),]
                        ## Fletching data
                        cor.vec <- c(cor.vec, cor(d.clean$sulfate, d.clean$nitrate))        
        }
        cor.vec
}
