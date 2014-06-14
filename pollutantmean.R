pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    # load the secified files into a dataframe
    
    frame <- loadFrame(directory,id)
    vpol <-frame[pollutant]
    mean(vpol[!is.na(vpol)])
          
}
loadFrame <- function(d,items)
{
    # get a list of the files in the directory
    
    firstItem <- TRUE
    for (idx in items)
    {
        currentFile <- paste(sprintf("%03d", idx),".csv", sep="")
        print(currentFile)
        if(file.exists(file.path(d,currentFile)))
        {
            if(firstItem == TRUE)
            {
                frame <- read.csv(file.path(d,currentFile),header=TRUE)
            }
            else
            {
                frameNext <- read.csv(file.path(d,currentFile),header=TRUE)
                frame <- rbind(frame,frameNext)
            }                
            firstItem<-FALSE
        }
    }
    frame
}
complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    frame <- loadFrame(directory,id)
    colNames <- c("sulfate","nitrate")
    ina <- !is.na(frame[colNames])
    rBoth <- as.numeric(ina[,1] & ina[,2])
    c <- cbind(frame["ID"],rBoth)
    result <- aggregate(c$rBoth,by=list(id=c$ID),FUN=sum)
    colnames(result) <- c("id","nobs")
    result
    
}
corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
}