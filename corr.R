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
complete <- function(directory, frame, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    ## 'id' is an integer vector indicating the monitor ID numbers
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    #frame <- loadFrame(directory,id)
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
    set <- 1:3
    
    for(item in set)
    {
      current <- loadFrame(directory, item)
      currentResult <- subset(complete(directory,current,item),id==item)
      print(currentResult)
      
    }
    
    
    #allItems <- complete(directory, 1:2)
    #gtThreshold <- subset(allItems, nobs >= threshold)
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    filteredId <- gtThreshold[,c("id")]
    ## Return a numeric vector of correlations
    fullDataFrame <- loadFrame(directory,filteredId)
    vCorr <- vector('numeric')
    for(currentId in filteredId)
    {
        cdata <- subset(fullDataFrame,ID==currentId)
        svector<-!is.na(cdata[,c("sulfate")])
        nvector<-!is.na(cdata[,c("nitrate")])
        vCorr<-append(vCorr, cor(cdata[(svector & nvector),c("sulfate")],cdata[(svector & nvector),c("nitrate")]))
        #print(paste(currentId,cor(cdata[(svector & nvector),c("sulfate")],cdata[(svector & nvector),c("nitrate")])))
        
    }
    vCorr
}