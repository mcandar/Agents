# serial and parallel computation
library(microbenchmark)

### CASE 1 ###
so_levels <- na.omit(as.integer(levels(factor(Raw$SenderZip))))[sample.int(600,5)]
so_levels

# serial computation, sapply() from R base
microbenchmark({
  temp <- sapply(so_levels,function(x) sum(na.omit(subset(Raw,SONumber == x)$AverageUnitPrice)))
}) # Average CPU time 565 milliseconds

microbenchmark({
  temp <- sapply(so_levels,function(x) sum(na.omit(Raw$AverageUnitPrice[which(Raw$SONumber==x)])))
}) # Average CPU time 101 milliseconds

# parallel computation
library(parallel)
cl <- makeCluster(detectCores()) # initiate a cluser and use all cores, there are 4 in my pc
lapply(c("Raw","so_levels"), function(x) clusterExport(cl,x)) # export variables to cluster, otherwise it cant see
microbenchmark({
  temp <- parSapply(cl,so_levels,function(x) sum(na.omit(Raw$AverageUnitPrice[which(Raw$SenderZip==x)])))
}) # Average CPU time 55 milliseconds
stopCluster(cl) # turn off the cluster

# cl <- makeCluster(detectCores()) # initiate a cluser and use all cores, there are 4 in my pc
# lapply(c("Raw","so_levels"), function(x) clusterExport(cl,x)) # export variables to cluster, otherwise it cant see
# microbenchmark({
#   n <- 0
#   temp <- parSapply(cl,so_levels,function(x) parSapply(cl,Raw$SenderZip,function(y) if_else(y==x,c(t))))
# }) # Average CPU time  milliseconds
# stopCluster(cl) # turn off the cluster
##############

### CASE 2 ###
require(dplyr)
require(microbenchmark)
require(parallel)

# serial
microbenchmark({
  temp <- arrange(Raw[1:1e3,-2],desc(AverageUnitPrice))
}) # Average CPU time 6.2 milliseconds

microbenchmark({
  temp <- Sort(Raw[1:1e3,-2],7,decreasing = TRUE)
}) # Average CPU time 4.7 milliseconds

# parallel
cl <- makeCluster(detectCores())
clusterExport(cl,"Raw")

microbenchmark({
  temp <- Sort(Raw[1:1e3,-2],7,decreasing = TRUE)
}) # does not make a noticeable difference

microbenchmark({
  temp <- arrange(Raw[1:1e3,-2],desc(AverageUnitPrice))
}) # does not make a noticeable difference
stopCluster(cl)
##############

# to delete
require(parallel)
cl <- makeCluster(detectCores())
temp <- parSapply(cl, 2:4, 
          function(exponent){
            x <- as.numeric(exponent)
            data.frame(base = 4^x, self = x^x)
          })
stopCluster(cl)
temp
class(temp)


library(parallel)

### AN IN-DEPTH INVESTIGATION FOR PARALLELIZATION ###
#############
par.Search.List <- function(source, # object to be searched
                            target, # object expected to include source at least once
                            col,    # target column
                            nthreads = NULL){ # number of cores to use
  
  if(is.null(nthreads)) nthreads = detectCores()
  require(parallel)
  cl <- makeCluster(nthreads) # initiate cluster
  clusterExport(cl,c("source","target","col")) # export variables to the cluster
  
  Result <- parSapply(cl,source,function(x){
    index <- which(target[,col]==x) # search the source in target, get indexes
    if(length(index)!=0)
      sapply(index, function(i) c(x,target[i,])) # THIS IS OKAY JUST NEEDS A CONVERTION TO DATA FRAME
  })
  
  Result <- parLapply(cl, parLapply(cl,Result, function(y)
    as.data.frame(matrix(y,length(y)/(ncol(target)+1),ncol(target)+1,byrow = T)))
    ,data.frame, stringsAsFactors=FALSE)
  stopCluster(cl)
  
  # convert to data frame from list, following is time consuming and is NOT parallel
  Result <- do.call(rbind, Result) ## THIS WORKS
  colnames(Result) <- c("Source",colnames(target))
  
  return(Result)
}


# input "source" as a data.frame
Match.rows <- function(source,col.sou,target,col.tar){
  Result <- data.frame() # initialize a data frame to fill in later
  for(i in 1:length(source[,col.sou])){
    cat("I'm at",i,"th cycle in Match.rows() for loop now\n")
    index <- which(target[,col.tar]==source[i,col.sou]) # search the source in target, get indexes of searched element in target
    len <- length(index) # store how many times it is encountered
    if(len!=0){ # if found
      for(j in 1:len){
        Result <- rbind(Result,cbind(source[i,],target[index[[j]],])) # write corresponding matches
        cat(j,"th cycle of multiple matchings in",source[i,col.sou],"index :",i,"\n")
      }
      target <- target[-index,] # exclude elements that has been listed, (on test)
    }
    else if(len==0){ # if could not be found
      temp <- matrix(NA,1,ncol(target))
      colnames(temp) <- colnames(target)
      Result <- rbind(Result,cbind(source[i,],temp)) # fill with NA's
      cat(source[i,col.sou],"cant be found at target data frame.(Match.rows)\n")
    }
    cat(i,"th item is found and listed by Match.Rows() \n")
  }
  return(Result)
}

source <- Zips[sample(nrow(Zips),1e4),]
col.sou <- 1
target <- my_samp
col.tar <- 5
nthreads <- NULL

Result <- data.frame() # initialize a data frame to fill in later
for(i in 1:length(source[,col.sou])){
  cat("I'm at",i,"th cycle in Match.rows() for loop now\n")
  index <- which(target[,col.tar]==source[i,col.sou]) # search the source in target, get indexes of searched element in target
  len <- length(index) # store how many times it is encountered
  if(len!=0){ # if found
    for(j in 1:len){
      Result <- rbind(Result,cbind(source[i,],target[index[[j]],])) # write corresponding matches
      cat(j,"th cycle of multiple matchings in",source[i,col.sou],"index :",i,"\n")
    }
    target <- target[-index,] # exclude elements that has been listed, (on test)
  }
  else if(len==0){ # if could not be found
    temp <- matrix(NA,1,ncol(target))
    colnames(temp) <- colnames(target)
    Result <- rbind(Result,cbind(source[i,],temp)) # fill with NA's
    cat(source[i,col.sou],"cant be found at target data frame.(Match.rows)\n")
  }
  cat(i,"th item is found and listed by Match.Rows() \n")
}

## parallel

par.Match.rows <- function(source,  # source data frame, this will be searched inside target
                           col.sou, # column number of source data to be searched
                           target,  # target data frame, expected to have source data frame's values
                           col.tar, # column number of target data frame to be inspected
                           nthreads = NULL # number of cores
                           ){
  if(is.null(nthreads)) nthreads = detectCores()
  require(parallel)
  cl <- makeCluster(nthreads) # initiate cluster
  clusterExport(cl,c("source","target","col.tar","col.sou")) # export variables to the cluster
  
  Result <- parLapply(cl,seq(nrow(source)),function(j){
    index <- which(source[j,col.sou]==target[,col.tar]) # search the source in target, get indexes
    if(length(index)!=0)
      sapply(index, function(i) c(source[j,],target[i,])) # THIS IS OKAY JUST NEEDS A CONVERTION TO DATA FRAME
    else
      c(source[j,],rep(NA,ncol(target))) # try with ifelse of base or if_else of dplyr
  })
  
  Result <- parLapply(cl, parLapply(cl,Result, function(y) 
    as.data.frame(matrix(y,length(y)/(ncol(target)+ncol(source)),ncol(target)+ncol(source),byrow = T)))
    ,data.frame, stringsAsFactors=FALSE)
  stopCluster(cl)
  
  Result <- do.call(rbind, Result) ## THIS WORKS
  colnames(Result) <- c(colnames(source),colnames(target))
  return(Result)
}


system.time({
  temp <- par.Match.rows(source,col.sou,target,col.tar)
})

system.time({
  temp_temp <- Match.rows(source,col.sou,target,col.tar)
})

source$Longitude <- as.numeric(source$Longitude)
system.time({
  temp <- Sort(source,7)
})


# matching 
Match.ShipData <- function(shipfiltered, # ultimate filtered and location data included shipping data 20 columns
                           month, # input a month number as integer
                           saledata, # raw sale data e.g. Month10.txt, Month11.txt etc.
                           iterations = 10, # number of part to divide into when computing
                           filename = "Most_Expensive_Matched_M_", # file name to be saved
                           begin = 1, # beginning of the for loop, could be continued from other steps
                           ship.sonumber = 8, # sonumber column in filtered shipping data
                           sale.sonumber = 1 # sonumber column in filtered sale data
){
  require("lubridate")
  # oninit: create a directory for tidier work
  main_path <- getwd() # "C:/Users/USER/Desktop/R"
  current_path <- paste(getwd(),paste("DataMatch",sample(100,1),sep="_"),sep = "/") # folder names
  dir.create(current_path) # create a new directory to store files
  setwd(current_path) # set new working directory
  cat("Working directory is set to",current_path,"\n")
  
  # pull out just one month, and sort according to shipping cost
  temp_ship <- Sort(shipfiltered[which(month(shipfiltered$DateShipped)==as.integer(month)),],7,decreasing = TRUE)
  cat("Total",nrow(temp_ship),"rows temp_ship (filtered ship data) has.\n")
  cat("Filtered shipping data is fractionated according to month.\n")
  
  # at following line temp_ship[,ship.sonumber] is changed to as.integer(levels(factor(temp_ship[,ship.sonumber])))
  temp_raw <- par.Search.List(as.integer(levels(factor(temp_ship[,ship.sonumber]))),saledata,sale.sonumber)[,-1] # get corresponding rows of saledata, by sonumber
  print(temp_raw)
  cat("Temporary fractional sale data is formed with corresponding SONumbers.\n")
  
  rows <- nrow(temp_ship) # get number of rows to use in following for loop
  cat("Levels of temp_ship SONumber :",length(levels(factor(temp_ship[,ship.sonumber]))),"total number of rows in temp_ship :",nrow(temp_ship),"\n")
  
  ## divide into fractions and then unify into one big file
  for(i in begin:iterations){
    pb <- txtProgressBar(style = 3)
    cat("Step",i,"\n")
    index <- seq((i-1)*(rows/iterations)+1,i*(rows/iterations)) # determine interval of indexes
    test_match <- par.Match.rows(temp_ship[index,],ship.sonumber,temp_raw,sale.sonumber) # match data and bind together as a data frame
    check_sen <- test_match[,4]==test_match[,23] & test_match[,5]==test_match[,24] # check receipent and sender zips whether they match
    test_match <- test_match[check_sen,] # take only who match by zips
    write.csv(test_match,paste(filename,i,".csv",sep = ""),row.names = FALSE) # write to file with order
    cat("File",paste(filename,i,".csv",sep = ""),"is saved to",getwd(),"\n") # inform user
    setTxtProgressBar(pb, i/iterations)
  }
  
  Result <- data.frame() # combine results into one data frame
  for(i in 1:iterations){
    Result <- rbind(Result,read.csv(paste(filename,i,".csv",sep = ""),row.names = NULL))
    cat(i,"th file is imported to finally merge together.\n")
  }
  
  setwd(main_path) # reset working directory
  write.csv(Result,paste(filename,".csv",sep = ""),row.names = FALSE)
  cat("File",paste(filename,".csv",sep = ""),"is saved to",getwd(),"\n") # inform user
  
  # ondeinit: remove the directory used for temporary storage
  unlink(current_path,recursive = TRUE,force = TRUE)
  
  return(Result)
}

temp <- par.LocationData(Raw_Ship)

# before 
system.time({
Ship <- par.Filter.ShippingData(Raw_Ship,file.write = FALSE)
})

# after
system.time({
  Ship <- par.Filter.ShippingData(Raw_Ship,file.write = FALSE)
})
