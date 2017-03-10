# PARALLEL VERSION, need improvements at do.call
# input "source" as a vector or a list
par.Search.List <- function(source, # object to be searched
                            target, # object expected to include source at least once
                            col,    # target column
                            nthreads = NULL){ # number of cores to use
  print("I'm at par.Search.list")
  if(is.null(nthreads)) nthreads = detectCores()
  require(parallel)
  cl <- makeCluster(nthreads) # initiate cluster
  print("I make cluster.")
  clusterExport(cl,c("source","target","col"),envir = environment()) # export variables to the cluster
  print("I export values")
  Result <- parSapply(cl,source,function(x){
    index <- which(target[,col]==x) # search the source in target, get indexes
    if(length(index)!=0)
      sapply(index, function(i) c(x,target[i,])) # THIS IS OKAY JUST NEEDS A CONVERTION TO DATA FRAME
  })
  print("I did first parsapply")
  # print(Result)
  Result <- parLapply(cl, lapply(Result, function(y){
    if(!is.null(y)) # this works for null values but BE CAREFUL!
      as.data.frame(matrix(y,length(y)/(ncol(target)+1),ncol(target)+1,byrow = T))
  })
  ,data.frame, stringsAsFactors=FALSE)
  stopCluster(cl) # stop cluster because the rest is serial
  print("I did first parlapply")
  # convert to data frame from list, following is time consuming and is NOT parallel
  Result <- do.call(rbind, Result) ## THIS WORKS
  colnames(Result) <- c("Source",colnames(target))
  
  return(Result)
}

par.Match.rows <- function(source,  # source data frame, this will be searched inside target
                           col.sou, # column number of source data to be searched
                           target,  # target data frame, expected to have source data frame's values
                           col.tar, # column number of target data frame to be inspected
                           nthreads = NULL # number of cores
){
  if(is.null(nthreads)) nthreads = detectCores()
  require(parallel)
  cl <- makeCluster(nthreads) # initiate cluster
  clusterExport(cl,c("source","target","col.tar","col.sou"),envir = environment()) # export variables to the cluster
  
  Result <- parLapply(cl,seq(nrow(source)),function(j){
    index <- which(source[j,col.sou]==target[,col.tar]) # search the source in target, get indexes
    if(length(index)!=0)
      sapply(index, function(i) c(source[j,],target[i,])) # THIS IS OKAY JUST NEEDS A CONVERTION TO DATA FRAME
    else
      c(source[j,],rep(NA,ncol(target))) # try with ifelse of base or if_else of dplyr
  })
  
  Result <- parLapply(cl, parLapply(cl,Result, function(y) # need arrangement
    as.data.frame(matrix(y,length(y)/(ncol(target)+ncol(source)),ncol(target)+ncol(source),byrow = T)))
    ,data.frame, stringsAsFactors=FALSE)
  stopCluster(cl)
  
  Result <- do.call(rbind, Result) ## THIS WORKS
  colnames(Result) <- c(colnames(source),colnames(target))
  return(Result)
}

# PARALLEL VERSION
# performs matching of filtered (rearranged) ship data and raw sale data
par.Match.ShipData <- function(shipfiltered, # ultimate filtered and location data included shipping data 20 columns
                               month, # input a month number as integer
                               saledata, # raw sale data e.g. Month10.txt, Month11.txt etc.
                               iterations = 10, # number of part to divide into when computing
                               filename = "Most_Expensive_Matched_M_", # file name to be saved
                               begin = 1, # beginning of the for loop, could be continued from other steps
                               ship.sonumber = 8, # sonumber column in filtered shipping data
                               sale.sonumber = 1, # sonumber column in filtered sale data
                               nthreads = NULL # number of cores to use
){
  # force(saledata)
  require("lubridate")
  require("parallel")
  # # oninit: create a directory for tidier work
  # main_path <- getwd() # "C:/Users/USER/Desktop/R"
  # current_path <- paste(getwd(),paste("DataMatch",sample(1000,1),sep="_"),sep = "/") # folder names
  # dir.create(current_path) # create a new directory to store files
  # setwd(current_path) # set new working directory
  # cat("Working directory is set to",current_path,"\n")
  
  # pull out just one month, and sort according to shipping cost
  temp_ship <- Sort(shipfiltered[which(month(shipfiltered$DateShipped)==as.integer(month)),],7,decreasing = TRUE)
  cat("Total",nrow(temp_ship),"rows temp_ship (filtered ship data) has.\n")
  cat("Filtered shipping data is fractionated according to month.\n")
  print(temp_ship[1:3,])
  # cl <- makeCluster(detectCores())
  # clusterExport(cl,Raw_SaleData,envir = .GlobalEnv)
  # at following line temp_ship[,ship.sonumber] is changed to as.integer(levels(factor(temp_ship[,ship.sonumber])))
  temp_raw <- par.Search.List(as.integer(levels(factor(temp_ship[,ship.sonumber]))), # source
                              saledata, # target
                              sale.sonumber, # col
                              nthreads = nthreads)[,-1] # get corresponding rows of saledata, by sonumber
  print(temp_raw[1:3,])
  cat("Temporary fractional sale data is formed with corresponding SONumbers.\n")
  
  rows <- nrow(temp_ship) # get number of rows to use in following for loop
  cat("Levels of temp_ship SONumber :",length(levels(factor(temp_ship[,ship.sonumber]))),
      "total number of rows in temp_ship :",nrow(temp_ship),"\n")
  Result <- data.frame() # combine results into one data frame
  ## divide into fractions and then unify into one big file
  pb <- txtProgressBar(style = 3)
  for(i in begin:iterations){
    # cat("\nStep",i)
    index <- seq((i-1)*(rows/iterations)+1,i*(rows/iterations)) # determine interval of indexes
    test_match <- par.Match.rows(temp_ship[index,],
                                 ship.sonumber,
                                 temp_raw,
                                 sale.sonumber,
                                 nthreads = nthreads) # match data and bind together as a data frame
    # print("par.MatchRows is completed without errors.")
    # cat("Class of test_match :",class(test_match),"\n")
    # cat("Class of test_match4 :",class(test_match[,4]),"\n")
    # cat("Class of test_match23 :",class(test_match[,23]),"\n")
    # print(test_match[1:10,4])
    # print(test_match[1:10,23])
    check_sen <- as.integer(test_match[,4])==as.integer(test_match[,23]) & 
      as.integer(test_match[,5])==as.integer(test_match[,24]) # check receipent and sender zips whether they match
    # print("I made checks.")
    test_match <- test_match[check_sen,] # take only who match by zips
    # print("I made cut of checks.")
    # print(head(test_match))
    # print("AS DATA FRAME")
    # print(head(as.data.frame(test_match)))
    # write.csv(as.data.frame(test_match),paste(filename,i,".csv",sep = ""),row.names = FALSE) # write to file with order
    # cat("File",paste(filename,i,".csv",sep = ""),"is saved to",getwd(),"\n") # inform user
    Result <- rbind(Result,test_match)
    setTxtProgressBar(pb, i/iterations)
  }
  
  # Result <- data.frame() # combine results into one data frame
  # for(i in 1:iterations){
  #   Result <- rbind(Result,read.csv(paste(filename,i,".csv",sep = ""),row.names = NULL))
  #   cat(i,"th file is imported to finally merge together.\n")
  # }
  
  # setwd(main_path) # reset working directory
  # write.csv(Result,paste(filename,".csv",sep = ""),row.names = FALSE)
  # cat("File",paste(filename,".csv",sep = ""),"is saved to",getwd(),"\n") # inform user
  
  # ondeinit: remove the directory used for temporary storage
  # unlink(current_path,recursive = TRUE,force = TRUE)
  
  return(Result)
}

# main
system.time({
matched_m10_test <- par.Match.ShipData(Ship,month = 10,Raw_SaleData,filename = "PAR_MATCHED_MONTH10")
}) # 70 seconds
matched_m10 <- Match.ShipData(Ship,month = 10,Raw_SaleData,filename = "PAR_MATCHED_MONTH10")

stopCluster(cl)


write.csv(matched_m10,"test_matched.csv",row.names = FALSE)
lapply(matched_m10, class)
temp <- sapply(matched_m10, unlist)

### FOLLOWING RESTATEMENTS MADE FOR LINUX-UBUNTU SERVER ###
par.Match.ShipData <- function(shipfiltered, # ultimate filtered and location data included shipping data 20 columns
                               month, # input a month number as integer
                               saledata, # raw sale data e.g. Month10.txt, Month11.txt etc.
                               iterations = 10, # number of part to divide into when computing
                               filename = "Most_Expensive_Matched_M_", # file name to be saved
                               begin = 1, # beginning of the for loop, could be continued from other steps
                               ship.sonumber = 8, # sonumber column in filtered shipping data
                               sale.sonumber = 1, # sonumber column in filtered sale data
                               nthreads = NULL # number of cores to use
){
  # force(saledata)
  require("lubridate")
  require("parallel")

  # pull out just one month, and sort according to shipping cost
  temp_ship <- Sort(shipfiltered[which(month(shipfiltered$DateShipped)==as.integer(month)),],7,decreasing = TRUE)
  cat("Total",nrow(temp_ship),"rows temp_ship (filtered ship data) has.\n")
  cat("Filtered shipping data is fractionated according to month.\n")
  print(temp_ship[1:3,])

  # at following line temp_ship[,ship.sonumber] is changed to as.integer(levels(factor(temp_ship[,ship.sonumber])))
  temp_raw <- par.Search.List(as.integer(levels(factor(temp_ship[,ship.sonumber]))), # source
                              saledata, # target
                              sale.sonumber, # col
                              nthreads = nthreads)[,-1] # get corresponding rows of saledata, by sonumber
  print(temp_raw[1:3,])
  cat("Temporary fractional sale data is formed with corresponding SONumbers.\n")
  
  rows <- nrow(temp_ship) # get number of rows to use in following for loop
  cat("Levels of temp_ship SONumber :",length(levels(factor(temp_ship[,ship.sonumber]))),
      "total number of rows in temp_ship :",nrow(temp_ship),"\n")
  Result <- data.frame() # combine results into one data frame
  
  ## divide into fractions and then unify into one big file
  pb <- txtProgressBar(style = 3)
  for(i in begin:iterations){
    index <- seq((i-1)*(rows/iterations)+1,i*(rows/iterations)) # determine interval of indexes
    test_match <- par.Match.rows(temp_ship[index,],
                                 ship.sonumber,
                                 temp_raw,
                                 sale.sonumber,
                                 nthreads = nthreads) # match data and bind together as a data frame
    check_sen <- as.integer(test_match[,4])==as.integer(test_match[,23]) & 
      as.integer(test_match[,5])==as.integer(test_match[,24]) # check receipent and sender zips whether they match
    test_match <- test_match[check_sen,] # take only who match by zips
    Result <- rbind(Result,test_match)
    setTxtProgressBar(pb, i/iterations)
  }
  return(Result)
}


par.Search.List <- function(source, # object to be searched
                            target, # object expected to include source at least once
                            col,    # target column
                            nthreads = NULL){ # number of cores to use
  force(source);force(target);force(col)
  print("I'm at par.Search.list")
  if(is.null(nthreads)) nthreads = detectCores()
  require(parallel)
  cl <- makeCluster(nthreads) # initiate cluster
  print("I make cluster.")
  clusterExport(cl,c("source","target","col"),envir = environment()) # export variables to the cluster
  print("I export values")
  Result <- parSapply(cl,source,function(x){
    index <- which(target[,col]==x) # search the source in target, get indexes
    if(length(index)!=0)
      sapply(index, function(i) c(x,target[i,])) # THIS IS OKAY JUST NEEDS A CONVERTION TO DATA FRAME
  })
  print("I did first parsapply")
  # print(Result)
  Result <- parLapply(cl, lapply(Result, function(y){
    if(!is.null(y)) # this works for null values but BE CAREFUL!
      as.data.frame(matrix(y,length(y)/(ncol(target)+1),ncol(target)+1,byrow = T))
  })
  ,data.frame, stringsAsFactors=FALSE)
  stopCluster(cl) # stop cluster because the rest is serial
  print("I did first parlapply")
  # convert to data frame from list, following is time consuming and is NOT parallel
  Result <- do.call(rbind, Result) ## THIS WORKS
  colnames(Result) <- c("Source",colnames(target))
  
  return(Result)
}

par.Match.rows <- function(source,  # source data frame, this will be searched inside target
                           col.sou, # column number of source data to be searched
                           target,  # target data frame, expected to have source data frame's values
                           col.tar, # column number of target data frame to be inspected
                           nthreads = NULL # number of cores
){
  require(parallel)
  force(source);force(target);force(col.sou);force(col.tar) # to be sure to export detect variables
  if(is.null(nthreads)) nthreads = detectCores()
  cl <- makeCluster(nthreads) # initiate cluster
  clusterExport(cl,c("source","target","col.tar","col.sou"),envir = environment()) # export variables to the cluster
  
  Result <- parLapply(cl,seq(nrow(source)),function(j){
    index <- which(source[j,col.sou]==target[,col.tar]) # search the source in target, get indexes
    if(length(index)!=0)
      sapply(index, function(i) c(source[j,],target[i,])) # THIS IS OKAY JUST NEEDS A CONVERTION TO DATA FRAME
    else
      c(source[j,],rep(NA,ncol(target))) # try with ifelse of base or if_else of dplyr
  })
  print(head(Result))
  print("I made matching")
  # following wrap of matrix spoils the POSIX time class, should be troubleshooted
  Result <- parLapply(cl, parLapply(cl,I(Result), function(y) # need arrangement
    as.data.frame(array(y,dim = c(length(y)/(ncol(target)+ncol(source)),ncol(target)+ncol(source)))))
    ,data.frame, stringsAsFactors=FALSE)
  stopCluster(cl)
  print(head(Result))
  print("I made rearrangements")
  Result <- do.call(rbind, Result) ## THIS WORKS
  print("I made do.call")
  colnames(Result) <- c(colnames(source),colnames(target))
  return(Result)
}

Ship <- par.Filter.ShippingData(Raw_Ship,file.write = FALSE)

# main
system.time({
  matched_m10_test <- par.Match.ShipData(Ship,month = 10,Raw_SaleData[sample.int(nrow(Raw_SaleData),1e4),])
}) 

system.time({
  matched_m10 <- Match.ShipData(Ship[sample.int(nrow(Ship),1e5),],month = 8,Raw_SaleData[sample.int(nrow(Raw_SaleData),1e5),],filename = "Matched_M10_test_test")
})

class(matched_m10)

library(parallel)
system.time({
temp1 <- par.Match.rows(Raw_Ship[1:1e3,],5,Raw_SaleData[1:1e3,],4)
})

system.time({
  temp <- Match.rows(Raw_Ship[1:1e3,],5,Raw_SaleData[1:1e3,],4)
})


#### to delete
null.omit <- function(x){ # remove a row containing null value(s)
  check <- sapply(x,function(i) ifelse(any(is.null(i)),TRUE,FALSE))
  if(!is.null(ncol(x)))
    if(ncol(x) > 0)
      return(x[check,])
  else
    return(x[check,])
}

head(null.omit(matched_m10))
#### up to here

## ERROR DIAGNOSSIS
source <- Raw_Ship[1:1e3,]
col.sou <- 5
target <- Raw_SaleData[1:1e3,]
col.tar <- 4

require(parallel)
force(source);force(target);force(col.sou);force(col.tar) # to be sure to export detect variables
if(is.null(nthreads)) nthreads = detectCores()
cl <- makeCluster(nthreads) # initiate cluster
clusterExport(cl,c("source","target","col.tar","col.sou"),envir = environment()) # export variables to the cluster

Result <- parLapply(cl,seq(nrow(source)),function(j){
  index <- which(source[j,col.sou]==target[,col.tar]) # search the source in target, get indexes
  if(length(index)!=0)
    sapply(index, function(i) c(source[j,],target[i,])) # THIS IS OKAY JUST NEEDS A CONVERTION TO DATA FRAME
  else
    c(source[j,],rep(NA,ncol(target))) # try with ifelse of base or if_else of dplyr
})

print(head(Result))
print("I made matching")
# following wrap of matrix spoils the POSIX time class, should be troubleshooted !!
Result <- parSapply(cl,Result, function(y) # need arrangement
  as.data.frame(array(y,dim = c(length(y)/(ncol(target)+ncol(source)),ncol(target)+ncol(source)))))

Result <- parLapply(cl, Result
  ,data.frame, stringsAsFactors=FALSE)
stopCluster(cl)
print(head(Result))
print("I made rearrangements")
Result <- do.call(rbind, Result) ## THIS WORKS
print("I made do.call")
colnames(Result) <- c(colnames(source),colnames(target))


############### EDUCATIONAL #################
par.Search.List <- function(source, # object to be searched
                            target, # object expected to include source at least once
                            col,    # target column
                            nthreads = NULL){ # number of cores to use
  require(parallel)
  lapply(list(source,target,col), force)
  if(is.null(nthreads)) nthreads = detectCores()
  Result <- data.frame()
  cl <- makeCluster(nthreads) # initiate cluster
  clusterExport(cl,c("Result","source","target","col"),envir = environment()) # export variables to the cluster
  print("Im gonna try parSapply now.")
  temp <- parSapply(cl,seq_along(source),function(i){ # returnnpothing, use just like a for loop
    index <- which(target[,col]==source[i]) # search the source in target, get indexes
    if(length(index)!=0){
      # print("found it!")
      # print(cbind(rep(source[i],length(index)),target[index,]))
      # Result <- rbind(Result,cbind(rep(source[i],length(index)),target[index,]))
      sapply(index, function(j) cbind(source[i],as.integer(j)))
    }
    # sapply(index, function(i) c(x,target[i,]))) # THIS IS OKAY JUST NEEDS A CONVERTION TO DATA FRAME
  })
  
  stopCluster(cl) # stop cluster because the rest is serial
  return(temp)
}

# system.time({
  temp1 <- par.Search.List(c("11354","32828"),Raw_Ship[1:1e4,],col = 5)
# })

temp1
length(temp1)
lengths(temp1)/2
as.data.frame(temp1)

sapply(seq_along(temp1), function(i){
  sapply(seq.int(lengths(temp1)[i]/2), function(j){
    cbind(temp1[[j]][[1]],temp1[[j]][[2]])
  })
})

for(i in 1:length(temp1)){
  for(j in seq.int(1,lengths(temp1)[i]-1,by = 2)){
    print(c(temp1[[i]][[j]],temp1[[i]][[j+1]]))
  }
}

a <- data.frame()
lapply(1:length(temp1),function(i){
  lapply(seq.int(1,lengths(temp1)[i]-1,by = 2),function(j){
    a <- rbind(a,c(temp1[[i]][[j]],temp1[[i]][[j+1]]))
  })
})

# do.call(cbind,a)

Raw_SaleData[c(1,1,1,1,2),1]

# index <- which(target[,4]=="11354")


library(doParallel)
library(foreach)
cl<-makeCluster(2) #change the 2 to your number of CPU cores
registerDoParallel(cl)

foreach(i=1:10) %dopar% {
  print(i)  
} 

stopCluster(cl)

par.Search.List <- function(source,target,col,nthreads=NULL){
  lapply(c(source,target,col), force)
  library(parallel)
  library(doParallel)
  library(foreach)
  if(is.null(nthreads)) nthreads <- detectCores()
  cl<-makeCluster(nthreads) #change the 2 to your number of CPU cores
  registerDoParallel(cl)
  print("Search List function is working.")
  Result <- data.frame() # initialize a data frame to fill in later
  print(1:length(source))
  source <- as.character(source)
  temp <- foreach(i=1:length(source)) %dopar% { # returns a 3-dim array as the form of list
    cat("I'm at",i,"th cycle in Search.List() for loop now.\n")
    index <- which(target[,col]==source[i]) # search the source in target, get indexes
    if(length(index)>0){ # if found
    #   print("Following cant be found at target vector(Search.List) :")
    #   print(source[i])    
    # }
    # else{
      # print("I found it")
      # Result <- rbind(Result,cbind(rep(source[i],length(index)),target[index,])) #
      cbind(as.character(rep(source[i],length(index))),target[index,])
    }
  }
  stopCluster(cl)
  print(lapply(temp,class))
  print("I prepared temp")
  
  a <- data.frame()
  for(i in 1:length(temp1))
    a <- rbind(a,temp1[[i]])
  
  return(a)
}
temp_DD <- Raw_Ship$DateDelivered
temp_DS <- Raw_Ship$DateShipped
as.matrix(temp_DD[100])

as.list(as.character(temp_DD[100]))
Raw_Ship$DateDelivered <- as.POSIXlt(Raw_Ship$DateDelivered)
Raw_Ship$DateShipped <- as.POSIXlt(Raw_Ship$DateShipped)

temp1 <- par.Search.List(c(11354,328286484),Raw_Ship[1:1e4,],col = 5) # returns a 3-dim array as the form of list
system.time({
temp1 <- par.Search.List(as.character(levels(factor(Raw_SaleData$SenderZip[1:1000]))),
                         Raw_Ship[1:1e4,],col = 5)
})
temp1
system.time({
  temp <- Search.List(as.character(levels(factor(Raw_SaleData$SenderZip[1:1000]))),
                           Raw_Ship[1:1e4,],col = 5)
})

a <- as.data.frame(unlist(temp1))
print(temp1)
temp1[[1]][[11]]
class(temp1[[1]][[11]][[1]])
length(temp1)
dim(temp1[[2]])[1]
a <- data.frame()
for(i in 1:length(temp1)){
  # for(j in 1:dim(temp1[[i]])[1]){
  #   
  # }
  a <- rbind(a,temp1[[i]])
}

temp1 <- Search.List(c(11354,32828),Raw_Ship[1:1e4,],col = 5)
temp1 <- Search.List(Raw_SaleData$SenderZip[1:10],Raw_Ship[1:1e4,],col = 5)

temp <- data.frame(Date1 = NA,Date2 = NA)
temp[,1] <- as.character(Raw_Ship[,9])

#### BELOW PARALLEL FUNCTION IS CURRENTLY WORKING PROPERLY !!! KEP IT SAFE ####
### WORKS IN SERVER TOO, BUT CAN BE KILLED BY SYSTEM ###
par.Search.List <- function(source,target,col,nthreads=NULL){
  lapply(c(source,target,col), force) # to prevent "object not found" error on linux
  library(parallel);library(doParallel);library(foreach)
  
  if(is.null(nthreads)) nthreads <- detectCores() # use all cores if not specified
  cl<-makeCluster(nthreads) # initiate the cluster
  registerDoParallel(cl) # this is must use
  source <- as.character(source)
  temp <- foreach(i=1:length(source)) %dopar% { # returns a 3-dim array as the form of list
    index <- which(target[,col]==source[i]) # search the source in target, get indexes
    if(length(index)>0) # if found
      cbind(rep(source[i],length(index)),target[index,])
  }
  stopCluster(cl) # turn the connections off

  Result <- data.frame() # declare a temporary data frame to give the final shape, TIME CONSUMING
  for(i in 1:length(temp))
    Result <- rbind(Result,temp[[i]])
  
  return(Result)
}
###############################################################################


## speed testing
system.time({
  temp1 <- par.Search.List(as.character(levels(factor(Raw_SaleData$SenderZip))),
                           Raw_Ship,col = 5)
})

system.time({
  temp <- Search.List(as.character(levels(factor(Raw_SaleData$SenderZip))),
                      Raw_Ship,col = 5)
}) # 65.6 seconds

## speed testing on server
system.time({
  temp <- par.Search.List(as.character(levels(factor(Raw_M7$SenderZip[1:1e4]))),
                           Raw_M7,col = 4)
})

system.time({
  temp <- Search.List(as.character(levels(factor(Raw_M7$SenderZip[1:1e4]))),
                      Raw_M7,col = 4)
})

system.time({
  temp <- par.Search.List(as.character(levels(factor(Raw_M7$ReceipentZip))),
                           Raw_Ship,col = 5)
})

system.time({
  temp <- Search.List(as.character(levels(factor(Raw_M7$ReceipentZip))),
                      Raw_Ship,col = 5)
})

########################
### TIMING
########################

system.time({
  temp <- Search.List(Raw_M7$SONumber[seq(1e4)],Raw_M7,col = 1)
}) # 704 seconds on linux server

# 2 nodes
system.time({
  temp <- par.Search.List(Raw_M7$SONumber[seq(1e4)],Raw_M7,col = 1,nthreads = 2)
}) # 72 seconds on linux server

# 4 nodes
system.time({
  temp <- par.Search.List(Raw_M7$SONumber[seq(1e4)],Raw_M7,col = 1,nthreads = 4)
}) # 59 seconds on linux server

# 6 nodes
system.time({
  temp <- par.Search.List(Raw_M7$SONumber[seq(1e4)],Raw_M7,col = 1,nthreads = 6)
}) # 67 seconds on linux server

# 8 nodes
system.time({
  temp <- par.Search.List(Raw_M7$SONumber[seq(1e4)],Raw_M7,col = 1,nthreads = 8)
}) # 101 seconds on linux server



#### REORGANIZE WITH FOREACH!! ####
par.Match.rows <- function(source,col.sou,target,col.tar,nthreads=NULL){
  
  lapply(c(source,col.sou,target,col.tar), force)
  library(parallel);library(doParallel);library(foreach)
  
  if(is.null(nthreads)) nthreads <- detectCores() # use all cores if not specified
  cl<-makeCluster(nthreads) # initiate the cluster
  registerDoParallel(cl) # this is must use
  
  # Result <- data.frame() # initialize a data frame to fill in later
  temp <- foreach(i=1:length(source[,col.sou])) %dopar% {
    index <- which(target[,col.tar]==source[i,col.sou]) # search the source in target, get indexes
    len <- length(index) # store how many times it is encountered
    if(len!=0){ # if found
      init <- data.frame()
      for(j in 1:len)
        init <- rbind(init,cbind(source[i,],target[index[[j]],])) # write corresponding matches
      init # return this
    }
  }
  stopCluster(cl) # turn the connections off
  
  print("I came here")
  Result <- data.frame() # declare a temporary data frame to give the final shape
  for(i in 1:length(temp))
    Result <- rbind(Result,temp[[i]])
  
  return(Result)
}

Match.rows <- function(source,col.sou,target,col.tar){
  Result <- data.frame() # initialize a data frame to fill in later
  for(i in 1:length(source[,col.sou])){
    index <- which(target[,col.tar]==source[i,col.sou]) # search the source in target, get indexes
    len <- length(index) # store how many times it is encountered
    if(len!=0) # if found
      for(j in 1:len)
        Result <- rbind(Result,cbind(source[i,],target[index[[j]],])) # write corresponding matches
  }
  return(Result)
}

system.time({
  temp1 <- par.Match.rows(Raw_SaleData,1,Raw_Ship[1:1e5,],8)
})

system.time({
  temp <- Match.rows(Raw_SaleData,1,Raw_Ship[1:1e3,],8)
})

par.Match.rows <- function(source,col.sou,target,col.tar,nthreads=NULL){
  lapply(c(source,col.sou,target,col.tar), force)
  library(parallel)
  if(is.null(nthreads)) nthreads <- detectCores() # use all cores if not specified
  cl<-makeCluster(nthreads) # initiate the cluster
  clusterExport(cl,c("source","target","col.sou","col.tar"),envir = environment()) # export variables
  temp <- parSapply(cl,1:length(source[,col.sou]),function(i){
    index <- which(target[,col.tar]==source[i,col.sou]) # search the source in target, get indexes
    len <- length(index) # store how many times it is encountered
    if(len!=0){ # if found
      init <- data.frame()
      for(j in 1:len)
        init <- rbind(init,cbind(source[i,],target[index[[j]],])) # write corresponding matches
      init # return this
    }})
  stopCluster(cl) # turn the connections off
  Result <- data.frame() # declare a temporary data frame to give the final shape
  for(i in 1:length(temp))
    Result <- rbind(Result,temp[[i]])
  return(Result)
}




par.Match.rows <- function(source,col.sou,target,col.tar,nthreads=NULL){
  lapply(c(source,col.sou,target,col.tar), force)
  library(parallel)
  if(is.null(nthreads)) nthreads <- detectCores() # use all cores if not specified
  Result <- data.frame() # declare a temporary data frame to give the final shape
  cl<-makeCluster(nthreads) # initiate the cluster
  clusterExport(cl,c("source","target","col.sou","col.tar","Result"),envir = environment()) # export variables
  temp <- parSapply(cl,1:length(source[,col.sou]),function(i){
    index <- which(target[,col.tar]==source[i,col.sou]) # search the source in target, get indexes
    len <- length(index) # store how many times it is encountered
    if(len!=0){ # if found
      init <- data.frame()
      for(j in 1:len)
        init <- rbind(init,cbind(source[i,],target[index[[j]],])) # write corresponding matches
      init # return this
    }})

  parSapply(cl,1,function(x){
  for(i in 1:length(temp))
    Result <- rbind(Result,temp[[i]])
  })
  stopCluster(cl) # turn the connections off
  return(Result)
}

################## INDEXING ATTEMPT #################
par.Search.List <- function(source, # object to be searched
                            target, # object expected to include source at least once
                            col,    # target column
                            nthreads = NULL){ # number of cores to use
  require(parallel);require(dplyr)
  lapply(list(source,target,col), force) # forcing is needed on linux systems
  if(is.null(nthreads)) nthreads = detectCores() # if not specified, use all threads
  cl <- makeCluster(nthreads) # initiate cluster
  clusterExport(cl,c("source","target","col"),envir = environment()) # export variables to the cluster
  print("cluster initiating and exporting finished")
  temp <- parSapply(cl,seq_along(source),function(i){ # store indexes
    index <- which(target[,col]==source[i]) # search the source in target, get indexes
    if(length(index)!=0)
      cbind(i,index)
  })
  print("I finish parSapply")
  temp <- parLapply(cl,temp,as.data.frame)
  stopCluster(cl) # stop cluster, rest is serial
  print("I finish parlappy")
  temp <- bind_rows(temp) # convert into a data frame to use it easily
  print("Indexing finished")
  return(cbind(source[temp[,1]],target[temp[,2],]))
}


## speed testing
system.time({
  temp1 <- par.Search.List(as.character(levels(factor(Raw_SaleData$ReceipentZip))),
                           Raw_SaleData,col = 4)
})

system.time({
  temp <- Search.List(as.character(levels(factor(Raw_SaleData$ReceipentZip))),
                      Raw_SaleData,col = 4)
})








################## INDEXING ATTEMPT #################
par.Match.rows <- function(source,col.sou,target,col.tar,nthreads=NULL){
  lapply(c(source,col.sou,target,col.tar), force)
  library(parallel);library(dplyr)
  if(is.null(nthreads)) nthreads <- detectCores() # use all cores if not specified
  cl<-makeCluster(nthreads) # initiate the cluster
  clusterExport(cl,c("source","target","col.sou","col.tar"),envir = environment()) # export variables
  print("I initiate the cluster and export variables")
  temp <- parSapply(cl,seq_along(source[,col.sou]),function(i){
    index <- which(target[,col.tar]==source[i,col.sou]) # search the source in target, get indexes
    if(length(index)!=0) # if found
        cbind(i,index) # store just indexes
    })
  
  print("I finish parSapply")
  temp <- parLapply(cl,temp,as.data.frame)
  stopCluster(cl) # stop cluster, rest is serial
  print("I finish parlappy")
  temp <- bind_rows(temp) # convert into a data frame to use it easily
  print("Indexing finished")
  return(cbind(source[temp[,1],],target[temp[,2],]))
}

system.time({
  temp1 <- par.Match.rows(Raw_SaleData[1:1e5,],1,Raw_Ship[1:1e6,],8)
}) # 485 seconds

system.time({
  temp <- Match.rows(Raw_SaleData,1,Raw_Ship[1:1e5,],8)
})

system.time({
  temp1 <- par.Match.rows(Raw_SaleData,1,Raw_Ship[1:1e5,],8)
}) # 32.08 seconds without listing


#######################################
### TIMING ON WINDOWS LOCAL MACHINE ###
#######################################

### 5*10^4 elements on windows local machine 1
system.time({ # complete serial
  temp <- zip.location(Raw_SaleData$ReceipentZip)
}) # ~84 seconds on local windows

system.time({ # with 2 nodes
  temp <- par.zip.location(Raw_SaleData$ReceipentZip,nthreads = 2)
}) # ~27 seconds on local windows

system.time({ # with 4 nodes
  temp <- par.zip.location(Raw_SaleData$ReceipentZip,nthreads = 3)
}) # ~21.23 on local windows

system.time({ # with 4 nodes
  temp <- par.zip.location(Raw_SaleData$ReceipentZip,nthreads = 4)
}) # ~20 seconds on local windows

time_list <- data.frame(Threads=seq.int(4),Duration=c(84,27,21.2,20))
a <- list(x = time_list$Threads[1],y = time_list$Duration[1],text = "Serial",xref = "x",yref = "y",showarrow = TRUE,
  arrowhead = 7,ax = 50,ay = 20)
library(plotly)
p_1 <- plot_ly(data = time_list,x = ~Threads,y = ~Duration,type = "scatter",mode = "lines",
               line= list(color = "darkviolet"),text = paste(c(1,2,3,4),"Cores")) %>%
  layout(title = "Location Finder with Zip<br>(50k Rows of Data)",annotations = a,yaxis = list(title="Duration(s)"))
p_1

## 1e4 rows input
# 4 nodes
system.time({
  temp <- par.Match.rows(Raw_SaleData[seq(1e5),],1,Raw_Ship[seq(1e6),],8,nthreads = 4)
}) # ?? seconds on windows local machine

# 3 nodes
system.time({
  temp <- par.Match.rows(Raw_SaleData[seq(1e5),],1,Raw_Ship[seq(1e6),],8,nthreads = 3)
}) # ?? seconds on windows local machine

# 2 nodes
system.time({
  temp <- par.Match.rows(Raw_SaleData[seq(1e5),],1,Raw_Ship[seq(1e6),],8,nthreads = 2)
}) # ?? seconds on windows local machine

# complete serial
system.time({
  temp <- Match.rows(Raw_SaleData[seq(1e5),],1,Raw_Ship[seq(1e6),],8)
}) # ?? seconds on windows local machine

## 5*1e4 rows input 2
# 4 nodes
system.time({
  temp <- par.Match.rows(Raw_SaleData[seq(5*1e4),],1,Raw_Ship[seq(1e6),],8,nthreads = 4)
}) # 268 seconds on windows local machine

# 3 nodes
system.time({
  temp <- par.Match.rows(Raw_SaleData[seq(5*1e4),],1,Raw_Ship[seq(1e6),],8,nthreads = 3)
}) # 275 seconds on windows local machine

# 2 nodes
system.time({
  temp <- par.Match.rows(Raw_SaleData[seq(5*1e4),],1,Raw_Ship[seq(1e6),],8,nthreads = 2)
}) # 322 seconds on windows local machine

# complete serial
system.time({
  temp <- Match.rows(Raw_SaleData[seq(5*1e4),],1,Raw_Ship[seq(1e6),],8)
}) # 1007 seconds on windows local machine

time_list <- data.frame(Threads=seq.int(4),Duration=c(1007,322,275,268))
a <- list(x = time_list$Threads[1],y = time_list$Duration[1],text = "Serial",xref = "x",yref = "y",showarrow = TRUE,
          arrowhead = 7,ax = 50,ay = 20)
library(plotly)
p_2 <- plot_ly(data = time_list,x = ~Threads,y = ~Duration,type = "scatter",mode = "lines",line= list(color = "crimson"),
               text = paste(c(1,2,3,4),"Cores")) %>%
  layout(title = "Row Matcher<br>(50k and 1M Rows of Data)",annotations = a,yaxis = list(title="Duration(s)"))
p_2

### 3
# complete serial
system.time({
  temp <- Search.List(Raw_SaleData$SONumber,Raw_Ship[seq(1e6),],col = 8)
}) # 723 seconds on windows local machine

# 2 nodes
system.time({
  temp <- par.Search.List(Raw_SaleData$SONumber,Raw_Ship[seq(1e6),],col = 8,nthreads = 2)
}) # 303 seconds on windows local machine

# 3 nodes
system.time({
  temp <- par.Search.List(Raw_SaleData$SONumber,Raw_Ship[seq(1e6),],col = 8,nthreads = 3)
}) # 264 seconds on windows local machine

# 4 nodes
system.time({
  temp <- par.Search.List(Raw_SaleData$SONumber,Raw_Ship[seq(1e6),],col = 8,nthreads = 4)
}) # 255 seconds on windows local machine

time_list <- data.frame(Threads=seq.int(4),Duration=c(723,303,264,255))
a <- list(x = time_list$Threads[1],y = time_list$Duration[1],text = "Serial",xref = "x",yref = "y",showarrow = TRUE,
          arrowhead = 7,ax = 50,ay = 20)
library(plotly)
p_3 <- plot_ly(data = time_list,x = ~Threads,y = ~Duration,type = "scatter",mode = "lines",line= list(color = "slategrey"),
               text = paste(c(1,2,3,4),"Cores")) %>%
  layout(title = "Search and List Function<br>(50k and 1M Rows of Data)",annotations = a,yaxis = list(title="Duration(s)"))
p_3

####### final aggregation

# complete serial
system.time({
  matched_m10 <- Match.ShipData(Ship,month = 10,Raw_SaleData[seq.int(1e4),],filename = "PAR_TEST_MATCHED_MONTH10.csv")
})

# with 2 nodes
system.time({
matched_m10 <- par.Match.ShipData(Ship,month = 10,Raw_SaleData[seq.int(1e4),],nthreads = 2)
})

# with 3 nodes
system.time({
  matched_m10 <- par.Match.ShipData(Ship,month = 10,Raw_SaleData[seq.int(1e4),],nthreads = 3)
})

# with 4 nodes
system.time({
  matched_m10 <- par.Match.ShipData(Ship,month = 10,Raw_SaleData[seq.int(1e4),],nthreads = 4)
})

###### SPEED TESTING ON SERVER ######
system.time({ # with 2 nodes
temp <- par.zip.location(Raw_SaleData$ReceipentZip[1:(2*1e6)],nthreads = 2)
}) # ~443 seconds

system.time({ # with 4 nodes
  temp <- par.zip.location(Raw_SaleData$ReceipentZip[1:(2*1e6)],nthreads = 4)
})

system.time({ # with 8 nodes
  temp <- par.zip.location(Raw_SaleData$ReceipentZip[1:(2*1e6)],nthreads = 8)
})




system.time({ # with 4 nodes
  temp <- par.which.containingString(Raw_Sale$ItemDescription,"MEM")
}) # 38 seconds

system.time({ # complete serial
  temp <- which.containingString(Raw_Sale$ItemDescription,"MEM")
}) # 254 seconds

time <- system.time({ # complete serial
  temp <- sales.daily(Raw_Sale,"MEM")
}) # 259 seconds
print(time)

system.time({ # with 4 nodes
  temp <- par.sales.daily(Raw_Sale,"MEM")
})



par.which.containingString <- function(cl = NULL,x,ask,sep = " ",nthreads = NULL){ # make the cluster as export
  require(parallel)
  force(x);force(ask);force(sep);
  if(is.null(cl)){
    if(is.null(nthreads)) nthreads = detectCores()
    cl <- makeCluster(nthreads)
    clusterExport(cl,c("x","ask","sep"),envir = environment())
  }
  Result <- na.omit(parSapply(cl,seq.int(x), function(i)
    ifelse(any(as.character(unlist(strsplit(x[i],split = sep))) %in% ask),i,NA)))
  if(is.null(cl))
    stopCluster(cl)
  return(Result)
}

cl <- makeCluster(detectCores())
clusterExport(cl,c("Raw_Sale"))
clusterExport(cl,"par1.which.containingString")
temp <- par1.which.containingString(cl,Raw_Sale$ItemDescription,"MEM")
stopCluster(cl)

temp <- par1.which.containingString(x = Raw_Sale$ItemDescription,ask = "MEM")

par.sales.daily <- function(raw_sale,category,nthreads=NULL){
  require(lubridate)
  require(parallel)
  force(raw_sale);force(category)
  if(is.null(nthreads)) nthreads = detectCores()
  cl <- makeCluster(nthreads)
  clusterExport(cl,c("raw_sale","category"),envir = environment())
  clusterExport(cl,"par.which.containingString")
  daynum <- days_in_month(raw_sale$ShippingDate[sample.int(nrow(raw_sale),1)]) # get number of days in that month
  print("I came here")
  Result <- parSapply(cl,category,function(x){ 
    temp <- raw_sale[par.which.containingString(raw_sale$ItemDescription,x),c(2,7),cl = cl] # only shipping date and unitsshipped
    sapply(seq.int(daynum),function(y) 
      sum(na.omit(temp$UnitsShipped[which(day(temp$ShippingDate)==y)])))
  })
  stopCluster(cl)
  return(data.frame(Days=seq.int(daynum),Result))
}

system.time({ # with 4 nodes
  temp <- par.sales.daily(Raw_Sale,"MEM")
})

library(lubridate)
Ship <- read.table("ShippingData_Months_10to12.txt",colClasses = "character",sep = ",")
Ship <- Format.ShippingData(Ship)
Ship_m10 <- subset(Ship,month(DateDelivered)==10 | month(DateShipped)==10) # subset for just October
Ship_ <- par.Filter.ShippingData(Ship_m10,filter = FALSE,file.write = FALSE) # get corresponding data
# match with MEM category sale info
matched_MEM <- par.Match.ShipData(Ship_m10,month = 10,Raw_MEM,filename = "MEM_MATCHED_MONTH10.csv")


