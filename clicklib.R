## hurriyet proj lib

# source("https://raw.githubusercontent.com/mcandar/Agents/master/clicklib.R")

## data import with ease
clickstream.import <- function(filename)
  read.csv(filename,row.names = NULL,stringsAsFactors = FALSE,na.strings = "null")

## rearrange and clean the click data
clickstream.organize <- function(data, # click data to read
                                 exclude_null_columns = TRUE, # exclude the columns filled with null's
                                 convert_datetime = TRUE){ # convert datetime objects to POSIXlt or POSIXct
  
  
  library(lubridate)
  ## replace the nonsense characters with meaningful ones, update the source if found any new pair
  chars <- data.frame(find=as.character(c("Ä±","Ã¼","Ã§","Ã‡","ÅŸ","Ã¶","Ä°","ÄŸ","Ã–")),
                      replace=as.character(c("i","u","c","C","s","o","I","g","O")))
  for(i in 1:nrow(chars)){
    data$pg_titl <- gsub(pattern = chars[i,1],replacement = chars[i,2],data$pg_titl)
  }
  
  ## exclude null columns
  if(exclude_null_columns){
    valid_columns <- sapply(seq(ncol(data)),function(x){
      if(all(is.na(data[,x])))
        0 # do not take the columns filled with NA's
      else
        x # record otherwise, will be taken into considering later
    })
    
    valid_columns <- which(valid_columns != 0)
    # data <- data[,valid_columns]
  }
  print(valid_columns)
  data <- data[,valid_columns]
  
  tempind <- unique(c(which(lengths(strsplit(data$ev_tm,split = " ")) != 2), # indexes that time is not in a valid format
  which(lengths(strsplit(data$srvr_tm,split = " ")) != 2),
  which(lengths(strsplit(as.character(data$ssn_id),split = " ")) != 1),
  which(lengths(strsplit(data$country,split = " ")) != 1)))
  print("Excluded rows are:")
  print(tempind)
  if(length(tempind) != 0)
    data <- data[-tempind,]
  
  # convert datetime columns into more convenient forms like POSIXlt or POSIXct
  if(convert_datetime){
    data$ev_tm <- as_datetime(data$ev_tm)
    data$srvr_tm <- as_datetime(data$srvr_tm)
  }
  
  # data <- data[-which(year(data$ev_tm)>),]
  ## SHOULD BE EDITED FOR FOLLOWING YEARS, OR IN NEW YEAR'S EVE
  # freq <- table(year(data$srvr_tm))
  # freq <- freq[order(freq,decreasing = TRUE)]
  # data <- data[-which(year(data$srvr_tm)!=as.integer(names(freq[1]))),]
  
  return(data)
}

# merge columns of a row as a datetie object with format "yyyy-mm-dd HH:MM:SS"
merge.datetime.mins <- function(data)
  lubridate::as_datetime(paste(paste(data[,1],data[,2],data[,3],sep = "-"),paste(data[,4],data[,5],sep = ":")),tz = "GMT")

perc.error <- function(query,reference){
  if(length(query)==1){
    print("Error on single value.")
    if(reference == 0)
      return("Can't calculate due to zero divide.")
    return((abs(query-reference)/reference)*100)
  }
  else{
    print("Mean error on elementwise values.")
    temp <- data.frame(query=query,reference=reference)
    ind <- which(temp$reference == 0)
    if(length(ind)!=0)
      temp <- temp[-ind,] # exclude zero divisions
    return(mean(na.omit((abs(temp$query-temp$reference)/temp$reference)*100)))
  }
}

# split datetime into columns AND REDUCE INTO HOURLY INTERVALS (FOR PV)
clickstream.splitDatetime <- function(data,col.datetime,breaks = "hours",splitOnly=FALSE,...){
  if(!splitOnly){
    min <- data.frame(PV = table(cut(data[which(data$ev_nm == "PV"),"srvr_tm"], breaks="hours",...)))
    print("I do split.")
  }
  else{
    min <- data
    print("I skip splitting.")
  }
  
  library(lubridate)
  temp <- strsplit(as.character(as_datetime(min[,col.datetime])),split = " ")
  temp <- t(as.data.frame(temp))
  rownames(temp) <- seq(nrow(temp))
  temp <- cbind(t(as.data.frame(strsplit(temp[,1],split = "-"))),t(as.data.frame(strsplit(temp[,2],split = ":"))))
  temp <- as.data.frame(apply(temp,2,function(x) as.integer(as.character(x))))
  temp$V6 <- NULL
  colnames(temp) <- c("year","month","day","hour","min")
  min <- data.frame(temp,min[,-col.datetime])
  return(min)
}


holidays.2017 <- function(data,col.datetime){
  require(lubridate)
  holidays <- data.frame(year = rep(2017,16), 
                         month = c(1,4,5,5,6,6,6,6,8,8,9,9,9,9,10,10), 
                         day = c(1,23,1,19,24,25,26,27,30,31,1,2,3,4,28,29))
  
  Result <- apply(data.frame(year(data$datetime),month(data$datetime),day(data$datetime)),1,function(x){
    temp <- matrix(as.numeric(x),nrow = nrow(holidays),ncol = 3,byrow = T)
    any(((temp[,1] - holidays[,1]) == 0) & ((temp[,2] - holidays[,2]) == 0) & ((temp[,3] - holidays[,3]) == 0))
  })
  
  return(Result)
}

####THIS WORKS 
split.Datetime <- function(x){
  library(lubridate)
  temp <- strsplit(as.character(as_datetime(x)),split = " ")
  temp <- t(as.data.frame(temp))
  rownames(temp) <- seq(nrow(temp))
  temp <- cbind(t(as.data.frame(strsplit(temp[,1],split = "-"))),t(as.data.frame(strsplit(temp[,2],split = ":"))))
  temp <- as.data.frame(apply(temp,2,function(x) as.integer(as.character(x))))
  temp$V6 <- NULL
  colnames(temp) <- c("year","month","day","hour","min")
  return(temp)
}


## be careful about timezones, keep the tz arguments as "GMT"
merge.datetime.mins <- function(data)
  as.POSIXct(paste(paste(data[,1],data[,2],data[,3],sep = "-"),paste(data[,4],data[,5],sep = ":")),tz = "GMT")

hidden.toStr <- function(hidden){
  len <- length(hidden)
  if(all(hidden == hidden[1]) & len > 1)
    return(paste(len,hidden[1],sep = "x"))
  if(all(hidden == hidden[1]) & len == 1)
    return(paste(len,hidden,sep = "x"))
  else
    return(hidden)
}

# for easy-import of hourly generated pv counts                              
clickstream.importHourly <- function(filename,time_interval=NULL){
  result <- read.csv(filename,row.names = NULL,stringsAsFactors=FALSE)
  colnames(result) <- c("date","hour","pv")
  ### DONT FORGET TO ADD tz = "GMT" otherwise it will adapt to gmt and add or subtract some time
  result$datetime <- as.POSIXct(paste(result$date,paste(result$hour,"00",sep = ":")),tz = "GMT") # get the time as is
  if(is.null(time_interval))
    return(data.frame(Datetime=result$datetime,PV=result$pv))
  else{
    result <- result[which(result$datetime %in% time_interval),]
    result <- result[order(result$datetime),]
    return(data.frame(Datetime=result$datetime,PV=result$pv))
  }
}
