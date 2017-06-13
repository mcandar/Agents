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
