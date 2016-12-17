# Server file
source("https://raw.githubusercontent.com/mcandar/Agents/master/appr.R")

# input "source" as a vector or a list
Search.List <- function(source,target,col){
  print("Search List function is working.")
  Result <- data.frame() # initialize a data frame to fill in later
  for(i in 1:length(source)){
    cat("I'm at",i,"th cycle in Search.List() for loop now.\n")
    index <- which(target[,col]==source[i]) # search the source in target, get indexes
    if(length(index)==0){ # if not found
      print("Following cant be found at target vector(Search.List) :")
      print(source[i])    
      }
    else # if found
      Result <- rbind(Result,cbind(rep(source[i],length(index)),target[index,])) #
  }
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
        # target <- target[-index[[j]],] # exclude elements that has been listed, (on test)
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
  # create a directory for tidier work
  main_path <- getwd() # "C:/Users/USER/Desktop/R"
  current_path <- paste(getwd(),paste("DataMatch",sample(100,1),sep="_"),sep = "/") # folder names
  dir.create(current_path) # create a new directory to store files
  setwd(current_path) # set new working directory
  cat("Working directory is set to",current_path,"\n")
  
  # pull out just one month, and sort according to shipping cost
  temp_ship <- Sort(shipfiltered[which(month(shipfiltered$DateShipped)==as.integer(month)),],7,decreasing = TRUE)
  cat("Total",nrow(temp_ship),"rows temp_ship (filtered ship data) has.\n")
  cat("Filtered shipping data is fractionated according to month.\n")
  temp_raw <- Search.List(temp_ship[,ship.sonumber],saledata,sale.sonumber)[,-1] # get corresponding rows of saledata, by sonumber
  print(temp_raw)
  cat("Temporary fractional sale data is formed with corresponding SONumbers.\n")
  rows <- nrow(temp_ship) # get number of rows to use in following for loop
  cat("Levels of temp_ship SONumber :",length(levels(factor(temp_ship[,ship.sonumber]))),"total number of rows in temp_ship :",nrow(temp_ship),"\n")
  
  ## divide into fractions and then unify into one big file
  for(i in begin:iterations){
    cat("Step",i,"\n")
    index <- seq((i-1)*(rows/iterations)+1,i*(rows/iterations)) # determine interval of indexes
    test_match <- Match.rows(temp_ship[index,],ship.sonumber,temp_raw,sale.sonumber) # match data and bind together as a data frame
    check_sen <- test_match[,4]==test_match[,23] & test_match[,5]==test_match[,24] # check receipent and sender zips whether they match
    test_match <- test_match[check_sen,] # take only who match by zips
    write.csv(test_match,paste(filename,i,".csv",sep = ""),row.names = FALSE) # write to file with order
    cat("File",paste(filename,i,".csv",sep = ""),"is saved to",getwd(),"\n") # inform user
  }
  
  Result <- data.frame()
  for(i in 1:iterations){
    Result <- rbind(Result,read.csv(paste(filename,i,".csv",sep = ""),row.names = NULL))
    cat(i,"th file is imported to finally merge together.\n")
  }
  
  setwd(main_path) # reset working directory
  write.csv(Result,paste(filename,".csv",sep = ""),row.names = FALSE)
  cat("File",paste(filename,".csv",sep = ""),"is saved to",getwd(),"\n") # inform user
  
  return(Result)
}

# main (for MONTH 11). note, change four inputs when you want to switch to another month: Shipping_Filtered_Q4.csv,
# Month11.txt, month = 11 (input of the match.shipdata function), and for file write, Most_Expensive_Matched_M11_test_.
Shipping_Filtered <- read.csv("Shipping_Filtered_Q4.csv",row.names=NULL) # for last quarter of 2012
Raw_SaleData <- Format.SaleData(read.table("Month11.txt",sep=",",colClasses = "character")) # for month11, as seen.
print("Sale data is imported")

print("I start Match.ShipData() function.\n")
matched_m11 <- Match.ShipData(Shipping_Filtered,month = 11,Raw_SaleData,filename = "Most_Expensive_Matched_M11_test_")