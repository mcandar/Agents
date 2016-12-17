# data summaries and visualization from matched data

# FOR MONTH 12
init <- data.frame()
# for(i in 1:10){
#   temp <- read.csv(paste("Most_Expensive_Matched_M12_",i,".csv",sep = ""),row.names = NULL)
#   init <- rbind(init,temp)
# }

write.table(matched_m12,"Most_Expensive_Matched_M12.csv",row.names = FALSE)

matched_m12 <- init

# calculate outcost of each item, by item names
level_m12 <- levels(factor(matched_m12[,25])) # product name levels of the most expensive 100 deliveries in October
output <- as.data.frame(matrix(NA,length(level_m12),8))
colnames(output) <- c("Product","TotalShippingCost","AveShippingCost","AveDistance",
                      "TimesShipped","Price","Weight","AveDuration")
for(i in 1:length(level_m12)){
  ind <- which(matched_m12[,25]==level_m12[i])
  output[i,] <- cbind(level_m12[i],
                      as.numeric(sum(na.omit(matched_m12[ind,7]))),
                      as.numeric(mean(na.omit(matched_m12[ind,7]))),
                      as.numeric(mean(na.omit(matched_m12[ind,20]))),
                      as.numeric(length(ind)),
                      as.numeric(matched_m12[match(level_m12[i],matched_m12[,25]),28]),
                      as.numeric(matched_m12[match(level_m12[i],matched_m12[,25]),26]),
                      as.numeric(mean(na.omit(matched_m12[ind,11]))))
}
class(output$TotalShippingCost) <- "numeric"
# class(output$AveShippingCost) <- "numeric"
# class(output$TimesShipped) <- "numeric"
# class(output$Price) <- "numeric"
# class(output$Weight) <- "numeric"
output <- Sort(output,2,decreasing = TRUE)
output_2 <- Convert(output,col = c(3,4,5,6,7,8))

output_3 <- output_2[which(output_2$TimesShipped>4),] # filter by number of uses, just take which is delivered more than 4
matched_m10_summary <- output_3
matched_m10_summary$Index <- (matched_m10_summary$AveDistance*matched_m10_summary$Weight*matched_m10_summary$Price)/
  (matched_m10_summary$AveShippingCost*matched_m10_summary$Duration)

# same from here
salenum <- data.frame()
for(i in 1:nrow(matched_m10_summary)){
  ind <- which(matched_m10_summary[i,1]==Raw_M10$ItemDescription)
  salenum[i,] <- c(matched_m10_summary[i,1],length(ind))
}

salenum <- as.data.frame(matrix(NA,nrow(matched_m10_summary),2))
for(i in 1:100)#nrow(matched_m10_summary))
  salenum[i,] <- c(matched_m10_summary[i,1],length(which(matched_m10_summary[i,1]==Raw_M10$ItemDescription)))

salenum_list <- as.data.frame(matrix(sapply(matched_m10_summary[,1], 
                                            function(x) c(x,sum(na.omit(Raw_M10[which(x==Raw_M10$ItemDescription),7])))),nrow(matched_m10_summary),2,byrow=TRUE))
# up to here, three algorithms, faster to below, same purpose


# BELOW BLOCK OF CODE IS FOR NOVEMBER DATA MATCHING - MONTH 11
source("https://raw.githubusercontent.com/mcandar/Agents/master/appr.R")
Ship_Filtered_Complete <- read.csv("ShippingData_Filtered_Complete.csv",row.names=NULL)

Raw_M11 <- read.table("Month11.txt",sep=",",colClasses = "character")
Raw_M11 <- Format.SaleData(Raw_M11)

Match.rows <- function(source,col.sou,target,col.tar){
  Result <- data.frame() # initialize a data frame to fill in later
  for(i in 1:length(source[,col.sou])){
    index <- which(target[,col.tar]==source[i,col.sou]) # search the source in target, get indexes
    len <- length(index) # store how many times it is encountered
    if(len!=0){ # if found
      for(j in 1:len){
        Result <- rbind(Result,cbind(source[i,],target[index[[j]],])) # write corresponding matches
        cat(j,"th cycle of multiple matchings in",source[i],"index :",i,"\n")
      }
    }
    else if(len==0){ # if could not be found
      temp <- matrix(NA,1,ncol(target))
      colnames(temp) <- colnames(target)
      Result <- rbind(Result,cbind(source[i,],temp)) # fill with NA's
    }
    cat(i,"th item is found and listed, by Match.Rows() \n")
  }
  return(Result)
}

# FUNCTIONIZE DATA MATCHING
# input ultimate filtered and location data included shipping data, for ONE quarter.
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
  
  # pull out just one month, and sort according to shipping cost
  temp_ship <- Sort(shipfiltered[which(month(shipfiltered$DateShipped)==as.integer(month)),],7,decreasing = TRUE)
  temp_raw <- Search.List(temp_ship[,ship.sonumber],saledata,ship.sonumber)[,-1] # get corresponding rows of saledata, by sonumber
  rows <- nrow(temp_ship) # get number of rows to use in following for loop
  ## divide into fractions and then unify into one big file, for Month10.txt
  for(i in begin:iterations){
    cat("Step",i,"\n")
    index <- seq((i-1)*(rows/it)+1,i*(rows/it)) # determine interval of indexes
    test_match <- Match.rows(temp_ship[index,],ship.sonumber,temp_raw,ship.sonumber) # match data and bind together as a data frame
    check_sen <- test_match[,4]==test_match[,23] & test_match[,5]==test_match[,24] # check receipent and sender zips whether they match
    test_match <- test_match[check_sen,] # take only who match by zips
    write.csv(test_match,paste(filename,i,".csv",sep = ""),row.names = FALSE) # write to file with order
    cat("File",paste(filename,i,".csv",sep = ""),"is saved to",getwd(),"\n") # inform user
  }
  
  Result <- data.frame()
  for(i in 1:iterations)
     Result <- rbind(Result,read.csv(paste(filename,i,".csv",sep = ""),row.names = NULL))
    
  setwd(main_path) # reset working directory
  write.csv(Result,paste(filename,".csv",sep = ""),row.names = FALSE)
  cat("File",paste(filename,".csv",sep = ""),"is saved to",getwd(),"\n") # inform user
  
  return(Result)
}

source("https://raw.githubusercontent.com/mcandar/Agents/master/appr.R")
Ship <- read.table("ShippingData_Months_07to09.txt",sep = ",",colClasses = "character") # read from txt
Ship <- Format.ShippingData(Ship) # format the data frame
ship_filtered_q3 <- Filter.ShippingData(Ship,file.name="Shipping_Filtered_Q3.csv")

# for month 7
Raw_M7 <- read.table("Month7.txt",sep=",",colClasses = "character")
Raw_M7 <- Format.SaleData(Raw_M7)
matched_m7 <- Match.ShipData(ship_filtered_q3,month = 7,Raw_M7,filename = "Most_Expensive_Matched_M7_")

# for month 8
Raw_M8 <- read.table("Month8.txt",sep=",",colClasses = "character")
Raw_M8 <- Format.SaleData(Raw_M8)
matched_m8 <- Match.ShipData(ship_filtered_q3,month = 8,Raw_M8,filename = "Most_Expensive_Matched_M8_")

# for month 9
Raw_M9 <- read.table("Month9.txt",sep=",",colClasses = "character")
Raw_M9 <- Format.SaleData(Raw_M9)
matched_m9 <- Match.ShipData(ship_filtered_q3,month = 9,Raw_M9,filename = "Most_Expensive_Matched_M9_")


# to delete
Raw_M11 <- read.table("Month11.txt",sep=",",colClasses = "character")
Raw_M11 <- Format.SaleData(Raw_M11)
matched_m11 <- Match.ShipData(Ship_Filtered_Complete,month = 11,Raw_M11,filename = "Most_Expensive_Matched_M11_",begin = 9)