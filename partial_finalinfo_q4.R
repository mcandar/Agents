# getting information according to item name
# PLEASE NOTE THAT THIS CURRENTLY WORKS FOR 3 MONTHS DATA i.e. QUARTERLY
# please specify the time interval below at main section

# import personal library
source("https://raw.githubusercontent.com/mcandar/Agents/master/appr.R")

# function for getting data byname, not ultimate merged data, but 20 columns without sale data
# TIME INTERVAL IS DEPENDENT TO GIVEN DATA SETS, i.e. raw_sale or raw_ship
Filter.ShippingData.Byname <- function(ItemNames, # contains the item names needed to be searched in raw shipdata
                                       raw_sale, # raw sale data to get corresponding SONumbers
                                       raw_ship, # input raw shipping data after it is formatted with Format.ShipData()
                                       location.info=TRUE, # in order to collect location lat/lon info about a row
                                       file.write=TRUE, # true if you want to save to csv file
                                       file.name="Shipping_Filtered.csv", # name of the file to be written
                                       filter = FALSE # filter by no cost and UPS Ground
){
  SONumbers <- raw_sale[which(raw_sale[,5] %in% ItemNames),1] # get corresponding SONumbers
  print("I listed the sonumbers")
  SONumbers <- as.character(levels(factor(SONumbers))) # get levels as character
  print("I factorized the sonumbers")
  print(length(SONumbers))
  
  Result <- raw_ship
  Result <- Result[which(Result$SONumber %in% SONumbers),] # filter by type
  if(filter){
    Result <- Result[-which(Result$ShippingCost==0),] # filter by cost, exclude transactions with no cost
    Result <- Result[-which(Result$Type == "UPS Ground"),] # filter by type, exclude UPS Ground shipping
  }
  print(nrow(Result))
  
  if(location.info) # Collect data for locations
    Result <- cbind(Result,LocationData(Result)) # column-bind them together
  
  if(file.write){ # Export to csv file
    write.csv(Result,file.name,row.names = FALSE)
    cat("File",file.name,"is saved to",getwd(),"\n")
  }
  
  return(Result)
}


# form the ultimate data shape for a given filtered and location data added shipdata
# this function performs matching primarily by SONumbers and seconderily by sender and receipent zips
Match.ShipData <- function(shipfiltered, # ultimate filtered and location data included shipping data 20 columns
                           month, # input a month number as integer
                           saledata, # raw sale data e.g. Month10.txt, Month11.txt etc.
                           iterations = 10, # number of part to divide into when computing
                           filename = "Most_Expensive_Matched_M11_", # file name to be saved
                           begin = 1, # beginning of the for loop, could be continued from other steps
                           ship.sonumber = 8, # sonumber column in filtered shipping data
                           sale.sonumber = 1 # sonumber column in filtered sale data
){
  # create a directory for tidier work
  main_path <- getwd()
  current_path <- paste(getwd(),paste("DataMatch",sample(1000,1),sep="_"),sep = "/") # folder names
  dir.create(current_path) # create a new directory to store files
  setwd(current_path) # set new working directory
  
  # pull out just one month, and sort according to shipping cost
  temp_ship <- Sort(shipfiltered[which(lubridate::month(shipfiltered$DateShipped)==as.integer(month)),],7,decreasing = TRUE)
  temp_raw <- Search.List(temp_ship[,ship.sonumber],saledata,sale.sonumber)[,-1] # get corresponding rows of saledata, by sonumber
  rows <- nrow(temp_ship) # get number of rows to use in following for loop
  
  ## divide into fractions and then unify into one big file
  # a user can start the procedure from which step it is stopped.
  for(i in begin:iterations){
    cat("Step",i,"\n")
    index <- seq((i-1)*(rows/iterations)+1,i*(rows/iterations)) # determine interval of indexes
    test_match <- Match.rows(temp_ship[index,],ship.sonumber,temp_raw,sale.sonumber) # match data and bind together as a data frame
    validation <- test_match[,4]==test_match[,23] & test_match[,5]==test_match[,24] # check receipent and sender zips whether they match
    test_match <- test_match[validation,] # take only who match by zips
    write.csv(test_match,paste(filename,i,".csv",sep = ""),row.names = FALSE) # write to file with order
    cat("File",paste(filename,i,".csv",sep = ""),"is saved to",getwd(),"\n") # inform user
  }
  
  # combine the fractions
  Result <- data.frame()
  for(i in 1:iterations)
    Result <- rbind(Result,read.csv(paste(filename,i,".csv",sep = ""),row.names = NULL))
  
  # reset working directory
  setwd(main_path)
  write.csv(Result,paste(filename,".csv",sep = ""),row.names = FALSE)
  cat("File",paste(filename,".csv",sep = ""),"is saved to",getwd(),"\n") # inform user
  
  return(Result)
}

# main
Ship <- Format.ShippingData(read.table("ShippingData_Months_07to09.txt",sep = ",",colClasses = "character"))
current_item <- as.character(read.csv("/home/candar/Data_Final/final_summary.csv",row.names = NULL)[1:10,1])
print(current_item)
file_name <- "Q4_OC_TOP_FINALDATA_TEST" # name of the final output file and temporary files
output <- data.frame() # final output

# from months 7 to 9
for(i in 7:9){
  cat("Working on month",i,"\n")
  Raw <- Format.SaleData(read.table(paste("Month",i,".txt",sep = ""),sep=",",colClasses = "character")) # import and format data
  filtered_byname <- Filter.ShippingData.Byname(current_item,raw_sale = Raw,raw_ship = Ship,file.write = FALSE) # monthly
  merged_temp <- Match.ShipData(filtered_byname,month = i,Raw,filename = paste(file_name,"M",i,sep = "")) # get corresponding SONumbers, monthly
  output <- rbind(output,merged_temp[which(merged_temp$ItemDescription %in% current_item),]) # take out irrelevant names
}

write.csv(output,paste(file_name,"final.csv",sep = ""),row.names=FALSE) # rewrite the ultimate filtered data