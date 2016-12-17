# Filter shipping data - KEEP THIS ORDER OF FILTRATION SAFE
source("https://raw.githubusercontent.com/mcandar/Agents/master/appr.R")

# Import raw data of shipping records, for LAST quarter of 2012 (months 10 to 12)
Ship <- read.table("ShippingData_Months_10to12.txt",sep = ",",colClasses = "character") # read from txt
Ship <- Format.ShippingData(Ship) # format the data frame
# Ship <- read.table("ShippingData_Months_07to09.txt",sep = ",",colClasses = "character") # read from txt
# Ship <- Format.ShippingData(Ship) # format the data frame

# FUNCTIONIZE
Filter.ShippingData <- function(Ship,location.info=TRUE,file.write=TRUE,file.name="Shipping_Filtered.csv"){
  # Filter
  Result <- Ship[-which(Ship$ShippingCost==0),] # filter by cost, exclude transactions with no cost
  Result <- Result[-which(Result$Type == "UPS Ground"),] # filter by type, exclude UPS Ground shipping
  
  # Further filtrate according to shipping types
  typeinfo <- levels.ship(Result,distances = FALSE) # get shipping type information
  print(typeinfo)
  
  # determine to pull out
  print("According to shipping types, take the corresponding data of rows :")
  from <- as.integer(readline(prompt="from : ")) # from which row
  to <- as.integer(readline(prompt="to : ")) # to which row
  Result <- Result[which(Result$Type %in% typeinfo[from:to,1]),] # filter by type
  
  # Collect data for locations
  if(location.info)
    Result <- cbind(Result,LocationData(Result)) # column-bind them together
  
  # Export to csv file
  if(file.write){
    write.csv(Result,file.name,row.names = FALSE)
    cat("File",file.name,"is saved to",getwd(),"\n")
    }
  
  return(Result)
}

# use it
# ship_filtered_q3 <- Filter.ShippingData(Ship,file.name="Shipping_Filtered_Q3.csv")
ship_filtered_q4 <- Filter.ShippingData(Ship,file.name="Shipping_Filtered_Q4.csv")

### THE ORDER OF SHIPPING ###
Filtered <- Ship[-which(Ship$ShippingCost==0),] # filter by cost, exclude transactions with no cost
Filtered <- Filtered[-which(Filtered$Type == "UPS Ground"),] # filter by type, exclude UPS Ground shipping

# Further filtrate according to shipping types
typeinfo <- levels.ship(Filtered,distances = FALSE) # get shipping type information
print(typeinfo)

# determine to pull out
print("According to shipping types, take the corresponding data of rows :")
from <- as.integer(readline(prompt="from : ")) # from which row
to <- as.integer(readline(prompt="to : ")) # to which row
Filtered <- Filtered[which(Filtered$Type %in% typeinfo[from:to,1]),] # filter by type

# Collect data for locations
filtered_temp <- cbind(Filtered,LocationData(Filtered)) # column-bind them together
### ENDS UP RIGHT HERE ###



# to delete
# for a given vector or a list of source, search elements of source in target and list them
Search.List <- function(source,target,col){
  Result <- data.frame() # initialize a data frame to fill in later
  for(i in 1:length(source)){
    index <- which(target[,col]==source[i]) # search the source in target, get indexes
    if (length(index)!=0) # if found
      Result <- rbind(Result,cbind(rep(source[i],length(index)),target[index,])) #
  }
  return(Result)
}

# t <- data.frame(T1=NA,T2=NA)
# for(i in 1:10){
#   temp <- sample(nrow(matched_m10_summary),100)
#   t[i,1] <- system.time({
#     res_1 <- Search.List(matched_m10_summary$Product[temp],Raw_M10,5)
#   })[3]
#   cat("Without checking empty list : t =",t[i,1],"\n")
#   t[i,2] <- system.time({
#     res_2 <- Search.List_test(matched_m10_summary$Product[temp],Raw_M10,5)
#   })[3]
#   cat("checking empty list : t =",t[i,2],"\n")
#   cat("is the same :",identical(res_1,res_2))
# }
# mean(t[,1])
# mean(t[,2])
# sum(t[,1])
# sum(t[,2])