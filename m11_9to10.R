#### JUST FOR COMPLETING THE MATCHING OF MONTH 11 ####

# BELOW BLOCK OF CODE IS FOR NOVEMBER DATA MATCHING - MONTH 11
source("https://raw.githubusercontent.com/mcandar/Agents/master/appr.R")
Ship_Filtered_Complete <- read.csv("ShippingData_Filtered_Complete.csv",row.names=NULL)

Raw_M11 <- read.table("Month11.txt",sep=",",colClasses = "character")
Raw_M11 <- Format.SaleData(Raw_M11)

require("lubridate")
temp_ship_m11 <- Sort(Ship_Filtered_Complete[which(
  month(Ship_Filtered_Complete$DateShipped)==11),],7,decreasing = TRUE) # pull out just Month11, and sort according to shipping cost
rows <- nrow(temp_ship_m11) # get number of rows to use in following for loop
## divide into fractions and then unify into one big file, for Month10.txt
it <- 10
for(i in 9:it){
  cat("Step",i,"\n")
  index <- seq((i-1)*(rows/it)+1,i*(rows/it)) # determine interval of indexes
  # following line gets only the corresponding rows according to variable index, contain data for one cycle of the for loop
  temp_raw_m11 <- Search.List(as.integer(levels(factor(temp_ship_m11[index,8]))),Raw_M11,1)[,-1] # get corresponding rows of saledata, by sonumber
  print("I have formed temp_raw_m11, and started matching.")
  test_match <- Match.rows(temp_ship_m11[index,],8,temp_raw_m11,1) # match data and bind together as a data frame
  check_sen <- test_match[,4]==test_match[,23] & test_match[,5]==test_match[,24] # check receipent and sender zips whether they match
  test_match <- test_match[check_sen,] # take only who match by zips
  write.csv(test_match,paste("Most_Expensive_Matched_M11_",i,".csv",sep = ""),row.names = FALSE) # write to file with order
  cat("File",paste("Most_Expensive_Matched_M11_",i,".csv",sep = ""),"is saved to",getwd(),"\n") # inform user
}