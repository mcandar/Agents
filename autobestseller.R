# FOCUS ON MOST PROFITABLE PRODUCT IN NOVEMBER 2012, Month10.txt,Month11.txt and Month12.txt (in this case)
source("https://raw.githubusercontent.com/mcandar/Agents/master/appr.R")

# nomenclature
month <- matrix(c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC",paste(c("Month"),1:12,sep="")),
                nrow = 2,ncol = 12,byrow = TRUE)
Month <- paste(month[2,],".txt",sep="") # names of sale data files
# order <- c("1ST","2ND","3RD",paste(c("4","5","6","7","8","9","10"),"TH",sep = ""))
type <- matrix(c("bestseller","mostprofitable","mostordered","BS","MP","MO"),2,3,byrow = TRUE)

Ship <- read.table("ShippingData_Months_10to12.txt",sep = ",",colClasses = "character")
Ship <- Format.ShippingData(Ship)

for(i in 10:12){
  Raw <- read.table(Month[i],sep=",",colClasses = "character") # take months from 10 to 12
  Raw <- Format.SaleData(Raw)
  for(j in 1:3){ # this is fixed
    # create a directory for output files (think for a common nomenclature for all following process)
    main_path <- getwd() # "C:/Users/USER/Desktop/R"
    current_path <- paste(getwd(),paste(month[2,i],type[2,j],sep="_"),sep = "/") # folder names
    dir.create(current_path) # create a new directory to store files
    setwd(current_path) # set new working directory
    
    Top10 <- Product.List(Raw,type[1,j],15,savetofile = TRUE,
                          filename = paste(type[2,j],"_ProductList_",month[1,i],".csv",sep="")) # get top 10 most profitable items
    
    for(k in 1:15){
      if(Top10[k,1] != "Electronic Waste Recycling Act15-34" &&
         Top10[k,1] != "1 Yr Service Net Replacement Plan" &&
         Top10[k,1] != "GIFT FREE CPU MAGAZINE COUPON" &&
         Top10[k,1] != "Newegg Promotional Gift Card"){ # should-exclude items
        TopProduct <- Top10[k,1] # take the top item
        break
      }
    }
    cat("Top product as",type[1,j],"in",month[1,i],"is",TopProduct,"\n")
    
    Top_SaleData <- Partial.SaleData(TopProduct,Raw,savetofile = TRUE,
                                     filename = paste(type[2,j],"_SALEDATA_",month[1,i],".csv",sep="")) # get its raw sale data
    Top_ShipData <- Partial.ShipData(Top_SaleData$SONumber,TopProduct,Ship,furtherinfo = TRUE,
                                     savetofile = TRUE,filename = paste(type[2,j],"_SHIPDATA_",month[1,i],".csv",sep="")) # collect shipping data and more
    Top_CargoTypes <- CargoTypes(TopProduct,Top_ShipData,savetofile = TRUE,
                                 filename = paste(type[2,j],"_CARGOTYPES_",month[1,i],".csv",sep=""))
    Top_Warehouses <- Warehouses(Top_ShipData$SenderZip,Top_SaleData,Top_ShipData,
                                 savetofile = TRUE,filename = paste(type[2,j],"_WAREHOUSES_",month[1,i],".csv",sep=""))
    
    # visualize data
    MultiGGPlot(Top_CargoTypes[,-1],Top_CargoTypes,main = paste(month[1,i],type[1,j],"Product Shipping Statistics"),
                fname = paste(month[1,i],type[2,j],"SHIP",sep="_"),colour = Top_CargoTypes$AverageCost,alpha = 1,
                colourname = "Average Cost",device = "pdf")
    
    MultiGGPlot(Top_Warehouses[,-c(3,5,6,7,12,13)],Top_Warehouses[,-c(3,5,6,7,12,13)],
                main = paste(month[1,i],type[1,j],"Product Supply of Warehouses"),
                fname = paste(month[1,i],type[2,j],"WHSUPPLY",sep="_"),colour = Top_Warehouses$Ave.Cost,alpha = 1,
                colourname = "Average Cost",device = "pdf")
    setwd(main_path) # reset working directory
  }
}
