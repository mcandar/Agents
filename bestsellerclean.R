# FOCUS ON MOST PROFITABLE PRODUCT IN NOVEMBER 2012, Month12.txt (in this case)
source("https://raw.githubusercontent.com/mcandar/Agents/master/appr.R")

Ship <- read.table("ShippingData_Months_10to12.txt",sep = ",",colClasses = "character")
Ship <- Format.ShippingData(Ship)
Raw <- read.table("Month12.txt",sep=",",colClasses = "character")
Raw <- Format.SaleData(Raw)

# create a directory for output files (think for a common nomenclature for all following process)
main_path <- getwd() # "C:/Users/USER/Desktop/R"
current_path <- paste(getwd(),"Month12_MP",sep = "/")
dir.create(current_path)
setwd(current_path)
# setwd(main_path)
month <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
order <- c("1ST","2ND","3RD",paste(c("4","5","6","7","8","9","10"),"TH",sep = ""))
type <- matrix(c("bestseller","mostprofitable","mostordered","BS","MP","MO"),2,3,byrow = TRUE)


# please note that following calculations may take an hour
Top10 <- Product.List(Raw,"mostprofitable",15,savetofile = TRUE,
                      filename = "MP_ProductList_Dec.csv") # get top 10 most profitable items
# Top10 <- Top10[-c(),] # manually exclude unwanted rows
TopProduct <- Top10[1,1] # take the top item
Top_SaleData <- Partial.SaleData(TopProduct,Raw,savetofile = TRUE,
                                 filename = "MP_SALEDATA_DEC.csv") # get its raw sale data
Top_ShipData <- Partial.ShipData(Top_SaleData$SONumber,TopProduct,Ship,furtherinfo = TRUE,
                                 savetofile = TRUE,filename = "MP_SHIPDATA_DEC.csv") # collect shipping data and more
Top_CargoTypes <- CargoTypes(TopProduct,Top_ShipData,savetofile = TRUE,
                             filename = "MP_CARGOTYPES_DEC.csv")
Top_Warehouses <- Warehouses(Top_ShipData$SenderZip,Top_SaleData,Top_ShipData,
                             savetofile = TRUE,filename = "MP_WAREHOUSES_DEC.csv")

# visualize data
MultiGGPlot(Top_CargoTypes[,-1],Top_CargoTypes,main = "December Most Profitable Product Shipping Statistics",
            fname = "Dec_MP_SHIP",colour = Top_CargoTypes$AverageCost,alpha = 1,colourname = "Average Cost",
            device = "pdf")

MultiGGPlot(Top_Warehouses[,-c(3,5,6,7,12,13)],Top_Warehouses[,-c(3,5,6,7,12,13)],main = "Supply of Warehouses December",
            fname = "Dec_WHSUPPLY", colour = Top_Warehouses$Ave.Cost,alpha = 1,colourname = "Average Cost",
            device = "pdf")