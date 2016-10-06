# focus on top items
Ship <- read.table("ShippingData_Months_10to12.txt",sep = ",",colClasses = "character")
Ship <- Format.ShippingData(Ship)
Raw <- read.table("Month10.txt",sep=",",colClasses = "character")
Raw <- Format.SaleData(Raw)

products <- data.frame(Name=levels(factor(Raw[,5])),Quantity=NA) # see and store how many different products

# be careful, both data must be white-space trimmed, even the lists must be. following line of code
# is like a for loop, calculates how much an item sold
products[,2] <- sapply(products[,1],function(x) sum(Raw[which(Raw[,5] == x),7])) # rowwise

# product information is backed up with following file, one can reset at anytimeby it
products <- read.csv("Products_M10.csv") # read previously written file (for this time)
MS <- data.frame(Name=Sort(products,2,decreasing = TRUE)[1:13,],Profit=NA,Price=NA,Orders=NA) # intentionally taken 3 more rows for the reason at following line
MS <- MS[-c(1,7,11),] # remove unwanted products or services (this should be changed according to file)
Raw$Total <- Raw$UnitsShipped*Raw$AverageUnitPrice # get the profit from that product

for(i in 1:nrow(MS)){
  index <- which(Raw[,5] == MS[i,1])
  MS[i,c(3,4,5)]<- c(sum(Raw[index,9]),Raw[index[1],8],length(index)) # find profit, price and number of orders
}

# MultiGGPlot(MS,MS,main="October 10 Most Profitable Products",fname="Oct_10",colour = MS$Profit,
#             colourname = "GrossProfit",alpha = 1,device = "pdf")
# MultiGGPlot(MS10,MS10,main="October 10 Bestseller Products",fname="Oct_10",colour = MS10$Profit,
#             colourname = "GrossProfit",alpha = 1,device = "pdf")

BSSaleData <- Search.List(MS[1,1],Raw,5)[,-1] # get the bestseller information from saledata
BSSaleData <- Sort(BSSaleData,1) # sort to reduce complexity
mylevels <- levels(factor(BSSaleData[,1])) # see levels of SONumbers

# convert to characters for faster searching, then find indexes that contain searched SONumber
ShipSONumber <- as.character(Ship[,8])
index <- list(a=c(1,2,3),b=c(1,2)) # arbitrarily initialize, let it store rows with various lengths
for(i in 1:length(mylevels))
  index[[i]] <- which(mylevels[i]==ShipSONumber)

# get corresponding shipping data from indexes
BSShipData <- data.frame()
for(i in 1:length(index))
  BSShipData <- rbind(BSShipData,Ship[index[[i]],])

# further information for BestSeller item's Shipping Data
levels(factor(BSShipData$Type)) # different shipment types
mean(na.omit(BSShipData$Duration)) # average delivery day
mean(na.omit(BSShipData$ShippingCost)) # average cost

# get coordinates as lat and lon from zips
crdns <- zip.coordinates(BSShipData[,4]) # find coordinates of sender zips
BSShipData$S.Lat <- crdns[,1]
BSShipData$S.Lon <- crdns[,2]
crdns <- zip.coordinates(BSShipData[,5]) # find coordinates of receipent zips
BSShipData$R.Lat <- crdns[,1]
BSShipData$R.Lon <- crdns[,2]

# get location names from zips
lctns <- zip.location(BSShipData[,4]) # find city/state of sender zips
BSShipData$S.City <- lctns[,1]
BSShipData$S.StateCode <- lctns[,2]
lctns <- zip.location(BSShipData[,5]) # find city/state of receipent zips
BSShipData$R.City <- lctns[,1]
BSShipData$R.StateCode <- lctns[,2]

# calculate the distance between supplier and customer
require(geosphere)
Dist <- distHaversine(cbind(as.numeric(BSShipData$S.Lon),as.numeric(BSShipData$S.Lat)),
                      cbind(as.numeric(BSShipData$R.Lon),as.numeric(BSShipData$R.Lat))) # in meters
BSShipData$Distance <- round(Dist/1000,3) # as kilometers
temp <- BSSaleData[1,5]
BSShipData$Product <- temp
# backupBSShipData <- BSShipData
############################################################## END FUNCTION
# shippnig costs
cargotypes <- data.frame(Product=BSSaleData[1,5],Type=levels(factor(BSShipData[,6])),AverageCost=NA,TotalUse=NA,
                         AverageDuration=NA,TotalCost=NA,AverageDistance=NA)
cargotypes[,3] <- sapply(cargotypes[,2], function(x) mean(BSShipData[which(BSShipData[,6]==x),7]))
cargotypes[,4] <- sapply(cargotypes[,2], function(x) length(BSShipData[which(BSShipData[,6]==x),7]))
cargotypes[,5] <- sapply(cargotypes[,2], function(x) mean(na.omit(BSShipData[which(BSShipData[,6]==x),11])))
cargotypes[,6] <- sapply(cargotypes[,2], function(x) sum(BSShipData[which(BSShipData[,6]==x),7]))
cargotypes[,7] <- sapply(cargotypes[,2], function(x) mean(na.omit(BSShipData[which(BSShipData[,6]==x),16])))
cargotypes <- Sort(cargotypes,3,decreasing = TRUE)

MultiGGPlot(cargotypes[,-1],cargotypes,main = "October Bestseller Product Shipping Statistics",
            fname = "Oct_BS_SHIP",colour = cargotypes$AverageCost,alpha = 1,colourname = "Average Cost")

# choose different cargo types
BSShipData <- BSShipData[which(BSShipData[,6] %in% as.character(cargotypes[1:5,2])),] # extract just some of them
levels(factor(BSShipData$Type)) # see what's included
# BSShipData <- backupBSShipData # reset if needed
wh_bs <- Warehouses(as.character(levels(factor(BSShipData$SenderZip))),BSSaleData,BSShipData)
# adjust a legend, determine each color's name (INCOMP)
# my_types <- with(BSShipData, cut(Type, quantile(Type), include.lowest = T))
# levels(df$q) <- paste(c("1st", "2nd", "3rd", "4th"), "Quantile")
# my_types <- as.ordered(BSShipData$Type)

###### ORGANIZE BELOW !!! ########
# see receipent and sender locations (organize them)
Points1 <- BSShipData[,c(12,13,17,18)]
colnames(Points1) <- c("v1","v2","v3","v4")
Points2 <- BSShipData[,c(14,15,19,20)]
colnames(Points2) <- c("v1","v2","v3","v4")
Points <- rbind(Points1,Points2) # sender and receipent respectively
Points$Class <- c(rep(1,nrow(BSShipData)),rep(2,nrow(BSShipData)))
Points$Type <- c(rep("Warehouse",nrow(BSShipData)),rep("Customer",nrow(BSShipData)))
Points$Hover <- paste(Points[,6],"<br>",Points[,3],",",Points[,4],sep = "")
Points$Color <- c(rep("green",nrow(BSShipData)),rep("blue",nrow(BSShipData)))
colnames(Points) <- c("Lat","Lon","City","StateCode","Class","Type","Hover","Color")

### PLOT THE MAP ###
library(plotly)
# map projection
geo <- list(
  scope = 'north america',
  projection = list(type = 'azimuthal equal area'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80")
)

p <- plot_ly(Points, lon = Lon, lat = Lat, text = Hover, type = 'scattergeo',
             locationmode = 'USA-states', marker = list(size = Class*2, color = Color),
             inherit = FALSE) %>%
  add_trace(lon = list(S.Lon,R.Lon), lat = list(S.Lat,R.Lat),group = 1:nrow(BSShipData),# opacity = Distance/(max(Distance)*10)
            opacity = (Distance/(max(Distance)*3))^(1/2), data = BSShipData,
            mode = 'lines', line = list(width = (Distance/(max(Distance)*3))^(1/4), color = Type),
            type = 'scattergeo', locationmode = 'USA-states') %>%
  layout(title = 'October Bestseller Product Shipping Paths<br>of the most sold 100 product (in meters)',
         geo = geo, showlegend = TRUE, height=800)
p

htmlwidgets::saveWidget(as.widget(p), "USA_MAP_BESTSELLER_SHIPPINGTYPES_OCT.html")
# htmlwidgets::saveWidget(as.widget(p), "USA_MAP_BESTSELLER_PRODUCT_UPS2NDDAY_OCT.html")

# Following nomenclature is aimed
# colnames(Lines) <- c("ProductName","SONumber","Units","Price","Profits","SenderZip","ReceipentZip","S.Lat","S.Lon",
#                      "R.Lat","R.Lon","Distance","S.City","S.StateCode","R.City","R.StateCode","DateShipped",
#                      "DateArrived","ShippingCost")

# upload to plot.ly for further demonstration and presentation purposes
# these work in server, one can use it instead of saving the plot locally through htmlwidgets package
Sys.setenv("plotly_username"="mcandar")
Sys.setenv("plotly_api_key"="z1odz2cuww")
plotly_POST(p, filename = "USA_LINE_MAP", world_readable=TRUE)

#############################################################################################
#############################################################################################
#############################################################################################

# FOCUS ON THE MOST PROFITABLE PRODUCT
products <- data.frame(Name=levels(factor(Raw[,5])),Profit=NA) # see and store how many different products
# contribution to gross profit per item
products[,2] <- sapply(as.character(products[,1]),function(x) sum(Raw[which(Raw[,5] == x),9]))
products <- read.csv("PRODUCTS_PROFIT_OCT.csv") # load from a previously collected data

MS <- as.data.frame(matrix(NA,13,5))
colnames(MS) <- c("Name","Units","Profit","Price","Orders") # intentionally taken 3 more rows for the reason at following line
MS[,1] <- Sort(products,2,decreasing = TRUE)[1:13,1]
MS[,3] <- Sort(products,2,decreasing = TRUE)[1:13,2]
MS <- MS[-c(11,12,13),] # remove unwanted products or services (this should be changed according to file)
# Raw$Total <- Raw$UnitsShipped*Raw$AverageUnitPrice # get the profit from that product

for(i in 1:nrow(MS)){
  index <- which(Raw[,5] == MS[i,1])
  MS[i,c(2,4,5)]<- c(sum(Raw[index,7]),Raw[index[1],8],length(index)) # find profit, price and number of orders
}

MPSaleData <- Search.List(MS[1,1],Raw,5)[,-1] # get the bestseller information from saledata
MPSaleData <- Sort(MPSaleData,1) # sort to reduce complexity
mylevels <- levels(factor(MPSaleData[,1])) # see levels of SONumbers

# convert to characters for faster searching, then find indexes that contain searched SONumber
ShipSONumber <- as.character(Ship[,8])
index <- list(a=c(1,2,3),b=c(1,2)) # arbitrarily initialize, let it store rows with various lengths
for(i in 1:length(mylevels))
  index[[i]] <- which(mylevels[i]==ShipSONumber)

# get corresponding shipping data from indexes
MPShipData <- data.frame()
for(i in 1:length(index))
  MPShipData <- rbind(MPShipData,Ship[index[[i]],])

# further information for BestSeller item's Shipping Data
levels(factor(MPShipData$Type)) # different shipment types
mean(na.omit(MPShipData$Duration)) # average delivery day
mean(na.omit(MPShipData$ShippingCost)) # average cost

# get coordinates as lat and lon from zips
crdns <- zip.coordinates(MPShipData[,4]) # find coordinates of sender zips
MPShipData$S.Lat <- crdns[,1]
MPShipData$S.Lon <- crdns[,2]
crdns <- zip.coordinates(MPShipData[,5]) # find coordinates of receipent zips
MPShipData$R.Lat <- crdns[,1]
MPShipData$R.Lon <- crdns[,2]

# get location names from zips
lctns <- zip.location(MPShipData[,4]) # find city/state of sender zips
MPShipData$S.City <- lctns[,1]
MPShipData$S.StateCode <- lctns[,2]
lctns <- zip.location(MPShipData[,5]) # find city/state of receipent zips
MPShipData$R.City <- lctns[,1]
MPShipData$R.StateCode <- lctns[,2]

# calculate the distance between supplier and customer
require(geosphere)
Dist <- distHaversine(cbind(as.numeric(MPShipData$S.Lon),as.numeric(MPShipData$S.Lat)),
                      cbind(as.numeric(MPShipData$R.Lon),as.numeric(MPShipData$R.Lat))) # in meters
MPShipData$Distance <- round(Dist/1000,3) # as kilometers
temp <- MPSaleData[1,5]
MPShipData$Product <- temp

levels(factor(MPShipData$Type)) # different shipment types
mean(na.omit(MPShipData$Duration)) # average delivery day
mean(na.omit(MPShipData$ShippingCost)) # average cost

cargotypes_MP <- data.frame(Product=MPSaleData[1,5],
                            Type=levels(factor(MPShipData[,6])),
                            AverageCost=NA,
                            TotalUse=NA,
                            AverageDuration=NA,
                            TotalCost=NA,
                            AverageDistance=NA)

cargotypes_MP[,3] <- sapply(cargotypes_MP[,2], function(x) mean(MPShipData[which(MPShipData[,6]==x),7]))
cargotypes_MP[,4] <- sapply(cargotypes_MP[,2], function(x) length(MPShipData[which(MPShipData[,6]==x),7])) # simplify
cargotypes_MP[,5] <- sapply(cargotypes_MP[,2], function(x) mean(na.omit(MPShipData[which(MPShipData[,6]==x),11])))
cargotypes_MP[,6] <- sapply(cargotypes_MP[,2], function(x) sum(MPShipData[which(MPShipData[,6]==x),7]))
cargotypes_MP[,7] <- sapply(cargotypes_MP[,2], function(x) mean(na.omit(MPShipData[which(MPShipData[,6]==x),20])))
cargotypes_MP <- Sort(cargotypes_MP,3,decreasing = TRUE)

# plot graphs
MultiGGPlot(cargotypes_MP[,-c(1,2)],cargotypes_MP,main = "October The Most Profitable Product Shipping Statistics",
            fname = "Oct_MP_SHIP",colour = cargotypes_MP$AverageCost,alpha = 1,colourname = "Average Cost")

### VISUALIZE ON MAP ###

# choose different cargo types
MPShipData <- MPShipData[which(MPShipData[,6] %in% as.character(cargotypes_MP[1:5,2])),] # extract just some of them
levels(factor(MPShipData$Type)) # see what's included
MPShipData <- MPShipData[which(MPShipData$SenderZip %in% as.character(WareHousesZip[c(3,4),1])),]
# MPShipData <- backupMPShipData # reset if needed

###### ORGANIZE BELOW !!! ########
# see receipent and sender locations (organize them)
Points1 <- MPShipData[,c(12,13,17,18)]
colnames(Points1) <- c("v1","v2","v3","v4")
Points2 <- MPShipData[,c(14,15,19,20)]
colnames(Points2) <- c("v1","v2","v3","v4")
Points <- rbind(Points1,Points2) # sender and receipent respectively
Points$Class <- c(rep(1,nrow(MPShipData)),rep(2,nrow(MPShipData)))
Points$Type <- c(rep("Warehouse",nrow(MPShipData)),rep("Customer",nrow(MPShipData)))
Points$Hover <- paste(Points[,6],"<br>",Points[,3],",",Points[,4],sep = "")
Points$Color <- c(rep("green",nrow(MPShipData)),rep("blue",nrow(MPShipData)))
colnames(Points) <- c("Lat","Lon","City","StateCode","Class","Type","Hover","Color")

library(plotly)

# map projection
geo <- list(
  scope = 'north america',
  projection = list(type = 'azimuthal equal area'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80")
)

p <- plot_ly(Points, lon = Lon, lat = Lat, text = Hover, type = 'scattergeo',
             locationmode = 'USA-states', marker = list(size = Class*2, color = Color),
             inherit = FALSE) %>%
  add_trace(lon = list(S.Lon,R.Lon), lat = list(S.Lat,R.Lat),group = 1:nrow(MPShipData),# opacity = Distance/(max(Distance)*10)
            opacity = (Distance/(max(Distance)*3))^(1/2), data = MPShipData,
            mode = 'lines', line = list(width = (Distance/(max(Distance)*3))^(1/4), color = Type),
            type = 'scattergeo', locationmode = 'USA-states') %>%
  layout(title = 'October Bestseller Product Shipping Paths<br>of the most sold 100 product (in meters)',
         geo = geo, showlegend = FALSE, height=800)
p

# htmlwidgets::saveWidget(as.widget(p), "USA_MAP_MOSTPROFITABLE_SHIPPINGTYPES_OCT.html")
# htmlwidgets::saveWidget(as.widget(p), "USA_MAP_MOSTPROFITABLE_WAREHOUSES_OCT.html")
htmlwidgets::saveWidget(as.widget(p), "USA_MAP_WAREHOUSES_OCT.html")

#############################################################################################
#############################################################################################
#############################################################################################

# List the warehouses a product is sent from
WareHousesZip <- Search.List(as.character(levels(factor(MPSaleData$SenderZip))),Zips,1)[,-1]
WareHousesZip$Uses <- sapply(WareHousesZip[,1], function(x) length(which(MPSaleData$SenderZip==x)))
WareHousesZip$Units <- sapply(WareHousesZip[,1], function(x) sum(MPSaleData[which(MPSaleData$SenderZip==x),7]))
WareHousesZip$Ave.Distance <- sapply(WareHousesZip[,1], 
                                     function(x) mean(na.omit(MPShipData[which(MPShipData$SenderZip==x),20])))
WareHousesZip$Ave.Duration <- sapply(WareHousesZip[,1], 
                                     function(x) mean(na.omit(MPShipData[which(MPShipData$SenderZip==x),11])))
WareHousesZip$Ave.Cost <- sapply(WareHousesZip[,1], 
                                 function(x) mean(na.omit(MPShipData[which(MPShipData$SenderZip==x),7])))

# find center of mass, first convert lattitude and longitude to numeric
MPShipData <- Convert(MPShipData,c(12,13,14,15))
WareHousesZip <- Convert(WareHousesZip,c(6,7))
WareHousesZip$Ave.R.Lat <- sapply(WareHousesZip[,6],
                                  function(x) mean(na.omit(MPShipData[which(MPShipData[,14]==x),14])))
WareHousesZip$Ave.R.Lon <- sapply(WareHousesZip[,7],
                                  function(x) mean(na.omit(MPShipData[which(MPShipData[,15]==x),15])))
WareHousesZip <- Sort(WareHousesZip,9,decreasing = TRUE)

MultiGGPlot(WareHousesZip[,-c(3,5,6,7,12,13)],WareHousesZip[,-c(3,5,6,7,12,13)],main = "Supply of Warehouses October",
            fname = "OCT_WHSUPPLY", colour = WareHousesZip$Ave.Cost,alpha = 1,colourname = "Average Cost")

# collect and organize provider's data
Warehouses <- function(senderzips,      # input levels of supplier postal codes
                       saledata,        # input sale data
                       shipdata,        # input shipping data
                       col.zip=4,       # column number of sender zips in SHIPDATA
                       col.zip.sale=3,  # column number of sender zips in SALEDATA
                       col.unit=7,      # col number of units in SALEDATA
                       col.distance=20, # col number in shipdata
                       col.duration=11, # in shipdata
                       col.cost=7,      # in shipdata
                       r.lat=14,        # in shipdata
                       r.lon=15){       # in shipdata
  
  if(!exists("Zips")) Zips <- GetZips() # if zip database not exists, import it.
  
  Result <- Search.List(senderzips,Zips,1)[,-1]
  Result <- cbind(Result,Uses=NA,Units=NA,Ave.Distance=NA,Ave.Duration=NA,Ave.Cost=NA,Ave.R.Lat=NA,Ave.R.Lon=NA)
  shipdata <- Convert(shipdata,c(12,13,14,15))
  Result <- Convert(Result,c(6,7))
  
  for(i in 1:nrow(Result)){
    index <- which(shipdata[,col.zip]==Result[i,1])
    Result[i,c(8,9,10,11,12,13,14)] <- c(length(index), # uses
                                         sum(saledata[which(saledata[,col.zip.sale]==Result[i,1]),col.unit]), # units 
                                         mean(na.omit(shipdata[index,col.distance])), # average distance
                                         mean(na.omit(shipdata[index,col.duration])), # average duration
                                         mean(na.omit(shipdata[index,col.cost])),     # average cost
                                         mean(na.omit(shipdata[which(shipdata[,r.lat]==Result[i,6]),r.lat])),
                                         mean(na.omit(shipdata[which(shipdata[,r.lon]==Result[i,7]),r.lon]))
    )
  }
  
  Result <- Sort(Result,9,decreasing = TRUE)
  return(Result)
}

# collect and organize data of shipping types
CargoTypes <- function(product,          # name of the product
                       shipdata,         # input shipping data (from Partial.Shipdata), NOT the raw shipping data
                       col.type=6,       # number of the column which contains shipment types in shipdata
                       col.cost=7,       # column number of the shipping costs in shipdata
                       col.duration=11,  # in shipdata
                       col.distance=20){ # in shipdata
  
  Result <- data.frame(Product=product,
                       Type=levels(factor(shipdata[,col.type])),
                       AverageCost=NA,
                       TotalUse=NA,
                       AverageDuration=NA,
                       TotalCost=NA,
                       AverageDistance=NA)
  
  Result[,3] <- sapply(Result[,2], function(x) mean(shipdata[which(shipdata[,col.type]==x),col.cost])) # average cost
  Result[,4] <- sapply(Result[,2], function(x) length(which(shipdata[,col.type]==x))) # total uses
  Result[,5] <- sapply(Result[,2], function(x) mean(na.omit(shipdata[which(shipdata[,col.type]==x),col.duration]))) # ave. dur.
  Result[,6] <- sapply(Result[,2], function(x) sum(shipdata[which(shipdata[,col.type]==x),col.cost])) # total cost
  Result[,7] <- sapply(Result[,2], function(x) mean(na.omit(shipdata[which(shipdata[,col.type]==x),col.distance]))) # ave. dist.
  Result <- Sort(Result,3,decreasing = TRUE)
  return(Result)
}




# write and optimize a function for easier obtaining of a product's shipping data 
Partial.ShipData <- function(sonumber,           # complete list of sonumbers from shipdata
                             product,            # name of the product, just one name, not a set of names
                             ship,               # raw ship data, i.e. ShippingData_Mont...txt
                             col.sonumber=8,     # number of the column containing sonumbers in raw ship data
                             furtherinfo=FALSE){ # show detailed info 
  mylevels <- as.character(levels(factor(sonumber))) # see levels of SONumbers
  
  # convert to characters for faster searching, then find indexes that contain searched SONumber
  ShipSONumber <- as.character(ship[,col.sonumber])
  index <- list(a=c(1,2,3),b=c(1,2)) # arbitrarily initialize, let it store rows with various lengths
  for(i in 1:length(mylevels))
    index[[i]] <- which(mylevels[i]==ShipSONumber)
  print("Indexes determined.")
  
  # get corresponding shipping data from indexes
  Result <- data.frame()
  for(i in 1:length(index))
    Result <- rbind(Result,ship[index[[i]],])
  print("Base data is extracted from raw shipping data.")
  
  # declare and preallocate new columns
  Result <- cbind(Result,S.Lat=NA,S.Lon=NA,R.Lat=NA,R.Lon=NA,S.City=NA,S.StateCode=NA,R.City=NA,
                  R.StateCode=NA,Distance=NA,Product=NA)
  
  # get coordinates as lat and lon from zips
  Result[,c(12,13)] <- zip.coordinates(Result[,4])[,c(1,2)] # find coordinates of sender zips
  Result[,c(14,15)] <- zip.coordinates(Result[,5])[,c(1,2)] # find coordinates of receipent zips
  
  # get location names from zips
  Result[,c(16,17)] <- zip.location(Result[,4])[,c(1,2)] # find city/state of sender zips
  Result[,c(18,19)] <- zip.location(Result[,5])[,c(1,2)] # find city/state of receipent zips
  
  # calculate the distance between supplier and customer
  require(geosphere)
  Dist <- distHaversine(cbind(as.numeric(Result$S.Lon),as.numeric(Result$S.Lat)),
                        cbind(as.numeric(Result$R.Lon),as.numeric(Result$R.Lat))) # in meters
  Result$Distance <- round(Dist/1000,3) # as kilometers
  Result$Product <- product # add product name to prevent confusions
  print("Location information is collected.")
  
  # further information for BestSeller item's Shipping Data
  if(furtherinfo){
    paste("\nFurther information:")
    paste("There are",levels(factor(Result$Type)),"different shipment types.") # different shipment types
    paste("Average delivery duration is",mean(na.omit(Result$Duration)),"days.") # average delivery day
    paste("Average cost for each delivery is",mean(na.omit(Result$ShippingCost)),"USD.") # average cost
  }
  return(Result)
}

# list the best products according to 3 type and number of products, e.g. top 100, top 10 etc.
Product.List <- function(saledata,type="bestseller",limit=110){   
  # be careful, both data must be white-space trimmed, even the lists must be. 
  products <- data.frame(Name=levels(factor(saledata[,5]))) # see and store how many different products
  saledata$Total <- saledata$UnitsShipped*saledata$AverageUnitPrice # get the profit from that product
  switch (type,
          "bestseller" = {
            #following line of code is like a for loop, calculates how much an item sold
            products$Quantity <- sapply(products[,1],function(x) sum(saledata[which(saledata[,5] == x),7])) # rowwise
            temp <- Sort(products,2,decreasing = TRUE)[1:limit,] # get the sorted array, name and quantity
            Result <- data.frame(Name=temp[,1],Quantity=temp[,2],Profit=NA,Price=NA,Orders=NA)
            for(i in 1:nrow(Result)){
              index <- which(saledata[,5] == Result[i,1])
              Result[i,c(3,4,5)]<- c(sum(saledata[index,9]), # total profit from that item
                                     saledata[index[1],8],   # price of the item
                                     length(index))          # number of orders
            }
            Result <- Sort(Result,2,decreasing = TRUE) # sort w.r.t. quantity
          },
          "mostprofitable" = {
            products$Profit <- sapply(products[,1],function(x) sum(saledata[which(saledata[,5] == x),9])) # rowwise
            temp <- Sort(products,2,decreasing = TRUE)[1:limit,] # get the sorted array, name and profit
            Result <- data.frame(Name=temp[,1],Quantity=NA,Profit=temp[,2],Price=NA,Orders=NA)
            for(i in 1:nrow(Result)){
              index <- which(saledata[,5] == Result[i,1])
              Result[i,c(2,4,5)]<- c(sum(saledata[index,7]), # sold quantity
                                     saledata[index[1],8],   # price of the item
                                     length(index))          # number of orders
            }
            Result <- Sort(Result,3,decreasing = TRUE) # sort w.r.t. profit
          },
          "mostordered" = {
            products[,2] <- sapply(products[,1],function(x) length(which(saledata[,5] == x)))
            temp <- Sort(products,2,decreasing = TRUE)[1:limit,] # get the sorted array, name and orders
            Result <- data.frame(Name=temp[,1],Quantity=NA,Profit=NA,Price=NA,Orders=temp[,2])
            for(i in 1:nrow(Result)){
              index <- which(saledata[,5] == Result[i,1])
              Result[i,c(3,4,2)]<- c(sum(saledata[index,9]), # total profit from that item
                                     saledata[index[1],8],   # price of the item
                                     sum(saledata[index,7])) # sold quantity
            }
            Result <- Sort(Result,5,decreasing = TRUE) # sort w.r.t. profit
          }
  )
  return(Result)
}

wh_bs <- Warehouses(BSShipData$SenderZip,BSSaleData,BSShipData,col.distance = 16)
wh_mp <- Warehouses(MPShipData$SenderZip,MPSaleData,MPShipData)

MultiGGPlot(wh_bs[,-c(4,5,6,7,13,14)],wh_bs[,-c(3,4,5,6,7,13,14)],main = "Bestseller Item Supply of Warehouses October",fname = "Oct_WHSUPPLY",
            colour = wh_mp$Ave.Cost,colourname = "Average Cost",alpha = 1,device = "pdf")


# multiplotly function

MultiPlotly <- function(x,                    # data of x axis
                        y,                 # data set of y axis
                        main = "GGPlot",      # main title
                        fname = "GG_",        # prefix for filename
                        color = "black",
                        mode = "markers",
                        text = NULL,
                        size = NULL
){      # height as inches
  
  require(plotly)
  if(is.null(colnames(x)))
    x <- as.data.frame(x) # convert to data frame if not already, (x maybe a multicolumn object too)
  
  f <- list(family = "Verdana, monospace",size = 18,color = "#7f7f7f")
  
  for(j in 1:ncol(x)){ # 1 through total column number (originally 1:dim(x)[2])
    for(i in 1:ncol(y)){
      if (identical(x[,j],y[,i])) next()
      filename <- paste(fname,"_",i+((j-1)*ncol(y)),"_",colnames(y[i]),"_vs_",colnames(x[j]),".html",sep = "") # form a file name with an index
      plotname <- list(title = paste(main," #",i,sep = ""),titlefont = f)
      xlabel <- list(title = colnames(x[,i]),titlefont = f)
      ylabel <- list(title = colnames(y[,i]),titlefont = f)
      p <- plot_ly(x=x[,i],y=y[,i],mode=mode,text = text,size = size,color = color) %>%
        layout(title = plotname,xaxis = xlabel,yaxis = ylabel)
      htmlwidgets::saveWidget(as.widget(p),filename)
      cat("Image",filename,"saved to",getwd(),"\n")
    }
  }
  return (TRUE)
}

m = list(
  l = 250,
  # r = 50,
  b = 80,
  # t = 100,
  pad = 4
)

myplot <- plot_ly(data = cargotypes[1:8,],
                  x = AverageDistance,
                  y = Type,
                  color = AverageDuration,
                  # size = 1,
                  mode = "markers") %>%
  layout(margin = m,font = f)
myplot



## PDF JOINER FUNCTION

library(plotflow)
library(reports)

# make a folder to store the pdfs
folder(deleteMe)

## create a bunch of various sized pdfs
lapply(1:3, function(i) {
  pdf(sprintf("deleteMe/test%s.pdf", i), width=sample(4:7, 1))
  plot(1:10, 1:10, main = sprintf("Title: test%s.pdf", i))
  dev.off()
})

## paste the paths to pdfs together in one string w/ spaces
plotflow:::mergePDF(
  in.file=paste(file.path("deleteMe", dir("deleteMe")), collapse=" "),
  file="merged.pdf"
)

# delete MWE
delete('deleteMe')

############################
############################
############################
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
Top10 <- Product.List(Raw,"mostprofitable",15,savetofile = FALSE,
                      filename = "MP_ProductList_Dec.csv") # get top 10 most profitable items
# Top10 <- Top10[-c(),] # manually exclude unwanted rows
TopProduct <- Top10[1,1] # take the top item
Top_SaleData <- Partial.SaleData(TopProduct,Raw,savetofile = FALSE,
                                 filename = "MP_SALEDATA_DEC.csv") # get its raw sale data
Top_ShipData <- Partial.ShipData(Top_SaleData$SONumber,TopProduct,Ship,furtherinfo = FALSE,
                                 savetofile = TRUE,filename = "MP_SHIPDATA_DEC.csv") # collect shipping data and more
Top_CargoTypes <- CargoTypes(TopProduct,Top_ShipData,savetofile = FALSE,
                             filename = "MP_CARGOTYPES_DEC.csv")
Top_Warehouses <- Warehouses(Top_ShipData$SenderZip,Top_SaleData,Top_ShipData,
                             savetofile = FALSE,filename = "MP_WAREHOUSES_DEC.csv")

# visualize data
# MultiGGPlot(Top_CargoTypes[,-1],Top_CargoTypes,main = "December Most Profitable Product Shipping Statistics",
#             fname = "Dec_MP_SHIP",colour = Top_CargoTypes$AverageCost,alpha = 1,colourname = "Average Cost",
#             device = "pdf")
# 
# MultiGGPlot(Top_Warehouses[,-c(3,5,6,7,12,13)],Top_Warehouses[,-c(3,5,6,7,12,13)],main = "Supply of Warehouses December",
#             fname = "Dec_WHSUPPLY", colour = Top_Warehouses$Ave.Cost,alpha = 1,colourname = "Average Cost",
#             device = "pdf")


BSShipData <- Top_ShipData
BSShipData <- BSShipData[which(BSShipData[,6] %in% as.character(Top_CargoTypes[1:4,2])),] # extract just some of them
levels(as.factor(BSShipData$Type)) # see what's included

###### ORGANIZE BELOW !!! ########
# see receipent and sender locations (organize them)
Points1 <- BSShipData[,c(12,13,17,18)]
colnames(Points1) <- c("v1","v2","v3","v4")
Points2 <- BSShipData[,c(14,15,19,20)]
colnames(Points2) <- c("v1","v2","v3","v4")
Points <- rbind(Points1,Points2) # sender and receipent respectively
Points$Class <- c(rep(1,nrow(BSShipData)),rep(2,nrow(BSShipData)))
Points$Type <- c(rep("Warehouse",nrow(BSShipData)),rep("Customer",nrow(BSShipData)))
Points$Hover <- paste(Points[,6],"<br>",Points[,3],",",Points[,4],sep = "")
Points$Color <- c(rep("green",nrow(BSShipData)),rep("blue",nrow(BSShipData)))
colnames(Points) <- c("Lat","Lon","City","StateCode","Class","Type","Hover","Color")

# BSShipData$q <- with(BSShipData, cut(Type, quantile(Type)))
# levels(BSShipData$q) <- paste(c("1st", "2nd", "3rd", "4th"), "Quantile")
# BSShipData$q <- as.ordered(BSShipData$q)

### PLOT THE MAP ###
library(plotly)
# map projection
geo <- list(
  scope = 'north america',
  projection = list(type = 'azimuthal equal area'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80")
)

p <- plot_ly(Points, lon = Lon, lat = Lat, text = Hover, type = 'scattergeo',
             locationmode = 'USA-states', marker = list(size = Class*2, color = Color),
             inherit = FALSE) %>%
  add_trace(lon = list(S.Lon,R.Lon), lat = list(S.Lat,R.Lat),group = levels(as.factor(Type)),
            opacity = Distance/(max(Distance)), data = BSShipData,
            mode = 'lines', line = list(width = 1, color = Type),
            type = 'scattergeo', locationmode = 'USA-states') %>%
  layout(title = 'October Bestseller Product Shipping Paths<br>of the most sold 100 product (in meters)',
         geo = geo, showlegend = TRUE, height=800)
p

htmlwidgets::saveWidget(as.widget(p), "USA_LINEMAP_TEST.html")

# numerate types according to levels for easier color selection
BSShipData$q <- NA
for(i in 1:nrow(BSShipData)){
  temp <- BSShipData[i,6]
  levels <- levels(factor(BSShipData[,6]))
  for(j in 1:length(levels)){
    if(temp == levels[j])
      BSShipData[i,22] <- j
  }
}


#########################################################################################################

#########################################################################################################

ratio <- data.frame(Cos.Dur=BSShipData$ShippingCost/BSShipData$Duration,
                    Cos.Dis=BSShipData$ShippingCost/BSShipData$Distance,
                    DateDelivered = BSShipData$DateDelivered,
                    DateShipped = BSShipData$DateShipped,
                    Company = BSShipData$Company,
                    Type = BSShipData$Type)
# ,z=DateDelivered
fraction <- ratio[which(ratio$Cos.Dur<10),] # extract unwanted
ggplot(data=fraction,aes(x=1:nrow(fraction),y=Cos.Dur))+
  geom_jitter(colour="blue",alpha = I(1/(1+fraction[,1])))

p1 <- plot_ly(data=fraction,x=1:nrow(fraction),y=DateShipped,z=Cos.Dur,type = "scatter3d",mode="markers",color = Type)
p2 <- plot_ly(data=fraction,x=1:nrow(fraction),y=DateShipped,z=Cos.Dis,type = "scatter3d",mode="markers",color = Type)








###
h <- matrix(seq(100),10,10,byrow = TRUE) # form square matrices
g <- h

cat("10^5 times of simulations take",system.time({
  for(i in 1:10^5)
    j <- h%*%g
})[3],"seconds.\n")
###

### Exclude Some Cargotypes
lvs <- levels(factor(Ship$Type))
mylevs <- data.frame(Type=lvs,
                     AverageCost=sapply(lvs,function(x) mean(na.omit(Ship[which(Ship$Type==x),7]))),
                     AverageDuration=sapply(lvs,function(x) mean(na.omit(Ship[which(Ship$Type==x),11])))
)
mylevs
mylevs <- matrix(mylevs,length(mylevs),5)
mylevs[,2] <- sapply(mylevs[,1],function(x) mean(na.omit(Ship[which(Ship$Type==x),7])))
mylevs[,3] <- sapply(mylevs[,1],function(x) mean(na.omit(Ship[which(Ship$Type==x),11])))


exclude <- levels(factor(Ship$Type))[c(1,4,7,10)]
exclude
asd <- Ship[-which(Ship$Type %in% exclude),]
levels(factor(asd$Type))

Top100 <- Product.List(Raw,"bestseller",110)
# Top100 <- Top100[-c(4,5,18,24,26,53,54,84),] # exclude unwanted
M12_Top100_SaleData <- Partial.SaleData(Top100[,1],Raw)
Top100_ShipData <- Partial.ShipData(Top_SaleData$SONumber,TopProduct,Ship,furtherinfo = TRUE,
                                    savetofile = TRUE,filename = paste(type[2,j],"_SHIPDATA_",month[1,i],".csv",sep="")) # collect shipping data and more
Top_CargoTypes <- CargoTypes(TopProduct,Top_ShipData,savetofile = TRUE,
                             filename = paste(type[2,j],"_CARGOTYPES_",month[1,i],".csv",sep=""))
Top_Warehouses <- Warehouses(Top_ShipData$SenderZip,Top_SaleData,Top_ShipData,
                             savetofile = TRUE,filename = paste(type[2,j],"_WAREHOUSES_",month[1,i],".csv",sep=""))



###--------------------------------------------------------------------------------------###
###--------------------------------------------------------------------------------------###
###--------------------------------------------------------------------------------------###

# for easier obtaining of a product's shipping data (NOT tested)
Partial.ShipData.List <- function(saledata,           # output of Partial.ShipData(), can include various product names
                             ship,               # raw ship data, i.e. ShippingData_Mont...txt
                             sale.sonumber=1,    # column index of SONumbers in saledata
                             sale.product=5,     # column index of product names in saledata
                             col.sonumber=8,     # number of the column containing sonumbers in raw ship data
                             furtherinfo=FALSE,  # show detailed info 
                             savetofile=FALSE,   # save to csv for backup and future recovers
                             filename="Partial_ShipData.csv"){ # filename for probable file write
  
  Output <- as.data.frame(matrix(NA,nrow=sum(lengths(index,use.names = FALSE)),
                                 ncol=21)) # preallocate main data frame, this will be final output
  colnames(Output) <- c("TrackingNumber","Company","ShippingCode","SenderZip","ReceipentZip","Type",
                        "ShippingCost","SONumber","DateShipped","DateDelivered","Duration","S.Lat",
                        "S.Lon","R.Lat","R.Lon","S.City","S.StateCode","R.City","R.StateCode",
                        "Distance","Product")
  
  prods <- list(a=c(1,2,3),b=c(1,2)) # just initialize a list. needed for a list with varying row lengths
  prodlevels <- as.character(levels(factor(saledata[,sale.product]))) # levels of names
  SaleSONumber <- as.character(saledata[,sale.product])
  for(i in 1:length(prodlevels)) # find indexes, to form corresponding "saledata" for each product
    prods[[i]] <- which(SaleSONumber==prodlevels[i]) # store indexes
  # cat("Length of prodlevels :",length(prodlevels),"\n")
  # perform data collection for each item, rbind() them respectively, main loop.
  # note that one loop corresponds to one item
  for(j in 1:length(prods)){
    mylevels <- as.character(levels(factor(saledata[prods[[j]],sale.sonumber]))) # store levels of SONumbers
    # cat("Length of mylevels :",length(mylevels),"\n")
    product <- prodlevels[j]
    # convert to characters for faster searching, then find indexes that contain searched SONumber
    ShipSONumber <- as.character(ship[,col.sonumber])
    index <- list(a=c(1,2,3),b=c(1,2)) # arbitrarily initialize, let it store rows with various lengths
    for(i in 1:length(mylevels))
      index[[i]] <- which(mylevels[i]==ShipSONumber) # each element corresponds to a row of result matrix
    print("Indexes determined.")
    
    # get corresponding shipping data from indexes
    temp <- data.frame() # declare a temporary data frame to use in for loop (this make algorithm simplar)
    Result <- as.data.frame(matrix(NA,nrow=sum(lengths(index,use.names = FALSE)),
                                   ncol=21)) # preallocate main data frame
    colnames(Result) <- c("TrackingNumber","Company","ShippingCode","SenderZip","ReceipentZip","Type",
                          "ShippingCost","SONumber","DateShipped","DateDelivered","Duration","S.Lat",
                          "S.Lon","R.Lat","R.Lon","S.City","S.StateCode","R.City","R.StateCode",
                          "Distance","Product")
    
    # match data
    for(i in 1:length(index))
      temp <- rbind(temp,ship[index[[i]],])
    
    Result[,1:11] <- temp # assign it to main data frame, for first 11 columns
    print("Base data is extracted from raw shipping data.")
    
    # get coordinates as lat and lon from zips
    Result[,c(12,13)] <- zip.coordinates(Result[,4])[,c(1,2)] # find coordinates of sender zips
    Result[,c(14,15)] <- zip.coordinates(Result[,5])[,c(1,2)] # find coordinates of receipent zips
    
    # get location names from zips
    Result[,c(16,17)] <- zip.location(Result[,4])[,c(1,2)] # find city/state of sender zips
    Result[,c(18,19)] <- zip.location(Result[,5])[,c(1,2)] # find city/state of receipent zips
    write.csv(Result,"Result.csv",row.names = FALSE) # test and to be deleted
    # calculate the distance between supplier and customer
    require(geosphere)
    ### There is and error at this function
    Dist <- distHaversine(cbind(as.numeric(Result$S.Lon),as.numeric(Result$S.Lat)),
                          cbind(as.numeric(Result$R.Lon),as.numeric(Result$R.Lat))) # in meters
    ### 
    # print("Test: Each distance is calculated.") # test
    Result$Distance <- round(Dist/1000,3) # as kilometers
    # print("Test: Distance values matched.") # test
    Result$Product <- product # add product name to prevent confusion
    # Result$Product <- rep(product,sum(lengths(index,use.names = FALSE))) # add product name to prevent confusion
    # print("Test: Product names listed.") # test
    print("Location information is collected.")
    
    Output <- rbind(Output,Result)
  }
  
  # save to a csv file as backup
  if(savetofile){
    write.csv(Output,file = filename,row.names = FALSE) # no row.names to prevent possible reading errors
    cat("File",filename,"saved to",getwd(),"\n")
  }
  
  # further information for BestSeller item's Shipping Data
  if(furtherinfo){
    paste("\nFurther information:")
    paste("There are",levels(factor(Output$Type)),"different shipment types.") # different shipment types
    paste("Average delivery duration is",mean(na.omit(Output$Duration)),"days.") # average delivery day
    paste("Average cost for each delivery is",mean(na.omit(Output$ShippingCost)),"USD.") # average cost
  }
  return(Output)
}


saledata <- M12_Top100_SaleData
prods <- list(a=c(1,2,3),b=c(1,2)) # just initialize a list. needed for a list with varying row lengths
prodlevels <- as.character(levels(factor(saledata[,sale.product]))) # levels of names
SaleSONumber <- as.character(saledata[,sale.product])
for(i in 1:length(prodlevels)) # find indexes, to form corresponding "saledata" for each product
  prods[[i]] <- which(SaleSONumber==prodlevels[i]) # store indexes
###--------------------------------------------------------------------------------------###
###--------------------------------------------------------------------------------------###
###--------------------------------------------------------------------------------------###