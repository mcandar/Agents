#  ARRANGE AND OPTIMIZE THESE CODE FOR FUNCTIONIZING
# analysis and gathering information of 10 most sold items (Month10.txt)

# get functions from personal library
# source("https://raw.githubusercontent.com/mcandar/Agents/master/appr.R")
### IMPORT DATA AND GET BASIC INFORMATION ###
Raw <- read.table("Month10.txt",sep=",")
Raw <- Format.SaleData(Raw)
products <- data.frame(Name=levels(factor(Raw[,5]))) # see and store how many different products
products$Quantity <- NA

# be careful, both data must be white-space trimmed, even the lists must be. following line of code
# is like a for loop, calculates how much an item sold
products[,2] <- sapply(products[,1],function(x) sum(Raw[which(Raw[,5] == x),7])) # rowwise

# backup the content
backupproducts <- products
# write.csv(products,"ProductInfoMonth10.csv",row.names = FALSE) # set header true when reading
# products <- read.csv("ProductInfoMonth10.csv",header = TRUE) # just an example

### PLOT GRAPHS ###
# take the first 100 of the most sold
MS <- Sort(products,2,decreasing = TRUE)[1:104,] # intentionally taken 4 more rows for the reason at following line
MS <- MS[-c(10,27,71,83),] # remove unwanted products or services (this should be changed according to file)
Raw$Total <- Raw$UnitsShipped*Raw$AverageUnitPrice # get the profit from that product
MS$Profit <- NA    # Initialize two columns 
MS$Price <- NA     # to later fill in

for(i in 1:nrow(MS)){
  index <- which(Raw[,5] == MS[i,1])
  MS[i,c(3,4)]<- c(sum(Raw[index,9]),Raw[index[1],8]) # find profit and price
}

# Plot just first 10 instead of whole file (100 rows=100 most sold)
MultiGGPlot(MS[1:10,-1],MS[1:10,],main = "October 10 Most Sold Products",
            fname = "Oct_10",colour = MS$Profit[1:10],alpha = 1)

### PREPARE AN APPROPRIATE LINE MAP DATA ###
MS$Orders <- NA
for(i in 1:nrow(MS)) MS[i,5] <- length(which(MS[i,1]==Raw[,5])) # how many times an item is ordered (not the units)
L <- rep(MS[,1],MS[,5]) # write names to later find corresponding information

Lines <- as.data.frame(matrix(NA,length(L),12)) # data of most sold 100 products
colnames(Lines) <- c("Name","SONumber","Units","Price","Profits","SenderZip","ReceipentZip","S.Lat","S.Lon","R.Lat","R.Lon","Distance")
Lines$Name <- L

# get information from Raw file
LRaw <- as.data.frame(matrix(NA,0,7))
for(i in 1:nrow(MS)){
  index <- which(Raw[,5]==MS[i,1])
  LRaw <- rbind(LRaw,cbind(rep(MS[i,1],length(index)),Raw[index,c(1,3,4,7,8,9)])) 
}
Lines[,c(1,2,6,7,3,4,5)] <- LRaw

# get sender lattitude and longitude info from zips
M <- levels(factor(Lines[,6])) # sender zip levels
M <- na.omit(M)
for(i in 1:length(M)){
  index <- which(M[i]==Lines[,6]) # get where those zips exists in Lines
  index <- index[which(is.na(Lines[index,8]))]
  n <- match(M[i],Zips[,1])
  if(length(index) != 0){
    for(j in 1:length(index))
      Lines[index[j],c(8,9)] <- Zips[n,c(6,7)]
  }
}

# get receipent lattitude and longitude info from zips
M <- levels(factor(Lines[,7])) # receipent zip levels
M <- na.omit(M)
for(i in 1:length(M)){
  index <- which(M[i]==Lines[,7]) # get where those zips exists in Lines
  index <- index[which(is.na(Lines[index,10]))]
  n <- match(M[i],Zips[,1])
  if(length(index) != 0){
    for(j in 1:length(index))
      Lines[index[j],c(10,11)] <- Zips[n,c(6,7)]
  }
}

# calculate the distance that a product travels
require(geosphere)
p1 <- cbind(as.numeric(Lines$S.Lon),as.numeric(Lines$S.Lat))
p2 <- cbind(as.numeric(Lines$R.Lon),as.numeric(Lines$R.Lat))
Dist <- distHaversine(p1,p2) # in meters
Lines$Distance <- Dist
LinesSorted <- Sort(Lines,12,decreasing = TRUE)[1:10^4,] # first 10000 preferably unwanted transitions of the
# most sold 10 products through the October
backupLines <- Lines # backup with a variable
# write.csv(Lines,"LinesMonth10(formapping).csv",row.names=FALSE) # backup with a csv file

# MERGE THEM WITH LAT LON FINDER FOR LOOP!!
# get sender city name
LinesSorted$S.City <- NA
LinesSorted$S.StateCode <- NA
M <- levels(factor(LinesSorted[,6])) # sender zip levels
for(i in 1:length(M)){
  index <- which(M[i]==LinesSorted[,6]) # get where those zips exists in Lines
  index <- index[which(is.na(LinesSorted[index,13]))]
  n <- match(M[i],Zips[,1]) # find in zip code database
  if(length(index) != 0){
    for(j in 1:length(index))
      LinesSorted[index[j],c(13,14)] <- Zips[n,c(2,4)]
  }
}

# MERGE THEM WITH LAT LON FINDER FOR LOOP!!
# get receipent city name
LinesSorted$R.City <- NA
LinesSorted$R.StateCode <- NA
M <- levels(factor(LinesSorted[,7])) # receipent zip levels
for(i in 1:length(M)){
  index <- which(M[i]==LinesSorted[,7]) # get where those zips exists in Lines
  index <- index[which(is.na(LinesSorted[index,15]))]
  n <- match(M[i],Zips[,1])
  if(length(index) != 0){
    for(j in 1:length(index))
      LinesSorted[index[j],c(15,16)] <- Zips[n,c(2,4)]
  }
}

# see receipent and sender locations (organize them)
Points1 <- LinesSorted[,c(8,9,13,14)]
colnames(Points1) <- c("v1","v2","v3","v4")
Points2 <- LinesSorted[,c(10,11,15,16)]
colnames(Points2) <- c("v1","v2","v3","v4")
Points <- rbind(Points1,Points2) # sender and receipent respectively
Points$Class <- c(rep(1,10000),rep(2,10000))
Points$Type <- c(rep("Warehouse",10000),rep("Customer",10000))
Points$Hover <- paste(Points$Type,"<br>",Points$City,",",Points$StateCode,sep = "")
Points$Color <- c(rep("green",10000),rep("blue",10000))
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
  add_trace(lon = list(S.Lon,R.Lon), lat = list(S.Lat,R.Lat),group = 1:10000,
            opacity = Distance/(max(Distance)*10), data = LinesSorted,
            mode = 'lines', line = list(width = 1, color = 'red'),
            type = 'scattergeo', locationmode = 'USA-states') %>%
  layout(title = 'October 10000 Longest Shipping Paths<br>of the most sold 100 product (in meters)',
         geo = geo, showlegend = FALSE, height=800)
p

htmlwidgets::saveWidget(as.widget(p), "USA_MAP_100MOSTSOLD_LONGESTPATHS_OCT.html")

### FURTHER INFORMATION ###

paste("Total",nrow(products),"different products were sold through October.")
paste("Total",sum(na.omit(Raw$UnitsShipped)),"products were sold through October.")
paste("Gross profit in October is",round(sum(na.omit(Raw$Total)),2),"USD.")
paste("Gross profit received from most sold 100 products is",round(sum(na.omit(MS100$Profit)),2),"USD.",
      "This holds",round((sum(na.omit(MS100$Profit))/sum(na.omit(Raw$Total)))*100,2),
      "% of total revenue in October.")
paste("Total",nrow(Raw),"orders received during October.",nrow(Lines),
      "of them for the 100 best selling products.","This ratio corresponds to",
      round((nrow(Lines)/nrow(Raw))*100,2),"% of the total orders.")
paste("Average units sold, profit and product price from best selling 100 are respectively:",mean(na.omit(MS100$Quantity)),",",
      mean(na.omit(MS100$Profit)),"USD,",mean(na.omit(MS100$Price)),"USD.")
paste("Average distance 100 best selling products travel is",round(mean(na.omit(Lines$Distance)),2),"meters.")
paste("Average of the 10000 longest distance among 100 best selling products is",
      round(mean(na.omit(LinesSorted$Distance)),2),"meters.")



### INSIDE THE MAIN CONTINENT ###
sc <- is.inside(as.numeric(LinesSorted[,8]),as.numeric(LinesSorted[,9]))
rc <- is.inside(as.numeric(LinesSorted[,10]),as.numeric(LinesSorted[,11]))
for(i in 1:length(sc))
  mc[i] <- sc[i] == TRUE && rc[i] == TRUE
# length(which(sc==T))
# length(which(rc==T))
# length(which(mc==T))
LinesMainland <- LinesSorted[which(mc==T),]

pts1 <- LinesMainland[,c(8,9,13,14)]
colnames(pts1) <- c("v1","v2","v3","v4")
pts2 <- LinesMainland[,c(10,11,15,16)]
colnames(pts2) <- c("v1","v2","v3","v4")
pts <- rbind(pts1,pts2) # sender and receipent respectively
colnames(pts) <- c("Lat","Lon","City","StateCode")
pts$Class <- c(rep(1,nrow(LinesMainland)),rep(2,nrow(LinesMainland)))
pts$Type <- c(rep("Warehouse",nrow(LinesMainland)),rep("Customer",nrow(LinesMainland)))
pts$Hover <- paste(pts$Type,"<br>",pts$City,",",pts$StateCode,sep = "")
pts$Color <- c(rep("green",nrow(LinesMainland)),rep("blue",nrow(LinesMainland)))
colnames(pts) <- c("Lat","Lon","City","StateCode","Class","Type","Hover","Color")


library(plotly)
# map projection
geo <- list(
  scope = 'north america',
  projection = list(type = 'azimuthal equal area'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80")
)

p <- plot_ly(pts, lon = Lon, lat = Lat, text = Hover, type = 'scattergeo',
             locationmode = 'USA-states', marker = list(size = Class*2, color = Color),
             inherit = FALSE) %>%
  add_trace(lon = list(S.Lon,R.Lon), lat = list(S.Lat,R.Lat),group = 1:nrow(LinesMainland),
            opacity = Distance/(max(Distance)*10), data = LinesMainland,
            mode = 'lines', line = list(width = 1, color = 'red'),
            type = 'scattergeo', locationmode = 'USA-states') %>%
  layout(title = 'October 10000 Longest Shipping Paths<br>
         of the most sold 100 products<br>
         inside main continent (in meters).',
         geo = geo, showlegend = FALSE, height=800)
p

htmlwidgets::saveWidget(as.widget(p),"USA_MAP_100MOSTSOLD_LONGESTPATHS_OCT_MAINCONTINENT.html")
