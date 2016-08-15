# US Bubble Map for Sender Postal Codes, Month10.txt
library(plotly) # include "plotly" library for following plot objects
Zips <- as.data.frame(read.table("US_Postal_Codes_Merged.txt",colClasses = "character"))
Raw <- as.data.frame(read.table("Month10.txt",sep = ",",colClasses = "character"))
colnames(Raw) <- c("SONumber","Shipping Date","Sender Zip","Receipent Zip","Item Description","Item Weight",
                   "Units Shipped","Average Unit Price")

# Clean and prepare data for processing
temp <- strsplit(as.character(Raw[,3]),"-") # separate zip info of the form XXXXX-XXXX
SenderZips <- as.data.frame(matrix(lapply(temp, "[",1))) # save first five digits as one column matrix
SenderZips <- trimws(SenderZips[[1]]) # trim white spaces for searching
ZipLevels <- levels(factor(unlist(SenderZips))) # determine how many different zips there are
ZipLevels <- as.data.frame(matrix(ZipLevels,length(ZipLevels),1)) # save the zips as one column matrix

Arranged <- as.data.frame(matrix(NA,nrow(ZipLevels),8)) # form a new data frame to later fill in
Arranged[,2] <- ZipLevels
colnames(Arranged) <- c("Location","Zip Code","Lat","Lon","Order Amount","States Abb.","Total Units",
                        "Total Payments")

for (i in 1:nrow(Arranged)){ # get information from data
  indexes <- which(SenderZips == Arranged[i,2]) # use which() to locate all zips
  Arranged[i,c(5,7,8)] <- c(length(indexes),                  # Amount of orders
                            sum(as.numeric(Raw[indexes,7])),  # Amount of shipped units
                            sum(as.numeric(Raw[indexes,8])))  # Total price
}

for(i in 1:nrow(Arranged)){ # get information from zip database
  if(is.na(Arranged[i,1]))
    Arranged[i,c(1,6,3,4)] <- Zips[match(ZipLevels[i,1],Zips[,1]),c(2,4,6,7)] # Match the data such as city, lat, lon
}

# final fixes and attachments
Arranged <- Arranged[-which(is.na(Arranged[,1])),] # exclude NA rows
# Arranged <- Arranged[order(as.numeric(Arranged$`Order Amount`)),] # sort w.r.t. Order Amount, ascending
# Arranged$Region <- rbind(matrix(3,nrow(Arranged)/3,1),       # Divide into regions
#                          matrix(2,(nrow(Arranged)/3),1),   # first region has the highest amount of orders
#                          matrix(1,(nrow(Arranged)/3),1))   # third has the lowest
# levels(Arranged$Region) <- paste(c("1st","2ns","3rd"),"Region")
# Arranged$Region <- as.ordered(Arranged$Region)

# prepare a note for each dot in graph, hover to see it
notes <- paste(Arranged[,1],",",Arranged[,6]," ",trimws(Arranged[,2]),"<br>Order amount : ",Arranged[,5],
               "<br>Total Units : ",Arranged[,7],"<br>Total Payments : ",Arranged[,8]," USD",sep = "")

# specify map options
g <- list(scope = 'usa',projection = list(type = 'albers usa'),showland = TRUE,landcolor = toRGB("gray85"),
          subunitwidth = 1,countrywidth = 1,subunitcolor = toRGB("white"),countrycolor = toRGB("white"))

# draw and create a plot object
p <- plot_ly(Arranged, lon = Lon, lat = Lat, text = notes,
             marker = list(size = sqrt(sqrt(as.numeric(Arranged[,5])/5)) + 1, line = list(width = 0)),
             type = 'scattergeo', locationmode = 'USA-states') %>%
  layout(title = 'Oct 2012 US Sender Distribution<br>(Click legend to toggle)', geo = g)

p # show it

# save as an html page to keep interactive tools
htmlwidgets::saveWidget(as.widget(p), "USA_SENDER_DIST_M10_.html")
