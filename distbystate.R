library(plotly)

Raw <- read.table("Month10.txt",sep=",")  #
Raw <- Format.SaleData(Raw,na.rm = TRUE)  # Import data and format it for further processing
Raw <- SetColNames(Raw)                   #

Raw <- GetTime(Raw,2)
a <- Raw[sample(nrow(Raw),10^4),]
backuparaw <- Raw
# Raw <- a
Raw <- backuparaw
#######################################################################################################
  ReceipentZips <- Convert(Raw,4,class = "character")[,4] # trim white spaces and convert to character for searching
  ZipLevels <- levels(factor(unlist(ReceipentZips))) # determine how many different zips there are
  ZipLevels <- as.data.frame(matrix(ZipLevels,length(ZipLevels),1)) # save the zips as one column matrix

  Arranged <- as.data.frame(matrix(NA,nrow(ZipLevels),8)) # form a new data frame to later fill in
  Arranged[,2] <- ZipLevels
  colnames(Arranged) <- c("Location","Zip Code","Lat","Lon","Order Amount","States Abb.","Total Units",
                          "Total Payments")

  Raw$TotalPayments <- Raw[,7]*Raw[,8] # multiply number of units with average unit price to get total payment

  for (i in 1:nrow(Arranged)){ # get information from data
    indexes <- which(ReceipentZips == Arranged[i,2]) # use which() to locate all zips
    Arranged[i,c(5,7,8)] <- c(length(indexes),                  # Amount of orders
                              sum(as.numeric(Raw[indexes,7])),  # Amount of shipped units
                              sum(as.numeric(Raw[indexes,9])))  # Total price (should be multiplied by unit number)
  }

  # Zips <- GetZips()
  for(i in 1:nrow(Arranged)){ # match and get information from zip database
    if(is.na(Arranged[i,1]))
      Arranged[i,c(1,6,3,4)] <- Zips[match(ZipLevels[i,1],Zips[,1]),c(2,4,6,7)] # Match the data such as city, lat, lon
  }
#######################################################################################################

Arranged <- LocationLevels(Raw,4) # receipent zips
print(levels(factor(Arranged$`States Abb.`)))

sl <- levels(factor(unlist(Arranged[,6]))) # get how many different states there are
StateLevels <- as.data.frame(matrix(NA,length(sl),5))
colnames(StateLevels) <- c("StateCode","State","Orders","Units","GrossProfit")
StateLevels[,1] <- sl 
# StateLevels[,1] <- as.data.frame(matrix(StateLevels[,1],length(StateLevels[,1]),1)) # save the state abbs. as one column matrix


for (i in 1:nrow(StateLevels)){ # get information from data
  # indexes <- which(ReceipentZips == Arranged[i,4]) # use which() to locate all zips
  indexes <- which(Arranged[,6] == StateLevels[i,1]) # use which() to locate all states
  StateLevels[i,c(3,4,5)] <- c(length(indexes),                  # Amount of orders
                               sum(as.numeric(Arranged[indexes,7])),  # Amount of shipped units
                               sum(as.numeric(Arranged[indexes,8])))  # Total price
}

for(i in 1:nrow(StateLevels)){ # get information from zip database
  if(is.na(StateLevels[i,2])) # according to state names
    StateLevels[i,2] <- Zips[match(StateLevels[i,1],Zips[,4]),3] # Match the data such as city, lat, lon
}

# write.table(StateLevels,"StateLevels.txt")
# backupstatelevels <- StateLevels
StateLevels <- na.omit(StateLevels)

StateLevels$hover <- with(StateLevels, paste(State, '<br>', 
                                             "Order Amount : ", Orders, "<br>",
                                             "Total Units : ", Units,  "<br>",
                                             "Gross Profit : ", round(GrossProfit,2)," USD",sep = ""))

l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

# With respect to Gross Profit
p <- plot_ly(StateLevels, z = GrossProfit, text = hover, locations = StateCode, type = 'choropleth',
        locationmode = 'USA-states', color = GrossProfit, colors = 'Blues',
        marker = list(line = l), colorbar = list(title = "Gross<br>Profit (USD)")) %>%
  layout(title = 'October 2012 Sale Data by State <br>(Hover for breakdown)', geo = g)

htmlwidgets::saveWidget(as.widget(p), "USA_GROSSPROFIT_BY_STATE_OCT.html")

# With respect to Total Shipped Units
p <- plot_ly(StateLevels, z = Units, text = hover, locations = StateCode, type = 'choropleth',
             locationmode = 'USA-states', color = Units, colors = 'Purples',
             marker = list(line = l), colorbar = list(title = "Total<br>Shipped<br>Units")) %>%
  layout(title = 'October 2012 Sale Data by State <br>(Hover for breakdown)', geo = g)

htmlwidgets::saveWidget(as.widget(p), "USA_SHIPPEDUNITS_BY_STATE_OCT.html")

# With respect to Amount of Orders
p <- plot_ly(StateLevels, z = Orders, text = hover, locations = StateCode, type = 'choropleth',
             locationmode = 'USA-states', color = Orders, colors = 'Greens',
             marker = list(line = l), colorbar = list(title = "Amount of<br>Orders")) %>%
  layout(title = 'October 2012 Sale Data by State <br>(Hover for breakdown)', geo = g)

htmlwidgets::saveWidget(as.widget(p), "USA_AMOUNTOFORDERS_BY_STATE_OCT.html")
