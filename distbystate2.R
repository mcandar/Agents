# following codes calculates the sale statistics per state and plots it on a map for Month11.txt

library(plotly)
source("https://raw.githubusercontent.com/itu-cms-group/candar/master/appr.R") # import functions from github

Raw2 <- read.table("Month11.txt",sep = ",",colClasses = "character")
Raw2 <- Format.SaleData(Raw2,zipclass = "character",na.rm = TRUE,solim = 0) # CONVERT ZIPS TO CHARACTER WHEN 
# SEARCHING IN ORDER TO PREVENT LOSS OF INFORMATION!!

Arranged2 <- LocationLevels(Raw2,4) # for receipent zips
backupArranged2 <- Arranged2

##############################################################################################################
# sl <- levels(factor(unlist(Arranged2[,6]))) # get how many different states there are
# StateLevels <- as.data.frame(matrix(NA,length(sl),5))
# colnames(StateLevels) <- c("StateCode","State","Orders","Units","GrossProfit")
# StateLevels[,1] <- sl 
# # StateLevels[,1] <- as.data.frame(matrix(StateLevels[,1],length(StateLevels[,1]),1)) # save the state abbs. as one column matrix
# 
# 
# for (i in 1:nrow(StateLevels)){ # get information from data
#   indexes <- which(Arranged2[,6] == StateLevels[i,1]) # use which() to locate all states
#   StateLevels[i,c(3,4,5)] <- c(length(indexes),                  # Amount of orders
#                                sum(as.numeric(Arranged2[indexes,7])),  # Amount of shipped units
#                                sum(as.numeric(Arranged2[indexes,8])))  # Total price
# }
# 
# for(i in 1:nrow(StateLevels)){ # get information from zip database
#   if(is.na(StateLevels[i,2])) # according to state names
#     StateLevels[i,2] <- Zips[match(StateLevels[i,1],Zips[,4]),3] # Match the data such as city, lat, lon
# }
# 
# StateLevels <- na.omit(StateLevels)
# # StateLevels[,6] <- NULL
# StateLevels$hover <- with(StateLevels, paste(State, '<br>', 
#                                              "Order Amount : ", Orders, "<br>",
#                                              "Total Units : ", Units,  "<br>",
#                                              "Gross Profit : ", round(GrossProfit,2)," USD",sep = ""))
##############################################################################################################
df <- StateLevels(Arranged2)

l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

# With respect to Gross Profit
p <- plot_ly(df, z = GrossProfit, text = hover, locations = StateCode, type = 'choropleth',
             locationmode = 'USA-states', color = GrossProfit, colors = 'Blues',
             marker = list(line = l), colorbar = list(title = "Gross<br>Profit (USD)")) %>%
  layout(title = 'November 2012 Sale Data by State <br>(Hover for breakdown)', geo = g)
p
htmlwidgets::saveWidget(as.widget(p), "USA_GROSSPROFIT_BY_STATE_NOV.html")

# With respect to Total Shipped Units
p <- plot_ly(df, z = Units, text = hover, locations = StateCode, type = 'choropleth',
             locationmode = 'USA-states', color = Units, colors = 'Purples',
             marker = list(line = l), colorbar = list(title = "Total<br>Shipped<br>Units")) %>%
  layout(title = 'November 2012 Sale Data by State <br>(Hover for breakdown)', geo = g)

htmlwidgets::saveWidget(as.widget(p), "USA_SHIPPEDUNITS_BY_STATE_NOV.html")

# With respect to Amount of Orders
p <- plot_ly(df, z = Orders, text = hover, locations = StateCode, type = 'choropleth',
             locationmode = 'USA-states', color = Orders, colors = 'Greens',
             marker = list(line = l), colorbar = list(title = "Amount of<br>Orders")) %>%
  layout(title = 'November 2012 Sale Data by State <br>(Hover for breakdown)', geo = g)

htmlwidgets::saveWidget(as.widget(p), "USA_AMOUNTOFORDERS_BY_STATE_NOV.html")
