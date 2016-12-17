source("https://raw.githubusercontent.com/mcandar/Agents/master/appr.R")

# Ship <- read.table("ShippingData_Months_10to12.txt",sep = ",",colClasses = "character")
# Ship <- Format.ShippingData(Ship)
Ship_Filtered_Complete <- read.csv("ShippingData_Filtered_Complete.csv",row.names=NULL)

require(lubridate)
Raw_M10 <- read.table("Month10.txt",sep=",",colClasses = "character")
Raw_M10 <- Format.SaleData(Raw_M10)

system.time({
temp_ship_m10 <- Sort(Ship_Filtered_Complete[which(month(Ship_Filtered_Complete$DateDelivered)==10),],7,decreasing = TRUE)[1:1000,]
temp_raw_m10 <- Search.List(temp_ship_m10[,8],Raw_M10,1)[,-1]

test_match <- Match.rows(temp_ship_m10,8,temp_raw_m10,1) # match data and bind together
check_sen <- test_match[,4]==test_match[,23] & test_match[,5]==test_match[,24]
test_match <- test_match[check_sen,]
write.csv(test_match,"Most_Expensive_Matched_M10.csv",row.names = FALSE)
})

# a greater calculation of above
system.time({
  temp_ship_m10 <- Sort(Ship_Filtered_Complete[which(month(Ship_Filtered_Complete$DateDelivered)==10),],7,decreasing = TRUE)
  temp_raw_m10 <- Search.List(temp_ship_m10[,8],Raw_M10,1)[,-1]
  
  test_match <- Match.rows(temp_ship_m10,8,temp_raw_m10,1) # match data and bind together
  check_sen <- test_match[,4]==test_match[,23] & test_match[,5]==test_match[,24]
  test_match <- test_match[check_sen,]
  write.csv(test_match,"Most_Expensive_Matched_M10_COMPLETE.csv",row.names = FALSE)
})

# Raw_M11 <- read.table("Month11.txt",sep=",",colClasses = "character")
# Raw_M11 <- Format.SaleData(Raw_M11)
# temp_ship_m11 <- Sort(Ship_Filtered_Complete[which(month(Ship_Filtered_Complete$DateDelivered)==11),],7,decreasing = TRUE)[1:1000,]
# temp_raw_m11 <- Search.List(temp_ship_m11[,8],Raw_M11,1)[,-1]
# 
# Raw_M12 <- read.table("Month12.txt",sep=",",colClasses = "character")
# Raw_M12 <- Format.SaleData(Raw_M12)
# temp_ship_m12 <- Sort(Ship_Filtered_Complete[which(month(Ship_Filtered_Complete$DateDelivered)==12),],7,decreasing = TRUE)[1:1000,]
# temp_raw_m12 <- Search.List(temp_ship_m12[,8],Raw_M12,1)[,-1]

# BELOW BLOCK OF CODE IS FOR OCTOBER DATA MATCHING - MONTH 10
require("lubridate")
temp_ship_m10 <- Sort(Ship_Filtered_Complete[which(
  month(Ship_Filtered_Complete$DateDelivered)==10),],7,decreasing = TRUE) # pull out just Month10, and sort according to shipping cost
temp_raw_m10 <- Search.List(temp_ship_m10[,8],Raw_M10,1)[,-1] # get corresponding rows of saledata
rows <- nrow(temp_ship_m10) # get number of rows to use in following for loop
Final <- as.data.frame(matrix(NA,0,28)) # initiate a matrix to append it in for loop
## divide into fractions and then unify into one big file, for Month10.txt
it <- 10
for(i in 1:it){
  cat("Step",i,"\n")
  index <- seq((i-1)*(rows/it)+1,i*(rows/it)) # determine interval of indexes
  test_match <- Match.rows(temp_ship_m10[index,],8,temp_raw_m10,1) # match data and bind together as a data frame
  check_sen <- test_match[,4]==test_match[,23] & test_match[,5]==test_match[,24] # check receipent and sender zips whether they match
  test_match <- test_match[check_sen,] # take only who match by zips
  if(i==1){colnames(Final) <- colnames(test_match)} # determine colnames of final output at first cycle
  Final <- rbind(Final,test_match) # append what is found at each cycle
  write.csv(test_match,paste("Most_Expensive_Matched_M10_",i,".csv",sep = ""),row.names = FALSE) # write to file with order
  cat("File",paste("Most_Expensive_Matched_M10_",i,".csv",sep = ""),"is saved to",getwd(),"\n") # inform user
}
write.csv(Final,"Most_Expensive_Matched_M10_FINAL.csv",row.names = FALSE) # write to file entire data gathered from the loop
cat("File","Most_Expensive_Matched_M10_FINAL.csv","is saved to",getwd(),"\n") # inform user

# to delete
require("lubridate")
temp_ship_m10 <- Sort(Ship_Filtered_Complete[which(
  month(Ship_Filtered_Complete$DateDelivered)==10),],7,decreasing = TRUE) # pull out just Month10, and sort according to shipping cost
temp_raw_m10 <- Search.List(temp_ship_m10[,8],Raw_M10,1)[,-1] # get corresponding rows of saledata
rows <- nrow(temp_ship_m10) # get number of rows to use in following for loop
## divide into fractions and then unify into one big file, for Month10.txt
it <- 10
for(i in 10:it){
  cat("Step",i,"\n")
  index <- seq((i-1)*(rows/it)+1,i*(rows/it)) # determine interval of indexes
  test_match <- Match.rows(temp_ship_m10[index,],8,temp_raw_m10,1) # match data and bind together as a data frame
  check_sen <- test_match[,4]==test_match[,23] & test_match[,5]==test_match[,24] # check receipent and sender zips whether they match
  test_match <- test_match[check_sen,] # take only who match by zips
  write.csv(test_match,paste("Most_Expensive_Matched_M10_",i,".csv",sep = ""),row.names = FALSE) # write to file with order
  cat("File",paste("Most_Expensive_Matched_M10_",i,".csv",sep = ""),"is saved to",getwd(),"\n") # inform user
}

# BELOW BLOCK OF CODE IS FOR NOVEMBER DATA MATCHING - MONTH 11
source("https://raw.githubusercontent.com/mcandar/Agents/master/appr.R")
Ship_Filtered_Complete <- read.csv("ShippingData_Filtered_Complete.csv",row.names=NULL)

Raw_M11 <- read.table("Month11.txt",sep=",",colClasses = "character")
Raw_M11 <- Format.SaleData(Raw_M11)

require("lubridate")
temp_ship_m11 <- Sort(Ship_Filtered_Complete[which(
  month(Ship_Filtered_Complete$DateShipped)==11),],7,decreasing = TRUE) # pull out just Month11, and sort according to shipping cost
# temp_raw_m11 <- Search.List(temp_ship_m11[,8],Raw_M11,1)[,-1] # get corresponding rows of saledata, by sonumber
# following line is a test
temp_raw_m11 <- Search.List(as.integer(levels(factor(temp_ship_m11[,8]))),Raw_M11,1)[,-1] # get corresponding rows of saledata, by sonumber
rows <- nrow(temp_ship_m11) # get number of rows to use in following for loop
## divide into fractions and then unify into one big file, for Month10.txt
it <- 10
for(i in 1:it){
  cat("Step",i,"\n")
  index <- seq((i-1)*(rows/it)+1,i*(rows/it)) # determine interval of indexes
  test_match <- Match.rows(temp_ship_m11[index,],8,temp_raw_m11,1) # match data and bind together as a data frame
  check_sen <- test_match[,4]==test_match[,23] & test_match[,5]==test_match[,24] # check receipent and sender zips whether they match
  test_match <- test_match[check_sen,] # take only who match by zips
  write.csv(test_match,paste("Most_Expensive_Matched_M11_",i,".csv",sep = ""),row.names = FALSE) # write to file with order
  cat("File",paste("Most_Expensive_Matched_M11_",i,".csv",sep = ""),"is saved to",getwd(),"\n") # inform user
}

# BELOW BLOCK OF CODE IS FOR DECEMBER DATA MATCHING - MONTH 12
source("https://raw.githubusercontent.com/mcandar/Agents/master/appr.R")
Ship_Filtered_Complete <- read.csv("ShippingData_Filtered_Complete.csv",row.names=NULL)

Raw_M12 <- read.table("Month12.txt",sep=",",colClasses = "character")
Raw_M12 <- Format.SaleData(Raw_M12)

require("lubridate")
temp_ship_m12 <- Sort(Ship_Filtered_Complete[which(
  month(Ship_Filtered_Complete$DateShipped)==12),],7,decreasing = TRUE) # pull out just Month12, and sort according to shipping cost
temp_raw_m12 <- Search.List(temp_ship_m12[,8],Raw_M12,1)[,-1] # get corresponding rows of saledata
rows <- nrow(temp_ship_m12) # get number of rows to use in following for loop
## divide into fractions and then unify into one big file, for Month12.txt
it <- 10
for(i in 1:it){
  cat("Step",i,"\n")
  index <- seq((i-1)*(rows/it)+1,i*(rows/it)) # determine interval of indexes
  test_match <- Match.rows(temp_ship_m12[index,],8,temp_raw_m12,1) # match data and bind together as a data frame
  check_sen <- test_match[,4]==test_match[,23] & test_match[,5]==test_match[,24] # check receipent and sender zips whether they match
  test_match <- test_match[check_sen,] # take only who match by zips
  write.csv(test_match,paste("Most_Expensive_Matched_M12_",i,".csv",sep = ""),row.names = FALSE) # write to file with order
  cat("File",paste("Most_Expensive_Matched_M12_",i,".csv",sep = ""),"is saved to",getwd(),"\n") # inform user
}

############################################################################################################

# bring fractions together into one file (partially formed csv's for lesser memory usage) for Month10.txt
init <- data.frame()
for(i in 1:10){
  temp <- read.csv(paste("Most_Expensive_Matched_M10_",i,".csv",sep = ""),row.names = NULL)
  init <- rbind(init,temp)
}
write.csv(init,"Most_Expensive_Matched_M10.csv",row.names = FALSE)

# bring fractions together into one file (partially formed csv's for lesser memory usage) for Month12.txt
init <- data.frame()
for(i in 1:10){
  temp <- read.csv(paste("Most_Expensive_Matched_M12_",i,".csv",sep = ""),row.names = NULL)
  init <- rbind(init,temp)
}
write.csv(init,"Most_Expensive_Matched_M12.csv",row.names = FALSE)


################ THIS IS IMPORTANT #################
# calculate outcost of each item, by item names FOR MONTH10
lvl <- levels(factor(matched_m10[,25])) # product name levels of the most expensive 100 deliveries in October
output <- as.data.frame(matrix(NA,length(lvl),8))
colnames(output) <- c("Product","TotalShippingCost","AveShippingCost","AveDistance",
                      "TimesShipped","Price","Weight","AveDuration")
for(i in 1:length(lvl)){
  ind <- which(matched_m10[,25]==lvl[i])
  output[i,] <- cbind(lvl[i],
                      as.numeric(sum(na.omit(matched_m10[ind,7]))),
                      as.numeric(mean(na.omit(matched_m10[ind,7]))),
                      as.numeric(mean(na.omit(matched_m10[ind,20]))),
                      as.numeric(length(ind)),
                      as.numeric(matched_m10[match(lvl[i],matched_m10[,25]),28]),
                      as.numeric(matched_m10[match(lvl[i],matched_m10[,25]),26]),
                      as.numeric(mean(na.omit(matched_m10[ind,11]))))
}
class(output$TotalShippingCost) <- "numeric"
# class(output$AveShippingCost) <- "numeric"
# class(output$TimesShipped) <- "numeric"
# class(output$Price) <- "numeric"
# class(output$Weight) <- "numeric"
output <- Sort(output,2,decreasing = TRUE)
output_2 <- Convert(output,col = c(3,4,5,6,7,8))

output_3 <- output_2[which(output_2$TimesShipped>4),] # filter by number of uses, just take which is delivered more than 4
matched_m10_summary <- output_3
matched_m10_summary$Index <- (matched_m10_summary$AveDistance*matched_m10_summary$Weight*matched_m10_summary$Price)/
  (matched_m10_summary$AveShippingCost*matched_m10_summary$Duration)

# calculate outcost of each item, by item names
lvl <- levels(factor(matched_m12[,25])) # product name levels of the most expensive 100 deliveries in October
output <- as.data.frame(matrix(NA,length(lvl),12))
colnames(output) <- c("Product","TotalShippingCost","AveShippingCost","AveDistance",
                      "TimesShipped","Price","Weight","AveDuration","S.AveLat","S.AveLon","R.AveLat","R.AveLon")
for(i in 1:length(lvl)){
  ind <- which(matched_m12[,25]==lvl[i])
  output[i,] <- cbind(lvl[i],
                      as.numeric(sum(na.omit(matched_m12[ind,7]))),
                      as.numeric(mean(na.omit(matched_m12[ind,7]))),
                      as.numeric(mean(na.omit(matched_m12[ind,20]))),
                      as.numeric(length(ind)),
                      as.numeric(matched_m12[match(lvl[i],matched_m12[,25]),28]),
                      as.numeric(matched_m12[match(lvl[i],matched_m12[,25]),26]),
                      as.numeric(mean(na.omit(matched_m12[ind,11]))),
                      as.numeric(mean(na.omit(matched_m12[ind,12]))),
                      as.numeric(mean(na.omit(matched_m12[ind,13]))),
                      as.numeric(mean(na.omit(matched_m12[ind,14]))),
                      as.numeric(mean(na.omit(matched_m12[ind,15]))))
}
class(output$TotalShippingCost) <- "numeric"
# class(output$AveShippingCost) <- "numeric"
# class(output$TimesShipped) <- "numeric"
# class(output$Price) <- "numeric"
# class(output$Weight) <- "numeric"
# output
output <- Sort(output,2,decreasing = TRUE)
output_2 <- Convert(output,col = c(3,4,5,6,7,8,9,10,11,12))

output_3 <- output_2[which(output_2$TimesShipped>4),] # filter by number of uses, just take which is delivered more than 4
matched_m12_summary <- output_3
matched_m12_summary$Index <- (matched_m12_summary$AveDistance*matched_m12_summary$Weight*matched_m12_summary$Price)/
  (matched_m12_summary$AveShippingCost*matched_m12_summary$Duration)



# same from here
salenum <- data.frame()
for(i in 1:nrow(matched_m10_summary)){
  ind <- which(matched_m10_summary[i,1]==Raw_M10$ItemDescription)
  salenum[i,] <- c(matched_m10_summary[i,1],length(ind))
}

salenum <- as.data.frame(matrix(NA,nrow(matched_m10_summary),2))
for(i in 1:100)#nrow(matched_m10_summary))
  salenum[i,] <- c(matched_m10_summary[i,1],length(which(matched_m10_summary[i,1]==Raw_M10$ItemDescription)))

salenum_list <- as.data.frame(matrix(sapply(matched_m10_summary[,1], 
                function(x) c(x,sum(na.omit(Raw_M10[which(x==Raw_M10$ItemDescription),7])))),nrow(matched_m10_summary),2,byrow=TRUE))
# up to here, three algorithms, faster to below, same purpose

# altan hoca graph 1
require(plotly)
# margins for better displaying of names axis
m <- list(
  # l = 50,
  # r = 50,
  b = 200
  # t = 100,
  # pad = 4
)

class(salenum_list)
salenum_list$V2 <- as.character(salenum_list$V2)
class(salenum_list$V2) <- "numeric"
df <- Sort(salenum_list,2,decreasing = TRUE)
xlabel <- list(title="Product")
ylabel <- list(title="Sold Quantity",type="log") # logarithmic
graph <- plot_ly(data =df,x = V1,y = V2,name = "Sold Quantity",mode = "lines+markers") %>%
  layout(title = "The Most Expensive Delivered Items October",margin = m,xaxis=xlabel,yaxis=ylabel)

graph

# please note that the order of the item names is descending according to outcost, the one 
# at left costs more than one at right in shipping
htmlwidgets::saveWidget(as.widget(graph),"MED_Quantity_vs_Name.html")


# altan hoca graph 2
require(plotly)
# margins for better displaying of names axis
m <- list(
  # l = 50,
  # r = 50,
  b = 200
  # t = 100,
  # pad = 4
)

# l <- list(x = 1,y = -0.09)
graph_1 <- plot_ly(data = matched_m10_summary,x = Product,y = TotalShippingCost,
                   name = "Sold Quantity",color = AveShippingCost,mode = "lines+markers") %>%
  # colorbar(title = "Mean Cost") %>%
  layout(title = "The Most Expensive Delivered Items October",margin = m,yaxis=list(type="log")) # logarithmic
  # add_trace(x = Product,y = AveDistance,name = "Distance",mode = "lines+markes") %>%
  # layout(legend = l)
graph_1

htmlwidgets::saveWidget(as.widget(graph_1),"MED_Qutcost_vs_Name_OCT.html")

graph_2 <- plot_ly(data = matched_m12_summary,x = Product,y = TotalShippingCost,
                   name = "Sold Quantity",color = AveShippingCost,mode = "lines+markers") %>%
  # colorbar(title = "Mean Cost") %>%
  layout(title = "The Most Expensive Delivered Items December",margin = m,yaxis=list(type="log")) # logarithmic
# add_trace(x = Product,y = AveDistance,name = "Distance",mode = "lines+markes") %>%
# layout(legend = l)
graph_2

htmlwidgets::saveWidget(as.widget(graph_2),"MED_Qutcost_vs_Name_DEC.html")

subplot(graph_1,graph_2)

# altan hoca graph 3
levs <- as.character(levels(factor(matched_m10$R.StateCode)))
cost_states <- as.data.frame(matrix(sapply(levs,function(x) c(x,sum(na.omit(matched_m10[
  which(matched_m10$R.StateCode==x),7])))),length(levs),2,byrow=TRUE))
colnames(cost_states) <- c("StateCode","TotalShippingCost")

cost_states$TotalShippingCost <- as.numeric(as.character(cost_states$TotalShippingCost))
df <- cost_states

l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

# With respect to Gross Profit
p <- plot_ly(df, z = TotalShippingCost, text = paste("OutCost :",df$TotalShippingCost), locations = StateCode, type = 'choropleth',
             locationmode = 'USA-states', color = TotalShippingCost, colors = 'Blues',
             marker = list(line = l), colorbar = list(title = "Total<br>Shipping<br>Cost (USD)")) %>%
  layout(title = 'Outcost by State October<br>(Hover for breakdown)', geo = g)
p
htmlwidgets::saveWidget(as.widget(p), "USA_OUTCOST_BYSTATE_OCT.html")

# altan hoca graph 4
my_data <- levels.ship(matched_m10,distances = TRUE)

require(plotly)
# margins for better displaying of names axis
m <- list(
  # l = 50,
  # r = 50,
  b = 200
  # t = 100,
  # pad = 4
)

# xlabel <- list(title="Product")
# ylabel <- list(title="Sold Quantity")
graph_10 <- plot_ly(data = Sort(my_data,5,decreasing = T),x = Type,y = Uses,name = "Uses",mode = "lines+markers") %>%
  layout(title = "Unwanted Shipping Types October")#,margin = m)
graph_10
graph_11 <- plot_ly(data = Sort(my_data,6,decreasing = T),x = Type,y = Distance,name = "Distance",mode = "lines+markers",showlegend=T) %>%
  layout(title = "Unwanted Shipping Types October")
graph_11

htmlwidgets::saveWidget(as.widget(subplot(graph_10,graph_11)),"Uses_&_Distance_vs_Type.html")

graph_12 <- plot_ly(data = Sort(my_data,2,decreasing = T),x = Type,y = AveCost,name = "AveCost",mode = "lines+markers") %>%
  layout(title = "Unwanted Shipping Types October")#,margin = m)
graph_12
graph_13 <- plot_ly(data = Sort(my_data,3,decreasing = T),x = Type,y = TotalCost,name = "TotalCost",mode = "lines+markers",showlegend=T) %>%
  layout(title = "Unwanted Shipping Types October")
graph_13
htmlwidgets::saveWidget(as.widget(subplot(graph_12,graph_13)),"AveCost_&_TotalCost_vs_Type.html")

# altan hoca graph 5
require(plotly)
# margins for better displaying of names axis
m <- list(
  # l = 50,
  # r = 50,
  b = 200
  # t = 100,
  # pad = 4
)

# l <- list(x = 1,y = -0.09)
graph_15 <- plot_ly(data = matched_m10,x = SenderZip,y = ItemDescription,
                   color = ShippingCost,mode = "lines+markers") %>%
  layout(title = "The Most Expensive Delivered Items October",margin = m)
# add_trace(x = Product,y = AveDistance,name = "Distance",mode = "lines+markes") %>%
# layout(legend = l)
graph_15

# htmlwidgets::saveWidget(as.widget(graph_1),"MED_Qutcost_vs_Name.html")

Sys.setenv("plotly_username"="mcandar")
Sys.setenv("plotly_api_key"="ekbij5ww3d")
plotly_POST(p,"USA_OUTCOST_BYSTATE")

write.csv(matched_m10[1:100,],"matched_data_october(sample).csv",row.names = FALSE)
write.csv(matched_m10_summary[1:100,],"mathced_data_oct_summary(sample).csv",row.names = FALSE)

# be careful about colour input, continuous to discrete error may occur
MultiGGPlot(matched_m10_TEST,matched_m10_TEST,main = "October Outcost Information",fname = "OutCost",
            alpha = I(1/100),colour = matched_m10_TEST$ItemWeight,colourname = "Weight")

lapply(matched_m10,function(x) class(x))
matched_m10_TEST <- matched_m10
lapply(matched_m10_TEST,function(x) class(x))
for(i in c(1,2,3,6,14,15,16,17,21))
  matched_m10_TEST[,i] <- as.character(matched_m10[,i])

# convert what is factor, to character
i <- sapply(matched_m10_TEST, is.factor)
i <- c(1,2,3,6,16,17,18,19,25) # as.character
matched_m10_TEST[i] <- lapply(matched_m10[i], as.character)
i <- c(9,10,22)
matched_m10_TEST[i] <- lapply(matched_m10[i], as.POSIXlt)

# to delete
# BELOW BLOCK OF CODE IS FOR OCTOBER DATA MATCHING - MONTH 10
require("lubridate")
temp_ship_m10 <- Sort(Ship_Filtered_Complete[which(
  month(Ship_Filtered_Complete$DateDelivered)==10),],7,decreasing = TRUE) # pull out just Month10, and sort according to shipping cost
temp_raw_m10 <- Search.List(temp_ship_m10[,8],Raw_M10,1)[,-1] # get corresponding rows of saledata
##
un_temp_raw_m10 <- unique(temp_raw_m10) # made unique after original calculation
lv_temp_raw_m10 <- Search.List(levels(factor(temp_ship_m10[,8])),Raw_M10,1)[,-1] # just levels of SONumbers entered
un2_temp_raw_m10 <- Search.List(unique(temp_ship_m10[,8]),Raw_M10,1)[,-1] # unique temp_ship is entered
##
rows <- nrow(temp_ship_m10) # get number of rows to use in following for loop

test_match <- Match.rows(temp_ship_m10,8,Raw_M10,1) # match data and bind together as a data frame
check_sen <- test_match[,4]==test_match[,23] & test_match[,5]==test_match[,24] # check receipent and sender zips whether they match
test_match <- test_match[check_sen,] # take only who match by zips
write.csv(test_match,paste("Most_Expensive_Matched_M10_(test).csv",sep = ""),row.names = FALSE) # write to file with order

## divide into fractions and then unify into one big file, for Month10.txt
it <- 10
for(i in 1:it){
  cat("Step",i,"\n")
  index <- seq((i-1)*(rows/it)+1,i*(rows/it)) # determine interval of indexes
  test_match <- Match.rows(temp_ship_m10[index,],8,temp_raw_m10,1) # match data and bind together as a data frame
  check_sen <- test_match[,4]==test_match[,23] & test_match[,5]==test_match[,24] # check receipent and sender zips whether they match
  test_match <- test_match[check_sen,] # take only who match by zips
  write.csv(test_match,paste("test_Most_Expensive_Matched_M10_",i,".csv",sep = ""),row.names = FALSE) # write to file with order
  cat("File",paste("test_Most_Expensive_Matched_M10_",i,".csv",sep = ""),"is saved to",getwd(),"\n") # inform user
}
cat("File","Most_Expensive_Matched_M10_FINAL.csv","is saved to",getwd(),"\n") # inform user
# to delete until right here

# ADD GRID OPTION FOR GRAPHS, MULTIPLE IN ONE PAGE
# Draw multiple higher quality graphs with ggplot2, inputs can be multi-column data frames
# color input may not work properly, and it does not add color parameter name to legend
MultiGGPlot <- function(x,                    # data of x axis
                        data,                 # data set of y axis
                        main = "GGPlot",      # main title
                        fname = "GG_",        # prefix for filename
                        geom = "jitter",      # geometry of the plot
                        colour = "black",     # colour scale
                        colourname = "colour",# title of the legend
                        alpha = I(1/10),      # transparecy of the points
                        device = "png",       # choose the extension of the file
                        smooth = FALSE,       # de/activate smoothing
                        smethod = "lm",       # smoothing method
                        formula=y ~ poly(x, 3, raw=TRUE), # formula for smoothing, linear, polynomial etc.
                        width = 6,            # width as inches
                        height = 3.375){      # height as inches
  
  require(ggplot2)
  if(is.null(colnames(x))) 
    x <- as.data.frame(x) # convert to data frame if not already, (x maybe a multicolumn object too)
  
  for(j in 1:ncol(x)){ # 1 through total column number (originally 1:dim(x)[2])
    for(i in 1:ncol(data)){
      if (identical(x[,j],data[,i])) next()
      filename <- paste(fname,"_",i+((j-1)*ncol(data)),"_",colnames(data[i]),"_vs_",colnames(x[j]),".",device,sep = "") # form a file name with an index
      plotname <- paste(main," #",i,sep = "")
      ggplot(data, 
             aes(x = x[,j],
                 y = data[,i],
                 colour = colour)) +
        
        scale_colour_gradientn(colours=rainbow(4)) +
        
        switch (geom,
                "jitter" = {geom_jitter(alpha = alpha)},
                "point" = {geom_point(alpha = alpha)},
                "path" = {geom_path(alpha = alpha)}
        ) +
        
        labs(title = main,
             x = colnames(x[j]),
             y = colnames(data[i]),
             colour = colourname) +
        
        if(smooth)
          geom_smooth(method=smethod, se=TRUE, fill=NA,
                      formula=formula,colour="blue")
      
      ggsave(filename,device = device,width = width,height = height)
      cat("Image",filename,"saved to",getwd(),"\n")
    }
  }
  return (TRUE)
}

##### RATIO GRAPHS #####

# bring them together, namely match the shared items
length(which(matched_m10_summary$Product %in% matched_m12_summary$Product)) # see how much of the items are shared
coupled <- data.frame()
for(i in 1:nrow(matched_m10_summary)){
  init <- which(matched_m10_summary$Product[i]==matched_m12_summary$Product)
  if(length(init)!=0)
    coupled <- rbind(coupled,cbind(matched_m10_summary[i,],matched_m12_summary[init,]))
}
colnames(coupled) <- append(paste("m10",colnames(coupled)[1:(ncol(coupled)/2)],sep = "_"),
                            paste("m12",colnames(coupled)[((ncol(coupled)/2)+1):16],sep = "_"))

require(plotly)
# margins for better displaying of names axis
m <- list(
  # l = 50,
  # r = 50,
  b = 200
  # t = 100,
  # pad = 4
)

### RATIO OF MONTH10 DATA OVER MONTH12 DATA
ratio_out <- as.data.frame(matrix(NA,nrow(coupled),12))
colnames(ratio_out) <- paste("Rat",colnames(matched_m10_summary),sep = "_")
ratio_out[,1] <- coupled$m10_Product
ratio_out[,2:12] <- coupled[,2:12]/coupled[,14:24]

l <- list(x = 1,y = -0.09)
p <- plot_ly(data =ratio_out,x = Rat_Product,y = Rat_TotalShippingCost,
                    mode = "lines") %>%
  layout(title = "Ratio Oct/Dec",margin = m,yaxis=list(type="log"))
# add_trace(x = Product,y = AveDistance,name = "Distance",mode = "lines+markes") %>%
# layout(legend = l)
p

l <- list(x = 1,y = -0.11)
p1 <- plot_ly(data =ratio_out,x = Rat_Product,y = Rat_AveShippingCost,
             color = Rat_TotalShippingCost,mode = "lines+markers",name="Average Cost") %>%
  layout(title = "Ratio Oct/Dec",margin = m,yaxis=list(type="log")) %>%
  add_trace(x = Rat_Product,y = rep(3,nrow(coupled)),mode="lines",name = "Band Upper") %>%
  add_trace(x = Rat_Product,y = rep(1/3,nrow(coupled)),mode="lines",name = "Band Lower") %>%
  layout(legend = l)
p1
htmlwidgets::saveWidget(as.widget(p1),"Ratio_OctDec_AveCosts.html")

ratio_out$Order <- 1:nrow(ratio_out)
sm <- as.data.frame(loess.smooth(ratio_out$Order,ratio_out$Rat_AveDistance,evaluation = nrow(ratio_out)))
l <- list(x = 1,y = -0.30)
p2 <- plot_ly(data =ratio_out,x = Rat_Product,y = Rat_AveDistance,
              color = Rat_AveDuration,mode = "lines+markers",name="Average Distance") %>%
  layout(title = "Ratio Oct/Dec",margin = m,yaxis=list(type="log")) %>%
  add_trace(x = Rat_Product,y = rep(2,nrow(coupled)),mode="lines",name = "Band Upper") %>%
  add_trace(x = Rat_Product,y = rep(1/2,nrow(coupled)),mode="lines",name = "Band Lower") %>%
  add_trace(x = Rat_Product,y=sm$y,name = "Loess Smoother")%>%
  add_trace(x = Rat_Product,y=fitted(lm(Rat_AveDistance ~ Order,data=ratio_out[,c(4,9)])),name = "Linear Fit")%>%
  layout(legend = l)
p2
htmlwidgets::saveWidget(as.widget(p2),"Ratio_OctDec_AveDistances.html")

a <- lm(Rat_AveDistance ~ Order,data=ratio_out[,c(4,9)])
a
anova(a)

pdf("LinearModel.pdf")
par(mfrow=c(2,2))
plot(a)
dev.off()

fitted(a)

# ratios of lattitudes, receipent
ratio_out$Order <- 1:nrow(ratio_out)
sm <- as.data.frame(loess.smooth(ratio_out$Order,ratio_out$Rat_R.AveLat,evaluation = nrow(ratio_out)))
l <- list(x = 1,y = -0.30)
p3 <- plot_ly(data =ratio_out,x = Rat_Product,y = Rat_R.AveLat,
              color = Rat_AveShippingCost,mode = "lines+markers",name="Ratio of Ave Lat") %>%
  layout(title = "Oct/Dec Ratio of Average Receipent Lattitudes",margin = m,yaxis=list(type="log")) %>%
  add_trace(x = Rat_Product,y = rep(14/10,nrow(ratio_out)),mode="lines",name = "Band Upper") %>%
  add_trace(x = Rat_Product,y = rep(10/14,nrow(ratio_out)),mode="lines",name = "Band Lower") %>%
  add_trace(x = Rat_Product,y=sm$y,name = "Loess Smoother")%>%
  add_trace(x = Rat_Product,y=fitted(lm(Rat_R.AveLat ~ Order,data=ratio_out)),name = "Linear Fit")%>%
  layout(legend = l)
p3
htmlwidgets::saveWidget(as.widget(p3),"Ratio_OctDec_Receipent_AveLat.html")


# ratios of lattitudes, receipent
ratio_out$Order <- 1:nrow(ratio_out)
sm <- as.data.frame(loess.smooth(ratio_out$Order,ratio_out$Rat_R.AveLat,evaluation = nrow(ratio_out)))
l <- list(x = 1,y = -0.30)
p4 <- plot_ly(ratio_out,x = Rat_Product,y = Rat_R.AveLat,z = Rat_R.AveLon,
              color = Rat_AveShippingCost,name="Ratio of Ave Lat",marker=list(color=Rat_AveShippingCost)) %>%
  # add_trace() %>%
  layout(title = "Oct/Dec Ratios of Average Receipent Lat,Lon",margin = m)

p4
htmlwidgets::saveWidget(as.widget(p4),"Ratio_OctDec_Receipent_AveLat.html")

# a <- lm(Rat_AveDistance ~ Order,data=ratio_out[,c(4,9)])
# a
# anova(a)

# pdf("LinearModel.pdf")
# par(mfrow=c(2,2))
# plot(a)
# dev.off()

fitted(a)
##### UP TO HERE RATIO GRAPHSS #####

# a function for easily extracting summarized data out of unified data, on monthly basis.
# needs more inputs for better specification
Outcost.Summary <- function(ship_unified, # the big unified shipdata, (latest) on monthly basis (e.g. month_m10 etc.)
                            filter.stat = FALSE, # filter according to uses
                            limit.stat = 5, # take if used equal or more than 5, remove rest
                            col.products = 25, # column of product names in unified shipdata
                            col.cost = 7, # column of shipping costs in unified shipdata
                            col.dist = 20, # column of average distances in unified shipdata
                            col.price = 28, #  column of prices in unified shipdata
                            col.wei = 26, #  column of weight in unified shipdata
                            col.dur = 11, # column of durations in unified shipdata
                            col.slat = 12, # column of sender lat in unified shipdata
                            col.slon = 13, # column of sender lon in unified shipdata
                            col.rlat = 14, # column of receipent lat in unified shipdata
                            col.rlon = 15 # column of receipent lon in unified shipdata
                            ){
  lvl <- levels(factor(ship_unified[,col.products])) # product name levels of the most expensive 100 deliveries in October
  output <- as.data.frame(matrix(NA,length(lvl),12))
  colnames(output) <- c("Product","TotalShippingCost","AveShippingCost","AveDistance",
                        "TimesShipped","Price","Weight","AveDuration","S.AveLat","S.AveLon","R.AveLat","R.AveLon")
  for(i in 1:length(lvl)){
    ind <- which(ship_unified[,col.products]==lvl[i])
    output[i,] <- cbind(lvl[i], # product name #1
                        as.numeric(sum(na.omit(ship_unified[ind,col.cost]))), # total cost #2
                        as.numeric(mean(na.omit(ship_unified[ind,col.cost]))), # average cost #3
                        as.numeric(mean(na.omit(ship_unified[ind,col.dist]))), # # average distance #4
                        as.numeric(length(ind)), # number of uses #5
                        as.numeric(ship_unified[match(lvl[i],ship_unified[,col.products]),col.price]), # price #6
                        as.numeric(ship_unified[match(lvl[i],ship_unified[,col.products]),col.wei]), # weight #7
                        as.numeric(mean(na.omit(ship_unified[ind,col.dur]))), # average duration #8
                        as.numeric(mean(na.omit(ship_unified[ind,col.slat]))), # sender lattitude #9
                        as.numeric(mean(na.omit(ship_unified[ind,col.slon]))), # sender longtitude #10
                        as.numeric(mean(na.omit(ship_unified[ind,col.rlat]))), # receipent lattitude #11
                        as.numeric(mean(na.omit(ship_unified[ind,col.rlon])))) # receipent longtitude #12
  }
  class(output$TotalShippingCost) <- "numeric"
  output <- Convert(Sort(output,2,decreasing = TRUE),col = c(3,4,5,6,7,8,9,10,11,12))
  if(filter.stat)
    output <- output[which(output$TimesShipped>=limit.stat),] # filter by number of uses, just take which is delivered more than 4
  return(output)
}

matched_m10_summary_TEST <- Outcost.Summary(matched_m10,filter.stat = TRUE)
matched_m12_summary_TEST <- Outcost.Summary(matched_m12,filter.stat = TRUE)

binit <- read.csv("Most_Expensive_Matched_M11_10.csv",row.names = NULL)
nrow(binit)
nrow(unique(binit))
nrow(res)

temp <- data.frame()
for(i in 1:10){
  temp <- rbind(temp,read.csv(paste("Most_Expensive_Matched_M9_test_",i,".csv",sep=""),row.names = NULL))
}
write.csv(temp,"Most_Expensive_Matched_M9.csv",row.names = FALSE)



# bring them together, namely match the shared items MONTH10/MONTH11
length(which(matched_m10_summary_TEST$Product %in% matched_m11_summary_TEST$Product)) # see how much of the items are shared
coupled <- data.frame()
for(i in 1:nrow(matched_m10_summary_TEST)){
  init <- which(matched_m10_summary_TEST$Product[i]==matched_m11_summary_TEST$Product)
  if(length(init)!=0)
    coupled <- rbind(coupled,cbind(matched_m10_summary_TEST[i,],matched_m11_summary_TEST[init,]))
}
colnames(coupled) <- append(paste("m10",colnames(coupled)[1:(ncol(coupled)/2)],sep = "_"),
                            paste("m11",colnames(coupled)[((ncol(coupled)/2)+1):16],sep = "_"))

### RATIO OF MONTH10 DATA OVER MONTH12 DATA
ratio_out <- as.data.frame(matrix(NA,nrow(coupled),13))
colnames(ratio_out) <- paste("Rat",colnames(matched_m10_summary_TEST),sep = "_")
ratio_out[,1] <- coupled$m10_Product
ratio_out[,2:13] <- coupled[,2:13]/coupled[,15:26]

ratio_out$Order <- 1:nrow(ratio_out)
sm <- as.data.frame(loess.smooth(ratio_out$Order,ratio_out$Rat_AveDistance,evaluation = nrow(ratio_out)))
l <- list(x = 1,y = -0.30)
p2 <- plot_ly(data =ratio_out,x = Rat_Product,y = Rat_AveDistance,
              color = Rat_AveDuration,mode = "lines+markers",name="Average Distance") %>%
  layout(title = "Ratio Oct/Nov",margin = m,yaxis=list(type="log")) %>%
  add_trace(x = Rat_Product,y = rep(5/4,nrow(coupled)),mode="lines",name = "Band Upper") %>%
  add_trace(x = Rat_Product,y = rep(4/5,nrow(coupled)),mode="lines",name = "Band Lower") %>%
  add_trace(x = Rat_Product,y=sm$y,name = "Loess Smoother")%>%
  add_trace(x = Rat_Product,y=fitted(lm(Rat_AveDistance ~ Order^4,data=ratio_out[,c(4,9)])),name = "Linear Fit")%>%
  layout(legend = l)
p2
plotly_POST(p2,"Ratio_OctNov_Distances")
htmlwidgets::saveWidget(as.widget(p2),"Ratio_OctNov_AveDistances.html")


########################
################3

p4 <- plot_ly(data =matched_m10_summary_TEST,x = Product,y = TotalShippingCost,z = AveShippingCost,
              color = AveDuration,mode = "markers",name="Average Distance",type = "scatter3d")
p4
Sys.setenv("plotly_username"="mcandar")
Sys.setenv("plotly_api_key"="ekbij5ww3d")
plotly_POST(p4,"3d_TotalShipCost_vs_AveShipCost_vs_Product")


p5 <- plot_ly(df, x = StateCode, y = GrossProfit, z = Equity, color = TotalShippingCost, colors = c('#BF382A', '#0C4B8E'),type = "scatter3d") %>%
  layout(scene = list(xaxis = list(title = 'State Code'),
                      yaxis = list(title = 'Gross Profit'),
                      zaxis = list(title = 'Equity')))
p5


require(plotly)
notes <- paste(matched_m10_summary_TEST$Product,
               "<br>Average Lat:",matched_m10_summary_TEST$R.AveLat,
               "<br>Average Lon:",matched_m10_summary_TEST$R.AveLon,
               "<br>Total Outcost:",matched_m10_summary_TEST$TotalShippingCost,
               "<br>Average Outcost:",round(matched_m10_summary_TEST$AveShippingCost,2),
               "<br>Delivered Units:",matched_m10_summary_TEST$TotalUnitsShipped,
               "<br>Delivered Parcels:",matched_m10_summary_TEST$NumberofUses)

g <- list(scope = 'usa',projection = list(type = 'albers usa'),showland = TRUE,landcolor = toRGB("gray85"),
          subunitwidth = 1,countrywidth = 1,subunitcolor = toRGB("white"),countrycolor = toRGB("white"))

# draw and create a plot object
p <- plot_ly(matched_m10_summary_TEST[1:100,], lon = R.AveLon, lat = R.AveLat, text = notes,
             marker = list(size = as.numeric((matched_m10_summary_TEST$TotalShippingCost)^0.4)), line = list(width = 0),
             type = 'scattergeo', locationmode = 'USA-states',color = AveDistance) %>%
  layout(title = 'Oct 2012 US Outcost Receipents Distribution<br>(Click legend to toggle)', geo = g)
p

htmlwidgets::saveWidget(as.widget(p),"Oct 2012 US Outcost Receipents Distribution.html")
plotly_POST(p,"Oct 2012 US Outcost Receipents Distribution")

p # show it



#################### FORECAST ####################

# original - taken from plot.ly
library(plotly)
library(forecast)

fit <- ets(USAccDeaths)
fore <- forecast(fit, h = 48, level = c(80, 95))

p <- plot_ly() %>%
  add_lines(x = time(USAccDeaths), y = USAccDeaths,
            color = I("black"), name = "observed") %>%
  add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], ymax = fore$upper[, 2],
              color = I("gray95"), name = "95% confidence") %>%
  add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1], ymax = fore$upper[, 1],
              color = I("gray80"), name = "80% confidence") %>%
  add_lines(x = time(fore$mean), y = fore$mean, color = I("blue"), name = "prediction")
p


###### ######
library(plotly)
library(forecast)

fit <- ets(USAccDeaths)
fore <- forecast(fit, h = 48, level = c(80, 95))

p <- plot_ly() %>%
  add_lines(x = time(USAccDeaths), y = USAccDeaths,
            color = I("black"), name = "observed") %>%
  add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], ymax = fore$upper[, 2],
              color = I("gray95"), name = "95% confidence") %>%
  add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1], ymax = fore$upper[, 1],
              color = I("gray80"), name = "80% confidence") %>%
  add_lines(x = time(fore$mean), y = fore$mean, color = I("blue"), name = "prediction")
p

# library(plotly)
library(forecast)
library(highcharter)

thm <- 
  hc_theme(
    colors = c("#1a6ecc", "#434348", "#90ed7d"),
    chart = list(
      backgroundColor = "transparent",
      style = list(fontFamily = "Source Sans Pro")
    ),
    xAxis = list(
      gridLineWidth = 1
    )
  )


AirPassengers %>% 
  forecast(level = 90) %>% 
  hchart() %>% 
  hc_add_theme(thm)

### PREDICTION - AREA OF CONFIDENCE AS PERCENTAGE ###
library(plotly)
library(forecast)

fit <- ets(USAccDeaths)
fore <- forecast(fit, h = 48, level = c(80, 95))

p <- plot_ly() %>%
  add_lines(x = time(USAccDeaths), y = USAccDeaths,
            color = I("black"), name = "observed") %>%
  add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], ymax = fore$upper[, 2],
              color = I("gray95"), name = "95% confidence") %>%
  add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1], ymax = fore$upper[, 1],
              color = I("gray80"), name = "80% confidence") %>%
  add_lines(x = time(fore$mean), y = fore$mean, color = I("blue"), name = "prediction")

p
