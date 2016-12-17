# GRAPHS FOR UNIFIED DATA, ITEMS THAT CAUSE OUTCOST
source("https://raw.githubusercontent.com/mcandar/Agents/master/appr.R")

### FUNCTIONS ###
# a function for easily extracting summarized data out of unified data, on monthly basis.
# this function lists the items and gets the corresponding information about them
Outcost.Summary <- function(ship_unified, # the big unified shipdata, (latest) on monthly basis (e.g. month_m10 etc.)
                            filter.stat = FALSE, # filter according to uses
                            limit.stat = 5, # take if used equal or more than 5, remove rest
                            col.products = 25, # column of product names in unified shipdata
                            col.cost = 7, # column of shipping costs in unified shipdata
                            col.dist = 20, # column of average distances in unified shipdata
                            col.units = 27, # column of amount of shipped units
                            col.price = 28, #  column of prices in unified shipdata
                            col.wei = 26, #  column of weight in unified shipdata
                            col.dur = 11, # column of durations in unified shipdata
                            col.slat = 12, # column of sender lat in unified shipdata
                            col.slon = 13, # column of sender lon in unified shipdata
                            col.rlat = 14, # column of receipent lat in unified shipdata
                            col.rlon = 15 # column of receipent lon in unified shipdata
){
  lvl <- levels(factor(ship_unified[,col.products])) # product name levels of the most expensive 100 deliveries in October
  output <- as.data.frame(matrix(NA,length(lvl),13))
  colnames(output) <- c("Product","TotalShippingCost","AveShippingCost","AveDistance","NumberofUses",
                        "TotalUnitsShipped","Price","Weight","AveDuration","S.AveLat","S.AveLon","R.AveLat","R.AveLon")
  for(i in 1:length(lvl)){
    ind <- which(ship_unified[,col.products]==lvl[i])
    output[i,] <- cbind(lvl[i], # product name #1
                        as.numeric(sum(na.omit(ship_unified[ind,col.cost]))), # total cost #2
                        as.numeric(mean(na.omit(ship_unified[ind,col.cost]))), # average cost #3
                        as.numeric(mean(na.omit(ship_unified[ind,col.dist]))), # # average distance #4
                        as.numeric(length(ind)), # number of uses #5 but not units
                        as.numeric(sum(na.omit(ship_unified[ind,col.units]))), # number of shipped units #6
                        as.numeric(ship_unified[match(lvl[i],ship_unified[,col.products]),col.price]), # price #7
                        as.numeric(ship_unified[match(lvl[i],ship_unified[,col.products]),col.wei]), # weight #8
                        as.numeric(mean(na.omit(ship_unified[ind,col.dur]))), # average duration #9
                        as.numeric(mean(na.omit(ship_unified[ind,col.slat]))), # sender lattitude #10
                        as.numeric(mean(na.omit(ship_unified[ind,col.slon]))), # sender longtitude #11
                        as.numeric(mean(na.omit(ship_unified[ind,col.rlat]))), # receipent lattitude #12
                        as.numeric(mean(na.omit(ship_unified[ind,col.rlon])))) # receipent longtitude #13
  }
  class(output$TotalShippingCost) <- "numeric"
  output <- Convert(Sort(output,2,decreasing = TRUE),col = c(3,4,5,6,7,8,9,10,11,12,13))
  if(filter.stat)
    output <- output[which(output$NumberofUses>=limit.stat),] # filter by number of uses, just take which is delivered more than 4
  return(output)
}

# Summarizes the content of the unified shipping data, gets information about states
ByState.Summary <- function(ship_unified, # the big unified shipping data, ultimate. (preferred as on monthly basis)
                            col.statecode = 19, # the number of column containing state codes either for sender or for receipent
                            col.cost = 7,
                            col.dist = 20,
                            col.wei = 26,
                            col.units = 27,
                            col.price = 28
                            ){
  levs <- as.character(levels(factor(ship_unified[,col.statecode])))
  output <- as.data.frame(matrix(NA,length(levs),8))
  colnames(output) <- c("StateCode","TotalShippingCost","AverageShippingCost","AverageDistance","TotalWeight",
                        "NumberofUses","TotalUnitsShipped","GrossProfit")
  for(i in 1:length(levs)){
    index <- which(ship_unified[,col.statecode]==levs[i])
    output[i,] <- cbind(levs[i], # state code
                        as.numeric(sum(na.omit(ship_unified[index,col.cost]))), # total shipping cost
                        as.numeric(mean(na.omit(ship_unified[index,col.cost]))), # average of shipping costs
                        as.numeric(mean(na.omit(ship_unified[index,col.dist]))), # average distances, sent to or from that state
                        as.numeric(sum(na.omit(ship_unified[index,col.wei]))), # total weight shipping in given time interval
                        as.numeric(length(index)), # how many deliveries a state received or sent, number of uses
                        as.numeric(sum(na.omit(ship_unified[index,col.units]))), # total number of items shipped
                        as.numeric(sum(na.omit(ship_unified[index,col.price]))) # gross profit made from expensive deliveries
                        )
  }
  output <- Convert(output,2:8)
  return(output)
}

# Distribution by state (choropleth) map
# quickly map the output of ByState.Summary function, (choropleth map)
ByState.Map <- function(data, # summarized data by state (output of function ByState.Summary()), monthly
                        main="Outcost by State", # main title of the map
                        filename = "USA_OUTCOST_BYSTATE.html" # name of the file to be saved
                        ){
  require(plotly)
  # specify line options
  l <- list(color = toRGB("white"), width = 2)
  
  # specify some map projection/options
  g <- list(scope = 'usa',projection = list(type = 'albers usa'),showlakes = TRUE,lakecolor = toRGB('white'))
  
  # list what should be displayed
  notes <- paste("Total OutCost:",data$TotalShippingCost,"<br>Average Shipping Cost:",data$AverageShippingCost,
                 "<br>Average Distance:",data$AverageDistance,"<br>Total Weight:",data$TotalWeight,
                 "<br>Number of Deliveries:",data$NumberofUses,"<br>Number of Shipped Units:",data$TotalUnitsShipped,
                 "<br>Associated Gross Profit:",data$GrossProfit)
  
  data[,2:8] <- round(data[,2:8],2) # round for better displaying
  # draw map with respect to Gross Profit
  p <- plot_ly(data = data , z = TotalShippingCost, text = notes, locations = StateCode, type = 'choropleth',
               locationmode = 'USA-states', color = TotalShippingCost, colors = 'Blues',
               marker = list(line = l), colorbar = list(title = "Total<br>Shipping<br>Cost (USD)")) %>%
    layout(title = paste(main,'<br>(Hover for breakdown)',sep=""), geo = g)
  
  # save as an html page
  htmlwidgets::saveWidget(as.widget(p), filename)
  return(p)
}

# find shared elements on given data frames, (plug this into incomplete ratio functions!)
Shared.Elements <- function(col,data1,data2,data3=NULL,data4=NULL,data5=NULL,data6=NULL){
  Result <- intersect(data1[,col],data2[,col])
  if(!is.null(data3))
    Result <- intersect(Result,data3[,col])
  if(!is.null(data4))
    Result <- intersect(Result,data4[,col])
  if(!is.null(data5))
    Result <- intersect(Result,data5[,col])
  if(!is.null(data6))
    Result <- intersect(Result,data6[,col])
  return(Result)
}

# ratio calculator of OutCost.Summary() function output (INCOMPLETE)
Rat.OutCost <- function(data1,data2){
  # check dimensions to reduce the possibility of an error
  if(ncol(data1)!=ncol(data2)){
    print("Dimensions does not match.")
    return(FALSE)
  }
  
  # take the smaller one to use in for loop and get shared items
  if(nrow(data1)>nrow(data2)) # if data2 is smaller
    lim <- nrow(data2) # take the row number
  else # if data1 is smaller or equivalent
    lim <- nrow(data1) # take the row number
  
  coupled <- data.frame()
  for(i in 1:lim){
    if(lim == nrow(data1))
      init <- which(data1$Product[i]==data2$Product)  
    else if(lim == nrow(data2))
      init <- which(data2$Product[i]==data1$Product)
    
    if(length(init)!=0)
      coupled <- rbind(coupled,cbind(matched_m10_summary[i,],matched_m12_summary[init,]))
  }
  
  Result <- as.data.frame(matrix(NA,nrow(coupled),ncol(data1)))
  colnames(Result) <- paste("Rat",colnames(data1),sep = "_")
  Result[,1] <- data1[,1]
  Result[,2:13] <- data1[,2:13]/data2[,2:13]
  return(Result)
}

# INCOMPLETE
Rat.ByState <- function(data1,data2){
  Result <- as.data.frame(matrix(NA,nrow(data1),8))
  colnames(Result) <- paste("Rat",colnames(data1),sep = "_")
  Result[,1] <- data1[,1]
  Result[,2:13] <- data1[,2:13]/data2[,2:13]
  return(Result)
}

# FOLLOWING TWO GRAPH FUNCTIONS SHOULD BE REARRANGED FOR CORRECTED PLOTLY
# Visualize Units & Uses vs Product, style 1
# note that if the descending order in graph is not as expected, check col.sort input
Outcost.Graph1 <- function(data_summary, # output of Outcost.Summary(), i.e. summary of the matched data on monthly basis
                           main = "Outcost", # main title of the graph
                           mode1 = "lines+markers", # primary line
                           mode2 = "histogram", # secondary
                           col.sort = 7, # the column indices to be sorted
                           save = FALSE, # save as html
                           postfix = "sample",
                           post = FALSE # true if wanted to post to plotly
){
  require(plotly)
  m <- list(b = 200)# l = 50, r = 50, t = 100, pad = 4) # margins from sides
  l <- list(x = 1,y = -0.15) # legend locating
  G <- plot_ly(data = Sort(data_summary,col.sort,decreasing = TRUE),x = Product,y = TotalUnitsShipped,
               name = "Shipped Quantity",color = AveShippingCost,showlegend = TRUE,mode = mode1) %>%
    layout(title = main,margin = m,yaxis=list(type="log")) %>%
    add_trace(x = Product,y = NumberofUses,name = "Number of Parcels",mode = mode2) %>%
    layout(legend = l)
  filename = paste("S1_Units_&_Uses_vs_Product_",postfix,".html",sep="")
  if(save) # save graph if user entered a filename
    htmlwidgets::saveWidget(as.widget(G),filename)
  if(post){ # post to plotly if variable post is true and a filename exist
    Sys.setenv("plotly_username"="mcandar")
    Sys.setenv("plotly_api_key"="ekbij5ww3d")
    plotly_POST(G,filename = filename)
  }
  return(G)
}

# Visualize ,total shipping cost & average shipping cost vs product style 2 (INCOMPLETE)
# note that if the descending order in graph is not as expected, check col.sort input
Outcost.Graph2 <- function(data_summary, # output of Outcost.Summary(), i.e. summary of the matched data on monthly basis
                           main = "Outcost", # main title of the graph
                           mode1 = "lines", # primary line
                           mode2 = "markers", # secondary
                           opacity = 1, # opacity of the dots, between 1 and 0. (a.k.a. alpha)
                           col.sort = 7, # the column indices to be sorted
                           save = FALSE, # save as html
                           postfix = "sample",
                           post = FALSE # true if wanted to post to plotly
){
  require(plotly)
  m <- list(b = 200)# l = 50, r = 50, t = 100, pad = 4) # margins from sides
  l <- list(x = 1,y = -0.15) # legend locating
  ay <- list(# tickfont = list(color = "blue"),
    overlaying = "y",
    side = "right",
    title = "second y axis",
    type = "log"
  )
  G <- plot_ly(data = data_summary,x = Product,y = TotalShippingCost,
               name = "Total Outcost",showlegend = TRUE,mode = mode1,line=list(width = 5)) %>%
    layout(title = main,margin = m,yaxis=list(type="log")) %>%
    add_trace(x = Product,y = AveShippingCost,name = "Average Shipping Cost",mode = mode2,yaxis="y2",
              color = AveDistance,colors = "Greys",marker = list(opacity = opacity)) %>%
    layout(legend = l,yaxis2 = ay)
  
  filename = paste("S2_TotalShipCost_&_AveShipCost_vs_Product_",postfix,".html",sep="")
  if(save) # save graph if user entered a filename
    htmlwidgets::saveWidget(as.widget(G),filename)
  if(post){ # post to plotly if variable post is true and a filename exist
    Sys.setenv("plotly_username"="mcandar")
    Sys.setenv("plotly_api_key"="ekbij5ww3d")
    plotly_POST(G,filename = filename)
  }
  return(G)
}


#### FUNCTIONIZE FOLLOWING ####

for(i in 7:12){
  data <- read.csv(paste("summary_final_matched_m",i,".csv",sep = ""),row.names = NULL)
  Outcost.Graph1(data,main = paste("Outcost in Month",i),mode2="lines",save = TRUE,postfix = i)
}

# 
for(i in 7:12){
  data <- read.csv(paste("summary_final_matched_m",i,".csv",sep = ""),row.names = NULL)
  Outcost.Graph2(data,main = paste("Outcost in Month",i),save = TRUE,postfix = paste("M",i,sep = ""))
}

ay <- list(
  # tickfont = list(color = "blue"),
  overlaying = "y",
  side = "right",
  title = "second y axis",
  type = "log"
)
G <- plot_ly(data = matched_m10_summary_TEST,x = Product,y = TotalShippingCost,
             name = "Total Outcost",showlegend = TRUE,mode = "lines",line=list(width = 5)) %>%
  layout(title = "Outcost in October",margin = m,yaxis=list(type="log")) %>%
  add_trace(x = Product,y = AveShippingCost,name = "Average Shipping Cost",mode = "markers",yaxis="y2",
            color = AveDistance,colors = "Greys",marker = list(opacity = 0.5)) %>%
  layout(legend = l,yaxis2 = ay)
# add_trace(y = Quantity, name = 'Quantity', mode = 'lines+markers')
G

plotly_POST(G,"TotalCost_AveCost_vs_Oct")
#### UP TO HERE ####

### MAIN ###
Sys.setenv("plotly_username"="mcandar")
Sys.setenv("plotly_api_key"="ekbij5ww3d")

# distribution by state (choropleth) maps
temp <- ByState.Summary(matched_m10)
map_oct <- ByState.Map(temp,main = "Outcost by state October",filename = "USA_OUTCOST_BYSTATE_OCT.html")
plotly_POST(map_oct,"USA_OUTCOST_BYSTATE_OCT")

temp <- ByState.Summary(matched_m11)
map_nov <- ByState.Map(temp,main = "Outcost by state November",filename = "USA_OUTCOST_BYSTATE_NOV.html")
plotly_POST(map_nov,"USA_OUTCOST_BYSTATE_NOV")

temp <- ByState.Summary(matched_m12)
map_dec <- ByState.Map(temp,main = "Outcost by state December",filename = "USA_OUTCOST_BYSTATE_DEC.html")
plotly_POST(map_dec,"USA_OUTCOST_BYSTATE_DEC")

matched_m10_summary_TEST <- Outcost.Summary(matched_m10,filter.stat = TRUE)
matched_m11_summary_TEST <- Outcost.Summary(matched_m11,filter.stat = TRUE)
matched_m12_summary_TEST <- Outcost.Summary(matched_m12,filter.stat = TRUE)




### KEEP FOLLOWING BLOCK OF CODE SAFE ###
df <- ByState.Summary(matched_m12) # default is set for receipents
# specify line options
l <- list(color = toRGB("white"), width = 2)

# specify some map projection/options
g <- list(scope = 'usa',projection = list(type = 'albers usa'),showlakes = TRUE,lakecolor = toRGB('white'))

# list what should be demonstrated
notes <- paste("Total OutCost:",df$TotalShippingCost,"<br>Average Shipping Cost:",df$AverageShippingCost,
               "<br>Average Distance:",df$AverageDistance,"<br>Total Weight:",df$TotalWeight,
               "<br>Number of Deliveries:",df$NumberofUses,"<br>Number of Shipped Units:",df$TotalUnitsShipped,
               "<br>Associated Gross Profit:",df$GrossProfit)

# draw map with respect to Gross Profit
p <- plot_ly(df, z = TotalShippingCost, text = notes, locations = StateCode, type = 'choropleth',
             locationmode = 'USA-states', color = TotalShippingCost, colors = 'Blues',
             marker = list(line = l), colorbar = list(title = "Total<br>Shipping<br>Cost (USD)")) %>%
  layout(title = 'Outcost by State December<br>(Hover for breakdown)', geo = g)

# save as an html page
htmlwidgets::saveWidget(as.widget(p), "USA_OUTCOST_BYSTATE_DEC.html")
#########################################

### ARRANGE BELOW FROM HERE ###

# get common items among 6 months
common <- Shared.Elements(1,summary_outcost_m7,summary_outcost_m8,summary_outcost_m9,
                          summary_outcost_m10,summary_outcost_m11,summary_outcost_m12)


# get the corresponding data of common elements through 6 months 
Result <- as.data.frame(matrix(NA,length(common),3))
for(i in 1:length(common)){
  # Result <- as.data.frame(matrix(NA,length(common),ncol(summary_outcost)))
  index <- which(as.character(common[i]) == as.character(summary_outcost[,1])) # get indices
  # print(index)
  # print(summary_outcost[index,])
  Result[i,] <- c(common[i], # names 1
                  sum(na.omit(summary_outcost[index,2])), # total shipping cost 2 
                  sum(na.omit(summary_outcost[index,2]))/sum(na.omit(summary_outcost[index,5])) # average ship cost 3
                  # NA, # 4
                  # sum(na.omit(summary_outcost[index,5])), # total uses 5
                  # sum(na.omit(summary_outcost[index,6])), # total units 6 
                  # summary_outcost[index[1],7], # price 7
                  # summary_outcost[index[1],8], # weight 8
                  # NA,
                  # NA,
                  # NA,
                  # NA,
                  # NA
                  )
}

Result <- Convert(Result,c(2,3))
Result <- Sort(Result,2,decreasing = TRUE)
colnames(Result) <- c("Product","TotalShippingCost","AveShippingCost")

library("plotly")
xform <- list(categoryorder = "array",
              categoryarray = Result$Product)
m <- list(b = 200)# l = 50, r = 50, t = 100, pad = 4) # margins from sides
p <- plot_ly(data = Result,
             x = ~Product,
             y = ~TotalShippingCost,
             type = "scatter",
             mode = "lines+markers",
             color = ~AveShippingCost,
             name = "Total Outcost") %>%
  
  colorbar(title="Average<br>Shipping<br>Cost") %>%
  
  layout(title = "Common Items' Outcost 2nd Half of 2012",yaxis = list(type = "log"),xaxis=xform,margin=m)
p

htmlwidgets::saveWidget(as.widget(p),"CommonItems_OutCost_2ndHalf_2012.html")


# THIS IS VERY IMPORTANT! PREVENTS THE SORTING OF AXIS VALUES, KEEPS THE ORIGINAL ORDER OF THE DATA!!
xform <- list(categoryorder = "array",
              categoryarray = df$V1)

p <- plot_ly(data = df,x = ~V1,y = ~V2,type = "scatter",mode = "lines+markers") %>%
  layout(title = "my title",xaxis = xform)
p

m <- list(b = 200)# l = 50, r = 50, t = 100, pad = 4) # margins from sides
# l <- list(x = 1,y = -0.15) # legend locating
xform <- list(categoryorder = "array",
              categoryarray = Result$V1)
G <- plot_ly(x = Result$V1,y = Result$V2,
             name = "Total Outcost",color = Result$V3,mode = "lines+markers",type = "scatter") %>%
  layout(title = "Common Items' Outcost 2nd Half of 2012",margin = m,yaxis=list(type="log"))
G
  # add_trace(x = Product,y = NumberofUses,name = "Number of Parcels",mode = mode2) %>%
  # layout(legend = l)

### ARRANGE ABOVE UP TO HERE ###

init <- data.frame()
for(i in 7:12){
  data <- read.csv(paste("final_matched_m",i,".csv",sep = ""),row.names = NULL)
  init <- rbind(init,data)
}
final_matched <- init

write.csv(final_matched,"final_matched.csv",row.names = FALSE) # backup the content
outcost_top10 <- Result[1:10,] # just pull the most 10 expensive out of it
top_outcost <- final_matched[which(final_matched$ItemDescription==outcost_top10[1,1]),] # get corresponding data

top_outcost$DateShipped <- as.POSIXlt(top_outcost$DateShipped)
top_outcost$DateDelivered <- as.POSIXlt(top_outcost$DateDelivered)
top_outcost$ShippingDate <- as.POSIXlt(top_outcost$ShippingDate)
# top_outcost[,c(9,10,22)] <- as.POSIXct(top_outcost[,c(9,10,22)])
# out <- top_outcost[rev(order(as.Date(top_outcost$DateShipped))),] # sort from newest to oldest
out <- top_outcost[order(as.Date(top_outcost$DateShipped)),] # sort from oldest to newest
# out <- out[nrow(out):1,] # reverse to oldest to newest


require("lubridate")
# top_outcost$DateShipped <- lubridate::dmy(top_outcost$DateShipped)
top_outcost$DateShipped <- dmy(top_outcost$DateShipped)
dplyr::arrange(top_outcost,DateShipped)

library("plotly")

plt1 <- plot_ly(data = out,
             x = ~DateShipped,
             y = ~R.Lat,
             type = "scatter",
             mode = "lines+markers",
             # color = ~Distance,
             name = "Receipent Latitude") %>%
  
  # colorbar(title="Distance") %>%
  
  layout(title = "Top Outcost Item 2nd Half of 2012",
         xaxis = list(range=c(top_outcost$DateShipped[1],top_outcost$DateShipped[nrow(top_outcost)])))
plt1

htmlwidgets::saveWidget(as.widget(plt1),"TOP_OutCost_2ndHalf_2012.html")

plt2 <- plot_ly(data = out,
                x = ~DateShipped,
                y = ~R.Lon,
                type = "scatter",
                mode = "lines+markers",
                # color = ~Distance,
                name = "Receipent Latitude") %>%
  
  # colorbar(title="Distance") %>%
  
  layout(title = "Top Outcost Item 2nd Half of 2012",
         xaxis = list(range=c(top_outcost$DateShipped[1],top_outcost$DateShipped[nrow(top_outcost)])))
plt2

un <- c(plt1,plt2)
un
library(gridExtra)
m <- marrangeGrob(un,nrow = 1,ncol = 1) # MULTIPAGING!!! VERY IMPORTANT!!

ggsave("test_multipage.pdf",plot = m,device = "pdf")

htmlwidgets::saveWidget(as.widget(m),"test_multipage.html")

# for prediction, dealing with time series
library(xts)
my_ts <- xts(out$R.Lat,out$DateDelivered)
my_ts <- ts(my_ts)

# for month 7
out_m7 <- out[which(month(out$DateShipped)==7),]
my_ts_M7 <- xts(out_m7$R.Lat,out_m7$DateDelivered)
my_ts_M7 <- ts(my_ts_M7)

library(forecast)
top_ari <- auto.arima(my_ts)
top_ari <- arima(my_ts,order = c(2,2,2))
plot(forecast(top_ari,h=200))
# plot(predict(top_ari,10)) # example arima prediction for 10 step further

# for month 7
top_ari_M7 <- arima(my_ts_M7[1:100],order = c(2,2,2))
top_ari_M7 <- auto.arima(my_ts_M7[1:100])
plot(top_ari_M7)
best_top_ari_M7 <- best.arima(my_ts_M7[1:100])
png("educational_arima.png",height = 450,width = 800)
plot(forecast(top_ari_M7,h=10))
dev.off()
fit <- ets(my_ts_M7) # exponential smoothing state space model
plot(fit)

fit <- ets(my_ts)
plot(fit)


### PREDICTION - AREA OF CONFIDENCE AS PERCENTAGE ###
library(plotly)
library(forecast)

# my_ts_M7_samp <- my_ts_M7[sample(length(my_ts_M7),100)]
# my_ts_M7_samp <- my_ts[1:100]
my_ts <- ts()
fit <- ets(my_ts_M7_samp)
# plot(fit)
fore <- forecast(fit, h = 10, level = c(80, 95))

p_f <- plot_ly() %>%
  add_lines(x = time(my_ts_M7_samp), y = my_ts_M7_samp,
            color = I("black"), name = "observed") %>%
  add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], ymax = fore$upper[, 2],
              color = I("gray95"), name = "95% confidence") %>%
  add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1], ymax = fore$upper[, 1],
              color = I("gray80"), name = "80% confidence") %>%
  add_lines(x = time(fore$mean), y = fore$mean, color = I("blue"), name = "prediction")

p_f


library(neuralnet)
train7 <- Sort(out[which(lubridate::month(out$DateShipped)==7),],7,decreasing = TRUE) # for month 7
test8 <- Sort(out[which(lubridate::month(out$DateShipped)==8),],7,decreasing = TRUE) # for month 8
test8 <- test8[-which(test8[,5]>10^5),] # an invalid zip exits, so find and remove it
nn7 <- neuralnet(ShippingCost~SenderZip+ReceipentZip,
                data=train7, hidden=5)#,linear.output=FALSE)
# print(nn7)
plot(nn7)
pred <- compute(nn7,test8[1:100,c(4,5)])$net.result
comparison <- data.frame(test8[1:100,c(4,5,7)],Predicted=pred)
comparison

# # 3d scatter representation of predicted values
# library(plotly)
# abc <- plot_ly(x = out[which(lubridate::month(out$DateShipped)==8),4],y=out[which(lubridate::month(out$DateShipped)==8),5],
#                z = pred,type = "scatter3d",mode="markers") %>%
#   layout(zaxis = list(type="log"))
# abc


abcd <- plot_ly(x = 1:nrow(train8),type = "scatter",mode = "lines+markers",y=out[which(lubridate::month(out$DateShipped)==8),7],name="Observed") %>%
  add_lines(x = 1:nrow(train8),y=pred,name="Predicted") %>%
  layout(title = "Neural Net with 4 hidden layer")
abcd

pred
plot(nn)

### TO BE DELETED ### from here
###  FUNCTION FOR FILTERING ONLY BY ITEM NAME ###
Filter.ShippingData.Byname <- function(ItemNames, # contains the item names needed to be searched in raw shipdata
                                       raw_sale, # raw sale data to get corresponding SONumbers
                                       raw_ship, # input raw shipping data after it is formatted with Format.ShipData()
                                       location.info=TRUE, # in order to collect location lat/lon info about a row
                                       file.write=TRUE, # true if you want to save to csv file
                                       file.name="Shipping_Filtered.csv" # name of the file to be written
){
  # Filter
  SONumbers <- raw_sale[which(raw_sale[,5] %in% ItemNames),1] # get corresponding SONumbers
  print("I listed the sonumbers")
  print(SONumbers)
  SONumbers <- as.character(levels(factor(SONumbers))) # get levels as character
  print("I factorized the sonumbers")
  print(nrow(SONumbers))
  # Result <- Ship[-which(Ship$ShippingCost==0),] # filter by cost, exclude transactions with no cost
  # Result <- Result[-which(Result$Type == "UPS Ground"),] # filter by type, exclude UPS Ground shipping
  
  Result <- raw_ship
  Result <- Result[which(Result$SONumber %in% SONumbers),] # filter by type
  print(nrow(Result))
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

# Ship <- read.table("ShippingData_Months_10to12.txt",sep = ",",colClasses = "character")
# Ship <- Format.ShippingData(Ship)
# Raw <- read.table("Month12.txt",sep=",",colClasses = "character")
# Raw <- Format.SaleData(Raw)
out$ItemDescription[1]
filtered_byname <- Filter.ShippingData.Byname(as.character(out$ItemDescription[1]),raw_sale = Raw,raw_ship = Ship,
                                              file.name = "FLASH_32G_TOP_OUTCOST_M12.csv")

init <- read.csv("FLASH_32G_TOP_OUTCOST_M12.csv",row.names = NULL)

# following function should be tested
Match.ShipData <- function(shipfiltered, # ultimate filtered and location data included shipping data 20 columns
                           month, # input a month number as integer
                           saledata, # raw sale data e.g. Month10.txt, Month11.txt etc.
                           iterations = 10, # number of part to divide into when computing
                           filename = "Most_Expensive_Matched_M11_", # file name to be saved
                           begin = 1, # beginning of the for loop, could be continued from other steps
                           ship.sonumber = 8, # sonumber column in filtered shipping data
                           sale.sonumber = 1 # sonumber column in filtered sale data
){
  require("lubridate")
  # create a directory for tidier work
  main_path <- getwd() # "C:/Users/USER/Desktop/R"
  current_path <- paste(getwd(),paste("DataMatch",sample(100,1),sep="_"),sep = "/") # folder names
  dir.create(current_path) # create a new directory to store files
  setwd(current_path) # set new working directory
  
  # pull out just one month, and sort according to shipping cost
  temp_ship <- Sort(shipfiltered[which(month(shipfiltered$DateShipped)==as.integer(month)),],7,decreasing = TRUE)
  temp_raw <- Search.List(temp_ship[,ship.sonumber],saledata,sale.sonumber)[,-1] # get corresponding rows of saledata, by sonumber
  rows <- nrow(temp_ship) # get number of rows to use in following for loop
  ## divide into fractions and then unify into one big file, for Month10.txt
  for(i in begin:iterations){
    cat("Step",i,"\n")
    index <- seq((i-1)*(rows/iterations)+1,i*(rows/iterations)) # determine interval of indexes
    test_match <- Match.rows(temp_ship[index,],ship.sonumber,temp_raw,sale.sonumber) # match data and bind together as a data frame
    check_sen <- test_match[,4]==test_match[,23] & test_match[,5]==test_match[,24] # check receipent and sender zips whether they match
    test_match <- test_match[check_sen,] # take only who match by zips
    write.csv(test_match,paste(filename,i,".csv",sep = ""),row.names = FALSE) # write to file with order
    cat("File",paste(filename,i,".csv",sep = ""),"is saved to",getwd(),"\n") # inform user
  }
  
  Result <- data.frame()
  for(i in 1:iterations)
    Result <- rbind(Result,read.csv(paste(filename,i,".csv",sep = ""),row.names = NULL))
  
  setwd(main_path) # reset working directory
  write.csv(Result,paste(filename,".csv",sep = ""),row.names = FALSE)
  cat("File",paste(filename,".csv",sep = ""),"is saved to",getwd(),"\n") # inform user
  
  return(Result)
}

merged_temp <- Match.ShipData(init,month = 10,Raw,filename = "FLASH_32G_MERGED.csv")
### TO BE DELETED ### up to here

final_summary <- read.csv("/home/candar/Data_Final/final_summary.csv",row.names = NULL)
library(neuralnet)
nn_oc_2 <- neuralnet(AveShippingCost~AveDuration+AveDistance,data=final_summary, hidden=2)

Ship <- Format.ShippingData(read.table("ShippingData_Months_10to12.txt",sep = ",",colClasses = "character"))
trainer_ship <- na.omit(Convert(Ship,c(4,5))) # form a training data

final_summary[1,1]
trainer_ship_filtered <- trainer_ship[]

nn_oc_4 <- neuralnet(Duration~SenderZip+ReceipentZip,data=trainer_ship, hidden=4)
nn_oc_10 <- neuralnet(AveDuration~S.AveLat+S.AveLon+R.AveLat+R.AveLon,data=final_summary, hidden=10)


# print(nn7)
plot(nn_oc_4)
pred <- compute(nn7,test8[1:100,c(4,5)])$net.result
comparison <- data.frame(test8[1:100,c(4,5,7)],Predicted=pred)
comparison

Compare.Prediction <- function(network,
                               test,
                               observed,
                               plot.nn = TRUE,
                               plot.results = TRUE){
  require(neuralnet)
  if(plot.nn)
    plot(network)
  pred <- compute(nn7,test)$net.result
  if(plot.results){
    plot(observed,col = "red")
    points(pred)
  }
  return(data.frame(Test=test,Observed=observed,Predicted=pred))
}

res <- Compare.Prediction(nn_oc_4,init[1:100,c(4,5)],init[1:100,11],plot.nn = FALSE)
res


######################### TO DELETE 
thermalexp <- read.table("thermalexp.txt",sep = "\t",header = TRUE)
thermalexp$deltaT <- thermalexp$T2 - 19
lm_thermal <- lm(formula = deltaL ~ deltaT,data = thermalexp)


p <- plot_ly(data = thermalexp,x = ~deltaT,y = ~deltaL,type = "scatter",mode = "markers",name = "Observed") %>%
  add_lines(x = ~deltaT,y = fitted(lm_thermal),name = "Linear Fit")

p

htmlwidgets::saveWidget(as.widget(p),"thermal_deltaL_deltaT.html")
