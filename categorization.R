### CATEGORIZATION ###

Computer_HW <- Raw_SaleData[which.containingString(Raw_SaleData$ItemDescription,c("DELL","ACER","ASUS")),]

Computer_HW <- Computer_HW[-which.containingString(Computer_HW$ItemDescription,"DVD"),] # exclude dvd burners

# note that first word (abbreviation like: NB = netbook, MNTR = monitor) is a classifier
cats <- sapply(Raw_SaleData$ItemDescription,function(x) strsplit(x,split = " ")[[1]][[1]])
print(as.list(cats))
cats <- as.data.frame(levels(factor(as.character(cats))))

## pull out just desktops
desktops <- Raw_SaleData[which.containingString(Raw_SaleData$ItemDescription,"DT"),]
desktops <- unique(desktops)
desktops_matched <- par.Match.ShipData(Ship,month = 10,desktops[1:100,]) # for gather and matched the for final data, in october
# desktops_matched_un <- unique(desktops_matched) # clear overrides
desktops_summary <- Outcost.Summary(desktops_matched) # get each item's summary data
desktops_summary_bystate <- ByState.Summary(desktops_matched) # get each state's summary data
desktops_map_bystate <- ByState.Map(unique(desktops_summary_bystate),
                                    z = ~TotalUnitsShipped,
                                    colorname = "Total<br>Units<br>Shipped",
                                    main = "Desktop Sale by State")
desktops_map_bystate
units <- sum(na.omit(desktops_matched[which(desktops_matched$R.StateCode=="OR"),27]))

temp <- desktops_matched[which(desktops_matched$R.StateCode=="OR"),]
ByState.Map <- function(data, # summarized data by state (output of function ByState.Summary()), monthly
                        z = ~TotalShippingCost,
                        colorname = "Total<br>Shipping<br>Cost",
                        main="Distribution by State", # main title of the map
                        filename = NULL # name of the file to be saved
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
  p <- plot_ly(data = data , z = z, text = notes, locations = ~StateCode, type = 'choropleth',
               locationmode = 'USA-states', color = z, colors = 'Blues',
               marker = list(line = l), colorbar = list(title = colorname)) %>%
    layout(title = paste(main,'<br>(Hover for breakdown)',sep=""), geo = g)
  
  # save as an html page
  if(!is.null(filename))
    htmlwidgets::saveWidget(as.widget(p), filename)
  return(p)
}

# desktops_matched <- par.Match.ShipData(Raw_Ship_20,month = 10,desktops,filename = "desktops_oct.csv") # in server
##### printers

printers <- Raw_SaleData[which.containingString(Raw_SaleData$ItemDescription,"PRINTER"),]
cart <- Raw_SaleData[which.containingString(Raw_SaleData$ItemDescription,"CARTRIDGE"),]

library(lubridate)
daily <- data.frame(Days=seq.int(31),Sale_Printer=NA,Sale_Cartridge=NA)
daily$Sale_Printer <- sapply(seq.int(31),function(x) 
  sum(na.omit(printers$UnitsShipped[which(day(printers$ShippingDate)==x)])))
daily$Sale_Cartridge <- sapply(seq.int(31),function(x) 
  sum(na.omit(cart$UnitsShipped[which(day(cart$ShippingDate)==x)])))

library(corrplot)
crr <- cor(daily)
crr
corrplot(crr,method = "circle",type = "upper")


library(plotly)

p <- plot_ly(data = daily,x = ~Days,y = ~Sale_Printer,name = "Printer Sale",type = "scatter",mode = "lines",lines = list(color = "navy")) %>%
  add_trace(x = ~Days,y = ~Sale_Cartridge,name = "Cartridge Sale") %>%
  layout(title = "Correlated Product Sale Rate in October")
p

library(xts)
library(dygraphs)
library(tseries)
# dp_asts <- as.ts(daily[,1:2]) # just the number of day in a month, simplar time series
inds <- seq(as.Date(printers$ShippingDate[which.min(as.Date(printers$ShippingDate))]),
            as.Date(printers$ShippingDate[which.max(as.Date(printers$ShippingDate))]),
            by = "day") # form a day sequence with dates, in a given range
inds

dp_asts <- data.frame(Date=inds,Sale_Printer=daily$Sale_Printer)
dp_asts # adjust to process in dygraphs and stationarity tests and forecasting

# temp <- ts(daily$Sale_Printer,start = c(2014, as.numeric(format(inds[1], "%j"))),frequency = 365)

adf.test(dp_asts,alternative = "stationary")


dc_asts <- as.ts(daily[,c(1,3)])
print(dc_asts)


# 
# # try overlapping histograms
# p <- plot_ly(alpha = 0.6) %>%
#   add_histogram(x = daily$Sale_Printer) %>%
#   add_histogram(x = daily$Sale_Cartridge) %>%
#   layout(barmode = "overlay")
# p
# 
# # bins itself
# p <- plot_ly(x = day(printers$ShippingDate), type = "histogram")
# p
# 
# 
# p <- plot_ly(x = ~rnorm(50), type = "histogram")
# p

##### CLASS ATTEMPT ######

# setClass("RangedNumeric",
#          contains = "numeric",
#          slots = list(min = "numeric", max = "numeric"))
# rn <- new("RangedNumeric", 1:10, min = 1, max = 10)
# 
# rn@min
# rn@max
# rn@.Data
# 
# 
# ########
# setClass("Person",
#          slots = list(name = "character", age = "numeric"))
# setClass("Employee",
#          slots = list(boss = "Person"),
#          contains = "Person")
# 
# alice <- new("Person", name = "Alice", age = 40)
# john <- new("Employee", name = "John", age = 20, boss = alice)
# 
# 
# 
# #######
# 
# setClass("Category",slots = list(content = "data.frame"))
# temp <- new("Category",content = Raw_SaleData)
# 
# head(temp@content)

# ################# CATEGORIZATION WITHOUT OOP BUT TEXT MINING !!! ###################
# library(tm)
# library(stringi)
# library(proxy)
# wiki <- "http://en.wikipedia.org/wiki/"
# titles <- c("Integral", "Riemann_integral", "Riemann-Stieltjes_integral", "Derivative",
#             "Limit_of_a_sequence", "Edvard_Munch", "Vincent_van_Gogh", "Jan_Matejko",
#             "Lev_Tolstoj", "Franz_Kafka", "J._R._R._Tolkien")
# articles <- character(length(titles))
# 
# for (i in 1:length(titles)) {
#   articles[i] <- stri_flatten(readLines(stri_paste(wiki, titles[i])), col = " ")
# }
# 
# docs <- Corpus(VectorSource(articles))
# docs[[1]]
# 
# docs2 <- tm_map(docs, function(x) stri_replace_all_regex(x, "<.+?>", " "))
# docs3 <- tm_map(docs2, function(x) stri_replace_all_fixed(x, "\t", " "))
# 
# docs4 <- tm_map(docs3, PlainTextDocument)
# docs5 <- tm_map(docs4, stripWhitespace)
# docs6 <- tm_map(docs5, removeWords, stopwords("english"))
# docs7 <- tm_map(docs6, removePunctuation)
# docs8 <- tm_map(docs7, tolower)
# 
# docs8[[1]]
# 
# docsTDM <- TermDocumentMatrix(docs8)
# 
# docsdissim <- dissimilarity(docsTDM, method = "cosine")
# 
# docsdissim2 <- as.matrix(docsdissim)
# rownames(docsdissim2) <- titles
# colnames(docsdissim2) <- titles
# docsdissim2
# h <- hclust(docsdissim, method = "ward")
# 
# plot(h, labels = titles, sub = "")


# amount of sale on daily basis
# if the amount of days are huge then do this: Raw_SaleData <- Raw_SaleData[-which(year(Raw_SaleData$ShippingDate)<2000),]
sales.daily <- function(raw_sale,category){
  library(lubridate)
  days <- as.numeric(as.Date(raw_sale$ShippingDate[which.max(as.Date(raw_sale$ShippingDate))])-
               as.Date(raw_sale$ShippingDate[which.min(as.Date(raw_sale$ShippingDate))]))
  if(days > 365) days <- 365
  Result <- data.frame(Days=seq.int(days),Sale_Number=NA)
  
  temp <- raw_sale[which.containingString(raw_sale$ItemDescription,category),c(2,7)]

  Result$Sale_Number <- sapply(seq.int(days),function(x) 
    sum(na.omit(temp$UnitsShipped[which(day(temp$ShippingDate)==x)])))
  return(Result)
}

temp <- sales.daily(Raw_SaleData,"MNTR")

library(lubridate)
days <- as.numeric(as.Date(raw_sale$ShippingDate[which.max(as.Date(raw_sale$ShippingDate))])-
                     as.Date(raw_sale$ShippingDate[which.min(as.Date(raw_sale$ShippingDate))]))
if(days > 365) days <- 365
Result <- data.frame(Days=seq.int(days),Sale_Number=NA)

temp <- sapply(c("DT","NB","MNTR"),function(x) 
  which.containingString(Raw_SaleData$ItemDescription[sample(nrow(Raw_SaleData),1e4)],x))


Result$Sale_Number <- sapply(seq.int(days),function(x) 
  sum(na.omit(temp$UnitsShipped[which(day(temp$ShippingDate)==x)])))



# raw <- Raw_SaleData[sample(nrow(Raw_SaleData),1e4),]
Result <- sapply(c("DT","NB","MNTR"),function(x){ 
  temp <- raw[which.containingString(raw$ItemDescription,x),c(2,7)] # only shipping date and unitsshipped
  sapply(seq.int(daynum),function(y) 
    sum(na.omit(temp$UnitsShipped[which(day(temp$ShippingDate)==y)])))
  })


head(raw[temp[[3]],])
lengths(temp)


# Please note that this function works just for ONE MONTH of data, e.g. Month10.txt etc.
# amount of daily sales for a full month, for a given list of item names as strings
# if the amount of days are huge then do this: Raw_SaleData <- Raw_SaleData[-which(year(Raw_SaleData$ShippingDate)<2000),]
sales.daily <- function(raw_sale,category){
  require(lubridate)
  daynum <- days_in_month(raw_sale$ShippingDate[sample.int(nrow(raw_sale),1)]) # get number of days in that month
  Result <- sapply(category,function(x){ 
    temp <- raw_sale[which.containingString(raw_sale$ItemDescription,x),c(2,7)] # only shipping date and unitsshipped
    sapply(seq.int(daynum),function(y) 
      sum(na.omit(temp$UnitsShipped[which(day(temp$ShippingDate)==y)])))
  })
  return(data.frame(Days=seq.int(daynum),Result))
}

temp <- sales.daily(Raw_SaleData,category = c("DT","NB","MNTR"))

cor(temp)

raw <- Raw_SaleData[sample(nrow(Raw_SaleData),nrow(Raw_SaleData)/100),]
cats <- sapply(na.omit(raw$ItemDescription),function(x) strsplit(x,split = " ")[[1]][[1]])
cats <- as.data.frame(levels(factor(as.character(cats))))
nums <- sapply(cats[1:5,],function(x) length(which.containingString(Raw_SaleData$ItemDescription,x)))

# TO DETERMINE THE CATEGORIES TO SEARCH FOR
## parallel test (test performed on linux server)
raw <- Raw_SaleData[sample(nrow(Raw_SaleData),nrow(Raw_SaleData)/100),]
cats <- sapply(na.omit(raw$ItemDescription),function(x) strsplit(x,split = " ")[[1]][[1]])
cats <- as.data.frame(levels(factor(as.character(cats))))
library(parallel)
# force(cats);force(Raw_SaleData)
cl <- makeCluster(detectCores())
clusterExport(cl,c("cats","Raw_SaleData","which.containingString"))
nums <- parSapply(cl,cats[1:nrow(cats),],function(x) length(which.containingString(Raw_SaleData$ItemDescription,x)))
stopCluster(cl)




sale_amounts <- sales.daily(Raw_SaleData,category = c("MEM","CPU","MB","MNTR","VGA","SSD","HDD","DT","NB"))
# sale_ams_big <- sales.daily(Raw_SaleData,category = as.character(Sort(catnums,2,decreasing = T)[1:100,1]))
sale_ams_big <- sales.daily(Raw_SaleData,category = as.character(Sort(temp,2,decreasing = T)[1:100,1])) # in server
write.csv(sale_ams_big,"CAT_SALES_OCTOBER.csv",row.names = FALSE)

sale_ams_big <- read.csv("CAT_SALES_OCTOBER.csv",row.names = NULL)
sale_ams_big_cor <- cor(sale_ams_big[,-1])
library(corrplot)
corrplot(sale_ams_big_cor[1:20,1:20])
temp <- sapply(seq.int(nrow(sale_ams_big_cor)),function(x){
  cbind(x,order(sale_ams_big_cor[x,],decreasing = T)[2]) # take the second max since the max is 1, and is trivial
})
library(dplyr)
temp <- as.data.frame(cbind(temp[1,],temp[2,]))
# temp <- lapply(temp,as.data.frame)
# temp <- bind_cols(temp)

tops <- data.frame(Val=NA,xind=NA,yind=NA)
for(i in 1:nrow(temp)){
  tops[i,] <- cbind(sale_ams_big_cor[temp[i,1],temp[i,2]],temp[i,1],temp[i,2])
  # print(sale_ams_big_cor[temp[i,1],temp[i,2]])
}
tops
which.max(tops$Val)
tops[which.max(tops$Val),]
print(sale_ams_big_cor[tops[which.max(tops$Val),2],tops[which.max(tops$Val),3]])
# arbitrarily choose two categories: "VGA" and "MB"

vga_mb <- sales.daily(Raw_SaleData,c("VGA","MB"))

library(plotly)

p <- plot_ly(data = vga_mb,x = ~Days,y = ~VGA,name = "Graphics Card Sale",type = "scatter",
             mode = "lines") %>%
  add_trace(x = ~Days,y = ~MB,name = "Motherboard Sale") %>%
  layout(title = "Correlated Product Sale Rate in October")
p

htmlwidgets::saveWidget(as.widget(p),"Correlated_OCT_VGA_MB.html")

library(dygraphs)
d <- dygraph(vga_mb,main = "Correlated Product Sales in October") %>%
  dySeries("VGA",label = "Graphics Card") %>%
  dySeries("MB",label = "Motherboard") %>%
  dyAxis("y",label = "Number of Sales") %>%
  dyAxis("x",label = "Days") %>%
  dyOptions(stackedGraph = T) %>%
  dyRangeSelector()

d
  
htmlwidgets::saveWidget(as.widget(d),"dygraphs_correlated_products.html")

library(forecast)
library(tseries)
library(xts)

temp <- xts(vga_mb[,2],seq(as.Date("2012-10-1"),as.Date("2012-10-31"),by = "day"))
temp <- ts(temp)

# hw <- HoltWinters(temp)
adf.test(temp,alternative = "stationary") # augmented dickey-fuller test
acf(temp) # auto cross correlation

# pred <- auto.arima(temp) ### 1

# pred <- msts(temp,seasonal.periods = c(7,365.5)) ### 2
# pred <- tbats(temp)

# pred <- ets(temp) ### 3

forecast(pred,h=31)

# pred <- predict(temp,n.ahead=30,prediction.interval=T)

### CATEGORY FINDER FUNCTION
# categories <-function()
raw <- Raw_SaleData[sample(nrow(Raw_SaleData),nrow(Raw_SaleData)/100),]
cats <- sapply(na.omit(raw$ItemDescription),function(x) strsplit(x,split = " ")[[1]][[1]])
cats <- as.data.frame(levels(factor(as.character(cats))))
library(parallel)
# force(cats);force(Raw_SaleData)
cl <- makeCluster(detectCores())
clusterExport(cl,c("cats","Raw_SaleData","which.containingString"))
nums <- parSapply(cl,cats[1:nrow(cats),],function(x) length(which.containingString(Raw_SaleData$ItemDescription,x)))
stopCluster(cl)


### ON SERVER 6 months evaluation
Result <- data.frame()
for(i in 1:6){
  paths <- c("/home/candar/ShippingData_Months_07to09/Month7.txt",
             "/home/candar/ShippingData_Months_07to09/Month8.txt",
             "/home/candar/ShippingData_Months_07to09/Month9.txt",
             "/home/candar/ShippingData_Months_10to12/Month10.txt",
             "/home/candar/ShippingData_Months_10to12/Month11.txt",
             "/home/candar/ShippingData_Months_10to12/Month12.txt")
  
  raw <- read.table(paths[i],sep = ",",colClasses = "character")
  raw <- Format.SaleData(raw)
  
  # get how much a sale number of a category
  temp <- sales.daily(raw,category = as.character(Sort(catnums,2,decreasing = T)[1:100,1])) # in server
  Result <- rbind(Result,temp)
}

write.csv(Result,"6MONTHS_CATEGORY_SALES.csv",row.names = FALSE)

# TO DELETE
temp <- sales.daily(Raw_SaleData[seq.int(5e4),],list("VGA","MB",c("PRT","PRINTER")))
temp1 <- sales.daily(Raw_SaleData[seq.int(5e4),],c("VGA","MB","PRINTER"))
temp2 <- sales.daily(Raw_SaleData[seq.int(5e4),],c("VGA","MB","PRT"))
#################################
#################################
#################################
#################################


daily_stats <- read.csv("6MONTHS_CATEGORY_SALES.csv",row.names = NULL)
daily_stats$Date <- seq(as.Date("2012-07-01"),as.Date("2012-12-31"),by = "day")

cor_stats <- cor(daily_stats[,-102])
df <- cor_stats[-1,-1]
library(plotly)
# clr <- colorRamp(c("white","deeppink","purple","black"))
# clr <- colorRamp(c("white","yellow","purple","navy"))
# clr <- colorRamp(c("navy","purple","yellow","white"))
# clr <- colorRamp(c("darkorchid","darkgreen","yellow"))
# clr <- colorRamp(c("white","gray","yellow","red","brown"))
p <- plot_ly(x = rownames(df),y = colnames(df),colors = clr,z = df,type = "heatmap") %>%
  layout(title = "Bestseller 100 Categories Daily Sale Correlations 2nd Half of 2012") %>% 
  colorbar(title = "Correlation")#,rangeslider = list("c"))
p

htmlwidgets::saveWidget(as.widget(p),"CORRELATIONS_HEATMAP_100_colors.html")

## make quantiles for 80%+, 60%+ and 40%+
quantiles <- matrix(NA,100,100)
for(i in 1:100+1){
  for(j in 1:100+1){
    if(cor_stats[i,j] < 0.20)
      quantiles[i-1,j-1] <- 0.0
    if(cor_stats[i,j] >= 0.20 && cor_stats[i,j] < 0.40)
      quantiles[i-1,j-1] <- 0.2
    if(cor_stats[i,j] >= 0.40 && cor_stats[i,j] < 0.60)
      quantiles[i-1,j-1] <- 0.4
    else if(cor_stats[i,j] >= 0.60 && cor_stats[i,j] < 0.80)
      quantiles[i-1,j-1] <- 0.6
    else if(cor_stats[i,j] >= 0.80)
      quantiles[i-1,j-1] <- 0.8
  }
}

quantiles <- as.data.frame(quantiles)
rownames(quantiles) <- rownames(df)
colnames(quantiles) <- colnames(df)

pq <- plot_ly(x = rownames(quantiles),y = colnames(quantiles),z = df, type = "heatmap") %>%
  layout(title = "Bestseller 100 Categories Daily Sale Correlations 2nd Half of 2012") %>% 
  colorbar(title = "Correlation")

pq

# htmlwidgets::saveWidget(as.widget(p),"CORRELATIONS_HEATMAP_100.html")

####
library(plotly)

p <- plot_ly(data = daily_stats,x = ~Date,y = ~VGA,name = "Graphics Card Sale",type = "scatter",
             mode = "lines") %>%
  add_trace(x = ~Days,y = ~MB,name = "Motherboard Sale") %>%
  layout(title = "Correlated Product Sale Rate<br>Second Half of 2012")
p

temp <- daily_stats[,c(102,2,3)]
xts(daily_stats[,c(2,3)],daily_stats[,102])

library(dygraphs)
d <- dygraph(xts(daily_stats[,c(2,3)],daily_stats[,102]),main = "Correlated Product Sales of 6 Months") %>%
  # dySeries("VGA",label = "Graphics Card") %>%
  # dySeries("MB",label = "Motherboard") %>%
  dyAxis("y",label = "Number of Sales") %>%
  dyAxis("x",label = "Days") %>%
  dyOptions(stackedGraph = T) %>%
  dyRangeSelector()

d

htmlwidgets::saveWidget(as.widget(d),"dygraphs_correlated_products_2ndhalf.html")

###------###------###------###------###------###------###------###------###------###------###------###------###------

library(forecast)
library(tseries)
library(xts)


# temp <- xts(daily_stats[,5],daily_stats[,102]) # take only motherboards
temp <- xts(daily_stats[,5],daily_stats[,102]) # take only motherboards
temp <- ts(temp)
temp
time(temp)

# hw <- HoltWinters(temp)
adf.test(temp,alternative = "stationary") # augmented dickey-fuller test
acf(temp) # auto cross correlation

pred <- auto.arima(temp) ### 1

# pred <- msts(temp,seasonal.periods = c(7,365.5)) ### 2
# pred <- tbats(temp)

# pred <- ets(temp) ### 3

fore <- forecast(pred,h=31)
fore
# pred <- predict(pred,n.ahead = 31)
# pred

# init_f <- cbind(fore$lower,fore$fitted,fore$upper)

# library(dygraphs)
# d <- dygraph(fore,main = "Correlated Product Sales of 6 Months") %>%
#   # dySeries("V1",label = "Motherboard") %>%
#   # dySeries(c("lwr", "fit", "upr"), label = "MotherBoard") %>%
#   dySeries(c("lower", "fitted", "upper"), label = "MotherBoard") %>%
#   dyAxis("y",label = "Number of Sales") %>%
#   dyAxis("x",label = "Days") %>%
#   # dyOptions(stackedGraph = T) %>%
#   dyRangeSelector()
# 
# d
library(plotly)
p_f <- plot_ly() %>%
  add_lines(x = time(temp), y = temp,
            color = I("darkorchid"), name = "observed") %>%
  add_ribbons(x = time(fore$mean), ymin = fore$lower[, 2], ymax = fore$upper[, 2],
              color = I("gray95"), name = "95% confidence") %>%
  add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1], ymax = fore$upper[, 1],
              color = I("gray80"), name = "80% confidence") %>%
  add_lines(x = time(fore$mean), y = fore$mean, color = I("blue"), name = "prediction") %>%
  layout(title = "Motherboard Category Sales Prediction with ARIMA<br>2nd Half of 2012",
         xaxis = list(title = "Days",rangeslider = list(type = "date")),
         yaxis = list(title = "Number of Sales"))

p_f

htmlwidgets::saveWidget(as.widget(p_f),"MB_SALES_ARIMA.html")

library(neuralnet)

traindata <- temp
testdata <- 


###------###------###------###------###------###------###------###------###------###------###------###------###------
library(forecast)
library(tseries)
library(xts)
library(neuralnet)

tn <- 153 # take first "tn" elements to form train data

# train data for first 5 months
train <- data.frame(Days=seq(nrow(daily_stats)),MB=daily_stats[,5],VGA=daily_stats[,7],MEM=daily_stats[,2])[seq(tn),]
train

# test data for last month, 30 days
test <- data.frame(Days=tn:nrow(daily_stats),
                   MB=daily_stats[tn:nrow(daily_stats),5],
                   VGA=daily_stats[tn:nrow(daily_stats),7],
                   MEM=daily_stats[tn:nrow(daily_stats),2])
test

# build a model of network
nn_model <- neuralnet(MB~VGA+MEM,data=train,hidden=10,learningrate=0.01)#,linear.output=FALSE)
plot(nn_model)

# make prediction
pred <- compute(nn_model,test[,-c(1,2)])$net.result
pred


Raw_MEM <- Raw_Sale[par.which.containingString(Raw_Sale$ItemDescription,"MEM"),]

sales.perElement <- function(data,col.source,col.target,category){
  src <- data[,col.source] # this will be searched
  tgt <- data[,col.target] # in this
  
  Result <- sapply(src,function(x){ 
    temp <- length(which(tgt==x)) # only shipping date and unitsshipped
    sapply(seq.int(daynum),function(y)
      sum(na.omit(temp$UnitsShipped[which(day(temp$ShippingDate)==y)])))
  })
  print(Result)
  return(data.frame(Days=seq.int(daynum),Result))
}

# search for a vector or a list in a data with a specific column of "col.target" and get total values of "col.info"
sales.perElement <- function(raw_sale, # data to be searched in
                             source, # vector or list to be searched for
                             col.target, # target column for raw_sale to search
                             col.info){ # get the information of this column and sum it for each element
  temp <- class(source) # store the initial type of data
  source <- levels(factor(source)) # get levels to prevent overlapping (do not search an element more than once)
  # please note that assigning the initial class speeds up the process
  class(source) <- temp # assign the initial type again
  Result <- sapply(source,function(x) # get values for each element
    temp <- sum(na.omit(raw_sale[which(raw_sale[,col.target]==x),col.info]))) # get corresponding data per source element (vector)
  return(data.frame(Source=source,Result))
}

# test the function sales.perElement
# temp <- sales.perElement(matched_MEM,matched_MEM$ReceipentZip,col.target = 5,col.info = 27)
# temp

temp <- sales.perElement(matched_MEM,1:10,col.target = 11,col.info = 27)
temp

temp <- cbind(Raw_MEM,par.LocationData(Raw_MEM,col.sen = 3,col.rec = 4))

# THIS IS NOT FOR MULTIPLE CATEGORIES, WORKS FOR JUST ONE CATEGORY, ONE MONTH
# input a list, function will search its elements in raw_sale in the column index col.target
sales.daily.perElement <- function(raw_sale,source,col.target){
  require(lubridate)
  daynum <- days_in_month(raw_sale$ShippingDate[sample.int(nrow(raw_sale),1)])
  Result <- sapply(source,function(x){ 
    temp <- raw_sale[which(raw_sale[,col.target]==x),c(2,7)] # only shipping date and unitsshipped
    sapply(seq.int(daynum),function(y) 
      sum(na.omit(temp$UnitsShipped[which(day(temp$ShippingDate)==y)])))
  })
  return(data.frame(Days=seq.int(daynum),Result))
}

temp_ <- sales.daily.perElement(raw_sale = Raw_MEM,Raw_MEM$ReceipentZip,4)

temp_ <- sales.daily.perElement(raw_sale = temp,state.abb,16)




Import.SaleData <- function(filename){
  temp <- read.table(filename,sep = ",",colClasses = "character")
  return(Format.SaleData(temp))
}

Import.ShipData <- function(filename){
  temp <- read.table(filename,sep = ",",colClasses = "character")
  return(Format.ShippingData(temp))
}

