# multiplotly function (INCOMPLETE)
MultiPlotly <- function(x,                    # data of x axis
                        y,                    # data set of y axis
                        main = "Plot_ly",      # main title
                        fname = "Plot_ly",        # prefix for filename
                        color = "black",
                        type = "scatter",
                        mode = "markers",
                        # text = NULL,
                        size = NULL,
                        col.blank = NULL
){      # height as inches
  
  require(plotly)
  # if(is.null(colnames(x)))
    # x <- as.data.frame(x) # convert to data frame if not already, (x maybe a multicolumn object too)
  
  # f <- list(family = "Verdana, monospace",size = 18,color = "#7f7f7f")
  
  # xit <- ncol(x)
  # if(is.null(xit)) xit <- 1
  # yit <- ncol(y)
  # if(is.null(yit)) yit <- 1
  m <- list(b = 200)
  
  x <- as.data.frame(x)
  y <- as.data.frame(y)
  for(j in 1:ncol(x)){ # 1 through total column number, for x
    
    # rearrange the order for factors and characters, prevent unwanted sorting
    if(class(x[,j])=="factor" || class(x[,j])=="character")
      xlabel <- list(title = colnames(x[,j]),categoryorder = "array",categoryarray = x[,j])
    
    # predetermined bottom margins for better display, apply for selected columns
    if(any(j == col.blank)) margin = m
    else margin = NULL
    
    for(i in 1:ncol(y)){ # 1 through total column number, for y
      
      if (identical(x[,j],y[,i])) next() # pass through next step if identical vectors encountered
      
      # initialize nomenclature
      filename <- paste(fname,"_",i+((j-1)*ncol(y)),"_",colnames(y[i]),
                        "_vs_",colnames(x[j]),".html",sep = "") # form a file name with an index
      plotname <- list(title = paste(main," #",i,sep = "")) #,titlefont = f)
      
      ylabel <- list(title = colnames(y[,i])) #,titlefont = f)
      
      # plot and save
      p <- plot_ly(x=x[,j],y=y[,i],type=type,mode=mode,size = size,color = color) %>%
        layout(title = plotname,xaxis = xlabel,yaxis = ylabel,margin = margin)
      htmlwidgets::saveWidget(as.widget(p),filename)
      cat("Image",filename,"saved to",getwd(),"\n")
    }
  }
  return (TRUE)
}

# test of above function, multiplotter for plotly graphs
samp <- sample(nrow(summary_outcost),100)
MultiPlotly(x = 1:100,
            y = summary_outcost[samp,],
            color = summary_outcost[samp,2])


# extract content for easier troubleshooting

# ad-hoc inputs
samp <- sample(nrow(summary_outcost),100)
x = 1:100
y = summary_outcost[samp,]
main = "Plot_ly"
fname = "Plot_ly"
# color = y$TotalShippingCost
color = NULL
type = "scatter"
mode = "markers"
size = NULL
col.blank = NULL

require(plotly)

m <- list(b = 200)

x <- as.data.frame(x)
# y <- as.data.frame(y)
for(j in 1:ncol(x)){ # 1 through total column number, for x
  
  # rearrange the order for factors and characters, prevent unwanted sorting
  if(class(x[,j])=="factor" || class(x[,j])=="character")
    xlabel <- list(title = colnames(x[j]),categoryorder = "array",categoryarray = x[,j])
  else
    xlabel <- list(title = colnames(x[j]))
  
  cat("x column name:",colnames(x[j]),"\n")
  
  # predetermined bottom margins for better display, apply for selected columns
  if(any(j == col.blank)) margin = m
  else margin = NULL
  
  for(i in 1:ncol(y)){ # 1 through total column number, for y
    cat("y column name:",colnames(y[j]),"\n")
    
    if (identical(x[,j],y[,i])) next() # pass through next step if identical vectors encountered
    
    # initialize nomenclature
    filename <- paste(fname,"_",i+((j-1)*ncol(y)),"_",colnames(y[i]),
                      "_vs_",colnames(x[j]),".html",sep = "") # form a file name with an index
    cat("filename : ",filename,"\n")
    
    plotname <- paste(main," #",i,sep = "") #,titlefont = f)
    cat("plotname :",plotname,"\n")
    
    ylabel <- list(title = colnames(y[i])) #,titlefont = f)
    
    # plot and save
    p <- plot_ly(x=x[,j],y=y[,i],type=type,mode=mode,size = size,color = color) %>%
      layout(title = plotname,xaxis = xlabel,yaxis = ylabel,margin = margin)
    htmlwidgets::saveWidget(as.widget(p),filename)
    cat("Image",filename,"saved to",getwd(),"\n")
  }
}



# multiplotly function  THIS FUNCTION IS WORKING!!
MultiPlotly <- function(x,                    # data of x axis
                        y,                    # data set of y axis
                        main = "Plot_ly",      # main title
                        fname = "Plot_ly",        # prefix for filename
                        color = NULL,
                        colorname = NULL,
                        type = "scatter",
                        mode = "markers",
                        text = NULL, # hover text
                        size = NULL,
                        logy = NULL,
                        col.blank = NULL # column numbers which require a margin from bottom for easy observation of names
){      # height as inches
  require(plotly)
  m <- list(b = 200)
  
  dt_classes <- c("POSIXct","POSIXlt","POSIXt")
  x <- as.data.frame(x)
  y <- as.data.frame(y)
  clr <- as.numeric(as.character(color))
  for(j in 1:ncol(x)){ # 1 through total column number, for x
    all_grobs <- NA
    # rearrange the order for factors and characters, prevent unwanted sorting
    if(class(x[,j])=="factor" || class(x[,j])=="character"){
      xlabel <- list(title = colnames(x[j]),categoryorder = "array",categoryarray = x[,j])
      print("Adjusting category order for x axis ...")
    }
    else
      xlabel <- list(title = colnames(x[j]))
    
    # predetermined bottom margins for better display, apply for selected columns
    if(any(j == col.blank)) margin = m
    else margin = NULL
    
    for(i in 1:ncol(y)){ # 1 through total column number, for y
      if (identical(x[,j],y[,i])) next() # pass through next step if identical vectors encountered

      plotname <- paste(main," #",i,sep = "") # initialize nomenclature
      
      # set the axis type to log scale according to column numbers
      if (!is.null(logy) && any(logy == i))
        current_type <- "log"
      else 
        current_type <- NULL
      
      # rearrange the order for factors and characters, prevent unwanted sorting
      # if(class(y[,i])=="factor" || class(y[,i])=="character"){
      if((class(y[,i])=="factor" || class(y[,i])=="character") && !any(class(y[,i]) %in% dt_classes)){
        ylabel <- list(title = colnames(y[i]),categoryorder = "array",categoryarray = y[,j],type = current_type)
        print("Adjusting category order for y axis ...")
      }
      else
        ylabel <- list(title = colnames(y[i]),type = current_type)
      
      # plot and save
      p <- plot_ly(x=x[,j],y=y[,i],type=type,mode=mode,size = size,color = clr,text = text) %>%
        layout(title = plotname,xaxis = xlabel,yaxis = ylabel,margin = margin) %>%
        colorbar(title = colorname)
      
      cat((j-1)*ncol(y)+i,colnames(x[j]),"vs",colnames(y[i]),"is plotted.\n")
      all_grobs <- list(all_grobs,p) # maybe try without wrapping with as.widget()
    }
    # save all of the graphic objects as an html page list
    print("Saving in progress...")
    savename <- paste(fname,colnames(x[j]),"vs_all_LIST.html",sep = "_")
    save_tags(all_grobs,savename)
    cat("Image",savename,"saved to",getwd(),"\n")
  }
  return (TRUE)
}

set.seed(12345)
samp <- sample(nrow(summary_outcost),100)
col.nonfactor <- lapply(final_summary,function(x) class(x))

MultiPlotly(x = 1:100,
            y = final_summary[samp,-1],
            mode = "lines+markers",
            fname = "Plotly_test_mult_",
            logy = c(1,2,3,4,5),
            text = final_summary$Product[samp],
            color = final_summary$TotalShippingCost[samp])
#######################################
# 2 dimensional plotting, prepare data
my_samp <- final_matched[sample(nrow(final_matched),10^4),-c(21,23,24)]
my_samp$DateShipped <- as.POSIXct(my_samp$DateShipped)
my_samp$DateDelivered <- as.POSIXct(my_samp$DateDelivered)
my_samp$ShippingDate <- as.POSIXct(my_samp$ShippingDate)
my_samp <- Convert(my_samp,c(1,2,3,6,16,17,18,19,22),class = "character")
# my_samp <- my_samp[,-which("factor"==lapply(my_samp, function(x) class(x)))] # take out which column is factor
MultiPlotly(x = my_samp,
            y = my_samp,
            # mode = "lines+markers",
            fname = "OutcostData_2D_Scatter",
            main = "Outcost Data 2-D Scatter Plot",
            # logy = c(1,2,3,4,5),
            text = my_samp$ItemDescription,
            color = my_samp$ShippingCost,
            colorname = "ShippingCost")

MultiPlotly(x = my_samp,
            y = my_samp,
            mode = "lines+markers",
            fname = "OutcostData_2D_Line",
            main = "Outcost Data 2-D Line Plot",
            # logy = c(1,2,3,4,5),
            text = my_samp$ItemDescription,
            color = my_samp$ShippingCost,
            colorname = "ShippingCost")

# rearranged receipent zips
temp <- my_samp[which(my_samp$ReceipentZip<10^5),]
MultiPlotly(x = temp,
            y = temp,
            mode = "lines+markers",
            fname = "OutcostData_2D_Line",
            main = "Outcost Data 2-D Line Plot",
            text = temp$ItemDescription,
            color = temp$ShippingCost,
            colorname = "ShippingCost")

MultiPlotly(x = temp,
            y = temp,
            fname = "OutcostData_2D_Scatter",
            main = "Outcost Data 2-D Scatter Plot",
            # logy = c(1,2,3,4,5),
            text = temp$ItemDescription,
            color = temp$ShippingCost,
            colorname = "ShippingCost")

MultiPlotly(x = 1:nrow(temp),
            y = temp,
            fname = "OutcostData_1D_Scatter",
            main = "Outcost Data 1-D Scatter Plot",
            text = temp$ItemDescription,
            color = temp$ShippingCost,
            colorname = "ShippingCost")
#######################################

# one dimensional presentation of summary of entire outcost data
tfs <- final_summary
tfs$Product <- as.character(final_summary$Product) # convert to character from factor
MultiPlotly(x = 1:nrow(tfs),
            y = tfs,
            fname = "Outcost_Summary_1D_Scatter",
            main = "Summary of Outcost Data as 1D Scatter Plot",
            color = tfs$TotalShippingCost,
            logy = c(2,3,4,5,6),
            text = tfs$Product,
            colorname = "TotalCost")

MultiPlotly(x = tfs,
            y = tfs,
            fname = "Outcost_Summary_2D_Scatter",
            main = "Summary of Outcost Data as 2D Scatter Plot",
            color = tfs$TotalShippingCost,
            logy = c(2,3,4,5,6),
            text = tfs$Product,
            colorname = "TotalCost")

MultiPlotly(x = tfs,
            y = tfs,
            fname = "Outcost_Summary_2D_Line",
            main = "Summary of Outcost Data as 2D Line Plot",
            mode = "lines+markers",
            color = tfs$TotalShippingCost,
            logy = c(2,3,4,5,6),
            text = tfs$Product,
            colorname = "TotalCost")


# all_grobs <- htmlwidgets::appendContent(all_grobs,p)

# test of MultipPlotly ends right here

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

Top100 <- Product.List(Raw_M10,"mostprofitable",3)
# Top100 <- Top100[-c(4,5,18,24,26,53,54,84),] # exclude unwanted
M12_Top100_SaleData <- Partial.SaleData(Top100[,1],Raw_M10)
M12_Top100_ShipData <- Partial.ShipData(saledata = M12_Top100_SaleData,
                                        ship = Ship)# collect shipping data and more

# ShipOriginal <- Ship
Ship <- ShipOriginal
Ship <- Ship[sample(nrow(ShipOriginal),10^5),]
####¦
Top10_BS_M10 <- Product.List(Raw_M10,"bestseller",10,samp.perc = 5) # as a test
# Top100 <- Top100[-c(4,5,18,24,26,53,54,84),] # exclude unwanted
M10_Top10_SaleData_BS <- Partial.SaleData(Top10_BS_M10[1:5,1],Raw_M10)
system.time({
M10_Top10_ShipData_BS <- Partial.ShipData.List(saledata = M10_Top10_SaleData_BS,
                                        ship = Ship[sample(nrow(Ship),10^4),]) # collect shipping data and more
})
write.csv(M10_Top10_ShipData_BS,"TOP10_M10_SHIP_BS.csv",row.names = FALSE)
###

system.time({
Top100_BS_M10 <- Product.List(Raw_M10,"bestseller",110,samp.perc = 20) # as a test
M10_Top100_SaleData_BS <- Partial.SaleData(Top100_BS_M10[,1],Raw_M10)
M10_Top100_ShipData_BS <- Partial.ShipData.List(saledata = M10_Top100_SaleData_BS,
                                                ship = Ship)# collect shipping data and more
write.csv(M10_Top10_ShipData_BS,"TOP100_M10_SHIP_BS.csv",row.names = FALSE)
})

####
# Ship[sample(nrow(Ship),10^4),]
Top_CargoTypes <- CargoTypes(TopProduct,Top_ShipData,savetofile = TRUE,
                             filename = paste(type[2,j],"_CARGOTYPES_",month[1,i],".csv",sep=""))
Top_Warehouses <- Warehouses(Top_ShipData$SenderZip,Top_SaleData,Top_ShipData,
                             savetofile = TRUE,filename = paste(type[2,j],"_WAREHOUSES_",month[1,i],".csv",sep=""))

require(plotly)
mydf <- read.csv("MP_WAREHOUSES_NOV.csv",row.names = NULL)

p <- plot_ly(mydf,
             x=Place.Name,
             y=Uses,
             z=Units,
             type = "scatter3d",
             mode = "markers",
             showlegend=TRUE,
             color = Ave.Cost
             # size = (Ave.Duration/10)
             ) %>%
  layout(title="Most Profitable Item Suppliers November")
p

htmlwidgets::saveWidget(as.widget(p),"MP_November_Suppliers.html")

# following block of code is for obtaining shipping data and statistics for month 10 (october)


Ship_Filtered <- Ship_Filtered[which(Ship_Filtered$Type %in% mylev[1:7,1]),] # filter by type
Ship_Atment <- cbind(Ship_Filtered,LocationData(Ship_Filtered)) # this takes time

# Ship_Filtered <- Ship_Filtered[-which(Ship_Filtered$Type %in% mylev[15:27,1]),] # filter by type
Ship_Atment <- read.csv("Ship_Filtered.csv",row.names=NULL) # read previously written data
Ship_Filtered <- cbind(Ship_Filtered,Ship_Atment)

# ShippingData_Months_10to12.txt file is filtered to following smaller version. Content of the file
# includes unwanted costful deliveries
Ship_Filtered_Complete <- read.csv("ShippingData_Filtered_Complete.csv",row.names=NULL) # read previously written data

mylev$Distance <- sapply(mylev$Type,function(x) mean(na.omit(Ship_Filtered[which(Ship_Filtered$Type == x),20])))

##### FILTER SHIPPING DATA EXCLUDE UNWANTED #####
require(lubridate)
Ship_Filtered <- Ship[which(month(Ship$DateDelivered)==11),] # filter by date
Ship_Filtered <- Ship_Filtered[-which(Ship_Filtered$ShippingCost==0),] # filter by cost
Ship_Filtered <- Ship_Filtered[-which(Ship_Filtered$Type == "UPS Ground"),] # filter by type

mylev <- as.data.frame(levels(factor(Ship_Filtered$Type)))
mylev$AveCost <- sapply(mylev[,1],function(x) mean(na.omit(Ship_Filtered[which(Ship_Filtered$Type==x),7])))
mylev$TotalCost <- sapply(mylev[,1],function(x) (na.omit(Ship_Filtered[which(Ship_Filtered$Type==x),7])))
mylev$AveDuration <- sapply(mylev[,1],function(x) mean(na.omit(Ship_Filtered[which(Ship_Filtered$Type==x),11])))
mylev$Uses <- sapply(mylev[,1],function(x) length(which(Ship_Filtered$Type==x)))
mylev <- Sort(mylev,2,decreasing = TRUE)
mylev
# print(mylev)
# n <- readline(prompt="At above list, to which row you want to select from beginning? Enter an integer : ")
Ship_Filtered <- Ship_Filtered[which(Ship_Filtered$Type %in% mylev[1:7,1]),] # filter by type
# }
mylev$Index <- mylev$Distance/(mylev$AveCost*mylev$AveDuration)

Ship.Filtered <- Ship.Filtered[which(Ship.Filtered$Type %in% mylev[1:8,1]),] # filter by type

mylev <- as.data.frame(levels(factor(Ship$Type)))
mylev$AveCost <- sapply(mylev[,1],function(x) mean(na.omit(Ship[which(Ship$Type==x),7])))
mylev$TotalCost <- sapply(mylev[,1],function(x) sum(na.omit(Ship[which(Ship$Type==x),7])))
mylev$AveDuration <- sapply(mylev[,1],function(x) mean(na.omit(Ship[which(Ship$Type==x),11])))
mylev$Uses <- sapply(mylev[,1],function(x) length(which(Ship$Type==x)))
mylev <- Sort(mylev,2,decreasing = TRUE)
mylev

Ship.Filtered <- Ship[-which(Ship$ShippingCost==0),] # filter by cost
Ship.Filtered <- Ship.Filtered[-which(Ship.Filtered$Type == "UPS Ground"),] # filter by type

levels.ship <- function(data,col.type=6,col.cost=7,col.duration=11,distances=FALSE){
  Result <- as.data.frame(levels(factor(data[,col.type])))
  Result$AveCost <- sapply(Result[,1],function(x) mean(na.omit(data[which(data[,col.type]==x),col.cost])))
  Result$TotalCost <- sapply(Result[,1],function(x) sum(na.omit(data[which(data[,col.type]==x),col.cost])))
  Result$AveDuration <- sapply(Result[,1],function(x) mean(na.omit(data[which(data[,col.type]==x),col.duration])))
  Result$Uses <- sapply(Result[,1],function(x) length(which(data$Type==x)))
  if(distances){
    Result$Distance <- sapply(Result[,1],function(x) mean(na.omit(data[which(data[,col.type] == x),20])))
    Result$Index <- Result$Distance/(Result$AveCost*Result$AveDuration)
    colnames(Result) <- c("Type","AveCost","TotalCost","AveDuration","Uses","Distance","Index")
  }
  else{
    colnames(Result) <- c("Type","AveCost","TotalCost","AveDuration","Uses") 
  }
  Result <- Sort(Result,2,decreasing = TRUE)
  return(Result)
}

levs <- levels.ship(Ship_Filtered_Complete,distances = TRUE)
MultiGGPlot(levs[,-1],levs,geom = "path",main = "Unwanted Deliveries of Last Quarter 2012",fname = "Line_Filtered",colour = levs$AveCost,
            colourname = "Average Cost",alpha = 1,device = "pdf")
# to delete
MultiGGPlot(levs[,-1],levs[,c(1,2)],main = "Unwanted Deliveries of Last Quarter 2012",fname = "ShippingTypes",colour = levs$AveCost,
            colourname = "Average\nCost",alpha = 1,device = "pdf")

# levs <- levels.ship(Ship)
# MultiGGPlot(levs,levs,main = "Total Deliveries of Last Quarter 2012",fname = "Original_",colour = levs$AveCost,
#             alpha = 1,device = "pdf")

# Ship.Filtered <- cbind(Ship.Filtered,LocationData(Ship.Filtered))
# write.csv(Ship.Filtered,"ShippingData_Filtered_Complete.csv",row.names=FALSE)


require(gridExtra)
p1 <- ggplot(data=levs,aes(x=Type,y=TotalCost,colour=levs$AveCost))+
  geom_jitter(alpha = 1)+
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(title = "Test1",
       x = "Type",
       y = "Total Cost",
       colour = "Average Cost")

p2 <- ggplot(data=levs,aes(x=1:nrow(levs),y=Distance,colour=levs$AveCost))+
  geom_line(alpha = 1)+
  labs(title = "Test2",
       x = "Number of Events",
       y = "Distance",
       colour = "Average Cost")

p1
p2

p3 <- grid.arrange(p1,p2,p1,p2,nrow=2,ncol=2) # ,width=6,height = 3.375
p3
ggsave("test_grid.pdf",plot = p3,device = "pdf",width=6,height = 3.375)

############ test ########
require(gridExtra)
p1 <- ggplot(data=matched_m10_summary,aes(x=Product,y=TotalShippingCost,colour=matched_m10_summary$AveShippingCost))+
  geom_jitter(alpha = 1)+
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(title = "Test1",
       x = "Product",
       y = "Total Cost",
       colour = "Average Cost")
p1

p2 <- ggplot(data=matched_m10_summary,aes(x=Product,y=Distance,colour=levs$AveCost))+
  geom_line(alpha = 1)+
  labs(title = "Test2",
       x = "Number of Events",
       y = "Distance",
       colour = "Average Cost")
p3 <- ggplot(data=matched_m10_summary,aes(x=Product,y=TotalCost,colour=levs$AveCost))+
  geom_jitter(alpha = 1)+
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(title = "Test1",
       x = "Type",
       y = "Total Cost",
       colour = "Average Cost")

p4 <- ggplot(data=matched_m10_summary,aes(x=Product,y=Distance,colour=levs$AveCost))+
  geom_line(alpha = 1)+
  labs(title = "Test2",
       x = "Number of Events",
       y = "Distance",
       colour = "Average Cost")


p1
p2
p3
p4

P <- grid.arrange(p1,p2,p1,p2,nrow=2,ncol=2) # ,width=6,height = 3.375
P
ggsave("test_grid.pdf",plot = P,device = "pdf",width=6,height = 3.375)

# Ship_Filtered_Complete <- read.csv("ShippingData_Filtered_Complete.csv",row.names = NULL)
require(lubridate)
# temp_ship_m10 <- levels(factor(Sort(Ship_Filtered_Complete[which(month(Ship_Filtered_Complete$DateDelivered)==10),],7,decreasing = TRUE)[1:1000,8]))
temp_ship_m10 <- Sort(Ship_Filtered_Complete[which(month(Ship_Filtered_Complete$DateDelivered)==10),],7,decreasing = TRUE)[1:100,]
system.time({
temp_raw_m10 <- Search.List(temp_ship_m10[,8],Raw_M10,1)[,-1]
})

Match.rows <- function(source,col.sou,target,col.tar){
  Result <- data.frame() # initialize a data frame to fill in later
  for(i in 1:length(source[,col.sou])){
    index <- which(target[,col.tar]==source[i,col.sou]) # search the source in target, get indexes
    len <- length(index)
    if(len!=0){
      for(j in 1:len)
        Result <- rbind(Result,cbind(source[i,],target[index[[j]],]))
    }
    else if(len==0){
      temp <- matrix(NA,1,ncol(target))
      colnames(temp) <- colnames(target)
      Result <- rbind(Result,cbind(source[i,],temp))
    }
  }
  return(Result)
}

test_match_2 <- Match.rows(temp_ship_m10,8,temp_raw_m10,1)
check_sen <- test_match_2[,4]==test_match_2[,23] & test_match_2[,5]==test_match_2[,24]
test_match_2 <- test_match_2[check_sen,]
# backup_test_match_2 <- test_match_2

######################################

bestsellers <- read.csv("BS_ProductList_OCT.csv",row.names = NULL)
bestsellers <- read.csv("Top1000_M10.csv",row.names = NULL) # top 1000 bestseller item
bestsellers <- read.csv("Most_Expensive_Matched_M10.csv",row.names = NULL) # educational

require(plotly)
m <- list(
  # l = 50,
  # r = 50,
  b = 200
  # t = 100,
  # pad = 4
)

l <- list(x = 1,y = -0.09)
graph <- plot_ly(data = bestsellers,x = Name,y = Quantity,name = "Sold Quantity",color = Profit,showlegend = TRUE,mode = "lines+markers") %>%
  layout(title = "Bestseller Products in October",margin = m,yaxis=list(type="log")) %>%
  add_trace(x = Name,y = Orders,name = "Orders",mode = "lines") %>%
  layout(legend = l)
  # add_trace(y = Quantity, name = 'Quantity', mode = 'lines+markers')
graph

htmlwidgets::saveWidget(as.widget(graph),"Oct_BS_1000_Quantity_&_Orders_vs_Product.html")
# htmlwidgets::saveWidget(as.widget(graph),"Oct_OutCost_1000_Quantity_&_Orders_vs_Product.html")


mostpro <- read.csv("MP_ProductList_OCT.csv",row.names = NULL)

matched_big <- read.csv("Most_Expensive_Matched_M10_10000.csv",row.names = NULL)

# here, list the product names which cause outcost
# outcost <- sapply(levels(factor(matched_big[1:100,25])),function(x) 
#   cbind(sum(na.omit(matched_big[which(matched_big[,25]==x),7])),
#         length(which(matched_big[,25]==x)),
#         match(x,matched_big[,25])))

####---KEEP FOLLOWING RESULT SAFE---####
# following result is the outcost of each item among the most expensive 10000 deliveries through October
lvl <- levels(factor(matched_big[,25])) # product name levels of the most expensive 100 deliveries in October
output <- as.data.frame(matrix(NA,length(lvl),5))
colnames(output) <- c("Product","TotalShippingCost","TimesShipped","Price","Weight")
for(i in 1:length(lvl)){
  output[i,] <- cbind(lvl[i],
                      as.numeric(sum(na.omit(matched_big[which(matched_big[,25]==lvl[i]),7]))),
                      as.numeric(length(which(matched_big[,25]==lvl[i]))),
                      matched_big[match(lvl[i],matched_big[,25]),28],
                      matched_big[match(lvl[i],matched_big[,25]),26])
}
class(output$TotalShippingCost) <- "numeric"
class(output$AveShippingCost) <- "numeric"
class(output$TimesShipped) <- "numeric"
class(output$Price) <- "numeric"
class(output$Weight) <- "numeric"
output <- Sort(output,2,decreasing = TRUE)

require(plotly)
m <- list(
  # l = 50,
  # r = 50,
  b = 200
  # t = 100,
  # pad = 4
)

l <- list(x = 1,y = -0.09)
### some corrections
class(output$TimesShipped) <- "numeric"
output$TimesShipped <- as.numeric(output$TimesShipped)
class(output$Price) <- "numeric"
class(output$Weight) <- "numeric"
max <- max(output$TimesShipped)
###
graph <- plot_ly(data = output,x = Product,y = TotalShippingCost,name = "Total Shipping Cost",color = Weight,showlegend = TRUE,mode = "lines+markers") %>%
  add_trace(x = Product,y = Price,name = "Price",mode = "lines+markes") %>%
  # add_trace(x = Product,y = TimesShipped,name = "TimesShipped",mode = "lines+markes") %>%
  layout(title = "The Most Expensive Shipped Products in October",margin = m,legend = l)

graph

# htmlwidgets::saveWidget(as.widget(graph),"Oct_BS_1000_Quantity_&_Orders_vs_Product.html")
htmlwidgets::saveWidget(as.widget(graph),"Oct_OutCost_10000_Cost_&_Price_vs_Product.html")

Sys.setenv("plotly_username"="mcandar")
Sys.setenv("plotly_api_key"="ekbij5ww3d")
plotly_POST(graph, filename = "filtered_product_outcost")

exp <- read.table("kitap1.txt",sep = ",",header = T)
colnames(exp) <- c("Sections","NumberofMarbles")

library("plotly")
library("broom")
m <- loess(NumberofMarbles ~ Sections, data = exp)
exp_plot <- plot_ly(data = exp,x = Sections,y = NumberofMarbles) %>%
  # add_markers(, text = rownames(exp), showlegend = FALSE) %>%
  add_trace(y = ~fitted(loess(NumberofMarbles ~ Sections)),
            line = list(color = 'rgba(7, 164, 181, 1)'),
            name = "Loess Smoother") %>%
  layout(title = "Maxwellian Velocity Distribution")

exp_plot
htmlwidgets::saveWidget(as.widget(exp_plot),"MaxwellianGraph.html")

ggplot(exp, 
       aes(x = Sections,
           y = NumberofMarbles,
           colour = "blue")) +
  
  geom_jitter(alpha = 1)+
  
  labs(title = "Maxwellian Velocity Distribution") +
  
  geom_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 4, raw=TRUE),colour="blue")+
  
  geom_smooth(method="loess", se=TRUE, fill=NA,colour="red")+

ggsave("MaxwellianGraph.pdf",device = "pdf",width = 6,height = 3.375)


# to delete
source("https://raw.githubusercontent.com/mcandar/Agents/master/appr.R")
Ship_1012 <- Format.ShippingData(read.csv("ShippingData_Months_10to12.txt",sep=",",colClasses = "character"))
write.csv("ShippingData_Months_10to12(Arranged).csv",row.names=FALSE)
levels_1012 <- levels.ship(Ship_1010)
write.csv("Types_1012.csv",row.names=FALSE)

# data <- data.frame()
# for(i in 1:10){
#   data <- rbind(data,read.csv(paste("Most_Expensive_Matched_M7_test_",i,".csv",sep = ""),row.names = NULL))
# }
# write.csv(data,"Most_Expensive_Matched_M7.csv",row.names = FALSE)
# write.csv(unique(data),"final_matched_m7.csv",row.names = FALSE)


multigg_test <- function(x,                    # data of x axis
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
  
  all_grobs <- NA
  
  for(j in 1:ncol(x)){ # 1 through total column number (originally 1:dim(x)[2])
    for(i in 1:ncol(data)){
      if (identical(x[,j],data[,i])) next()
      filename <- paste(fname,"_",i+((j-1)*ncol(data)),"_",colnames(data[i]),"_vs_",colnames(x[j]),".",device,sep = "") # form a file name with an index
      plotname <- paste(main," #",i,sep = "")
      current_grob <- ggplot(data, 
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
      
      print("I start to combine grobs.")
      all_grobs <- list(all_grobs,current_grob)
    }
  }
  filename <- "test_multipage.pdf"
  require(gridExtra)
  cat("Class of grob list is:",class(all_grobs),"\n")
  cat("Length of grob list is:",length(all_grobs),"\n")
  print("I start to arrange grobs")
  # mp_grobs <- marrangeGrob(all_grobs,ncol = 2,nrow = 2) # an error right here
  print("I start to save grobs")
  # ggsave(filename,mp_grobs)#,device = device,width = width,height = height)
  # cat("Image",filename,"saved to",getwd(),"\n")
  return (TRUE)
}

multigg_test(x = 1:100,data = summary_outcost[1:100,4:5],main = "gg_multipage_test",
             fname = "multipage_test",device = "pdf")

my_grobs

set.seed(123)
pl <- lapply(1:11, function(.x) 
  qplot(1:10, rnorm(10), main=paste("plot", .x)))
ml <- marrangeGrob(pl, nrow=2, ncol=2)
## non-interactive use, multipage pdf
ggsave("multipage.pdf", ml) #- compatible with ggsave
## interactive use; calling `dev.new` multiple times
ml

class(pl)
class(all_grobs)

current_grob <- ggplot(data = summary_outcost,aes(x = TotalShippingCost,y = AveShippingCost,colour = 1)) +
  geom_jitter()
current_grob
gg <- ggplotly(current_grob)


###### TEST SAVING MULTIPLE PAGE GRAPHS ON PLOTLY #######

library(plotly)


### a save function for multpile graphs ###
save_tags <- function (tags, file, selfcontained = F, libdir = "./lib") 
{
  if (is.null(libdir)) {
    libdir <- paste(tools::file_path_sans_ext(basename(file)), 
                    "_files", sep = "")
  }
  htmltools::save_html(tags, file = file, libdir = libdir)
  if (selfcontained) {
    if (!htmlwidgets:::pandoc_available()) {
      stop("Saving a widget with selfcontained = TRUE requires pandoc. For details see:\n", 
           "https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md")
    }
    htmlwidgets:::pandoc_self_contained_html(file, file)
    unlink(libdir, recursive = TRUE)
  }
  return(htmltools::tags$iframe(src= file, height = "400px", width = "100%", style="border:0;"))
}
#########################################################

##### THIS FUNCTION WORKSS ##### !!!!!!!!!!!!!!!! KEEP IT SAFE!!!1
save_tags(list(plt1,plt2),"myfile.html") # save multiple plotly objects in a graph like a list, and includes slider if needed
##############################


### SUBPLOTS OF CHOROPLETH MAP USA, sample
library(plotly)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  lakecolor = toRGB('white')
)
# create a map of population density
density <- state.x77[, "Population"] / state.x77[, "Area"]
map <- plot_geo(z = ~density, text = state.name, 
                locations = state.abb, locationmode = 'USA-states') %>%
  layout(geo = g)
# create a bunch of horizontal bar charts 
vars <- colnames(state.x77)
barcharts <- lapply(vars, function(var) {
  plot_ly(x = state.x77[, var], y = state.name) %>%
    add_bars(orientation = "h", name = var) %>%
    layout(showlegend = FALSE, hovermode = "y",
           yaxis = list(showticklabels = FALSE))
})
subplot(
  subplot(barcharts, margin = 0.01), map, 
  nrows = 2, heights = c(0.3, 0.7), margin = 0.1
)

### SUBPLOTS OF CHOROPLETH MAP USA for month 7 A DETAILED VERSION !!!
library(plotly)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  lakecolor = toRGB('white')
)
# create a map of population density
# density <- state.x77[, "Population"] / state.x77[, "Area"]
state_dist <- ByState.Summary(final_matched[which(month(final_matched$DateShipped)==7),]) # import data
map <- plot_geo(data = state_dist, z = ~TotalShippingCost, text = ~StateCode, 
                locations = ~StateCode, locationmode = 'USA-states') %>%
  layout(geo = g)
map
# create a bunch of horizontal bar charts 
vars <- colnames(state_dist)
barcharts <- lapply(vars, function(var) {
  plot_ly(x = state_dist[, var], y = state_dist$StateCode) %>%
    add_bars(orientation = "h", name = var) %>%
    layout(showlegend = FALSE, hovermode = "y",
           yaxis = list(showticklabels = FALSE))
})

temp <- subplot(barcharts, margin = 0.01)
sub_p <- subplot(temp, map,nrows = 2, heights = c(0.3, 0.7), margin = 0.1)

htmlwidgets::saveWidget(as.widget(sub_p),"m7_choropleth_with_subplots.html")
