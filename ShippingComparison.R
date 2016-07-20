# Following code visually compares two files ShippingData_Months07to09.txt and ShippingData_Months10to12.txt

# DetecTime function is to convert and arrange time column from 
# characters to catch any exception and for a more reliable analysis
DetectTime <- function(data,col,format="%m/%d/%Y %H:%M:%S"){ 
  Result <- data # back up the data
  Result[[col]] <- strptime(data[[col]],format=format) # convert date and time from characters
  return(Result)
}

# MultiPlot function helps visualing a given data by graphing all variables against a selected variable
MultiParPlot <- function(#x,                    # data of x axis
                         data1,                # first data set of y axis
                         data2,                # second data set of y axis
                         mfrow=c(2,1),         # positioning of graphs
                         excludecols = c(0),   # index of unwanted columns in graph  
                         labelx = "Index",     # x label
                         name = "Plot_",       # name of the file which graph will be saved
                         width = 1920,         # width as pixels
                         height = 1080){       # height as pixels
  
  if (ncol(data1) > ncol(data2)) cols <- ncol(data2) # if not equal, determine the number of iterations according
  else cols <- ncol(data1)                           # to smaller one with respect to number of columns
  # 
  # if (nrow(data1) > nrow(data2)) rows <- nrow(data2) # if not equal, trim the x axis according to smaller value
  # else rows <- nrow(data1)
  # 
  # # trim out of bounds
  # if (all(class(x) == "integer")) x <- 1:rows # if a sequence type of integer
  # else if (any(class(x) == "POSIXlt")){       # if a time series
  #   temp <- x[1:rows]
  #   remove(x)
  #   x <- temp
  # }
  
  plotname <- matrix(NA,cols,2)
  
  for(i in 1:cols){
    filename <- paste(name,"_",i,".png",sep = "")
    plotname[i,1] <- paste("Months From 07 to 09 Column ",i,sep = "")
    plotname[i,2] <- paste("Months From 10 to 12 Column ",i,sep = "")
    if (any(excludecols == i)) next() # pass to next step if excluded columns encountered
    png(filename,width = width,height = height)
    
    par(mfrow=c(2,1))
    plot(1:nrow(data1),data1[,i],xlab = labelx,ylab = colnames(data1[i]),main = plotname[i,1])
    plot(1:nrow(data2),data2[,i],xlab = labelx,ylab = colnames(data2[i]),main = plotname[i,2])
      
    dev.off()
  }
  return (TRUE)
}

# main
Shipping1 <- DetectTime(DetectTime(read.table("ShippingData_Months_07to09.txt",sep = ","),9),10)
Shipping2 <- DetectTime(DetectTime(read.table("ShippingData_Months_10to12.txt",sep = ","),9),10)

colnames(Shipping1) <- c("07to09_Col1","07to09_Col2","07to09_Col3","07to09_Col4","07to09_Col5","07to09_Col6",
                         "07to09_Col7","07to09_Col8","07to09_Col9","07to09_Col10","07to09_Col11")
colnames(Shipping2) <- c("10to12_Col1","10to12_Col2","10to12_Col3","10to12_Col4","10to12_Col5","10to12_Col6",
                         "10to12_Col7","10to12_Col8","10to12_Col9","10to12_Col10","10to12_Col11")

## Visualize ##
MultiParPlot(Shipping1,Shipping2,excludecols = c(1,2,6),name = "ShippingComparison")
# MultiParPlot(Shipping1[,9],Shipping1,Shipping2,excludecols = c(1,2,6,9),name = "Shipping07to09Time#1",
#              labelx = "07 to 09 Time, Column 9")
# MultiParPlot(Shipping1[,10],Shipping1,Shipping2,excludecols = c(1,2,6,10),name = "Shipping07to09Time#2",
#              labelx = "07 to 09 Time, Column 10")
# MultiParPlot(Shipping2[,9],Shipping1,Shipping2,excludecols = c(1,2,6,9),name = "Shipping10to12Time#1",
#              labelx = "10 to 12 Time, Column 9")
# MultiParPlot(Shipping2[,10],Shipping1,Shipping2,excludecols = c(1,2,6,10),name = "Shipping10to12Time#2",
#              labelx = "10 to 12 Time, Column 10")
